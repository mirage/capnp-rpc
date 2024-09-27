open Eio.Std
open Capnp_rpc_lwt

module Api = Calculator.MakeRPC(Capnp_rpc_lwt)

type calc = [`Calculator_97983392df35cc36]
type value = [`Value_c3e69d34d3ee48d2]
type fn = [`Function_ede83a3d96840394]

let or_fail msg = function
  | Some x -> x
  | None -> failwith msg

module Expr = struct
  type t =
    | Float of float
    | Prev of Api.Reader.Calculator.Value.t Capability.t
    | Param of int
    | Call of Api.Reader.Calculator.Function.t Capability.t * t list

  let rec parse r =
    let open Api.Reader.Calculator in
    match Expression.get r with
    | Expression.Literal f -> Float f
    | Expression.PreviousResult None -> failwith "PreviousResult but no cap!"
    | Expression.PreviousResult (Some v) -> Prev v
    | Expression.Parameter p -> Param (Stdint.Uint32.to_int p)
    | Expression.Call c ->
      let fn_obj = Expression.Call.function_get c |> or_fail "Missing fn" in
      let params = Expression.Call.params_get_list c |> List.map parse in
      Call (fn_obj, params)
    | Expression.Undefined _ ->
      failwith "Unknown expression type"

  let rec release = function
    | Float _ -> ()
    | Prev v -> Capability.dec_ref v
    | Param _ -> ()
    | Call (f, params) ->
      Capability.dec_ref f;
      List.iter release params
end

(* Write an expression into a message, exporting function capabilities. *)
let rec write_expr b expr =
  let open Api.Builder.Calculator in
  let open Expr in
  match expr with
  | Float f -> Expression.literal_set b f
  | Prev v -> Expression.previous_result_set b (Some v)
  | Param i -> Expression.parameter_set b (Stdint.Uint32.of_int i)
  | Call (f, args) ->
    let c = Expression.call_init b in
    Expression.Call.function_set c (Some f);
    let args_b = Expression.Call.params_init c (List.length args) in
    args |> List.iteri (fun i arg ->
        write_expr (Capnp.Array.get args_b i) arg
      )

module C = Api.Client.Calculator

type t = calc Capability.t

let evaluate t expr =
  let open C.Evaluate in
  let req, p = Capability.Request.create Params.init_pointer in
  write_expr (Params.expression_init p) expr;
  Capability.call_for_caps t method_id req Results.value_get_pipelined

let getOperator t op =
  let open C.GetOperator in
  let module O = Api.Builder.Calculator.Operator in
  let req, p = Capability.Request.create ~message_size:200 Params.init_pointer in
  Params.op_set p (match op with
      | `Add -> O.Add
      | `Subtract -> O.Subtract
      | `Multiply -> O.Multiply
      | `Divide -> O.Divide
    );
  Capability.call_for_caps t method_id req Results.func_get_pipelined

module Value = struct
  type t = value Capability.t

  let read v =
    let open Api.Client.Calculator.Value.Read in
    let req = Capability.Request.create_no_args () in
    Capability.call_for_value_exn v method_id req |> Results.value_get

  let final_read v =
    let result = read v in
    Capability.dec_ref v;
    result

  let local f =
    let module Value = Api.Service.Calculator.Value in
    Value.local @@ object
      inherit Value.service

      val id = Capnp_rpc.Debug.OID.next ()

      method! pp fmt = Fmt.pf fmt "Literal(%a) = %f" Capnp_rpc.Debug.OID.pp id f

      method read_impl _ release_params =
        let open Value.Read in
        release_params ();
        let resp, c = Service.Response.create ~message_size:200 Results.init_pointer in
        Results.value_set c f;
        Service.return resp
    end
end

let call_fn fn args =
  let open Api.Client.Calculator.Function.Call in
  let req, p = Capability.Request.create Params.init_pointer in
  ignore (Params.params_set_list p args);
  Capability.call_for_value_exn fn method_id req |> Results.value_get

let pp_result_promise f x =
  match Promise.peek x with
  | Some (Ok v) -> Fmt.float f v
  | Some (Error ex) -> Fmt.exn f ex
  | None -> Fmt.string f "(still calculating)"

(* Evaluate an expression, where some sub-expressions may require remote calls.
   Immediately returns a service for the result, while the calculation continues in [sw]. *)
let rec eval ~sw ?(args=[||]) : _ -> Api.Reader.Calculator.Value.t Capability.t =
  let open Expr in function
  | Float f -> Value.local f
  | Prev v -> Capability.inc_ref v; v
  | Param p -> Value.local args.(p)
  | Call (f, params) ->
    let result = Fiber.fork_promise ~sw (fun () ->
        params
        |> Fiber.List.map (fun p ->
            let value = eval ~sw ~args p in
            Value.final_read value
          )
        |> call_fn f
      )
    in
    let open Api.Service.Calculator in
    Value.local @@ object
      inherit Value.service

      val id = Capnp_rpc.Debug.OID.next ()

      method! pp f =
        Fmt.pf f "EvalResultValue(%a) = %a" Capnp_rpc.Debug.OID.pp id pp_result_promise result

      method read_impl _ release_params =
        let open Value.Read in
        release_params ();
        let result = Promise.await_exn result in
        let resp, c = Service.Response.create Results.init_pointer in
        Results.value_set c result;
        Service.return resp
    end

module Fn = struct
  type t = fn Capability.t

  let call = call_fn

  let local n_args body =
    let module Function = Api.Service.Calculator.Function in
    Function.local @@ object
      inherit Function.service

      method call_impl params release_params =
        let open Function.Call in
        let args = Params.params_get_array params in
        assert (Array.length args = n_args);
        (* Functions return floats, not Value objects, so we have to wait here. *)
        Switch.run @@ fun sw ->
        let value = eval ~sw ~args body in
        release_params ();
        let value = Value.final_read value in
        let resp, r = Service.Response.create ~message_size:200 Results.init_pointer in
        Results.value_set r value;
        Service.return resp
    end

  let local_binop op : Api.Builder.Calculator.Function.t Capability.t =
    let module Function = Api.Service.Calculator.Function in
    Function.local @@ object
      inherit Function.service

      method call_impl params release_params =
        release_params ();
        let open Function.Call in
        match Params.params_get_array params with
        | [| a; b |] ->
          let resp, r = Service.Response.create Results.init_pointer in
          let ans = op a b in
          Logs.info (fun f -> f "%f op %f -> %f" a b ans);
          Results.value_set r ans;
          Service.return resp
        | _ -> failwith "Wrong number of args"
    end

  let add = local_binop ( +. ) 
  let sub = local_binop ( -. )
  let mul = local_binop ( *. )
  let div = local_binop ( /. )
end

(* The main calculator service *)
let local ~sw =
  let module Calculator = Api.Service.Calculator in
  Calculator.local @@ object
    inherit Calculator.service

    method def_function_impl params release_params =
      let open Calculator.DefFunction in
      let n_args = Params.param_count_get_int_exn params in
      let body = Params.body_get params |> Expr.parse in
      let fn_obj = Fn.local n_args body in
      Expr.release body;
      release_params ();
      let resp, results = Service.Response.create ~message_size:200 Results.init_pointer in
      Results.func_set results (Some fn_obj);
      Service.return resp

    method evaluate_impl params release_params =
      let open Calculator.Evaluate in
      let expr = Expr.parse (Params.expression_get params) in
      release_params ();
      let value_obj = eval ~sw expr in
      Expr.release expr;
      let resp, results = Service.Response.create ~message_size:200 Results.init_pointer in
      Results.value_set results (Some value_obj);
      Capability.dec_ref value_obj;
      Service.return resp

    method get_operator_impl params release_params =
      release_params ();
      let open Calculator.GetOperator in
      let module O = Api.Reader.Calculator.Operator in
      let op_obj =
        match Params.op_get params with
        | O.Add         -> Fn.add
        | O.Subtract    -> Fn.sub
        | O.Multiply    -> Fn.mul
        | O.Divide      -> Fn.div
        | O.Undefined _ -> failwith "Unknown operator"
      in
      let resp, results = Service.Response.create ~message_size:200 Results.init_pointer in
      Results.func_set results (Some op_obj);
      Service.return resp
  end
