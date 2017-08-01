(* This is the OCaml version of the C++ capnp calculator example. *)

open Lwt.Infix
open Capnp_rpc_lwt

module Api = Calculator.MakeRPC(Capnp.BytesMessage)(Capnp_rpc_lwt)

type expr =
  | Float of float
  | Prev of Api.Reader.Calculator.Value.t Capability.t
  | Param of int
  | Call of Api.Reader.Calculator.Function.t Capability.t * expr list

let or_fail msg = function
  | Some x -> x
  | None -> failwith msg

(* Write an expression into a message, exporting function capabilities. *)
let rec write_expr b expr =
  let open Api.Builder.Calculator in
  match expr with
  | Float f -> Expression.literal_set b f
  | Prev v -> Expression.previous_result_set b (Some v)
  | Param i -> Expression.parameter_set b (Uint32.of_int i)
  | Call (f, args) ->
    let c = Expression.call_init b in
    Expression.Call.function_set c (Some f);
    let args_b = Expression.Call.params_init c (List.length args) in
    args |> List.iteri (fun i arg ->
        write_expr (Capnp.Array.get args_b i) arg
      )

(* A more user-friendly API for the Calculator interface *)
module Client = struct
  let evaluate t expr =
    let module P = Api.Builder.Calculator.Evaluate_params in
    let module R = Api.Reader.Calculator.Evaluate_results in
    let req, p = Capability.Request.create P.init_pointer in
    write_expr (P.expression_init p) expr;
    Capability.call_for_caps t Api.Reader.Calculator.evaluate_method req R.value_get_pipelined

  let getOperator t op =
    let module P = Api.Builder.Calculator.GetOperator_params in
    let module R = Api.Reader.Calculator.GetOperator_results in
    let module O = Api.Builder.Calculator.Operator in
    let req, p = Capability.Request.create P.init_pointer in
    P.op_set p (match op with
        | `Add -> O.Add
        | `Subtract -> O.Subtract
        | `Multiply -> O.Multiply
        | `Divide -> O.Divide
      );
    Capability.call_for_caps t Api.Reader.Calculator.get_operator_method req R.func_get_pipelined

  let read v =
    let module P = Api.Reader.Calculator.Value.Read_params in
    let module R = Api.Reader.Calculator.Value.Read_results in
    let req = Capability.Request.create_no_args () in
    Capability.call_for_value_exn v Api.Reader.Calculator.Value.read_method req >|= fun resp ->
    R.of_payload resp |> R.value_get

  let final_read v =
    read v >|= fun result ->
    Capability.dec_ref v;
    result

  let call fn args =
    let module P = Api.Builder.Calculator.Function.Call_params in
    let module R = Api.Reader.Calculator.Function.Call_results in
    let req, p = Capability.Request.create P.init_pointer in
    ignore (P.params_set_list p args);
    Capability.call_for_value_exn fn Api.Reader.Calculator.Function.call_method req >|= fun resp ->
    R.of_payload resp |> R.value_get
end

(* Export a literal float as a service with a [read] method. *)
let literal f =
  let open Api.Builder.Calculator in
  Value.local @@ object
    inherit Value.service

    val id = Capnp_rpc.Debug.OID.next ()

    method! pp fmt = Fmt.pf fmt "Literal(%a) = %f" Capnp_rpc.Debug.OID.pp id f

    method read_impl _ =
      let resp, c = Service.Response.create Value.Read_results.init_pointer in
      Value.Read_results.value_set c f;
      Service.return resp
  end

let pp_result_lwt f x =
  match Lwt.state x with
  | Lwt.Return v -> Fmt.float f v
  | Lwt.Fail ex -> Fmt.exn f ex
  | Lwt.Sleep -> Fmt.string f "(still calculating)"

(* Evaluate an expression, where some sub-expressions may require remote calls. *)
let rec eval ?(args=[||]) : _ -> Api.Reader.Calculator.Value.t Capability.t = function
  | Float f -> literal f
  | Prev v -> v
  | Param p -> literal args.(p)
  | Call (f, params) ->
    let params = params |> Lwt_list.map_p (fun p ->
        let value = eval ~args p in
        Client.final_read value
      ) in
    let result = params >>= Client.call f in
    let open Api.Builder.Calculator in
    Value.local @@ object
      inherit Value.service

      val id = Capnp_rpc.Debug.OID.next ()

      method! pp f =
        Fmt.pf f "EvalResultValue(%a) = %a" Capnp_rpc.Debug.OID.pp id pp_result_lwt result

      method read_impl _ =
        Service.return_lwt (fun () ->
            result >|= fun result ->
            let resp, c = Service.Response.create Value.Read_results.init_pointer in
            Value.Read_results.value_set c result;
            Ok resp
          )
    end

let rec release_expr = function
  | Float _ -> ()
  | Prev v -> Capability.dec_ref v
  | Param _ -> ()
  | Call (f, params) ->
    Capability.dec_ref f;
    List.iter release_expr params

(* A local service that provides the function [body]. *)
let fn n_args body =
  let open Api.Builder.Calculator in
  Function.local @@ object
    inherit Function.service

    method call_impl req =
      let module P = Api.Reader.Calculator.Function.Call_params in
      let module R = Api.Builder.Calculator.Function.Call_results in
      let params = P.of_payload req in
      let args = P.params_get_array params in
      assert (Array.length args = n_args);
      let value = eval ~args body in
      (* Functions return floats, not Value objects, so we have to wait here. *)
      Service.return_lwt (fun () ->
          Client.final_read value >|= fun value ->
          let resp, r = Service.Response.create R.init_pointer in
          R.value_set r value;
          Ok resp
        )
  end

(* A local service exporting function [op] (of two arguments). *)
let binop op : Api.Builder.Calculator.Function.t Capability.t =
  let open Api.Builder.Calculator in
  Function.local @@ object
    inherit Function.service

    method call_impl req =
      let module P = Api.Reader.Calculator.Function.Call_params in
      let module R = Api.Builder.Calculator.Function.Call_results in
      let params = P.of_payload req in
      match P.params_get_array params with
      | [| a; b |] ->
        let resp, r = Service.Response.create R.init_pointer in
        let ans = op a b in
        Logs.info (fun f -> f "%f op %f -> %f" a b ans);
        R.value_set r ans;
        Service.return resp
      | _ -> failwith "Wrong number of args"
  end

let add = binop ( +. ) 
let sub = binop ( -. )
let mul = binop ( *. )
let div = binop ( /. )

let rec parse_expr r =
  let open Api.Reader.Calculator in
  match Expression.get r with
  | Expression.Literal f -> Float f
  | Expression.PreviousResult None -> failwith "PreviousResult but no cap!"
  | Expression.PreviousResult (Some v) -> Prev v
  | Expression.Parameter p -> Param (Uint32.to_int p)
  | Expression.Call c ->
    let fn_obj = Expression.Call.function_get c |> or_fail "Missing fn" in
    let params = Expression.Call.params_get_list c |> List.map parse_expr in
    Call (fn_obj, params)
  | Expression.Undefined _ ->
    failwith "Unknown expression type"

(* The main calculator service *)
let service =
  Api.Builder.Calculator.local @@
  object
    inherit Api.Builder.Calculator.service

    method def_function_impl req =
      let module P = Api.Reader.Calculator.DefFunction_params in
      let module R = Api.Builder.Calculator.DefFunction_results in
      let params = P.of_payload req in
      let n_args = P.param_count_get_int_exn params in
      let body = P.body_get params |> parse_expr in
      let fn_obj = fn n_args body in
      release_expr body;
      let resp, results = Service.Response.create R.init_pointer in
      R.func_set results (Some fn_obj);
      Service.return resp

    method evaluate_impl req =
      let module P = Api.Reader.Calculator.Evaluate_params in
      let module R = Api.Builder.Calculator.Evaluate_results in
      let params = P.of_payload req in
      let expr = parse_expr (P.expression_get params) in
      Payload.release req;
      let value_obj = eval expr in
      release_expr expr;
      let resp, results = Service.Response.create R.init_pointer in
      R.value_set results (Some value_obj);
      Capability.dec_ref value_obj;
      Service.return resp

    method get_operator_impl req =
      let module P = Api.Reader.Calculator.GetOperator_params in
      let module R = Api.Builder.Calculator.GetOperator_results in
      let module O = Api.Reader.Calculator.Operator in
      let params = P.of_payload req in
      let op_obj =
        match P.op_get params with
        | O.Add -> add
        | O.Subtract -> sub
        | O.Multiply -> mul
        | O.Divide -> div
        | O.Undefined _ -> failwith "Unknown operator"
      in
      let resp, results = Service.Response.create R.init_pointer in
      R.func_set results (Some op_obj);
      Service.return resp
  end
