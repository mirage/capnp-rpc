(* This is the OCaml version of the C++ capnp calculator example. *)

open Lwt.Infix
open Capnp_rpc

module Api = Calculator.MakeRPC(Capnp.BytesMessage)(Capnp_rpc)

type expr =
  | Float of float
  | Prev of Api.Reader.Calculator.Value.t Capability.t
  | Param of int
  | Call of Api.Reader.Calculator.Function.t Capability.t * expr list

let or_fail msg = function
  | Some x -> x
  | None -> failwith msg

(* A more user-friendly API for the Value interface *)
let value_client proxy =
  let proxy = new Api.Reader.Calculator.Value.client proxy in
  object
    method read =
      let module P = Api.Reader.Calculator.Value.Read_params in
      let module R = Api.Reader.Calculator.Value.Read_results in
      let req = Capability.Request.create_no_args () in
      Capability.call_for_value proxy#read req >|= fun resp ->
      R.of_payload resp |> R.value_get
  end

(* A more user-friendly API for the Function interface *)
let fn_client proxy =
  let proxy = new Api.Reader.Calculator.Function.client proxy in
  object
    method call args =
      let module P = Api.Builder.Calculator.Function.Call_params in
      let module R = Api.Reader.Calculator.Function.Call_results in
      let req, p = Capability.Request.create P.init_pointer in
      ignore (P.params_set_list p args);
      Capability.call_for_value proxy#call req >|= fun resp ->
      R.of_payload resp |> R.value_get
  end

(* Write an expression into a message, exporting function capabilities. *)
let rec write_expr ~export b expr =
  let open Api.Builder.Calculator in
  match expr with
  | `Float f -> Expression.literal_set b f
  | `Prev v -> Expression.previous_result_set b v
  | `Param i -> Expression.parameter_set b (Uint32.of_int i)
  | `Call (f, args) ->
    let c = Expression.call_init b in
    Expression.Call.function_set c (Some (export f));
    let args_b = Expression.Call.params_init c (List.length args) in
    args |> List.iteri (fun i arg ->
        write_expr ~export (Capnp.Array.get args_b i) arg
      )

(* A more user-friendly API for the Calculator interface *)
let client proxy =
  let proxy = new Api.Reader.Calculator.client proxy in
  object
    method evaluate expr =
      let module P = Api.Builder.Calculator.Evaluate_params in
      let module R = Api.Reader.Calculator.Evaluate_results in
      let req, p = Capability.Request.create P.init_pointer in
      write_expr ~export:(Capability.Request.export req) (P.expression_init p) expr;
      Capability.call_for_cap proxy#evaluate req
      |> R.value_get_pipelined
      |> value_client

    method getOperator op =
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
      Capability.call_for_cap proxy#get_operator req
      |> R.func_get_pipelined
  end

(* Export a literal float as a service with a [read] method. *)
let literal f =
  let open Api.Builder.Calculator in
  Value.local @@ object (_ : Value.service)
    method read _ =
      let resp, c = Service.Response.create Value.Read_results.init_pointer in
      Value.Read_results.value_set c f;
      Service.return resp
  end

(* Evaluate an expression, where some sub-expressions may require remote calls. *)
let rec eval ?(args=[||]) : _ -> Api.Reader.Calculator.Value.t Capability.t = function
  | Float f -> literal f
  | Prev v -> v
  | Param p -> literal args.(p)
  | Call (f, params) ->
    let params = params |> Lwt_list.map_p (fun p ->
        let value = value_client (eval ~args p) in
        value#read
      ) in
    let f = fn_client f in
    let result = params >>= f#call in
    let open Api.Builder.Calculator in
    Value.local @@ object (_ : Value.service)
      method read _ =
        Service.return_lwt (
          result >|= fun result ->
          let resp, c = Service.Response.create Value.Read_results.init_pointer in
          Value.Read_results.value_set c result;
          resp
        )
    end

(* A local service that provides the function [body]. *)
let fn n_args body =
  let open Api.Builder.Calculator in
  Function.local @@ object (_ : Function.service)
    method call req =
      let module P = Api.Reader.Calculator.Function.Call_params in
      let module R = Api.Builder.Calculator.Function.Call_results in
      let params = P.of_payload req in
      let args = P.params_get_array params in
      assert (Array.length args = n_args);
      let value = value_client (eval ~args body) in
      (* Functions return floats, not Value objects, so we have to wait here. *)
      Service.return_lwt (
        value#read >|= fun value ->
        let resp, r = Service.Response.create R.init_pointer in
        R.value_set r value;
        resp
      )
  end

(* A local service exporting function [op] (of two arguments). *)
let binop op : Api.Builder.Calculator.Function.t Capability.t =
  let open Api.Builder.Calculator in
  Function.local @@ object
    method call req =
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

let rec parse_expr ~req r =
  let open Api.Reader.Calculator in
  match Expression.get r with
  | Expression.Literal f -> Float f
  | Expression.PreviousResult None -> failwith "PreviousResult but no cap!"
  | Expression.PreviousResult (Some v) -> Prev (Payload.import req v)
  | Expression.Parameter p -> Param (Uint32.to_int p)
  | Expression.Call c ->
    let fn_i = Expression.Call.function_get c |> or_fail "Missing fn" in
    let fn_obj = Payload.import req fn_i in
    let params = Expression.Call.params_get_list c |> List.map (parse_expr ~req) in
    Call (fn_obj, params)
  | Expression.Undefined _ ->
    failwith "Unknown expression type"

(* The main calculator service *)
let service =
  Api.Builder.Calculator.local @@
  object (_ : Api.Builder.Calculator.service)
    method def_function req =
      let module P = Api.Reader.Calculator.DefFunction_params in
      let module R = Api.Builder.Calculator.DefFunction_results in
      let params = P.of_payload req in
      let n_args = P.param_count_get_int_exn params in
      let body = P.body_get params |> parse_expr ~req in
      let fn_obj = fn n_args body in
      let resp, results = Service.Response.create R.init_pointer in
      let fn_id = Service.Response.export resp fn_obj in
      R.func_set results (Some fn_id);
      Service.return resp

    method evaluate req =
      let module P = Api.Reader.Calculator.Evaluate_params in
      let module R = Api.Builder.Calculator.Evaluate_results in
      let params = P.of_payload req in
      let expr = parse_expr ~req (P.expression_get params) in
      let value_obj = eval expr in
      let resp, results = Service.Response.create R.init_pointer in
      let value_id = Service.Response.export resp value_obj in
      R.value_set results (Some value_id);
      Service.return resp

    method get_operator req =
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
      let op_id = Service.Response.export resp op_obj in
      R.func_set results (Some op_id);
      Service.return resp
  end
