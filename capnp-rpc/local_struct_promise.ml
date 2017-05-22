module Make (C : S.CORE_TYPES) = struct
  open C

  module Struct_proxy = Struct_proxy.Make(C)

  type target = (struct_ref -> unit) Queue.t

  let rec local_promise () = object (_ : #struct_resolver)
    inherit [target] Struct_proxy.t (Queue.create ())

    method private do_pipeline q i msg caps =
      let result = local_promise () in
      q |> Queue.add (fun p ->
          result#connect ((p#cap i)#call msg caps)
        );
      (result :> struct_ref)

    method! pp f =
      Fmt.pf f "local-struct-ref -> %a" Struct_proxy.pp_state state

    method private on_resolve q x =
      Queue.iter (fun fn -> fn x) q

    method private do_finish _ = ()
  end

  let make () = (local_promise () :> struct_resolver)
end
