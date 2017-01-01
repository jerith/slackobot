(*
* Some extra tools for working with yojson.
*)

module ObjMap = struct
  module type ObjVal = sig
    type t
    val pp : Format.formatter -> t -> unit
    val to_yojson : t -> Yojson.Safe.json
    val of_yojson : Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or
  end

  module StringMap = Map.Make(String)
  type +'a mt = 'a StringMap.t

  module type S = sig
    type value
    type t = value StringMap.t

    val empty : t
    val is_empty : t -> bool
    val mem : string -> t -> bool
    val add : string -> value -> t -> t
    val singleton : string -> value -> t
    val remove : string -> t -> t
    val merge : (string -> value option -> value option -> value option) -> t -> t -> t
    (* This was introduced on 4.03, so we don't get it in earlier versions. *)
    (* val union : (string -> value -> value -> value option) -> t -> t -> t *)
    val compare : (value -> value -> int) -> t -> t -> int
    val equal : (value -> value -> bool) -> t -> t -> bool
    val iter : (string -> value -> unit) -> t -> unit
    val fold : (string -> value -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (string -> value -> bool) -> t -> bool
    val exists : (string -> value -> bool) -> t -> bool
    val filter : (string -> value -> bool) -> t -> t
    val partition : (string -> value -> bool) -> t -> t * t
    val cardinal : t -> int
    val bindings : t -> (string * value) list
    val min_binding : t -> string * value
    val max_binding : t -> string * value
    val choose : t -> string * value
    val split : string -> t -> t * value option * t
    val find : string -> t -> value
    val map : (value -> value) -> t -> t
    val mapi : (string -> value -> value) -> t -> t

    val map_to : (value -> 'a) -> t -> 'a StringMap.t

    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val to_yojson : t -> Yojson.Safe.json
    val of_yojson : Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Make(Obj : ObjVal) : (S with type value = Obj.t) = struct
    type value = Obj.t
    type t = value mt
    include (StringMap : Map.S with type 'a t := 'a mt and type key := string)

    let map_to f a =
      StringMap.map f a

    let pp_pair fmt sep (k, v) =
      if sep then Format.fprintf fmt ";";
      Format.fprintf fmt "@ \"%s\": " k;
      Obj.pp fmt v;
      true

    let pp fmt a =
      Format.fprintf fmt "@[<2>{";
      ignore @@ List.fold_left (pp_pair fmt) false (bindings a);
      Format.fprintf fmt "@,}@]"

    let show a =
      pp Format.str_formatter a;
      Format.flush_str_formatter ()

    let to_yojson a =
      `Assoc (List.map (fun (k, v) -> k, Obj.to_yojson v) (bindings a))

    let pair_of_yojson (k, j) =
      let open Ppx_deriving_yojson_runtime in
      Obj.of_yojson j >|= fun v -> (k, v)

    let of_yojson = function
      | `Assoc alist ->
        let open Ppx_deriving_yojson_runtime in
        map_bind pair_of_yojson [] alist >|=
        List.fold_left (fun a (k, v) -> add k v a) empty
      | _ -> Error "Unexpected value"

  end
end
