(*
* Tests for Oyjson.
*)

open OUnit2
open Oyjson


module Test_ObjMap = struct
  module IntObj = struct
    type t = int [@@deriving show, yojson]
  end

  module IntObjMap = ObjMap.Make(IntObj)

  let uniqassoc gk gv =
    let rec revuniq seen rl = function
      | [] -> rl
      | (a, _) :: l when List.mem a seen -> revuniq seen rl l
      | (a, b) :: l -> revuniq (a :: seen) ((a, b) :: rl) l
    in
    QCheck.(list (pair gk gv) |> map_same_type (revuniq [] []))

  let assoc_si = QCheck.(uniqassoc small_string int)
  let map_si arb = QCheck.(pair assoc_si (fun1 int arb))

  let add_pair iom (k, v) = IntObjMap.add k v iom
  let mkiom pairs = List.fold_left add_pair IntObjMap.empty pairs
  let mkyj pairs = `Assoc (List.map (fun (k, v) -> k, `Int v) pairs)

  let prop_of_yojson pairs =
    (mkyj pairs |> IntObjMap.of_yojson) = (Result.Ok (mkiom pairs))

  let prop_to_yojson pairs =
    (mkiom pairs |> IntObjMap.to_yojson |> Yojson.Safe.sort) =
    (mkyj pairs |> Yojson.Safe.sort)

  let showpair_re (k, v) =
    Printf.sprintf "\"%s\": %d" (Str.quote k) v |> Str.regexp

  let check_pair str re =
    try Str.search_forward re str 0 |> ignore; true with Not_found -> false

  let check_pairs iomstr res =
    List.for_all (check_pair iomstr) res

  let prop_show pairs =
    let iomstr = mkiom pairs |> IntObjMap.show in
    List.for_all (fun x -> x) [
      iomstr.[0] = '{';
      iomstr.[String.length iomstr - 1] = '}';
      pairs |> List.map showpair_re |> check_pairs iomstr;
    ]

  let prop_map_to (pairs, f) =
    let mapped = IntObjMap.map_to f (mkiom pairs) in
    (List.map (fun (k, v) -> k, f v) pairs |> List.sort compare) =
    (ObjMap.StringMap.bindings mapped)

  let mktest ?count:(count=50) name arb prop =
    QCheck.Test.make ~count ~name arb prop |> QCheck_runner.to_ounit2_test

  let suite = "test_ObjMap" >::: [
      mktest "test_of_yojson" assoc_si prop_of_yojson;
      mktest "test_to_yojson" assoc_si prop_to_yojson;
      mktest "test_show" assoc_si prop_show;
      mktest "test_map_to_int" (map_si QCheck.int) prop_map_to;
      mktest "test_map_to_float" (map_si QCheck.float) prop_map_to;
      mktest "test_map_to_boollist" (map_si QCheck.(list bool)) prop_map_to;
    ]
end

let suite = "test_oyjson" >::: [
    Test_ObjMap.suite;
  ]
