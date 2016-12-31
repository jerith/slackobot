(*
* Miscellaneous small utilities.
*)

let optmap f = function
  | None -> None
  | Some a -> Some (f a)

let optmap2 f a b =
  match a, b with
  | None, None -> None
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b -> Some (f a b)
