(*
* Some HTTP helpers.
*)

open Lwt
open Cohttp
open Cohttp_lwt_unix


let get_location uri resp =
  Response.headers resp |> fun hs -> Header.get hs "Location" |> function
    | None -> failwith "ERROR: Redirect with no location header"
    | Some str -> Uri.of_string str |> Uri.resolve "http" uri

let meth_to_get = function `HEAD -> `HEAD | _ -> `GET

let rec call_with_redirect' ?ctx ?headers ?body ?chunked meth uri times =
  if times <= 0 then failwith "Too many redirects.";
  let recall meth uri =
    call_with_redirect' ?ctx ?headers ?body ?chunked meth uri (times - 1)
  in
  Client.call ?ctx ?headers ?body ?chunked meth uri >>= fun (resp, rbody) ->
  match Response.status resp with
  (* These should be retried at the new location as-is. *)
  | `Moved_permanently (* 301 *)
  | `Found (* 302 *)
  | `Temporary_redirect (* 307 *)
  | `Resume_incomplete (* 308, actually "Moved permanently" *) ->
    recall meth (get_location uri resp)
  (* This should be retried at the new location with the method changed. *)
  | `See_other (* 303 *) ->
    recall (meth_to_get meth) (get_location uri resp)
  (* Not a redirect, return the response. *)
  | _ -> return (resp, rbody)

let call_with_redirect ?ctx ?headers ?body ?chunked meth uri =
  call_with_redirect' ?ctx ?headers ?body ?chunked meth uri 10

let get_with_redirect ?ctx ?headers ?body ?chunked uri =
  call_with_redirect ?ctx ?headers ?body ?chunked `GET uri



let get_json uri =
  get_with_redirect (Uri.of_string uri) >>= fun (resp, body) ->
  body |> Cohttp_lwt_body.to_string >|= Yojson.Safe.from_string
