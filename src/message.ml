(*
* Message plugin.
*)

let string_of_response = function
  | `Account_inactive -> "Error: Account inactive."
  | `Channel_not_found -> "Error: Channel not found."
  | `Invalid_auth -> "Error: Invalid auth."
  | `Is_archived -> "Error: Archived."
  | `Msg_too_long -> "Error: Message too long."
  | `Not_authed -> "Error: Not authed."
  | `Rate_limited -> "Error: Rate limited."
  | `Success json -> Yojson.Basic.pretty_to_string json
  | `Unhandled_error str -> "Unhandled error: " ^ str
  | `Unknown_error -> "Unknown error."

let print_response resp = string_of_response resp |> print_endline


let post_message slackopts message =
  let open Lwt in
  Common.post_message slackopts message >|= print_response
  |> Lwt_main.run


module Cmd = struct
  open Cmdliner

  let optstr f = f Arg.(some string) None

  let message =
    let doc = "Message to send" in
    Arg.(required & optstr (pos 0) & info [] ~docv:"MSG" ~doc)

  let message_cmd =
    let doc = "Post a message." in
    Term.(const post_message $ Common.slackopts_t $ message),
    Term.info "message" ~doc
end

let commands = [Cmd.message_cmd]
