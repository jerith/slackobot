(*
* Message plugin.
*)

let string_of_chat_obj {Slacko.ts; chat; text} =
  Printf.sprintf "{ts: %.f, chat: %%s, text: %s}"
    ts (* chat *) (match text with None -> "" | Some t -> t)

let string_of_response = function
  | `Account_inactive -> "Error: Account inactive."
  | `Channel_not_found -> "Error: Channel not found."
  | `Invalid_auth -> "Error: Invalid auth."
  | `Is_archived -> "Error: Archived."
  | `Msg_too_long -> "Error: Message too long."
  | `Not_authed -> "Error: Not authed."
  | `Rate_limited -> "Error: Rate limited."
  | `Success chatobj -> string_of_chat_obj chatobj
  | `Unhandled_error str -> "Unhandled error: " ^ str
  | `Unknown_error -> "Unknown error."
  | `User_is_bot -> "Error: User is bot."
  | `ParseFailure str -> "Parse failure: " ^ str

let print_response resp = string_of_response resp |> print_endline

let post_message slackopts message =
  let open Lwt in
  Common.make_message message |> Common.post_message slackopts >|=
  print_response
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
