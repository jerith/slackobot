(*
* slackobot -- doesn't do very much yet.
*)


module Cmd = struct
  open Cmdliner

  let cmds = List.concat [Message.commands; Mesos.commands]

  let default_cmd =
    let doc = "A slack bot." in
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ Common.slackopts_t)),
    Term.info "slackobot" ~doc

  let run () =
    match Term.eval_choice default_cmd cmds with
    | `Error _ -> exit 1
    | _ -> exit 0
end

let () = Cmd.run ()
