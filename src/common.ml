(*
* Common functionality.
*)

open Cmdliner

type slackopts = {
  token : string;
  channel : string;
  username : string option;
}

let slackopts token channel username = {token; channel; username}

let post_message slackopts message =
  let token = Slacko.token_of_string slackopts.token in
  let channel = Slacko.Channel (Slacko.channel_of_string slackopts.channel) in
  let message = Slacko.message_of_string message in
  Slacko.chat_post_message token channel
    ?username:(slackopts.username)
    ~icon_emoji:":camel:"
    message


let optstr f = f Arg.(some string) None

let token =
  let doc = "Slack API access token." in
  let env = Arg.env_var "SLACKOBOT_API_TOKEN" ~doc in
  Arg.(required & optstr opt & info ["t"; "token"] ~env ~docv:"TOKEN" ~doc)

let channel =
  let doc = "Slack channel to post to." in
  let env = Arg.env_var "SLACKOBOT_CHANNEL" ~doc in
  Arg.(required & optstr opt & info ["c"; "channel"] ~env ~docv:"CHANNEL" ~doc)

let username =
  let doc = "Slack username to post as." in
  let env = Arg.env_var "SLACKOBOT_USERNAME" ~doc in
  Arg.(value & optstr opt & info ["u"; "username"] ~env ~docv:"USERNAME" ~doc)

let slackopts_t = Term.(const slackopts $ token $ channel $ username)
