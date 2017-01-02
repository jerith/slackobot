(*
* Mesos plugin for slackobot.
*)

open Format
open Lwt


let rec truncdecimal s =
  let l = String.length s - 1 in
  match s.[l] with
  | '.' -> String.sub s 0 l
  | '0' -> String.sub s 0 l |> truncdecimal
  | _ -> s

let format_float_nice precision ff a =
  let s = sprintf "%.*f" precision a in
  fprintf ff "%s" (truncdecimal s)

let format_n pr ff n = fprintf ff "%a" (format_float_nice pr) n
let format_gb pr ff n = fprintf ff "%aG" (format_float_nice pr) (n /. 1000.)

let format_field fmt ff (used, unused, total) =
  fprintf ff "%a / %a" fmt unused fmt total

let cap_field getter cap =
  let open Mesos_tools in
  (Capacity.used cap |> getter,
   Capacity.unused cap |> getter,
   Capacity.total cap |> getter)

let cap_opt getter cap =
  let open Mesos_tools in
  match
    Capacity.used cap |> getter,
    Capacity.unused cap |> getter,
    Capacity.total cap |> getter
  with
  | Some used, Some unused, Some total -> Some (used, unused, total)
  | _ -> None

let pp_field value fmt ff name =
  fprintf ff "*%s*: %a" name (format_field fmt) value

let pp_null fmt ff name = ()

let pp_sep ff () = pp_print_break ff 0 (-2)

let format_cap ?gpus:(gpus=false) ff cap =
  let open Mesos_api in
  let pp_req getter = pp_field (cap_field getter cap) in
  let pp_opt getter =
    match cap_opt getter cap with
    | None -> pp_null
    | Some value -> pp_field value
  in
  let filter_gpus = List.filter (fun (n, _) -> n <> "gpus" || gpus) in
  pp_open_vbox ff 0;
  filter_gpus [
    "disk", pp_req Resources.disk (format_gb 1);
    "mem", pp_req Resources.mem (format_gb 3);
    "gpus", pp_opt Resources.gpus (format_n 2);
    "cpus", pp_req Resources.cpus (format_n 2);
  ] |> pp_print_list (fun ff (n, pp) -> pp ff n) ~pp_sep:pp_print_cut ff;
  pp_close_box ff ()

let format_cluster_capacity_attachments ?gpus cc =
  let open Mesos_tools in
  let mkattach (res, cap) =
    let pretext = asprintf "role: `%s`" res in
    let fallback = asprintf "%s: %a" res (format_cap ?gpus) cap in
    let text = asprintf "%a" (format_cap ?gpus) cap in
    let mrkdwn_in = ["pretext"; "fields"; "text"] in
    Slacko.attachment ~fallback ~pretext ~text ~mrkdwn_in ()
  in
  cc |> Capacities.bindings |> List.map mkattach


let format_cluster_capacity_fields ?gpus cc =
  let open Mesos_tools in
  let mkfallback (res, cap) =
    asprintf "%s\n%a" res (format_cap ?gpus) cap
  in
  let mkfield (res, cap) =
    let title = asprintf "[role: %s]" res in
    let value = asprintf "%a" (format_cap ?gpus) cap in
    Slacko.field ~title ~value ~short:true ()
  in
  let bindings = Capacities.bindings cc in
  let fallback = bindings |> List.map mkfallback |> String.concat "\n" in
  let fields = bindings |> List.map mkfield in
  let mrkdwn_in = ["pretext"; "fields"; "text"] in
  [Slacko.attachment ~color:"#8A2BE2" ~fallback ~fields ~mrkdwn_in ()]


let build_msg format_cluster_capacity ?gpus ?width cluster cc =
  let text = sprintf ":dcos: _*%s*_ cluster capacity:" cluster in
  let attachments = format_cluster_capacity ?gpus cc in
  Common.make_message ~attachments text

let get_cluster_capacity ?gpus ?width ?cluster base_uri =
  Mesos_api.get_state_summary base_uri >|= function
  | Result.Error str ->
    Common.make_message ("Error getting cluster capacity: " ^ str)
  | Result.Ok ss ->
    let cluster =
      match cluster, Mesos_api.State_summary.cluster ss with
      | None, None -> "<anonymous>"
      | None, Some cluster -> cluster
      | Some cluster, _ -> cluster
    in
    Mesos_tools.cluster_capacity ss |>
    build_msg format_cluster_capacity_fields ?gpus ?width cluster



let string_of_response = function
  | `Account_inactive -> "Error: Account inactive."
  | `Channel_not_found -> "Error: Channel not found."
  | `Invalid_auth -> "Error: Invalid auth."
  | `Is_archived -> "Error: Archived."
  | `Msg_too_long -> "Error: Message too long."
  | `Not_authed -> "Error: Not authed."
  | `Rate_limited -> "Error: Rate limited."
  | `Success json -> "Success!"
  | `Unhandled_error str -> "Unhandled error: " ^ str
  | `Unknown_error -> "Unknown error."
  | `User_is_bot -> "Error: User is bot."
  | `ParseFailure str -> "Parse failure: " ^ str

let print_response resp = string_of_response resp |> print_endline

let post_cluster_capacity slackopts gpus width cluster base_uri =
  let open Lwt in
  get_cluster_capacity ~gpus ?width ?cluster base_uri >>=
  Common.post_message slackopts >|=
  print_response
  |> Lwt_main.run



module Cmd = struct
  open Cmdliner

  let optstr f = f Arg.(some string) None

  let gpus =
    let doc = "Include GPUs." in
    let gpus = true, Arg.info ["g"; "gpus"] ~doc in
    let doc = "Exclude GPUs (the default)." in
    let no_gpus = false, Arg.info ["no-gpus"] ~doc in
    Arg.(value & vflag false [gpus; no_gpus])

  let width =
    let doc = "Maximum width of output." in
    Arg.(value & opt (some int) (Some 100) &
         info ["w"; "width"] ~docv:"CHARS" ~doc)

  let cluster =
    let doc = "Override cluster name." in
    Arg.(value & optstr opt & info ["n"; "cluster-name"] ~docv:"NAME" ~doc)

  let base_uri =
    let doc = "Base URI of mesos master to query." in
    Arg.(required & optstr (pos 0) & info [] ~docv:"URI" ~doc)

  let capacity_cmd =
    let doc = "Post mesos cluster capacity." in
    Term.(const post_cluster_capacity $ Common.slackopts_t $
          gpus $ width $ cluster $ base_uri),
    Term.info "mesos-capacity" ~doc
end

let commands = [Cmd.capacity_cmd]
