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
  fprintf ff "%a`/`%a" fmt unused fmt total

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
  fprintf ff "*`%s`*: `%a`" name (format_field fmt) value

let pp_null fmt ff name = ()

let pp_sep ff () = pp_print_break ff 2 0

let format_cap ?gpus:(gpus=false) ff cap =
  let open Mesos_api in
  let pp_req getter = pp_field (cap_field getter cap) in
  let pp_opt getter =
    match cap_opt getter cap with
    | None -> pp_null
    | Some value -> pp_field value
  in
  let filter_gpus = List.filter (fun (n, _) -> n <> "gpus" || gpus) in
  fprintf ff "@[";
  filter_gpus [
    "disk", pp_req Resources.disk (format_gb 1);
    "mem", pp_req Resources.mem (format_gb 3);
    "gpus", pp_opt Resources.gpus (format_n 2);
    "cpus", pp_req Resources.cpus (format_n 2);
  ] |> pp_print_list (fun ff (n, pp) -> pp ff n) ~pp_sep ff;
  fprintf ff "@]"


let format_cluster_capacity ?gpus ?width cluster ff cc =
  (match width with None -> () | Some width -> pp_set_margin ff width);
  let open Mesos_tools in
  let reslen = Capacities.fold (fun n _ m -> max m (String.length n)) cc 0 in
  fprintf ff "@[<v 2>:dcos: %s cluster capacity:" cluster;
  cc |> Capacities.iter (fun res cap ->
      fprintf ff "@;`%*s`=> %a" reslen res (format_cap ?gpus) cap);
  fprintf ff "@]"


let get_cluster_capacity ?gpus ?width ?cluster base_uri =
  Mesos_api.get_state_summary base_uri >|= function
  | Result.Error str -> "Error getting cluster capacity: " ^ str
  | Result.Ok ss ->
    let cluster =
      match cluster, Mesos_api.State_summary.cluster ss with
      | None, None -> "<anonymous>"
      | None, Some cluster -> cluster
      | Some cluster, _ -> cluster
    in
    Mesos_tools.cluster_capacity ss |>
    asprintf "%a" (format_cluster_capacity ?gpus ?width cluster)

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

let print_response resp = string_of_response resp |> print_endline

let post_cluster_capacity slackopts gpus width cluster base_uri =
  let open Lwt in
  get_cluster_capacity ~gpus:gpus ?width ?cluster base_uri >>=
  Common.post_message slackopts >|= print_response
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
