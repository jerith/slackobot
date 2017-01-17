(*
* Mesos plugin for slackobot.
*)

open Lwt


let pp_float_gb pr ppf n = Fmt.pf ppf "%aG" (Fmt.float_dfrac pr) (n /. 1024.)

let pp_availcap fmt ppf (used, unused, total) =
  Fmt.pf ppf "%a / %a" fmt unused fmt total

let cap_field getter cap =
  let open Mesos_tools in
  match
    Capacity.used cap |> getter,
    Capacity.unused cap |> getter,
    Capacity.total cap |> getter
  with
  | Some used, Some unused, Some total -> Some (used, unused, total)
  | _ -> None

let pp_field name fmt value =
  let pp_field' v ppf () = Fmt.pf ppf "*%s*: %a" name (pp_availcap fmt) v in
  Mesos_utils.optmap pp_field' value

let filter_opt l =
  let rec f' l = function
    | [] -> List.rev l
    | None :: tl -> f' l tl
    | Some v :: tl -> f' (v :: l) tl
  in
  f' [] l

let format_cap ?gpus:(gpus=false) ppf cap =
  let open Mesos_api in
  let get_disk cap = Some (Resources.disk cap) in
  let get_mem cap = Some (Resources.mem cap) in
  let get_gpus cap = if gpus then Resources.gpus cap else None in
  let get_cpus cap = Some (Resources.cpus cap) in
  let pp_cap = Fmt.vbox @@ Fmt.list (fun ppf v -> v ppf ()) in
  Fmt.pf ppf "%a" pp_cap @@ filter_opt [
    pp_field "disk" (pp_float_gb 1) (cap_field get_disk cap);
    pp_field "mem" (pp_float_gb 3) (cap_field get_mem cap);
    pp_field "gpus" (Fmt.float_dfrac 2) (cap_field get_gpus cap);
    pp_field "cpus" (Fmt.float_dfrac 2) (cap_field get_cpus cap);
  ]

let format_cluster_capacity_attachments ?gpus cc =
  let open Mesos_tools in
  let mkattach (res, cap) =
    let pretext = match res with
      | "*" -> "unreserved"
      | _ -> Fmt.strf "reserved: `%s`" res
    in
    let fallback = Fmt.strf "%s: %a" res (format_cap ?gpus) cap in
    let text = Fmt.strf "%a" (format_cap ?gpus) cap in
    let mrkdwn_in = ["pretext"; "fields"; "text"] in
    Slacko.attachment ~fallback ~pretext ~text ~mrkdwn_in ()
  in
  cc |> Capacities.bindings |> List.map mkattach


let format_cluster_capacity_fields ?gpus cc =
  let open Mesos_tools in
  let mkfallback (res, cap) = Fmt.strf "%s\n%a" res (format_cap ?gpus) cap in
  let mkfield (res, cap) =
    let title = match res with
      | "*" -> "[unreserved]"
      | _ -> Fmt.strf "[reserved: %s]" res
    in
    let value = Fmt.strf "%a" (format_cap ?gpus) cap in
    Slacko.field ~title ~short:true value
  in
  let bindings = Capacities.bindings cc in
  let fallback = bindings |> List.map mkfallback |> String.concat "\n" in
  let fields = bindings |> List.map mkfield in
  let mrkdwn_in = ["pretext"; "fields"; "text"] in
  [Slacko.attachment ~color:"#8A2BE2" ~fallback ~fields ~mrkdwn_in ()]


let build_msg format_cluster_capacity ?gpus ?width cluster cc =
  let text = Fmt.strf ":dcos: _*%s*_ cluster capacity:" cluster in
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
  | `Too_many_attachments -> "Error: Too many attachments."
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
