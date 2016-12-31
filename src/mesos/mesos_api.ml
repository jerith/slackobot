(*
* Mesos API client.
*)

open Lwt
open Oyjson
open Mesos_utils

module Resources = struct
  type t = {
    disk : float;
    mem : float;
    gpus : float option; [@default None]
    cpus : float;
    ports : string; [@default ""]
  } [@@deriving show, yojson {strict = false}]

  let disk a = a.disk
  let mem a = a.mem
  let gpus a = a.gpus
  let cpus a = a.cpus
  let ports a = a.ports

  let combine_without_ports f a b = {
    disk = f a.disk b.disk;
    mem = f a.mem b.mem;
    gpus = optmap2 f a.gpus b.gpus;
    cpus = f a.cpus b.cpus;
    ports = "";
  }

  let add = combine_without_ports (+.)
  let min = combine_without_ports Pervasives.min
  let sub = combine_without_ports (fun a b -> max (a -. b) 0.)
end

module Reservations = struct
  include ObjMap.Make(Resources)

  let add rs0 rs1 =
    merge (fun _ -> optmap2 Resources.add) rs0 rs1
end

module Slave = struct
  type t = {
    id : string;
    hostname : string;
    resources : Resources.t;
    used_resources : Resources.t;
    reserved_resources : Reservations.t;
    unreserved_resources : Resources.t;
  } [@@deriving show, yojson {strict = false}]

  let used s = s.used_resources
  let reserved s = s.reserved_resources
  let unreserved s = s.unreserved_resources
end

module Framework = struct
  type t = {
    id : string;
    name : string;
  } [@@deriving show, yojson {strict = false}]
end

module State_summary = struct
  type t = {
    hostname : string;
    cluster : string option; [@default None]
    slaves : Slave.t list;
    frameworks : Framework.t list;
  } [@@deriving show, yojson {strict = false}]

  let hostname a = a.hostname
  let cluster a = a.cluster
  let slaves a = a.slaves
  let frameworks a = a.frameworks
end


let get_state_summary base_uri =
  Http.get_json (base_uri ^ "/state-summary") >|= State_summary.of_yojson
