(*
* Mesos API client.
*)

open Lwt
open Mesos_api

module Capacity = struct
  type t = {
    used : Resources.t;
    unused : Resources.t;
    total : Resources.t;
  } [@@deriving show, yojson]

  let used a = a.used
  let unused a = a.unused
  let total a = a.total

  let capacity rtotal rused = {
    used = Resources.min rtotal rused;
    unused = Resources.sub rtotal rused;
    total = rtotal;
  }

  let combine f a b = {
    used = f a.used b.used;
    unused = f a.unused b.unused;
    total = f a.total b.total;
  }

  let add = combine Resources.add
end

module Capacities = struct
  include Oyjson.ObjMap.Make(Capacity)

  let of_slave slave =
    let reservations = Reservations.singleton "*" (Slave.unreserved slave) in
    let reservations = Reservations.add reservations (Slave.reserved slave) in
    let res_capacity r = Capacity.capacity r (Slave.used slave) in
    Reservations.map_to res_capacity reservations

  let add rs0 rs1 =
    merge (fun _ -> Mesos_utils.optmap2 Capacity.add) rs0 rs1

  let sum capslist = List.fold_left add empty capslist
end


let cluster_capacity ss =
  let slave_caps = List.map Capacities.of_slave (State_summary.slaves ss) in
  Capacities.sum slave_caps
