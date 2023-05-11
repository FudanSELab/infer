(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** invariant: if [package = Some str] then [not (String.equal str "")]. [typename] appears first
    so that the comparator fails earlier *)
type t = {package: string option; typename: string}
[@@deriving compare, equal, yojson_of, sexp, hash]

let make ~package ~typename =
  match package with
  | Some "" -> {package= None; typename}
  | _ -> {package; typename}
 

let from_string str =
  match String.rsplit2 str ~on:'.' with
  | None ->
      {typename= str; package= None}
  | Some ("", _) ->
      L.die InternalError "Empty package path in Swift qualified typename.@."
  | Some (pkg, typename) ->
      {typename; package= Some pkg}


let to_string = function
  | {typename; package= None} ->
      typename
  | {typename; package= Some pkg} ->
      String.concat ~sep:"." [pkg; typename]


let pp fmt = function
  | {typename; package= None} ->
      F.pp_print_string fmt typename
  | {typename; package= Some pkg} ->
      F.fprintf fmt "%s.%s" pkg typename
 
 
let package {package} = package

let typename {typename} = typename

let pp_with_verbosity ~verbose fmt t =
  if verbose then pp fmt t else F.pp_print_string fmt (typename t)
 
 
module Normalizer = HashNormalizer.Make (struct
  type nonrec t = t [@@deriving equal, hash]

  let normalize t =
    let typename = HashNormalizer.StringNormalizer.normalize t.typename in
    let package =
      IOption.map_changed t.package ~equal:phys_equal ~f:HashNormalizer.StringNormalizer.normalize
    in
    if phys_equal typename t.typename && phys_equal package t.package then t
    else {package; typename}
end)
 