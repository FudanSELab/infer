(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** invariant: if [package = Some str] then [not (String.equal str "")]. [structname] appears first
    so that the comparator fails earlier *)
type t = {package: string option; structname: string}
[@@deriving compare, equal, yojson_of, sexp, hash]

let make ~package ~structname =
  match package with
  | Some "" -> {package= None; structname}
  | _ -> {package; structname}


let from_string str =
  match String.rsplit2 str ~on:'.' with
  | None ->
      {structname= str; package= None}
  | Some ("", _) ->
      L.die InternalError "Empty package path in Swift qualified structname.@."
  | Some (pkg, structname) ->
      {structname; package= Some pkg}


let to_string = function
  | {structname; package= None} ->
      structname
  | {structname; package= Some pkg} ->
      String.concat ~sep:"." [pkg; structname]


let pp fmt = function
  | {structname; package= None} ->
      F.pp_print_string fmt structname
  | {structname; package= Some pkg} ->
      F.fprintf fmt "%s.%s" pkg structname


let package {package} = package

let structname {structname} = structname

let pp_with_verbosity ~verbose fmt t =
  if verbose then pp fmt t else F.pp_print_string fmt (structname t)


module Normalizer = HashNormalizer.Make (struct
  type nonrec t = t [@@deriving equal, hash]

  let normalize t =
    let structname = HashNormalizer.StringNormalizer.normalize t.structname in
    let package =
      IOption.map_changed t.package ~equal:phys_equal ~f:HashNormalizer.StringNormalizer.normalize
    in
    if phys_equal structname t.structname && phys_equal package t.package then t
    else {structname; package}
end)
