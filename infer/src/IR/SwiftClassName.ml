(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** invariant: if [package = Some str] then [not (String.equal str "")]. [classname] appears first
    so that the comparator fails earlier *)
type t = {package: string option; classname: string}
[@@deriving compare, equal, yojson_of, sexp, hash]

let make ~package ~classname =
  match package with
  | Some "" -> {package= None; classname}
  | _ -> {package; classname}
 

let from_string str =
  match String.rsplit2 str ~on:'.' with
  | None ->
      {classname= str; package= None}
  | Some ("", _) ->
      L.die InternalError "Empty package path in Swift qualified classname.@."
  | Some (pkg, classname) ->
      {classname; package= Some pkg}


let to_string = function
  | {classname; package= None} ->
      classname
  | {classname; package= Some pkg} ->
      String.concat ~sep:"." [pkg; classname]


let pp fmt = function
  | {classname; package= None} ->
      F.pp_print_string fmt classname
  | {classname; package= Some pkg} ->
      F.fprintf fmt "%s.%s" pkg classname
 
 
let package {package} = package

let classname {classname} = classname

let pp_with_verbosity ~verbose fmt t =
  if verbose then pp fmt t else F.pp_print_string fmt (classname t)
 
 
module Normalizer = HashNormalizer.Make (struct
  type nonrec t = t [@@deriving equal, hash]

  let normalize t =
    let classname = HashNormalizer.StringNormalizer.normalize t.classname in
    let package =
      IOption.map_changed t.package ~equal:phys_equal ~f:HashNormalizer.StringNormalizer.normalize
    in
    if phys_equal classname t.classname && phys_equal package t.package then t
    else {classname; package}
end)
 