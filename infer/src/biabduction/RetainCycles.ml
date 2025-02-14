(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module MF = MarkupFormatter
module F = Format

let desc_retain_cycle tenv (cycle : RetainCyclesType.t) =
  let open RetainCyclesType in
  Logging.d_strln "Proposition with retain cycle:" ;
  let do_edge index_ edge =
    let index = index_ + 1 in
    let node = AnalysisState.get_node_exn () in
    let from_exp_str edge_obj =
      let type_str =
        let typ_str = Typ.to_string edge_obj.rc_from.rc_node_typ in
        (* TODO: only objc_object*)
        if String.equal typ_str "objc_object" then MF.monospaced_to_string "id"
        else MF.monospaced_to_string (Format.sprintf "%s*" typ_str)
      in
      match Errdesc.find_outermost_dereference tenv node edge_obj.rc_from.rc_node_exp with
      | Some de -> (
          let decomp = DecompiledExp.to_string de in
          match de with
          | DecompiledExp.Dretcall _ ->
              Format.sprintf "object %s of type %s" decomp type_str
          | _ ->
              Format.sprintf "%s of type %s" (MF.monospaced_to_string decomp) type_str )
      | None ->
          Format.sprintf "object of type %s" type_str
    in
    let location_str =
      match edge with
      | Object obj ->
          let update_str_list =
            Localise.access_desc (Errdesc.access_opt obj.rc_field.rc_field_inst)
          in
          if List.is_empty update_str_list then " "
          else ", " ^ String.concat ~sep:"," update_str_list
      | Block _ ->
          ""
    in
    let cycle_item_str =
      match edge with
      | Object obj ->
          Format.sprintf "%s --> %s" (from_exp_str obj)
            (MF.monospaced_to_string (Fieldname.to_string obj.rc_field.rc_field_name))
      | Block (_, var) ->
          Format.sprintf "a block that captures %s" (MF.monospaced_to_string (Pvar.to_string var))
    in
    Format.sprintf "(%d) %s%s" index cycle_item_str location_str
  in
  let cycle_str = List.mapi ~f:do_edge cycle.rc_elements in
  List.fold_left cycle_str ~f:(fun acc s -> Format.sprintf "%s\n %s" acc s) ~init:""


let edge_is_strong tenv obj_edge =
  let open RetainCyclesType in
  let has_weak_type (t : Typ.t) =
    match t.Typ.desc with
    | Typ.Tptr (_, Typ.Pk_objc_weak) | Typ.Tptr (_, Typ.Pk_objc_unsafe_unretained)
    | Typ.Tptr (_, Typ.Pk_swift_weak) | Typ.Tptr (_, Typ.Pk_swift_unowned_safe)
    | Typ.Tptr (_, Typ.Pk_swift_unowned_unsafe) ->
        true
    | _ ->
        false
  in
  let has_weak_or_unretained_or_assign params =
    List.exists
      ~f:(fun Annot.{value} ->
        Annot.has_matching_str_value value ~pred:(fun att ->
            String.equal Config.unsafe_unret att
            || String.equal Config.weak att || String.equal Config.assign att ) )
      params
  in
  let rec filter ((fn, typ, _) : Struct.field) = 
    if Fieldname.equal obj_edge.rc_field.rc_field_name fn then
      true
    else
      match typ.desc with
      | Tstruct name -> (
          match Tenv.lookup tenv name with
          | Some {fields} -> 
            (match (List.find fields filter) with Some _ -> true | None -> false)
          | None -> false
            )
      | _ -> false
  in
  let rc_field =
    match obj_edge.rc_from.rc_node_typ.desc with
    | Tstruct name -> (
      (* 比较 from_node & to_node 的 field_name *)
      match Tenv.lookup tenv name with
      | Some {fields} ->
          (* List.find ~f:(fun (fn, typ, _) ->
            ignore typ.desc;
            Fieldname.equal obj_edge.rc_field.rc_field_name fn) fields *)
          List.find fields ~f:filter
      | None ->
          None )
    | _ ->
        None
  in
  (* false and true and false *)
  has_weak_type obj_edge.rc_from.rc_node_typ |> string_of_bool |> print_endline;
  ( match rc_field with
       | Some (_, _, ia) ->
        (* print_endline "run into Some (_, _, ia) case"; *)
           List.exists
             ~f:(fun (ann : Annot.t) ->
               ( String.equal ann.class_name Config.property_attributes
               || String.equal ann.class_name Config.ivar_attributes )
               && has_weak_or_unretained_or_assign ann.parameters )
             ia
       | _ ->
           (* print_endline "run into _ case"; *)
           (* Assume the edge is weak if the type or field cannot be found in the tenv, to avoid FPs *)
           true )
           |> string_of_bool |> print_endline;
  (match rc_field with Some (_, typ, _) -> has_weak_type typ | None -> false) |> string_of_bool |> print_endline;
  (* if edge_is_strong = true, rc_field must be Some _ *)
  not
    ( (* Weak edge - by type of from-node *)
      has_weak_type obj_edge.rc_from.rc_node_typ
    (* Weak edge - by annotation of from-node/field *)
    || ( match rc_field with
       | Some (_, _, ia) ->
        print_endline "run into Some (_, _, ia) case";
           List.exists
             ~f:(fun (ann : Annot.t) ->
               ( String.equal ann.class_name Config.property_attributes
               || String.equal ann.class_name Config.ivar_attributes )
               && has_weak_or_unretained_or_assign ann.parameters )
             ia
       | _ ->
           print_endline "run into _ case";
           (* Assume the edge is weak if the type or field cannot be found in the tenv, to avoid FPs *)
           true )
    ||
    (* Weak edge - by type of from-node/field *)
    match rc_field with Some (_, typ, _) -> has_weak_type typ | None -> false )

(* let rec filter (fn, typ, _) = 
  if Fieldname.equal obj_edge.rc_field.rc_field_name fn then
    true
  else
    match typ.desc with
    | Tstruct name -> (
        match Tenv.lookup tenv name with
        | Some {fields} -> 
          (match (List.find fields filter) with Some _ -> true | None -> false)
        )
    | _ -> false *)

exception Max_retain_cycles of RetainCyclesType.Set.t

let add_cycle found_cycles rev_path =
  match RetainCyclesType.create_cycle (List.rev rev_path) with
  | Some cycle ->
      if RetainCyclesType.Set.cardinal found_cycles < 10 then
        RetainCyclesType.Set.add cycle found_cycles
      else raise (Max_retain_cycles found_cycles)
  | None ->
      found_cycles


let get_cycle_blocks root_node exp =
  match exp with
  | Exp.Closure {name; captured_vars} ->
      List.find_map
        ~f:(fun (e, var, typ, _) ->
          match typ.Typ.desc with
          | Typ.Tptr (_, Typ.Pk_objc_weak) | Typ.Tptr (_, Typ.Pk_objc_unsafe_unretained)
          | Typ.Tptr (_, Typ.Pk_swift_weak) | Typ.Tptr (_, Typ.Pk_swift_unowned_safe)
          | Typ.Tptr (_, Typ.Pk_swift_unowned_unsafe) ->
              None
          | _ ->
              if Exp.equal e root_node.RetainCyclesType.rc_node_exp then Some (name, var) else None
          )
        captured_vars
  | _ ->
      None


let get_weak_alias_type prop e =
  let sigma = prop.Prop.sigma in
  let check_weak_alias hpred =
    match hpred with
    | Predicates.Hpointsto (_, Eexp (e', _), Sizeof {typ}) -> (
      match typ.Typ.desc with
      | Typ.Tptr (_, Typ.Pk_objc_weak) | Typ.Tptr (_, Typ.Pk_objc_unsafe_unretained)
      | Typ.Tptr (_, Typ.Pk_swift_weak) | Typ.Tptr (_, Typ.Pk_swift_unowned_safe)
      | Typ.Tptr (_, Typ.Pk_swift_unowned_unsafe)
        when Exp.equal e' e ->
          Some typ
      | _ ->
          None )
    | _ ->
        None
  in
  List.find_map ~f:check_weak_alias sigma


let get_cycles found_cycles root tenv prop =
  let open RetainCyclesType in
  let sigma = prop.Prop.sigma in
  let get_points_to e =
    List.find
      ~f:(fun hpred ->
        match hpred with Predicates.Hpointsto (e', _, _) -> Exp.equal e' e | _ -> false )
      sigma
  in
  (* Perform a dfs of a graph stopping when e_root is reached. Returns the set of cycles reached. *)
  let rec dfs ~found_cycles ~root_node ~from_node ~rev_path ~fields ~visited =
    let from_node =
      match get_weak_alias_type prop from_node.rc_node_exp with
      | Some typ ->
          {from_node with rc_node_typ= typ}
      | None ->
          from_node
    in
    let () = print_string "fields's size: "; print_int (List.length fields) ; print_string "\n"  in
    match fields with
    | [] ->
        found_cycles
    | (field, Predicates.Eexp (f_exp, f_inst)) :: el' ->
        let rc_field = {rc_field_name= field; rc_field_inst= f_inst} in
        let obj_edge = {rc_from= from_node; rc_field} in
        let edge = Object obj_edge in
        let visited' = from_node.rc_node_exp :: visited in
        let found_cycles' =
          (* found root, finish the cycle *)
          let () = edge_is_strong tenv obj_edge |> string_of_bool |> print_string ; print_string "???\n" in
          let () = Exp.equal f_exp root_node.rc_node_exp |> string_of_bool |> print_string ; print_string "!!!\n" in
          let () = Exp.pp F.std_formatter f_exp ; print_endline "" in
          let () = Exp.pp F.std_formatter root_node.rc_node_exp ; print_endline "" in
          if edge_is_strong tenv obj_edge && Exp.equal f_exp root_node.rc_node_exp then
            add_cycle found_cycles (edge :: rev_path) (* we already visited f_exp, stop *)
          else if List.mem ~equal:Exp.equal visited f_exp then found_cycles
          else
            (* cycle with a block *)
            let cycle_opt = get_cycle_blocks root_node f_exp in
            if edge_is_strong tenv obj_edge && Option.is_some cycle_opt then
              let procname, captured_var = Option.value_exn cycle_opt in
              let edge2 = Block (procname, captured_var) in
              let rev_path' = edge2 :: edge :: rev_path in
              add_cycle found_cycles rev_path'
            else
              match get_points_to f_exp with
              | None ->
                  found_cycles
              | Some (Predicates.Hpointsto (_, Estruct (new_fields, _), Exp.Sizeof {typ= te}))
                when edge_is_strong tenv obj_edge ->
                  let rc_to = {rc_node_exp= f_exp; rc_node_typ= te} in
                  dfs ~found_cycles ~root_node ~from_node:rc_to ~rev_path:(edge :: rev_path)
                    ~fields:new_fields ~visited:visited'
              | _ ->
                  found_cycles
        in
        dfs ~found_cycles:found_cycles' ~root_node ~from_node ~rev_path ~fields:el'
          ~visited:visited'
    | (_field, Predicates.Estruct (sub_fields, _inst)) :: el' ->
      print_endline "running in Estruct : ";
      Fieldname.pp F.std_formatter _field;
      print_endline "";
      let rec destruct fields el' =
        match fields with
        | [] -> el'
        | (field_name, Predicates.Eexp (f_exp, f_inst)) :: t ->
          destruct t ((field_name, Predicates.Eexp (f_exp, f_inst)) :: el')
        | (_field_name, Predicates.Estruct (sub_fields, _inst)) :: t ->
          destruct sub_fields [] @ destruct t el'
        (* | Predicates.Earray TODO: *)
        | _ -> el'
      in
      let el'' = destruct sub_fields el' in
      dfs ~found_cycles:found_cycles ~root_node ~from_node ~rev_path ~fields:el''
        ~visited:visited
    | _ ->
        found_cycles
  in
  match root with
  | Predicates.Hpointsto (e_root, Estruct (fl, _), Exp.Sizeof {typ= te})
    (* when Predicates.is_objc_object root *) ->
      let se_root = {rc_node_exp= e_root; rc_node_typ= te} in
      (* start dfs with empty path and expr pointing to root *)
      dfs ~found_cycles ~root_node:se_root ~from_node:se_root ~rev_path:[] ~fields:fl ~visited:[]
  | _ ->
      found_cycles


(** Find all the cycles available in prop, up to a limit of 10 *)
let get_retain_cycles tenv prop =
  let () = print_endline "get_retain_cycles" in
  let get_retain_cycles_with_root acc_set root =
    let cycles = get_cycles acc_set root tenv prop in
    let () =
      print_string "cycles size: " ;
      RetainCyclesType.Set.to_seq cycles |> Seq.length |> string_of_int |> print_string ;
      print_string "\n" in
    cycles
  in
  let sigma = prop.Prop.sigma in
  try List.fold ~f:get_retain_cycles_with_root sigma ~init:RetainCyclesType.Set.empty
  with Max_retain_cycles cycles -> cycles


let exn_retain_cycle tenv cycle =
  let retain_cycle = desc_retain_cycle tenv cycle in
  let cycle_dotty = Format.asprintf "%a" RetainCyclesType.pp_dotty cycle in
  if Config.debug_mode then (
    let rc_dotty_dir = ResultsDir.get_path RetainCycles in
    Utils.create_dir rc_dotty_dir ;
    let rc_dotty_file = Filename.temp_file ~in_dir:rc_dotty_dir "rc" ".dot" in
    RetainCyclesType.write_dotty_to_file rc_dotty_file cycle ) ;
  let desc =
    Localise.desc_retain_cycle retain_cycle (AnalysisState.get_loc_exn ()) (Some cycle_dotty)
  in
  Exceptions.Retain_cycle (desc, __POS__)


let report_cycle {InterproceduralAnalysis.proc_desc; tenv; err_log} prop =
  (* When there is a cycle in objc we ignore it only if it's empty or it has weak or
     unsafe_unretained fields.  Otherwise we report a retain cycle. *)
  let () = match proc_desc.attributes.proc_name with
  | Swift proc_name -> print_endline proc_name.method_name
  | _ -> print_endline ""
in 
  let () = print_string "report_cycle begin\n" in
  let cycles = get_retain_cycles tenv prop in
  RetainCyclesType.Set.iter RetainCyclesType.d_retain_cycle cycles ;
  if not (RetainCyclesType.Set.is_empty cycles) then (
    let () = print_endline "is not empty" in
    RetainCyclesType.Set.iter
      (fun cycle ->
        let exn = exn_retain_cycle tenv cycle in
        BiabductionReporting.log_issue_using_state proc_desc err_log exn )
      cycles ;
    (* we report the retain cycles above but need to raise an exception as well to stop the analysis *)
    raise (Exceptions.Analysis_stops (Localise.verbatim_desc "retain cycle found", Some __POS__)) )
      else 
        print_endline "is empty"
