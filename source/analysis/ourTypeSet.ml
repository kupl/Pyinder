(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

let weaken_typ typ =
  let weaken_typ = Type.weaken_literals typ in
  let weaken_typ =
    match weaken_typ with
    | Type.IntExpression _ -> Type.Primitive "int"
    | _ -> weaken_typ
  in
  weaken_typ

module ClassSummary = struct
  type t = (Reference.t, Refinement.Store.t) Base.Hashtbl.t

  let create () = Hashtbl.create (module Reference)

  
  let extract_self store =
    Refinement.Store.update_with_filter ~old_store:Refinement.Store.empty ~new_store:store ~filter:(fun reference _ -> 
      String.is_suffix ~suffix:"$self" (Reference.last reference)
    )

  let extract_element_type store =
    let filter_element_type (typ: Type.t) =
      match typ with
      | Bottom | Any | Top -> print_endline "NOPE"; false
      | _ -> print_endline "GOTCHA"; true
    in
    let rec fold_unit u =
      let base = 
        match Refinement.Unit.base u with
        | Some base -> 
          if filter_element_type base.annotation then Some base else None
        | None -> None
      in
      let attributes = 
        Map.Tree.fold (Refinement.Unit.attributes u) ~init:(Identifier.Map.Tree.empty) ~f:(fun ~key ~data u ->
          Identifier.Map.Tree.set u ~key ~data:(fold_unit data)
        )
      in
      Refinement.Unit.create_all base attributes
    in
    Format.printf "[ Before Extract ] \n\n%a \n\n" Refinement.Store.pp store;
    let store = Refinement.Store.fold_map ~f:(fun ~key:reference ~data:unit store ->
      (*Format.printf "[ ITER ] \n\n%a \n\n%a \n\n" Reference.pp reference Refinement.Unit.pp unit;*)
      Reference.Map.set store ~key:reference ~data:(fold_unit unit)
    ) Refinement.Store.empty store
    in
    let x = Refinement.Store.update_with_filter ~old_store:Refinement.Store.empty ~new_store:store ~filter:(fun _ typ -> 
      (*Format.printf "%a \n\n%a \n\n" Reference.pp reference Annotation.pp typ;*)
        (match typ.annotation with
        | Bottom | Any | Top -> false
        | _ -> true
        )
    )
    in
    (*Format.printf "[ After Extract ] \n\n%a \n\n" Refinement.Store.pp x;*)
    x
  
  let set t ~key ~data =
    Hashtbl.set t ~key ~data:(extract_element_type (extract_self data))

  let meet ~global_resolution (t: t) class_name method_postcondition =
    let current_postcondition = Hashtbl.find t class_name in
    match current_postcondition with
    | Some postcondition ->
      set t ~key:class_name ~data:(Refinement.Store.meet ~global_resolution postcondition method_postcondition)
    | None -> 
      set t ~key:class_name ~data:method_postcondition
    

  let outer_join ~global_resolution (t: t) class_name method_postcondition =
    Format.printf "[ Before SET ] \n\n%a \n\n" Refinement.Store.pp method_postcondition;
    let current_postcondition = Hashtbl.find t class_name |> Option.value ~default:(Refinement.Store.empty) in
    set t ~key:class_name ~data:(Refinement.Store.outer_join ~global_resolution current_postcondition method_postcondition)

  let outer_widen ~global_resolution (t: t) class_name method_postcondition =
    let current_postcondition = Hashtbl.find t class_name |> Option.value ~default:(Refinement.Store.empty) in
    Hashtbl.set t ~key:class_name ~data:(Refinement.Store.outer_widen ~global_resolution current_postcondition method_postcondition ~iteration:0 ~widening_threshold:3)

  let pp format table =
    Hashtbl.iteri table ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Class Info ]]] \n %a \n -> %a \n" Reference.pp key Refinement.Store.pp data
    )

  let pp_json table =
    let json_string =
    Hashtbl.fold table ~init:"" ~f:(fun ~key ~data json_string ->
      json_string ^ (Format.asprintf {|{"%a" : {%a},|} Reference.pp key Refinement.Store.to_yojson data)
    )
    in
    let json_string = String.chop_suffix_exn json_string ~suffix:"," in
    let json_string = json_string ^ "}" in
    json_string

  let equal t1 t2 =
    Hashtbl.equal (fun ref1 ref2 -> Refinement.Store.equal ref1 ref2) t1 t2

  let copy t = Hashtbl.copy t
end

module ArgTypes = struct
  type t = (Identifier.t, Type.t) Base.Hashtbl.t

  let create () = Hashtbl.create (module Identifier)

  let add_arg_type t ident typ =
    let modified_typ = weaken_typ typ in
    let exn_typ = Hashtbl.find t ident |> Option.value ~default:modified_typ in
    match exn_typ with
    | Bottom | Any | Top -> ()
    | _ ->
      Hashtbl.set t ~key:ident ~data:(Type.union [modified_typ; exn_typ])
(*
  let modify_arg_typ t ident typ =
    Hashtbl.set t ~key:ident ~data:typ
*)

  let equal t1 t2 =
    Hashtbl.equal (fun typ1 typ2 -> Type.equal typ1 typ2) t1 t2

  let pp format t =
    Hashtbl.iteri t ~f:(fun ~key ~data ->
      Format.fprintf format "%a -> %a \n" Identifier.pp key Type.pp data;
    )

  let export_to_resolution t resolution = 
    Hashtbl.fold t ~init:resolution ~f:(fun ~key ~data resolution ->
        Resolution.new_local resolution ~reference:(Reference.create key) ~annotation:(Annotation.create_immutable data)
    )
     
end

module FunctionSummary = struct
  type t = {
    arg_types : ArgTypes.t;
    return_types : Type.t;
  }

  let create () = { arg_types = ArgTypes.create (); return_types = Type.Bottom; }

    (*
  let extract_self store =
    Refinement.Store.update_with_filter ~old_store:Refinement.Store.empty ~new_store:store ~filter:(fun reference _ -> 
      String.is_suffix ~suffix:"$self" (Reference.last reference)
    )

    
  let meet_refinement ~global_resolution self_types method_postcondition =
      Refinement.Store.meet ~global_resolution self_types method_postcondition

  let meet_type return_types actual =
    Type.union [return_types; actual]

  let meet_summary ~global_resolution {self_types=left_self_types; return_types=left_return_types;} {self_types=right_self_types; return_types=right_return_types;} =
    {
      self_types = meet_refinement ~global_resolution left_self_types right_self_types;
      return_types = meet_type left_return_types right_return_types;
    }
    *)
  let equal {arg_types=ref1; return_types=typ1;} {arg_types=ref2; return_types=typ2;} =
      (ArgTypes.equal ref1 ref2) && (Type.equal typ1 typ2)

  let pp format {arg_types; return_types;} =
    Format.fprintf format "%a \nreturn : %a \n" ArgTypes.pp arg_types Type.pp return_types

  let add_arg_types {arg_types; _} arg_typ_list =
    List.iter arg_typ_list ~f:(fun (arg, typ) -> ArgTypes.add_arg_type arg_types arg typ)

  let add_return_info {arg_types; return_types} return_type =
    let modified_typ = weaken_typ return_type in
    {arg_types; return_types=Type.union [return_types; modified_typ]}

  let get_arg_types {arg_types; _} = arg_types

  let get_return_types {return_types; _} = return_types
end

module FunctionTable = struct
  type t = (Reference.t, FunctionSummary.t) Base.Hashtbl.t
  let create () = Hashtbl.create (module Reference)

  let copy t = Hashtbl.copy t
  let equal t1 t2 =
    Hashtbl.equal (fun fs1 fs2 -> FunctionSummary.equal fs1 fs2) t1 t2

  let pp format table =
    Hashtbl.iteri table ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Function Info ]]] \n%a \n%a \n" Reference.pp key FunctionSummary.pp data
    )

  let add_arg_types t reference arg_typ_list =
    let func_summary = Hashtbl.find t reference |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.add_arg_types func_summary arg_typ_list;
    Hashtbl.set t ~key:reference ~data:func_summary

  let add_return_info t func return_type =
    let func_summary = Hashtbl.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    Hashtbl.set t ~key:func ~data:(FunctionSummary.add_return_info func_summary return_type)

  let get_func_arg_types t func_name =
    let func_summary = Hashtbl.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_arg_types func_summary

  let get_func_return_types t func_name =
    let func_summary = Hashtbl.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_return_types func_summary

end

module OurSummary = struct
  type t = {
    class_summary : ClassSummary.t;
    function_table : FunctionTable.t;
    current_function : Reference.t option;
  }
  [@@deriving equal]

  let create () = {class_summary = ClassSummary.create (); function_table = FunctionTable.create (); current_function=None}

  let class_summary {class_summary; _} = class_summary

  let set_current_function {class_summary; function_table; _} current_function = {class_summary; function_table; current_function=Some current_function}

  let copy {class_summary; function_table; current_function;} = 
    {
      class_summary = ClassSummary.copy class_summary;
      function_table = FunctionTable.copy function_table;
      current_function;
    }
  
  let equal {class_summary=cs1; function_table=ft1; _} {class_summary=cs2; function_table=ft2; _} =
    (ClassSummary.equal cs1 cs2) && (FunctionTable.equal ft1 ft2)

  let pp_class format {class_summary; _} =
    Format.fprintf format "%a" ClassSummary.pp class_summary
  let pp_func format {function_table; _} = 
    Format.fprintf format "%a" FunctionTable.pp function_table

  let add_arg_types { function_table; _} reference arg_typ_list =
    FunctionTable.add_arg_types function_table reference arg_typ_list

  let add_return_info {function_table; current_function; _} return_type =
    match current_function with
    | Some func ->
      FunctionTable.add_return_info function_table func return_type
    | None -> ()

  let get_func_arg_types {function_table; _} func_name =
    FunctionTable.get_func_arg_types function_table func_name

  let get_func_return_types {function_table; _} func_name =
    FunctionTable.get_func_return_types function_table func_name
end

let our_model = ref (OurSummary.create ());;