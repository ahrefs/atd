open Stdlib_extra
open Lexing


type loc = Lexing.position * Lexing.position

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

exception Atd_error of string

type module_ = {
  module_head: module_head;
  imports: import list;
  type_defs: type_def list;
}

and module_head = loc * annot

and annot = annot_section list

and annot_section = string * (loc * annot_field list)

and annot_field = string * (loc * string option)

and type_def = {
  loc: loc;
  name: type_name;
  param: type_param;
  annot: annot;
  value: type_expr;
  orig: type_def option;
}

and import = {
  loc: loc;
  path: string list;
  alias: string option;
  name: string;
  annot: annot
}

and type_param = string list

and type_expr =
  | Sum of loc * variant list * annot
  | Record of loc * field list * annot
  | Tuple of loc * cell list * annot
  | List of loc * type_expr * annot
  | Option of loc * type_expr * annot
  | Nullable of loc * type_expr * annot
  | Shared of loc * type_expr * annot
  | Wrap of loc * type_expr * annot
  | Name of loc * type_inst * annot
  | Tvar of loc * string
  (* List, Option, Nullable, Shared and Wrap are
     the only predefined types with a type
     parameter (and no special syntax). *)

and type_inst = loc * type_name * type_expr list

and type_name = TN of string list

and variant =
  | Variant of loc * (string * annot) * type_expr option
  | Inherit of loc * type_expr

and cell = loc * type_expr * annot

and field_kind =
  | Required
  | Optional
  | With_default

and simple_field = (loc * (string * field_kind * annot) * type_expr)

and field =
  | Field of simple_field
  | Inherit of (loc * type_expr)

type any =
  | Module of module_
  | Import of import
  | Type_def of type_def
  | Type_expr of type_expr
  | Variant of variant
  | Cell of cell
  | Field of field

let loc_of_type_expr = function
  | Sum (loc, _, _)
  | Record (loc, _, _)
  | Tuple (loc, _, _)
  | List (loc, _, _)
  | Option (loc, _, _)
  | Nullable (loc, _, _)
  | Shared (loc, _, _)
  | Wrap (loc, _, _)
  | Name (loc, _, _)
  | Tvar (loc, _) -> loc

let rec amap_type_expr f = function
  | Sum (loc, vl, a) ->  Sum (loc, List.map (amap_variant f) vl, f a)
  | Record (loc, fl, a) -> Record (loc, List.map (amap_field f) fl, f a)
  | Tuple (loc, tl, a) -> Tuple (loc, List.map (amap_cell f) tl, f a)
  | List (loc, t, a) -> List (loc, amap_type_expr f t, f a)
  | Option (loc, t, a) -> Option (loc, amap_type_expr f t, f a)
  | Nullable (loc, t, a) -> Nullable (loc, amap_type_expr f t, f a)
  | Shared (loc, t, a) -> Shared (loc, amap_type_expr f t, f a)
  | Wrap (loc, t, a) -> Wrap (loc, amap_type_expr f t, f a)
  | Tvar _ as x -> x
  | Name (loc, (loc2, name, args), a) ->
      Name (loc, (loc2, name, List.map (amap_type_expr f) args), f a)

and amap_variant f = function
  | Variant (loc, (name, a), o) ->
      let o = Option.map (amap_type_expr f) o in
      Variant (loc, (name, f a), o)
  | Inherit (loc, x) ->
      Inherit (loc, amap_type_expr f x)

and amap_field f = function
  | Field (loc, (name, kind, a), x) ->
      Field (loc, (name, kind, f a), amap_type_expr f x)
  | Inherit (loc, x) ->
      Inherit (loc, amap_type_expr f x)

and amap_cell f (loc, x, a) =
  (loc, amap_type_expr f x, f a)

let amap_import f (x : import) =
  { x with annot = f x.annot }

let amap_type_def f (x : type_def) : type_def =
  { x with value = amap_type_expr f x.value }

let amap_head f (loc, a) = (loc, f a)

let map_all_annot f (x : module_) =
  {
    module_head = amap_head f x.module_head;
    imports = List.map (amap_import f) x.imports;
    type_defs = List.map (amap_type_def f) x.type_defs;
  }

let set_type_expr_loc loc = function
  | Sum (_, a, b) -> Sum (loc, a, b)
  | Record (_, a, b) -> Record (loc, a, b)
  | Tuple (_, a, b) -> Tuple (loc, a, b)
  | List (_, a, b) -> List (loc, a, b)
  | Option (_, a, b) -> Option (loc, a, b)
  | Nullable (_, a, b) -> Nullable (loc, a, b)
  | Shared (_, a, b) -> Shared (loc, a, b)
  | Wrap (_, a, b) -> Wrap (loc, a, b)
  | Name (_, a, b) -> Name (loc, a, b)
  | Tvar (_, a) -> Tvar (loc, a)

let string_of_loc (pos1, pos2) =
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  Printf.sprintf "File %S, line %i, characters %i-%i"
    pos1.pos_fname line1
    (pos1.pos_cnum - start1)
    (pos2.pos_cnum - start1)

let error s = raise (Atd_error s)

let error_at loc s = error (string_of_loc loc ^ ":\n" ^ s)

let annot_of_type_expr = function
  | Sum (_, _, an)
  | Record (_, _, an)
  | Tuple (_, _, an)
  | List (_, _, an)
  | Option (_, _, an)
  | Nullable (_, _, an)
  | Shared (_, _, an)
  | Wrap (_, _, an)
  | Name (_, _, an) -> an
  | Tvar (_, _) -> []

let annot_of_variant (x : variant) =
  match x with
  | Variant (_, (_, an), _) -> an
  | Inherit _ -> []

let annot_of_field (x : field) =
  match x with
  | Field (_, (_, _, an), _) -> an
  | Inherit _ -> []

let map_annot f = function
  | Sum (loc, vl, a) ->  Sum (loc, vl, f a)
  | Record (loc, fl, a) -> Record (loc, fl, f a)
  | Tuple (loc, tl, a) -> Tuple (loc, tl, f a)
  | List (loc, t, a) -> List (loc, t, f a)
  | Option (loc, t, a) -> Option (loc, t, f a)
  | Nullable (loc, t, a) -> Nullable (loc, t, f a)
  | Shared (loc, t, a) -> Shared (loc, t, f a)
  | Wrap (loc, t, a) -> Wrap (loc, t, f a)
  | Tvar _ as x -> x
  | Name (loc, (loc2, name, args), a) ->
      Name (loc, (loc2, name, args), f a)

type visitor_hooks = {
  module_: (module_ -> unit) -> module_ -> unit;
  import: (import -> unit) -> import -> unit;
  type_def: (type_def -> unit) -> type_def -> unit;
  type_expr: (type_expr -> unit) -> type_expr -> unit;
  variant: (variant -> unit) -> variant -> unit;
  cell: (cell -> unit) -> cell -> unit;
  field: (field -> unit) -> field -> unit;
}

let rec visit_type_expr hooks x =
  let cont x =
    match x with
    | Sum (loc, vl, a) -> List.iter (visit_variant hooks) vl
    | Record (loc, fl, a) -> List.iter (visit_field hooks) fl
    | Tuple (loc, tl, a) -> List.iter (visit_cell hooks) tl
    | List (loc, t, a) -> visit_type_expr hooks t
    | Option (loc, t, a) -> visit_type_expr hooks t
    | Nullable (loc, t, a) -> visit_type_expr hooks t
    | Shared (loc, t, a) -> visit_type_expr hooks t
    | Wrap (loc, t, a) -> visit_type_expr hooks t
    | Tvar _ -> ()
    | Name (loc, (loc2, name, args), a) ->
        List.iter (visit_type_expr hooks) args
  in
  hooks.type_expr cont x

and visit_variant hooks x =
  let cont x =
    match (x : variant) with
    | Variant (loc, (name, a), o) ->
        (match o with
         | None -> ()
         | Some x -> visit_type_expr hooks x)
    | Inherit (loc, x) -> visit_type_expr hooks x
  in
  hooks.variant cont x

and visit_field hooks x =
  let cont x =
    match (x : field) with
    | Field (loc, (name, kind, a), x) -> visit_type_expr hooks x
    | Inherit (loc, x) -> visit_type_expr hooks x
  in
  hooks.field cont x

and visit_cell hooks x =
  let cont (loc, x, a) = visit_type_expr hooks x in
  hooks.cell cont x

let visit_import hooks x =
  hooks.import (fun _ -> ()) x

let visit_type_def hooks (x : type_def) =
  let cont x = visit_type_expr hooks x.value in
  hooks.type_def cont x

let visit_module hooks (x : module_) =
  let cont (x : module_) =
    List.iter (visit_import hooks) x.imports;
    List.iter (visit_type_def hooks) x.type_defs
  in
  hooks.module_ cont x

let visit
  ?(module_ = fun cont x -> cont x)
  ?(import = fun cont x -> cont x)
  ?(type_def = fun cont x -> cont x)
  ?(type_expr = fun cont x -> cont x)
  ?(variant = fun cont x -> cont x)
  ?(cell = fun cont x -> cont x)
  ?(field = fun cont x -> cont x)
  () =
  let hooks : visitor_hooks = {
    module_;
    import;
    type_def;
    type_expr;
    variant;
    cell;
    field;
  } in
  let visit (any : any) =
    match any with
    | Module x -> visit_module hooks x
    | Import x -> visit_import hooks x
    | Type_def x -> visit_type_def hooks x
    | Type_expr x -> visit_type_expr hooks x
    | Variant x -> visit_variant hooks x
    | Cell x -> visit_cell hooks x
    | Field x -> visit_field hooks x
  in
  visit

let fold_annot
  ?module_
  ?import
  ?type_def
  ?type_expr
  ?variant
  ?cell
  ?field
  any init =
  let acc = ref init in
  let fold opt_folder get_annot =
    match opt_folder with
    | None -> (fun cont x -> cont x)
    | Some f ->
        (fun cont x ->
           acc := f x (get_annot x) !acc;
           cont x)
  in
  let visitor =
    visit
      ~module_:(fold module_
                      (fun { module_head = (_, an); _ } -> an))
      ~import:(fold import (fun (x : import) -> x.annot))
      ~type_def:(fold type_def (fun (x : type_def) -> x.annot))
      ~type_expr:(fold type_expr annot_of_type_expr)
      ~variant:(fold variant annot_of_variant)
      ~cell:(fold cell (fun (_, _, an) -> an))
      ~field:(fold field annot_of_field)
      ()
  in
  visitor any;
  !acc

(* TODO: rewrite this more compactly using the visitor machinery above *)
let rec fold (f : type_expr -> 'a -> 'a) (x : type_expr) acc =
  let acc = f x acc in
  match x with
    Sum (_, variant_list, _annot) ->
      List.fold_right (fold_variant f) variant_list acc

  | Record (_, field_list, _annot) ->
      List.fold_right (fold_field f) field_list acc

  | Tuple (_, l, _annot) ->
      List.fold_right (fun (_, x, _) acc -> fold f x acc) l acc

  | List (_, type_expr, _annot) ->
      fold f type_expr acc

  | Option (_, type_expr, _annot) ->
      fold f type_expr acc

  | Nullable (_, type_expr, _annot) ->
      fold f type_expr acc

  | Shared (_, type_expr, _annot) ->
      fold f type_expr acc

  | Wrap (_, type_expr, _annot) ->
      fold f type_expr acc

  | Name (_, (_2, _name, type_expr_list), _annot) ->
      List.fold_right (fold f) type_expr_list acc

  | Tvar (_, _string) ->
      acc

and fold_variant f (x : variant) acc =
  match x with
    Variant (_, _, Some type_expr) -> fold f type_expr acc
  | Variant _ -> acc
  | Inherit (_, type_expr) -> fold f type_expr acc

and fold_field f (x : field) acc =
  match x with
    Field (_, _, type_expr) -> fold f type_expr acc
  | Inherit (_, type_expr) -> fold f type_expr acc


module Type_names = Set.Make (struct
  type t = type_name
  let compare = Stdlib.compare
end)

let extract_type_names ?(ignorable = []) x =
  let ign s = List.mem s ignorable in

  let add s set =
    if ign s then set
    else Type_names.add s set
  in

  let acc =
    fold (
      fun x acc ->
        match x with
          Name (_, (_, name, _), _) -> add name acc
        | _ -> acc
    )
      x Type_names.empty
  in
  Type_names.elements acc

let is_parametrized x =
  fold (fun x b -> b || match x with Tvar _ -> true | _ -> false) x false

let is_required = function
  | Optional
  | With_default -> false
  | Required -> true

let local_name_of_import (x : import) =
  match x.alias with
  | None -> x.name
  | Some local_name -> local_name

module Map = struct

  type mappers = {
    (* TODO: support other node types *)
    type_expr: type_expr -> type_expr;
  }

  let default_mappers = {
    type_expr = (fun x -> x);
  }

  let rec type_expr m (x : type_expr) : type_expr =
    match m.type_expr x with
    | Sum (loc, variant_list, an) ->
       Sum (loc, List.map (variant m) variant_list, an)

    | Record (loc, field_list, an) ->
       Record (loc, List.map (field m) field_list, an)

    | Tuple (loc, cells, an) ->
       let cells =
         List.map (fun (loc, x, an) -> (loc, type_expr m x, an)) cells in
       Tuple (loc, cells, an)

    | List (loc, x, an) ->
       List (loc, type_expr m x, an)

    | Option (loc, x, an) ->
       Option (loc, type_expr m x, an)

    | Nullable (loc, x, an) ->
       Nullable (loc, type_expr m x, an)

    | Shared (loc, x, an) ->
       Shared (loc, type_expr m x, an)

    | Wrap (loc, x, an) ->
       Wrap (loc, type_expr m x, an)

    | Name (loc, (loc2, name, args), an) ->
       let args = List.map (type_expr m) args in
       Name (loc, (loc2, name, args), an)

    | Tvar (_, _) as x -> x

  and variant m x =
    match x with
    | Variant (loc, name, Some x) -> Variant (loc, name, Some (type_expr m x))
    | Variant _ as x -> x
    | Inherit (loc, x) -> Inherit (loc, type_expr m x)

  and field m (x : field) =
    match x with
    | Field (loc, k, x) -> Field (loc, k, type_expr m x)
    | Inherit (loc, x) -> Inherit (loc, type_expr m x)

  let type_def m (x : type_def) : type_def =
    { x with value = type_expr m x.value }

  let module_ m { module_head; imports; type_defs } =
    let type_defs = List.map (type_def m) type_defs in
    { module_head; imports; type_defs }
end

let use_only_specific_variants x =
  let type_expr x =
    match x with
    | Name (loc, (loc2, name, [arg]), an) ->
        (match name with
        | TN ["list"] -> List (loc, arg, an)
        | TN ["option"] -> Option (loc, arg, an)
        | TN ["nullable"] -> Nullable (loc, arg, an)
        | TN ["shared"] -> Shared (loc, arg, an)
        | TN ["wrap"] -> Wrap (loc, arg, an)
        | _ -> x)

    | Name (loc, (loc2, name, _), an) as x ->
        x

    | Sum _
    | Record _
    | Tuple _
    | Tvar _
    | List _
    | Option _
    | Nullable _
    | Shared _
    | Wrap _ as x -> x
  in
  let mappers = { Map.type_expr } in
  Map.module_ mappers x

let use_only_name_variant x =
  let type_expr x =
    match x with
    | List (loc, arg, an) -> Name (loc, (loc, TN ["list"], [arg]), an)
    | Option (loc, arg, an) -> Name (loc, (loc, TN ["option"], [arg]), an)
    | Nullable (loc, arg, an) -> Name (loc, (loc, TN ["nullable"], [arg]), an)
    | Shared (loc, arg, an) -> Name (loc, (loc, TN ["shared"], [arg]), an)

    | Name _
    | Sum _
    | Record _
    | Tuple _
    | Tvar _
    | Wrap _ as x -> x
  in
  let mappers = { Map.type_expr } in
  Map.module_ mappers x

let create_import ~loc ~path ?alias ~annot () : import =
  let name =
    match List.rev path, alias with
    | [], _ -> invalid_arg "Ast.create_import: empty path"
    | name :: _, None -> name
    | _ :: _, Some override -> override
  in
  {
    loc;
    path;
    alias;
    name;
    annot;
  }
