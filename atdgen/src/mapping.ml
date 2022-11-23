open Atd.Stdlib_extra

type loc = Atd.Ast.loc

(*
  Generic mapping, based on the core ATD types
*)
type ('a, 'b) t =
  | Unit of loc * 'a * 'b
  | Bool of loc * 'a * 'b
  | Int of loc * 'a * 'b
  | Float of loc * 'a * 'b
  | String of loc * 'a * 'b
  | Abstract of loc * 'a * 'b
  | Sum of loc * ('a, 'b) variant_mapping array * 'a * 'b
  | Record of loc * ('a, 'b) field_mapping array * 'a * 'b
  | Tuple of loc * ('a, 'b) cell_mapping array * 'a * 'b
  | List of loc * ('a, 'b) t * 'a * 'b
  | Option of loc * ('a, 'b) t * 'a * 'b
  | Nullable of loc * ('a, 'b) t * 'a * 'b
  | Wrap of loc * ('a, 'b) t * 'a * 'b
  | Name of loc
            * Atd.Ast.type_name
            * ('a, 'b) t list
            * 'a * 'b
  | External of loc * string * ('a, 'b) t list * 'a * 'b
  | Tvar of loc * string

and ('a, 'b) cell_mapping = {
  cel_loc : loc;
  cel_value : ('a, 'b) t;
  cel_arepr : 'a;
  cel_brepr : 'b
}

and ('a, 'b) field_mapping = {
  f_loc : loc;
  f_name : string;
  f_kind : Atd.Ast.field_kind;
  f_value : ('a, 'b) t;
  f_arepr : 'a;
  f_brepr : 'b
}

and ('a, 'b) variant_mapping = {
  var_loc : loc;
  var_cons : string;
  var_arg : ('a, 'b) t option;
  var_arepr : 'a;
  var_brepr : 'b
}

type ('a, 'b) def = {
  def_loc : loc;
  def_name : string;
  def_param : string list;
  def_value : ('a, 'b) t option;
  def_arepr : 'a;
  def_brepr : 'b;
  def_orig : Atd.Ast.type_def;
}

module Type_param_map = Map.Make (String)
module Type_name_map = Map.Make (String)

let as_abstract = function
    Atd.Ast.Name (_, (loc, TN ["abstract"], l), a) ->
      if l <> [] then
        Error.error loc "\"abstract\" takes no type parameters";
      Some (loc, a)
  | _ ->
      None

let is_abstract x = as_abstract x <> None

let loc_of_mapping x =
  match (x : (_, _) t) with
    | Unit (loc, _, _)
    | Bool (loc, _, _)
    | Int (loc, _, _)
    | Float (loc, _, _)
    | String (loc, _, _)
    | Abstract (loc, _, _)
    | Sum (loc, _, _, _)
    | Record (loc, _, _, _)
    | Tuple (loc, _, _, _)
    | List (loc, _, _, _)
    | Option (loc, _, _, _)
    | Nullable (loc, _, _, _)
    | Wrap (loc, _, _, _)
    | Name (loc, _, _, _, _)
    | External (loc, _, _, _, _)
    | Tvar (loc, _) -> loc

let rec subst env (x : (_, _) t) =
  match x with
    Unit (_, _, _)
  | Bool (_, _, _)
  | Int (_, _, _)
  | Float (_, _, _)
  | String (_, _, _)
  | Abstract (_, _, _) -> x
  | Sum (loc, ar, a, b) ->
      Sum (loc, Array.map (subst_variant env) ar, a, b)
  | Record (loc, ar, a, b) ->
      Record (loc, Array.map (subst_field env) ar, a, b)
  | Tuple (loc, ar, a, b) ->
      Tuple (loc, Array.map (subst_cell env) ar, a, b)
  | List (loc, x, a, b) ->
      List (loc, subst env x, a, b)
  | Option (loc, x, a, b) ->
      Option (loc, subst env x, a, b)
  | Nullable (loc, x, a, b) ->
      Nullable (loc, subst env x, a, b)
  | Wrap (loc, x, a, b) ->
      Wrap (loc, subst env x, a, b)
  | Name (loc, name, args, a, b) ->
      Name (loc, name, List.map (subst env) args, a, b)
  | External (loc, name, args, a, b) ->
      External (loc, name, List.map (subst env) args, a, b)
  | Tvar (_, s) ->
      try Type_param_map.find s env
      with Not_found ->
        invalid_arg (sprintf "Mapping.subst_var: '%s" s)

and subst_variant env x =
  match x.var_arg with
      None -> x
    | Some v -> { x with var_arg = Some (subst env v) }

and subst_field env x =
  { x with f_value = subst env x.f_value }

and subst_cell env x =
  { x with cel_value = subst env x.cel_value }

(*
  Substitute type variables param in x by args
*)
let apply param x args =
  if List.length param <> List.length args then
    invalid_arg "Mapping.apply";
  let env =
    List.fold_left2
      (fun env var value -> Type_param_map.add var value env)
      Type_param_map.empty param args
  in
  subst env x


let rec find_name loc env visited name =
  if List.mem name visited then
    Error.error loc "Cyclic type definition"
  else
    match Type_name_map.find_opt name env with
    | Some (param, x) ->
        Some (param, deref_expr env (name :: visited) x)
    | None ->
        None

and deref_expr env visited x =
  match x with
  | Name (loc, name, args, _, _) ->
      (match name with
       | TN [simple_name] ->
           (match find_name loc env visited simple_name with
            | Some (param, x) ->
                apply param x args
            | None ->
                x)
       | TN _ -> x)
  | _ -> x

let make_deref
    (l : (bool * ('a, 'b) def list) list) :
    (('a, 'b) t -> ('a, 'b) t) =

  let defs =
    List.fold_left
      (fun env d ->
         match d.def_value with
             None -> env
           | Some v -> Type_name_map.add d.def_name (d.def_param, v) env)
      Type_name_map.empty (List.concat_map snd l) in

  fun x -> deref_expr defs [] x

(*
   Resolve names and unwrap `wrap` constructs
   (discarding annotations along the way)
*)
let rec unwrap (deref: ('a, 'b) t -> ('a, 'b) t) x =
  match deref x with
  | Wrap (_, x, _, _) -> unwrap deref x
  | x -> x
