(*
  Monomorphization of type expressions.

  The goal is to inline each parametrized type definition as much as possible,
  allowing code generators to create more efficient code directly:

  type ('a, 'b) t = [ Foo of 'a | Bar of 'b ]
  type int_t = (int, int) t

  becomes:

  type int_t = _1
  type _1 = [ Foo of int | Bar of int ]

  A secondary goal is to factor out type subexpressions in order for
  the code generators to produce less code:

  type x = { x : int list }
  type y = { y : int list option }

  becomes:

  type x = { x : _1 }
  type y = { y : _2 }
  type _1 = int list   (* `int list' now occurs only once *)
  type _2 = _1 option


  By default, only parameterless type definitions are returned.
  The [keep_poly] option allows to return parametrized type definitions as
  well.

  Input:

  type 'a abs = abstract
  type int_abs = int abs
  type 'a tree = [ Leaf of 'a | Node of ('a tree * 'a tree) ]
  type t = int tree
  type x = [ Foo | Bar ] tree

  Output (pseudo-syntax where quoted strings indicate unique type identifiers):

  type "int abs" = int abs
  type int_abs = "int abs"

  type 'a tree = [ Leaf of 'a | Node of ('a tree * 'a tree) ]
    (* only if keep_poly = true *)

  type "int tree" = [ Leaf of int | Node of ("int tree" * "int tree") ]
  type t = "int tree"
  type "[ Foo | Bar ] tree" =
    [ Leaf of [ Foo | Bar ]
    | Node of ("[ Foo | Bar ] tree" * "[ Foo | Bar ] tree") ]
  type x = "[ Foo | Bar ] tree"

*)

open Import

open Ast

module S = Stdlib.Set.Make (String)
module M = Stdlib.Map.Make (String)

(*
  Format of the table:
  key = type name (without arguments)
  value = (order in the file,
           number of parameters,
           original annotations of the right-hand type expression,
           original type definition,
           rewritten type definition)

  Every entry has an original type definition except the predefined
  atoms (int, string, etc.) and newly-created type definitions
  (type _1 = ...).
*)
let init_table () =
  let seqnum = ref 0 in
  let tbl = Hashtbl.create 20 in
  List.iter (
    fun (k, n, opt_td) ->
      incr seqnum;
      Hashtbl.add tbl k (!seqnum, n, opt_td, None)
  ) Predef.list;
  seqnum, tbl


let rec mapvar_expr
    (f : string -> string) (x : Ast.type_expr) : Ast.type_expr =
  match x with
    Sum (loc, vl, a) ->
      Sum (loc, List.map (mapvar_variant f) vl, a)
  | Record (loc, fl, a) ->
      Record (loc, List.map (mapvar_field f) fl, a)
  | Tuple (loc, tl, a) ->
      Tuple (loc,
             List.map (fun (loc, x, a) -> (loc, mapvar_expr f x, a)) tl,
             a)
  | List (loc, t, a) ->
      List (loc, mapvar_expr f t, a)
  | Name (loc, (loc2, "list", [t]), a) ->
      Name (loc, (loc2, "list", [mapvar_expr f t]), a)

  | Option (loc, t, a) ->
      Option (loc, mapvar_expr f t, a)
  | Name (loc, (loc2, "option", [t]), a) ->
      Name (loc, (loc2, "option", [mapvar_expr f t]), a)

  | Nullable (loc, t, a) ->
      Nullable (loc, mapvar_expr f t, a)
  | Name (loc, (loc2, "nullable", [t]), a) ->
      Name (loc, (loc2, "nullable", [mapvar_expr f t]), a)

  | Shared (loc, t, a) ->
      Shared (loc, mapvar_expr f t, a)
  | Name (loc, (loc2, "shared", [t]), a) ->
      Name (loc, (loc2, "shared", [mapvar_expr f t]), a)

  | Wrap (loc, t, a) ->
      Wrap (loc, mapvar_expr f t, a)
  | Name (loc, (loc2, "wrap", [t]), a) ->
      Name (loc, (loc2, "wrap", [mapvar_expr f t]), a)

  | Tvar (loc, s) -> Tvar (loc, f s)

  | Name (loc, (loc2, k, args), a) ->
      Name (loc, (loc2, k, List.map (mapvar_expr f) args), a)

and mapvar_field f = function
  | Field (loc, k, t) -> Field (loc, k, mapvar_expr f t)
  | Inherit (loc, t) -> Inherit (loc, mapvar_expr f t)

and mapvar_variant f = function
  | Variant (loc, k, opt_t) ->
      Variant (loc, k, (Option.map (mapvar_expr f) opt_t))
  | Inherit (loc, t) -> Inherit (loc, mapvar_expr f t)


let var_of_int i =
  let letter = i mod 26 in
  let number = i / 26 in
  let prefix = String.make 1 (Char.chr (letter + Char.code 'a')) in
  if number = 0 then prefix
  else prefix ^ string_of_int number

let vars_of_int n = List.init n var_of_int

let is_special s = String.length s > 0 && s.[0] = '@'


(*
  Standardize a type expression by numbering the type variables
  using the order in which they are encountered.

  input:

  (int, 'b, 'z) foo

  output:

  - new_name: "@(int, 'a, 'b) foo"
  - new_args: [ 'b; 'z ]
  - new_env: [ ('b, 'a); ('z, 'b) ]

  new_name and new_args constitute the type expression that replaces the
  original one:

  (int, 'b, 'z) foo   -->   ('b, 'z) "@(int, 'a, 'b) foo"


  new_env allows the substitution of the type variables of the original
  type expression into the type variables defined by the new type definition.
*)
let make_type_name loc orig_name args an =
  let tbl = Hashtbl.create 10 in
  let n = ref 0 in
  let mapping = ref [] in
  let assign_name s =
    try Hashtbl.find tbl s
    with Not_found ->
      let name = var_of_int !n in
      mapping := (s, name) :: !mapping;
      incr n;
      name
  in
  let normalized_args = List.map (mapvar_expr assign_name) args in
  let new_name =
    sprintf "@(%s)"
      (Print.string_of_type_name orig_name normalized_args an)
  in
  let mapping = List.rev !mapping in
  let new_args =
    List.map (fun (old_s, _) -> Tvar (loc, old_s)) mapping in
  let new_env =
    List.map (fun (old_s, new_s) -> old_s, Tvar (loc, new_s)) mapping
  in
  new_name, new_args, new_env

let is_abstract (x : type_expr) =
  match x with
    Name (_, (_, "abstract", _), _) -> true
  | _ -> false

let expr_of_lvalue loc name param annot =
  Name (loc, (loc, name, List.map (fun s -> Tvar (loc, s)) param), annot)


let is_cyclic lname t =
  match t with
    Name (_, (_, rname, _), _) -> lname = rname
  | _ -> false

let is_tvar = function
    Tvar _ -> true
  | _ -> false



let add_annot (x : type_expr) a : type_expr =
  Ast.map_annot (fun a0 -> Annot.merge (a @ a0)) x


let expand
    ?(keep_builtins = false) ?(keep_poly = false) (l : type_def list)
  : type_def list =

  let seqnum, tbl = init_table () in

  let rec subst env (t : type_expr) : type_expr =
    match t with
      Sum (loc, vl, a) ->
        Sum (loc, List.map (subst_variant env) vl, a)
    | Record (loc, fl, a) ->
        Record (loc, List.map (subst_field env) fl, a)
    | Tuple (loc, tl, a) ->
        Tuple (loc,
               List.map (fun (loc, x, a) -> (loc, subst env x, a)) tl, a)

    | List (loc as loc2, t, a)
    | Name (loc, (loc2, "list", [t]), a) ->
        let t' = subst env t in
        if keep_builtins then
          Name (loc, (loc2, "list", [t']), a)
        else
          subst_type_name loc loc2 "list" [t'] a

    | Option (loc as loc2, t, a)
    | Name (loc, (loc2, "option", [t]), a) ->
        let t' = subst env t in
        if keep_builtins then
          Name (loc, (loc2, "option", [t']), a)
        else
          subst_type_name loc loc2 "option" [t'] a

    | Nullable (loc as loc2, t, a)
    | Name (loc, (loc2, "nullable", [t]), a) ->
        let t' = subst env t in
        if keep_builtins then
          Name (loc, (loc2, "nullable", [t']), a)
        else
          subst_type_name loc loc2 "nullable" [t'] a

    | Shared (loc as loc2, t, a)
    | Name (loc, (loc2, "shared", [t]), a) ->
        let t' = subst env t in
        if keep_builtins then
          Name (loc, (loc2, "shared", [t']), a)
        else
          subst_type_name loc loc2 "shared" [t'] a

    | Wrap (loc as loc2, t, a)
    | Name (loc, (loc2, "wrap", [t]), a) ->
        let t' = subst env t in
        if keep_builtins then
          Name (loc, (loc2, "wrap", [t']), a)
        else
          subst_type_name loc loc2 "wrap" [t'] a

    | Tvar (_, s) as x -> Option.value (List.assoc s env) ~default:x

    | Name (loc, (loc2, name, args), a) ->
        let args' = List.map (subst env) args in
        if List.for_all is_tvar args' then
          Name (loc, (loc2, name, args'), a)
        else
          subst_type_name loc loc2 name args' a

  and subst_type_name loc loc2 name args an =
    (*
      Reduce the number of arguments of the type by creating
      an intermediate type, e.g.:
      ('x, int) t   becomes   'x "('a, int) t"
      and the following type is created:
      type 'a "('a, int) t" = ...


      input:
      - type name with arguments expressed in the environment where the
        type expression was extracted
      - annotations for that type expression

      output:
      - equivalent type expression valid in the same environment

      side-effects:
      - creation of a type definition for the output type expression.
    *)
    let new_name, new_args, new_env = make_type_name loc2 name args an in
    let n_param = List.length new_env in
    if not (Hashtbl.mem tbl new_name) then
      create_type_def loc name args new_env new_name n_param an;
    (*
      Return new type name with new arguments.
      The annotation has been transferred to the right-hand
      expression of the new type definition.
    *)
    Name (loc, (loc2, new_name, new_args), [])


  and create_type_def loc orig_name orig_args env name n_param an0 =
    (*
      Create the type definition needed to support the new type name
      [name] expecting [n_param] parameters.

      The right-hand side of the definition is obtained by looking up the
      definition for type [orig_name]:

      type ('a, 'b) t = [ Foo of 'a | Bar of 'b ]
      type 'c it = (int, 'c) t

      output:

      type ('a, 'b) t = [ Foo of 'a | Bar of 'b ]
      type 'a _1 = [ Foo of int | Bar of 'a ]  (* new name = _1, n_param = 1 *)
      type 'c it = 'c _1
    *)
    incr seqnum;
    let i = !seqnum in

    (* Create entry in the table, indicating that we are working on it *)
    Hashtbl.add tbl name (i, n_param, None, None);

    (* Get the original type definition *)
    let (_, _, orig_opt_td, _) =
      try Hashtbl.find tbl orig_name
      with Not_found ->
        assert false (* All original type definitions must
                        have been put in the table initially *)
    in
    let (td' : type_def) =
      match orig_opt_td with
        None ->
          assert false (* Original type definitions must all exist,
                          even for predefined types and abstract types. *)
      | Some (x : type_def) ->
          assert (x.name = orig_name);
          let new_params = vars_of_int n_param in
          let t = add_annot x.value an0 in
          let t = set_type_expr_loc loc t in

            (*
               First replace the type expression being specialized
               (orig_name, orig_args) by the equivalent expression
               in the new environment (variables 'a, 'b, ...)

               (int, 'b) foo  -->  (int, 'a) foo
            *)
          let args = List.map (subst env) orig_args in

            (*
              Then expand the expression into its definition,
              replacing each variable by the actual argument:

              original definition:

              type ('x, 'y) foo = [ Foo of 'x | Bar of 'y ]


              new definition:

              type 'a _1 = ...

              right-hand expression becomes:

              [ Foo of int | Bar of 'a ]

              using the following environment:

              'x -> int
              'y -> 'a

            *)
          let env = List.map2 (fun var value -> (var, value)) x.param args in

          let t' =
            if is_abstract t then
                (*
                  e.g.: type 'a t = abstract
                  use 'a t and preserve "t"
                *)
              let t =
                expr_of_lvalue loc orig_name x.param
                  (Ast.annot_of_type_expr t)
              in
              subst_only_args env t
            else
              let t' = subst env t in
              if is_cyclic name t' then
                subst_only_args env t
              else
                t'
          in
          ({ x with
             loc;
             name;
             param = new_params;
             annot = x.annot;
             value = t';
           } : type_def)
    in
    Hashtbl.replace tbl name (i, n_param, None, Some td')

  and subst_field env = function
    | Field (loc, k, t) -> Field (loc, k, subst env t)
    | Inherit (loc, t) -> Inherit (loc, subst env t)

  and subst_variant env = function
      Variant (loc, k, opt_t) as x ->
        (match opt_t with
           None -> x
         | Some t -> Variant (loc, k, Some (subst env t))
        )
    | Inherit (loc, t) -> Inherit (loc, subst env t)

  and subst_only_args env = function
      List (loc, t, a)
    | Name (loc, (_, "list", [t]), a) ->
        List (loc, subst env t, a)

    | Option (loc, t, a)
    | Name (loc, (_, "option", [t]), a) ->
        Option (loc, subst env t, a)

    | Nullable (loc, t, a)
    | Name (loc, (_, "nullable", [t]), a) ->
        Nullable (loc, subst env t, a)

    | Shared (loc, t, a)
    | Name (loc, (_, "shared", [t]), a) ->
        Shared (loc, subst env t, a)

    | Wrap (loc, t, a)
    | Name (loc, (_, "wrap", [t]), a) ->
        Wrap (loc, subst env t, a)

    | Name (loc, (loc2, name, args), an) ->
        Name (loc, (loc2, name, List.map (subst env) args), an)

    | _ -> assert false
  in

  (* first pass: add all original definitions to the table *)
  List.iter (
    fun (x : type_def) ->
      incr seqnum;
      let i = !seqnum in
      let n = List.length x.param in
      Hashtbl.add tbl x.name (i, n, Some x, None)
  ) l;

  (* second pass: perform substitutions and insert new definitions *)
  List.iter (
    fun (td : type_def) ->
      if td.param = [] || keep_poly then (
        let (i, n, _, _) =
          try Hashtbl.find tbl td.name
          with Not_found -> assert false
        in
        let t' = subst [] td.value in
        let td' = { td with value = t' } in
        Hashtbl.replace tbl td.name (i, n, Some td, Some td')
      )
  ) l;

  (* third pass: collect all parameterless definitions *)
  let l =
    Hashtbl.fold (
      fun _ (i, n, _, opt_td') l ->
        match opt_td' with
          None -> l
        | Some td' ->
            if n = 0 || keep_poly then (i, td') :: l
            else l
    ) tbl []
  in
  let l = List.sort (fun (i, _) (j, _) -> compare i j) l in
  List.map snd l



let replace_type_names (subst : string -> string) (t : type_expr) : type_expr =
  let rec replace (t : type_expr) : type_expr =
    match t with
      Sum (loc, vl, a) -> Sum (loc, List.map replace_variant vl, a)
    | Record (loc, fl, a) -> Record (loc, List.map replace_field fl, a)
    | Tuple (loc, tl, a) ->
        Tuple (loc, List.map (fun (loc, x, a) -> loc, replace x, a) tl, a)
    | List (loc, t, a) -> List (loc, replace t, a)
    | Option (loc, t, a) -> Option (loc, replace t, a)
    | Nullable (loc, t, a) -> Nullable (loc, replace t, a)
    | Shared (loc, t, a) -> Shared (loc, replace t, a)
    | Wrap (loc, t, a) -> Wrap (loc, replace t, a)
    | Tvar (_, _) as t -> t
    | Name (loc, (loc2, k, l), a) ->
        Name (loc, (loc2, subst k, List.map replace l), a)

  and replace_field = function
    | Field (loc, k, t) -> Field (loc, k, replace t)
    | Inherit (loc, t) -> Inherit (loc, replace t)

  and replace_variant = function
      Variant (loc, k, opt_t) as x ->
        (match opt_t with
           None -> x
         | Some t -> Variant (loc, k, Some (replace t))
        )
    | Inherit (loc, t) -> Inherit (loc, replace t)
  in
  replace t

(* Prefer MD5 over Hashtbl.hash because it won't change. *)
let hex_hash_string s =
  Digest.string s
  |> Digest.to_hex
  |> fun s -> String.sub s 0 7

(*
   Remove punctuation and non-ascii symbols from a name and replace them
   with underscores. The original case is preserved.
   The result is of the form [A-Za-z][A-Za-z0-9_]+.

   Example:

     "@((@(bool wrap_) * type_) option)" -> "bool_wrap_type_option"

   The original name can contain ATD annotations. It would be nice to
   ignore them but it's not clear how. Ideally we want this:

        "@(string list <ocaml valid='fun l -> true'>)"
     -> "string_list"

   But we get this:

     "true_6a9832c"

   Since it's misleading, when we see a suspected annotation, we
   use just "x" followed by a hash of the original contents.
   The hash has the property of making the name stable i.e. it is unlikely
   to change when unrelated type definitions change.

     "x_6a9832c"
*)
let suggest_good_name =
  let rex = Re.Pcre.regexp "([^a-zA-Z0-9])+" in
  fun name_with_punct ->
    let components =
      Re.Pcre.split ~rex name_with_punct
      |> List.filter ((<>) "")
    in
    let full_name = String.concat "_" components in
    let hash = hex_hash_string full_name in
    let name =
      if String.contains name_with_punct '<' then
        (* Avoid misleading names to due ATD annotations embedded in the
           type name. See earlier comments. *)
        "x_" ^ hash
      else if List.length components > 5 then
        (* Avoid insanely long type names *)
        match List.rev components with
        | [] -> assert false
        | [_] -> assert false
        | main :: rev_details ->
            (* Place the hash after the main name rather than before because
               it often starts with a digit which would have to be prefixed
               by an extra letter so it can be a valid name. *)
            main ^ "_" ^ hash
      else
        (* A full name that's not too long and makes sense such as
           'int_bracket' for the type 'int bracket'. *)
        String.concat "_" components
    in
    (* Ensure the name starts with a letter. *)
    if name = "" then "x"
    else
      match name.[0] with
      | 'a'..'z' | 'A'..'Z' -> name
      | _ (* digit *) -> "x" ^ name

let standardize_type_names
    ~prefix (defs : type_def list) : type_def list =
  let reserved_identifiers =
    List.map (fun (name, _, _) -> name) Predef.list
    @ List.filter_map (fun (x : type_def) ->
      let name = x.name in
      if is_special name then None
      else Some name
    ) defs
  in
  let name_registry =
    Unique_name.init
      ~reserved_identifiers
      ~reserved_prefixes:[]
      ~safe_prefix:""
  in
  (* The value v of the type is for extracting a good, short fallback name *)
  let new_id id =
    (* The leading underscore is used to identify generated type names
       in other places. *)
    Unique_name.translate
      name_registry
      ~preferred_translation:(prefix ^ suggest_good_name id)
      id
  in
  let replace_name k =
    if is_special k then
      new_id k
    else
      k
  in
  let defs =
    List.map (
      fun (x : type_def) ->
        { x with name = replace_name x.name }
    ) defs
  in
  let subst id =
    match Unique_name.translate_only name_registry id with
    | Some x -> x
    | None ->
        (* must have been defined as abstract *)
        id
  in
  List.map (fun (x : type_def) ->
    { x with value = replace_type_names subst x.value }
  ) defs

let expand_type_defs
    ?(prefix = "_")
    ?keep_builtins
    ?keep_poly
    ?(debug = false)
    td_list =
  let td_list = expand ?keep_poly td_list in
  if debug then td_list
  else standardize_type_names ~prefix td_list
