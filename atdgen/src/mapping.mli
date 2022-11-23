(** Generic decorated ATD AST.

    Decorating a tree means adding extra information to the nodes
    of an existing tree.

    The generic types provided here take two type parameters.
    They are meant to accommodate options relevant to two data representations
    (e.g. ocaml and json), so that we can produce code that converts
    between those two representations.
*)

type loc = Atd.Ast.loc

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

  (* Original (source) type definition before any specialization *)
  def_orig: Atd.Ast.type_def;
}

val as_abstract : Atd.Ast.type_expr -> (loc * Atd.Ast.annot) option

val is_abstract : Atd.Ast.type_expr -> bool

val loc_of_mapping : ('a, 'b) t -> loc

val make_deref
  : (bool * ('a, 'b) def list) list
  -> ('a, 'b) t
  -> ('a, 'b) t

val unwrap
  : (('a, 'b) t -> ('a, 'b) t)
  -> ('a, 'b) t
  -> ('a, 'b) t
