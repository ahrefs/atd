
type 'a def = ('a, Ag_biniou.biniou_repr) Ag_mapping.def
type 'a grouped_defs = (bool * 'a def list) list

val check : _ grouped_defs -> unit
