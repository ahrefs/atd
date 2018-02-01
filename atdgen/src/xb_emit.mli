
type 'a def = ('a, Biniou.biniou_repr) Mapping.def
type 'a grouped_defs = (bool * 'a def list) list

val check : _ grouped_defs -> unit
