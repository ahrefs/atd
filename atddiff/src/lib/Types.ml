(*
   Type definitions used to build comparison results
*)

type direction = Forward | Backward | Both

type incompatibility_kind =
  | Missing_field of { field_name: string }
  | Missing_variant of { variant_name: string }
  | Missing_variant_argument of { variant_name: string }
  | Default_required of { field_name: string }
  | Incompatible_type
  | Deleted_root_type
  | Added_root_type
  | Parametrized_root_type

(*
   Important things we want to report:
   - the name of the root type definition
   - the location of the mismatch in the old file and in the new file:

       Old:
         type a = int
                  ^^^
       New:
         type a = b
         type b = int
                  ^^^
     Note the location of the mismatch is not necessarily in the root type
     definition.

   - no duplicates. The same mismatch can affect multiple root types:

       Old:
         type a = { x: c }
         type b = { x: c list }
         type c = int
                  ^^^
       New:
         type a = { x: c }
         type b = { x: c list }
         type c = float
                  ^^^^^
     The same int/float mismatch affects the root type definitions 'a' and 'b'.
     In this case, we would report one finding that affects 'a' and 'b'.

     Findings are deduplicated by their full contents i.e. we can have two
     different findings at the same location(s).
*)
type finding = {
  direction: direction;
  kind: incompatibility_kind;
  location_old: Atd.Ast.loc option;
  location_new: Atd.Ast.loc option;

  (* The description should not mention the affected root type definition
     so as to allow the deduplication of findings. *)
  description: string;
}

(*
   A result is a list of unique findings and the list of root types
   affected by the finding.

   For now, we don't try to identify root type renames so each finding is
   associated to just one root type name which exists in one or both versions
   of the file.
*)
type result = (finding * string list) list
