(* Helper classes *)

open Atd.Import

(* TODO: Extract to to a plain file? *)

let output_atds env =
  let out = Atds_trans.open_class env "Atds" in
  fprintf out "\
/**
 * Common utility interface.
 */
trait Atds {
  /**
   * Get the Argonaut JSON representation
   */
  def toJson(): Json
}
";
  close_out out
