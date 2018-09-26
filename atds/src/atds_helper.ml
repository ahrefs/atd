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
  def toJson: Json
}
object Atds {
  implicit def argonautCodecAtds[A <: Atds] = new argonaut.EncodeJson[A] {
    override def encode(a: A) = a.toJson
  }
}
";
  close_out out
