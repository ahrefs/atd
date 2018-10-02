(* Helper classes *)

open Atd.Import
open Atds_env

(* TODO: Extract to to a plain file? *)

let output_atds env =
  fprintf env.output "\
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
"
