(* Helper classes *)

open Atd.Import
open Atdjs_env

(* TODO: Extract to to a plain file? *)

let output_atdjs env =
  fprintf env.output "\
/**
 * Common utility interface.
 */
trait Atdjs {

  /**
   * Get the Argonaut JSON representation.
   * Please use the argonaut encoder rather than calling this directly.
   */
  protected def toArgonaut: argonaut.Json

  // These may be optimized later, and the dependency on Argonaut could be removed.

  /**
   * Get the JSON string representation.
   * @return The JSON string.
   */
  def toJson: String = toArgonaut.nospaces

  /**
   * Write the JSON representation to a buffer.
   */
  def toJsonBuffer(out: java.lang.StringBuilder): Unit = out.append(toJson)

}

object Atdjs {

  implicit def argonautCodecAtdjs[A <: Atdjs] = new argonaut.EncodeJson[A] {
    override def encode(a: A) = a.toArgonaut
  }

}
"
