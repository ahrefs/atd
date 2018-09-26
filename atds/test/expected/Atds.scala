// Automatically generated; do not edit
package com.mylife.test;
import argonaut._, Argonaut._
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
