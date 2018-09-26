// Automatically generated; do not edit
package com.mylife.test;
import argonaut._, Argonaut._
/**
 * Construct objects of type e.
 */
sealed abstract class E extends Atds

  /**
   * Define tags for sum type e.
   */
object E {

  case object Alpha extends E {
    def toJson: argonaut.Json = jString("Alpha")
  }

  case object Beta extends E {
    def toJson: argonaut.Json = jString("Beta")
  }

}
