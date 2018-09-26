// Automatically generated; do not edit
package com.mylife.test;
import argonaut._, Argonaut._
/**
 * Construct objects of type sample_sum.
 */
sealed abstract class SampleSum extends Atds

  /**
   * Define tags for sum type sample_sum.
   */
object SampleSum {

  case class SimpleTag() extends SampleSum {

    def toJson: argonaut.Json =
      jString("Simple_tag")
  }

}
