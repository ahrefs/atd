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

  case object SimpleTag extends SampleSum {
    def toJson: argonaut.Json = jString("Simple_tag")
  }

    case class Bool(data: Boolean) extends SampleSum {
      def toJson: argonaut.Json = argonaut.Json.array(
        jString("Bool"),
        data.asJson
      )
    }
    case class Int(data: Integer) extends SampleSum {
      def toJson: argonaut.Json = argonaut.Json.array(
        jString("Int"),
        data.asJson
      )
    }
    case class Float(data: Double) extends SampleSum {
      def toJson: argonaut.Json = argonaut.Json.array(
        jString("Float"),
        data.asJson
      )
    }
    case class String(data: String) extends SampleSum {
      def toJson: argonaut.Json = argonaut.Json.array(
        jString("String"),
        data.asJson
      )
    }
    case class SimpleRec(data: SimpleRecord) extends SampleSum {
      def toJson: argonaut.Json = argonaut.Json.array(
        jString("Simple_rec"),
        data.asJson
      )
    }
    case class ComplexRecord(data: ComplexRecord) extends SampleSum {
      def toJson: argonaut.Json = argonaut.Json.array(
        jString("Complex_record"),
        data.asJson
      )
    }
    case class RecordWithDefaults(data: RecordWithDefaults) extends SampleSum {
      def toJson: argonaut.Json = argonaut.Json.array(
        jString("Record_with_defaults"),
        data.asJson
      )
    }
}
