// Automatically generated; do not edit
package com.mylife.test;
import argonaut._, Argonaut._
/**
 * wibble
 */
case class ComplexRecord(
  b : Boolean,
  i : Integer,
  s : String,
  l : List[Boolean],
  sample_sum : SampleSum,
  class_ : Option[Integer],
  final_ : Option[Integer],
  kase : String,
  l2 : List[RecordWithDefaults],
) extends Atds {

  override def toJson: Json = Json(
    "b" := b,
    "i" := i,
    "s" := s,
    "l" := l,
    "sample_sum" := sample_sum,
    "class" := class_,
    "final" := final_,
    "case" := kase,
    "l2" := l2,
  )
}
