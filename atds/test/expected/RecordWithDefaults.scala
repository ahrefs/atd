// Automatically generated; do not edit
package com.mylife.test;
import argonaut._, Argonaut._
case class RecordWithDefaults(
  b : Boolean = false,
  i : Integer = 0,
  s : String = "",
  l : List[Boolean] = Nil,
  e : E,
) extends Atds {

  override def toJson: Json = Json(
    "b" := b,
    "i" := i,
    "s" := s,
    "l" := l,
    "e" := e,
  )
}
