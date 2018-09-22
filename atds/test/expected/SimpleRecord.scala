// Automatically generated; do not edit
package com.mylife.test;
import argonaut._, Argonaut._
case class SimpleRecord(
  int_field: Integer,
  opt: Option[Boolean],
) extends Atds {

  override def toJson: Json = Json(
    "int_field" := int_field,
    "opt" := opt,
  )
}
