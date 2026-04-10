// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

/**
 * Construct objects of type shape.
 */

public class Shape {
  Tag t = null;

  public Shape() {
  }

  public Tag tag() {
    return t;
  }

  /**
   * Define tags for sum type shape.
   */
  public enum Tag {
    CIRCLE, SQUARE, POINT
  }

  public Shape(Object o) throws JSONException {
    String tag = Util.extractTag(o);
    if (tag.equals("Circle")) {
      field_circle = ((JSONObject)o).getDouble("Circle");

      t = Tag.CIRCLE;
    }
    else if (tag.equals("Square")) {
      field_square = ((JSONObject)o).getDouble("Square");

      t = Tag.SQUARE;
    }
    else if (tag.equals("Point"))
      t = Tag.POINT;
    else
      throw new JSONException("Invalid tag: " + tag);
  }

  Double field_circle = null;
  public void setCircle(Double x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.CIRCLE;
    field_circle = x;
  }
  public Double getCircle() {
    if (t == Tag.CIRCLE)
      return field_circle;
    else
      return null;
  }

  Double field_square = null;
  public void setSquare(Double x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.SQUARE;
    field_square = x;
  }
  public Double getSquare() {
    if (t == Tag.SQUARE)
      return field_square;
    else
      return null;
  }

  public void setPoint() {
    /* TODO: clear previously-set field and avoid memory leak */
    t = Tag.POINT;
  }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    if (t == null)
      throw new JSONException("Uninitialized Shape");
    else {
      switch(t) {
      case CIRCLE:
         _out.append("{\"Circle\":");
         _out.append(String.valueOf(field_circle));
         _out.append("}");
         break;
      case SQUARE:
         _out.append("{\"Square\":");
         _out.append(String.valueOf(field_square));
         _out.append("}");
         break;
      case POINT:
        _out.append("\"Point\"");
        break;
      default:
        break; /* unused; keeps compiler happy */
      }
    }
  }

  public String toJson() throws JSONException {
    StringBuilder out = new StringBuilder(128);
    toJsonBuffer(out);
    return out.toString();
  }
}
