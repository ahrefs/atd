// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

class Util {
  // Extract the tag of a sum-typed value.
  // Handles three encodings:
  //   - String: unit variant e.g. "Foo"
  //   - JSONArray: two-element array e.g. ["Foo", payload]
  //       (the default ATD encoding for tagged variants)
  //   - JSONObject: single-key object e.g. {"Foo": payload}
  //       (<json repr="object">, the Rust/Serde default externally-tagged
  //       encoding; also maps naturally to YAML as a single-key mapping)
  static String extractTag(Object value) throws JSONException {
    if (value instanceof String)
      return (String)value;
    else if (value instanceof JSONArray)
      return ((JSONArray)value).getString(0);
    else if (value instanceof JSONObject) {
      JSONObject obj = (JSONObject)value;
      if (obj.length() != 1)
        throw new JSONException("Expected single-key object for sum type");
      return obj.keys().next();
    }
    else throw new JSONException("Cannot extract type");
  }

  // Is an option value a none?
  static boolean isNone(Object value) throws JSONException {
    return (value instanceof String) && (((String)value).equals("None"));
  }

  // Is an option value a Some?
  static boolean isSome(Object value) throws JSONException {
    return (value instanceof JSONArray)
      && ((JSONArray)value).getString(0).equals("Some");
  }

  /*
    Encode a JSON string into a buffer
   */
  static void writeJsonString(StringBuilder out, String s) {
    out.append("\"");
    for (int i = 0; i < s.length(); ++i) {
      char c = s.charAt(i);
      switch (c) {
      case '\b':
        out.append("\\b");
        break;
      case '\f':
        out.append("\\f");
        break;
      case '\n':
        out.append("\\n");
        break;
      case '\r':
        out.append("\\r");
        break;
      case '\t':
        out.append("\\t");
        break;
      case '\\':
        out.append("\\\\");
        break;
      case '"':
        out.append("\\\"");
        break;
      default:
        if (c < 32 || c == 127)
          out.append(String.format("\\u%04x", (int) c));
        else
          out.append(c);
      }
    }
    out.append("\"");
  }

  static String jsonStringOfString(String s) {
    StringBuilder out = new StringBuilder();
    writeJsonString(out, s);
    return out.toString();
  }

  // Unescape escaped backslashes and double quotations.
  // All other escape sequences are considered invalid
  // (this is probably too strict).
  static String unescapeString(String str) throws JSONException {
    StringBuilder buf = new StringBuilder();
    for (int i = 0; i < str.length(); ++i) {
      if (str.charAt(i) == '\\') {
        if (i == str.length() - 1 ||
            (str.charAt(i + 1) != '\\' && str.charAt(i + 1) != '"'))
          throw new JSONException("Invalid escape");
        else {
          buf.append(str.charAt(i + 1));
          ++i;
        }
      } else {
        buf.append(str.charAt(i));
      }
    }
    return buf.toString();
  }
}
