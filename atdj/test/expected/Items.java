// Automatically generated; do not edit
package com.mylife.test;
import org.json.*;

public class Items implements Atdj {
  /**
   * Construct a fresh empty list.
   */
  public Items() {
    value = new java.util.ArrayList<SimpleRecord>();
  }

  /**
   * Construct from an existing list.
   */
  public Items(java.util.ArrayList<SimpleRecord> arr) {
    value = arr;
  }

  /**
   * Construct from a JSON string.
   */
  public Items(String s) throws JSONException {
    this(new JSONArray(s));
  }

  Items(JSONArray ja) throws JSONException {
    value = new java.util.ArrayList<SimpleRecord>();
    for (int _i = 0; _i < ja.length(); ++_i) {
      JSONObject _tmp = ja.getJSONObject(_i);
      value.add(new SimpleRecord(_tmp));
    }
  }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    _out.append("[");
    for (int i = 0; i < value.size(); ++i) {
      value.get(i).toJsonBuffer(_out);
      if (i < value.size() - 1)
        _out.append(",");
    }
    _out.append("]");
  }

  public String toJson() throws JSONException {
    StringBuilder out = new StringBuilder(128);
    toJsonBuffer(out);
    return out.toString();
  }

  public java.util.ArrayList<SimpleRecord> value;
}
