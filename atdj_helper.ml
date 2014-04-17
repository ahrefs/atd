(* Helper classes *)

open Printf
open Atdj_env

let output_atdj env =
  let out = Atdj_trans.open_class env "Atdj" in
  fprintf out "\
/**
 * Common utility interface.
 */
public interface Atdj {
  /**
   * Get the JSON string representation.
   * @return The JSON string.
   */
  String toString();
  /**
   * Get the JSON string representation, with each line indented.
   * @param indent The number of spaces to indent by.
   * @return The indented JSON string.
   */
  String toString(int indent);
  /**
   * Get the hash code.
   * @return The hash code.
   */
  int hashCode();
  /**
   * Accept a visitor.
   * @param visitor The visitor.
   * @return The same visitor, for convenience.
   */
  Visitor accept(Visitor visitor);
}";
  close_out out

let output_util env =
  let out = Atdj_trans.open_class env "Util" in
  fprintf out "\
class Util {
  static String indent(int n) {
    String str = \"\";
    for (int i = 0; i < n; ++i)
       str += \" \";
    return str;
  }

  static int compareTo(boolean[] xs, boolean[] ys) {
    int minLen = Math.min(xs.length, ys.length);
    for (int i = 0; i < minLen; ++i) {
      int cmp = new Boolean(xs[i]).compareTo(new Boolean(ys[i]));
      if (cmp != 0)
        return cmp;
    }
    if (xs.length < ys.length)
      return -1;
    else if (xs.length > ys.length)
      return 1;
    else
      return 0;
  }

  static int compareTo(int[] xs, int[] ys) {
    int minLen = Math.min(xs.length, ys.length);
    for (int i = 0; i < minLen; ++i) {
      int cmp = new Integer(xs[i]).compareTo(new Integer(ys[i]));
      if (cmp != 0)
        return cmp;
    }
    if (xs.length < ys.length)
      return -1;
    else if (xs.length > ys.length)
      return 1;
    else
      return 0;
  }

  static int compareTo(double[] xs, double[] ys) {
    int minLen = Math.min(xs.length, ys.length);
    for (int i = 0; i < minLen; ++i) {
      int cmp = new Double(xs[i]).compareTo(new Double(ys[i]));
      if (cmp != 0)
        return cmp;
    }
    if (xs.length < ys.length)
      return -1;
    else if (xs.length > ys.length)
      return 1;
    else
      return 0;
  }

  static int compareTo(String[] xs, String[] ys) {
    int minLen = Math.min(xs.length, ys.length);
    for (int i = 0; i < minLen; ++i) {
      int cmp = xs[i].compareTo(ys[i]);
      if (cmp != 0)
        return cmp;
    }
    if (xs.length < ys.length)
      return -1;
    else if (xs.length > ys.length)
      return 1;
    else
      return 0;
  }

  // Extract the tag of sum-typed value
  static String tag(Object value) throws JSONException {
    if (value instanceof String)
      return (String)value;
    else if (value instanceof JSONArray)
      return ((JSONArray)value).getString(0);
    else throw new JSONException(\"Cannot extract type\");
  }

  // Is an option value a none?
  static boolean isNone(Object value) throws JSONException {
    return (value instanceof String) && (((String)value).equals(\"None\"));
  }

  // Is an option value a Some?
  static boolean isSome(Object value) throws JSONException {
    return (value instanceof JSONArray)
      && ((JSONArray)value).getString(0).equals(\"Some\");
  }

  // Escape double quotes and backslashes
  static String escape(String str) {
    return str.replace(\"\\\\\", \"\\\\\\\\\").replace(\"\\\"\", \"\\\\\\\"\");
  }

  // Parse a JSON string, strictly
  static String parseJSONString(String str) throws JSONException {
    if (str.length() < 1 || str.charAt(0) != '\"')
      throw new JSONException(\"Expected '\\\"'\");
    for (int i = 1; i < str.length(); ++i)
      if (str.charAt(i) == '\"'
          && str.charAt(i - 1) != '\\\\'
          && i < str.length() - 1)
        throw new JSONException(\"Trailing characters\");
    if (str.length() < 2 || str.charAt(str.length() - 1) != '\"')
      throw new JSONException(\"Unterminated string '\\\"'\");
    return str.substring(1, str.length() - 1);
  }

  // Unescape escaped backslashes and double quotations.
  // All other escape sequences are considered invalid
  // (this is probably too strict).
  static String unescapeString(String str) throws JSONException {
    StringBuffer buf = new StringBuffer();
    for (int i = 0; i < str.length(); ++i) {
      if (str.charAt(i) == '\\\\') {
        if (i == str.length() - 1 ||
            (str.charAt(i + 1) != '\\\\' && str.charAt(i + 1) != '\"'))
          throw new JSONException(\"Invalid escape\");
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
";
  close_out out

let output_package_javadoc env (loc, annots) =
  let out = open_out (env.package_dir ^ "/" ^ "package.html") in
  output_string out "<body>\n";
  let from_doc_para acc para =
    List.fold_left
      (fun acc -> function
         | `Text text -> text :: acc
         | `Code _ -> failwith "Not yet implemented: code in javadoc comments"
      )
      acc
      para in
  let from_doc = function
    | `Text blocks ->
        List.fold_left
          (fun acc -> function
             | `Paragraph para -> from_doc_para acc para
             | `Pre _ ->
                 failwith "Not yet implemented: \
                           preformatted text in javadoc comments"
          )
          []
          blocks in
  (match Ag_doc.get_doc loc annots with
     | Some doc ->
         let str = String.concat "\n<p>\n" (List.rev (from_doc doc)) in
         output_string out str
     | _ -> ()
  );
  output_string out "\n</body>";
  close_out out
