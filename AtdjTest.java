import java.io.*;
import java.util.*;
import org.json.*;
import org.junit.Test;
import static org.junit.Assert.*;
import com.mylife.test.*;

public class AtdjTest {
  @Test
  public void testU() throws JSONException {
    T t = TFactory.make("\"U\"");
    assertTrue(t instanceof U);
  }

  @Test
  public void testUEquals() throws JSONException {
    T t1 = TFactory.make("\"U\"");
    T t2 = TFactory.make("\"U\"");
    T t3 = TFactory.make("[\"W\", {\"e\":\"Alpha\"}]");
  }

  @Test
  public void testV() throws JSONException {
    T t = TFactory.make("[\"V\", {\"b\": true, \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true], \"l\": [true, false], \"l2\":[]}]");
    assertTrue(t instanceof V1);

    V v = ((V1)t).value;
    assertEquals(v.b, true);
    v.b = false;
    assertEquals(v.i, 42);
    assertEquals(v.s, "foo");
    assertTrue(v.o.is_set);
    assertEquals(v.o.value, true);
    assertEquals(v.l.size(), 2);
    assertEquals(v.l.get(0), true);
    assertEquals(v.l.get(1), false);
  }

  @Test
  public void testVEquals() throws JSONException {
    T t1  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, false], \"l2\":[]}]");
    T t2  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, false], \"l2\":[]}]");
    T t3  = TFactory.make("[\"V\", {\"b\": false, \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, false], \"l2\":[]}]");
    T t4  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 43, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, false], \"l2\":[]}]");
    T t5  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"bar\", \"o\": [\"Some\", true],  \"l\": [true, false], \"l2\":[]}]");
    T t6  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", false], \"l\": [true, false], \"l2\":[]}]");
    T t7  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": \"None\",          \"l\": [true, false], \"l2\":[]}]");
    T t8  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, true], \"l2\":[]}]");
    T t9  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [], \"l2\":[]}]");
    T t10 = TFactory.make("\"U\"");

  }

  @Test
  public void testW() throws JSONException {
    T t = TFactory.make("[\"W\", {\"e\":\"Alpha\"}]");
    assertTrue(t instanceof W1);

    W w = ((W1)t).value;
    assertEquals(w.b, false);
    assertEquals(w.i, 0);
    assertEquals(w.s, "");
    assertEquals(w.o.is_set, false);
    assertEquals(w.l.size(), 0);
  }

  @Test
  public void testWEquals() throws JSONException {
    T t1 = TFactory.make("[\"W\", {\"e\":\"Alpha\"}]");
    T t2 = TFactory.make("[\"W\", {\"e\":\"Alpha\"}]");
    T t3 = TFactory.make("\"U\"");
  }

  @Test
  public void testXNone() throws JSONException {
    T t = TFactory.make("[\"X\", {}]");
    assertTrue(t instanceof X1);

    X x = ((X1)t).value;
    assertEquals(x.o.is_set, false);
  }

  @Test
  public void testXSome() throws JSONException {
    T t = TFactory.make("[\"X\", {\"o\" : true}]");
    assertTrue(t instanceof X1);

    X x = ((X1)t).value;
    assertEquals(x.o.is_set, true);
    assertEquals(x.o.value, true);
  }

  @Test
  public void testXEquals() throws JSONException {
    T t1 = TFactory.make("[\"X\", {}]");
    T t2 = TFactory.make("[\"X\", {}]");
    T t3 = TFactory.make("[\"X\", {\"o\" : true}]");
    T t4 = TFactory.make("\"U\"");
  }

  public static void main(String[] args) {
    org.junit.runner.JUnitCore.main("AtdjTest");
  }
}
