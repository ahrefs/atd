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
    assertEquals(true, v.b);
    v.b = false;
    assertEquals(42, (int)v.i);
    assertEquals("foo", v.s);
    assertEquals(2, v.l.size());
    assertEquals(true, v.l.get(0));
    assertEquals(false, v.l.get(1));
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
    assertEquals(false, w.b);
    assertEquals(0, (int)w.i);
    assertEquals("", w.s);
    assertEquals(0, w.l.size());
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
  }

  @Test
  public void testXSome() throws JSONException {
    T t = TFactory.make("[\"X\", {\"o\" : true}]");
    assertTrue(t instanceof X1);

    X x = ((X1)t).value;
    assertEquals(true, x.o);
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
