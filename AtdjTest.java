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

    assertTrue(t1.equals(t1));
    assertTrue(t1.compareTo(t1) == 0);
    assertTrue(t1.equals(TFactory.make(t1.toString())));

    assertTrue(t1.equals(t2));
    assertTrue(t2.equals(t1));
    assertTrue(t2.compareTo(t1) == 0);
    assertTrue(t1.compareTo(t2) == 0);
    assertTrue(t2.equals(TFactory.make(t2.toString())));
    assertTrue(t1.hashCode() == t2.hashCode());

    assertFalse(t1.equals(t3));
    assertFalse(t3.equals(t1));
    assertFalse(t1.hashCode() == t3.hashCode());
    assertTrue(t1.compareTo(t3) < 0);
    assertTrue(t3.compareTo(t1) > 0);
    assertTrue(t3.equals(TFactory.make(t3.toString())));
  }

  @Test
  public void testV() throws JSONException {
    T t = TFactory.make("[\"V\", {\"b\": true, \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true], \"l\": [true, false]}]");
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
    T t1  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, false]}]");
    T t2  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, false]}]");
    T t3  = TFactory.make("[\"V\", {\"b\": false, \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, false]}]");
    T t4  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 43, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, false]}]");
    T t5  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"bar\", \"o\": [\"Some\", true],  \"l\": [true, false]}]");
    T t6  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", false], \"l\": [true, false]}]");
    T t7  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": \"None\",          \"l\": [true, false]}]");
    T t8  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": [true, true]}]");
    T t9  = TFactory.make("[\"V\", {\"b\": true,  \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true],  \"l\": []}]");
    T t10 = TFactory.make("\"U\"");

    assertTrue(t1.equals(t1));
    assertTrue(t1.compareTo(t1) == 0);
    assertTrue(t1.equals(TFactory.make(t1.toString())));
    assertTrue(t1.equals(TFactory.make(t1.toString())));

    assertTrue(t1.equals(t2));
    assertTrue(t2.equals(t1));
    assertTrue(t1.hashCode() == t2.hashCode());
    assertTrue(t1.compareTo(t2) == 0);
    assertTrue(t2.compareTo(t1) == 0);
    assertTrue(t2.equals(TFactory.make(t2.toString())));

    assertFalse(t1.equals(t3));
    assertFalse(t3.equals(t1));
    assertFalse(t1.hashCode() == t3.hashCode());
    assertTrue(t1.compareTo(t3) > 0);
    assertTrue(t3.compareTo(t1) < 0);
    assertTrue(t3.equals(TFactory.make(t3.toString())));

    assertFalse(t1.equals(t4));
    assertFalse(t4.equals(t1));
    assertFalse(t1.hashCode() == t4.hashCode());
    assertTrue(t1.compareTo(t4) < 0);
    assertTrue(t4.compareTo(t1) > 0);
    assertTrue(t4.equals(TFactory.make(t4.toString())));

    assertFalse(t1.equals(t5));
    assertFalse(t5.equals(t1));
    assertFalse(t1.hashCode() == t5.hashCode());
    assertTrue(t1.compareTo(t5) > 0);
    assertTrue(t5.compareTo(t1) < 0);
    assertTrue(t5.equals(TFactory.make(t5.toString())));

    assertFalse(t1.equals(t6));
    assertFalse(t6.equals(t1));
    assertFalse(t1.hashCode() == t6.hashCode());
    assertTrue(t1.compareTo(t6) > 0);
    assertTrue(t6.compareTo(t1) < 0);
    assertTrue(t6.equals(TFactory.make(t6.toString())));

    assertFalse(t1.equals(t7));
    assertFalse(t7.equals(t1));
    assertFalse(t1.hashCode() == t7.hashCode());
    assertTrue(t1.compareTo(t7) > 0);
    assertTrue(t7.compareTo(t1) < 0);
    assertTrue(t7.equals(TFactory.make(t7.toString())));

    assertFalse(t1.equals(t8));
    assertFalse(t8.equals(t1));
    assertFalse(t1.hashCode() == t8.hashCode());
    assertTrue(t1.compareTo(t8) < 0);
    assertTrue(t8.compareTo(t1) > 0);
    assertTrue(t8.equals(TFactory.make(t8.toString())));

    assertFalse(t1.equals(t9));
    assertFalse(t9.equals(t1));
    assertFalse(t1.hashCode() == t9.hashCode());
    assertTrue(t1.compareTo(t9) > 0);
    assertTrue(t9.compareTo(t1) < 0);
    assertTrue(t9.equals(TFactory.make(t9.toString())));

    assertFalse(t1.equals(t10));
    assertFalse(t10.equals(t1));
    assertFalse(t1.hashCode() == t10.hashCode());
    assertTrue(t1.compareTo(t10) > 0);
    assertTrue(t10.compareTo(t1) < 0);
    assertTrue(t10.equals(TFactory.make(t10.toString())));
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

    assertTrue(t1.equals(t1));
    assertTrue(t1.compareTo(t1) == 0);
    assertTrue(t1.equals(TFactory.make(t1.toString())));

    assertTrue(t1.equals(t2));
    assertTrue(t2.equals(t1));
    assertTrue(t1.hashCode() == t2.hashCode());
    assertTrue(t1.compareTo(t2) == 0);
    assertTrue(t2.compareTo(t1) == 0);
    assertTrue(t2.equals(TFactory.make(t2.toString())));

    assertFalse(t1.equals(t3));
    assertFalse(t3.equals(t1));
    assertFalse(t1.hashCode() == t3.hashCode());
    assertTrue(t1.compareTo(t3) > 0);
    assertTrue(t3.compareTo(t1) < 0);
    assertTrue(t3.equals(TFactory.make(t3.toString())));
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

    assertTrue(t1.equals(t1));
    assertTrue(t1.compareTo(t1) == 0);
    assertTrue(t1.equals(TFactory.make(t1.toString())));

    assertTrue(t1.equals(t2));
    assertTrue(t2.equals(t1));
    assertTrue(t1.hashCode() == t2.hashCode());
    assertTrue(t1.compareTo(t2) == 0);
    assertTrue(t2.compareTo(t1) == 0);
    assertTrue(t2.equals(TFactory.make(t2.toString())));

    assertFalse(t1.equals(t3));
    assertFalse(t3.equals(t1));
    assertFalse(t1.hashCode() == t3.hashCode());
    assertTrue(t1.compareTo(t3) < 0);
    assertTrue(t3.compareTo(t1) > 0);
    assertTrue(t3.equals(TFactory.make(t3.toString())));

    assertFalse(t1.equals(t4));
    assertFalse(t4.equals(t1));
    assertFalse(t1.hashCode() == t4.hashCode());
    assertTrue(t1.compareTo(t4) > 0);
    assertTrue(t4.compareTo(t1) < 0);
    assertTrue(t4.equals(TFactory.make(t4.toString())));
  }

  public static void main(String[] args) {
    org.junit.runner.JUnitCore.main("AtdjTest");
  }
}
