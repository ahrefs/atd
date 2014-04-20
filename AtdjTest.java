import java.io.*;
import java.util.*;
import org.json.*;
import org.junit.Test;
import static org.junit.Assert.*;
import com.mylife.test.*;

public class AtdjTest {
  @Test
  public void testU() throws JSONException {
    T t = new T();
    t.set_U();
    assertEquals(T.TTag.U, t.tag());

    X x = new X();
    assertEquals(null, x.o);
    x.o = true;
    assertEquals(true, x.o);

    t.set_X(x);
    assertEquals(T.TTag.X, t.tag());
    assertTrue(t.get_X() != null);
    assertTrue(t.get_V() == null);
  }

  @Test
  public void testV() throws JSONException {
    V v = new V("{\"b\": true, \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true], \"l\": [true, false], \"t\": \"U\", \"l2\":[]}");
    V v2 = new V("{\"b\": true, \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true], \"l\": [true, false], \"t\": [\"X\",true], \"l2\":[]}");

    assertEquals(true, v.b);
    v.b = false;
    assertEquals(42, (int)v.i);
    assertEquals("foo", v.s);
    assertEquals(2, v.l.size());
    assertEquals(true, v.l.get(0));
    assertEquals(false, v.l.get(1));
  }

  public static void main(String[] args) {
    org.junit.runner.JUnitCore.main("AtdjTest");
  }
}
