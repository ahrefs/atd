import java.io.*;
import java.util.*;
import org.json.*;
import org.junit.Test;
import static org.junit.Assert.*;
import com.mylife.test.*;

public class AtdjTest {
  @Test
  public void testU() throws JSONException {
    SampleSum s = new SampleSum();

    boolean errorDetected;
    try {
      s.toJson();
      errorDetected = false;
    } catch (JSONException e) {
      errorDetected = true;
    }
    assertTrue(errorDetected);

    s.setU();
    assertEquals(SampleSumTag.U, s.tag());

    SimpleRecord r = new SimpleRecord();
    assertEquals(null, r.o);
    r.o = true;
    assertEquals(true, r.o);

    s.setSimpleRecord(r);
    assertEquals(SampleSumTag.SIMPLE_RECORD, s.tag());
    assertTrue(s.getSimpleRecord() != null);
    assertTrue(s.getV() == null);
  }

  @Test
  public void testV() throws JSONException {
    V v = new V("{\"b\": true, \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true], \"l\": [true, false], \"sample_sum\": \"U\", \"l2\":[]}");
    V v2 = new V("{\"b\": true, \"i\": 42, \"s\": \"foo\", \"o\": [\"Some\", true], \"l\": [true, false], \"sample_sum\": [\"Simple_record\",{\"o\":true}], \"l2\":[]}");

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
