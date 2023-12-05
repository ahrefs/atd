import everything_atd;

import std.traits;
import std.stdio;
import std.conv;
import core.exception;

void function()[string] tests;
bool testFailed = false;

void setupTests()
{
    tests["basicTypes"] = {
        auto floatList = [0.1, 0.5, -0.8];
        auto jsonStr = floatList.toJsonString;

        assert(floatList.toJsonString == jsonStr.fromJsonString!(float[]).toJsonString);
    };

    tests["simpleRecord"] = {
        auto record = IntFloatParametrizedRecord(32, [5.4, 3.3]);

        auto json = record.toJson;
        auto recordFromJson = fromJson!IntFloatParametrizedRecord(json);

        assert(record == recordFromJson);
    };

    tests["simpleRecordMissingInt"] = {
        auto json = "{\"field_b\":[5.40000009536743164,3.29999995231628418]}";
        try
        {
            fromJsonString!IntFloatParametrizedRecord(json);
            assert(false);
        }
        catch (Exception e)
        {
            assert(true);
        }
    };

    tests["validPair"] = {

        auto str = "[\"hello\",2]";
        auto p = str.fromJsonString!Pair;
        assert(str == p.toJsonString!Pair);
    };

    tests["invalidPairWrongType"] = {

        assertThrows(
        { "[2, 3]".fromJsonString!Pair; }
        );
    };

    tests["invalidPairTooLong"] = {

        assertThrows(
        { "[\"hello\", 2, 3]".fromJsonString!Pair; }
        );
    };

    tests["everything"] = () {
        import std.typecons;
        import std.json;

        auto obj = Root();

        obj.x___init__ = 0.32f;
        obj.items = [[], [1, 2]];
        obj.extras = [17, 53];
        obj.aliased = [1, 6, 8];
        obj.maybe = 43;
        obj.point = tuple(4.3, 1.2);
        obj.assoc1 = [tuple(3.4f, 2), tuple(1.1f, 2)]; // Can be not ordered by key
        obj.assoc2 = [tuple("d", 3), tuple("e", 7)]; // Must be ordered by key because we lose ordering when writing
        obj.assoc3 = [4.4f: 4, 5.5f: 5];
        obj.assoc4 = ["g": 7, "h": 8];
        obj.kind = Root_().to!Kind;
        obj.kinds = [
            WOW().to!Kind, 99.to!Thing.to!Kind, ["a", "b"].to!Amaze.to!Kind, Root_().to!Kind
        ];
        obj.nullables = [
            12.Nullable!int, Nullable!int.init, Nullable!int.init,
            42.Nullable!int
        ];
        obj.options = [
            56.Nullable!int, Nullable!int.init, 78.Nullable!int
        ];
        obj.untyped_things = [
            JSONValue("hello"),
            JSONValue(),
            JSONValue(new int[string]),
            JSONValue(123)
        ];
        obj.parametrized_record = IntFloatParametrizedRecord(42, [9.9f, 8.8f]);
        obj.parametrized_tuple = KindParametrizedTuple(WOW(), WOW(), 100);

        () @safe {
            auto jsonStr = obj.toJsonString;
            auto newObj = jsonStr.fromJsonString!Root;

            assert(obj.toJsonString == newObj.toJsonString);
        }();
    };

    tests["defaultListWithThings"] = {
        auto obj = DefaultList();
        obj.items = [3, 4, 5, 6];

        auto newObj = obj.toJsonString.fromJsonString!DefaultList;

        assert(obj == newObj);
    };

    tests["defaultWithoutThings"] = {
        auto obj = DefaultList();

        auto newObj = obj.toJsonString.fromJsonString!DefaultList;
        assert(obj == newObj);
    };

    tests["defaultListInit"] = {
        auto json = "{}";

        json.fromJsonString!DefaultList;
    };

    tests["frozenDefaultA"] = {
        auto obj = Frozen();
        auto newObj = obj.toJsonString.fromJsonString!Frozen;
        assert(obj == newObj);
    };

    tests["frozenA"] = {
        auto obj = Frozen(A());

        auto newObj = obj.toJsonString.fromJsonString!Frozen;
        assert(obj == newObj);
    };

    tests["frozenB"] = {
        auto obj = Frozen(B(-43));

        auto newObj = obj.toJsonString.fromJsonString!Frozen;
        assert(obj == newObj);
    };

    tests["frozenBadJson"] = {
        auto json = "[\"B\"]";

        assertThrows({ json.fromJsonString!Frozen; });
    };

    tests["requireFieldLoop"] = {
        auto obj = RequireField("test");

        assert(obj == obj.toJsonString.fromJsonString!RequireField);
    };

    tests["requireFieldFails"] = {
        auto json = "{}";

        assertThrows({json.fromJsonString!RequireField;});
    };

    tests["requireFieldNotFail"] = {
        auto json = "{ \"req\" : \"hello\"}";

        json.fromJsonString!RequireField;
    };

    tests["recursiveClass"] = {
        import std.typecons;
        auto child = new Nullable!RecursiveClass(RecursiveClass(1, true, null));
        auto a_obj = RecursiveClass(0, false, child);

        assert (a_obj.toJsonString == a_obj.toJsonString.fromJsonString!RecursiveClass.toJsonString);
    };

    tests["using wrapped type"] = {
        auto og = RecordWithWrappedType(42);
        assert(og.toJsonString.fromJsonString!RecordWithWrappedType == og);
    };

    tests["should be able to deserialize into type name and used as base type"] = {
        Credentials credientials;

        auto json = `[{"name": "henry", "password": 123}, {"name":"mark", "password":42}]`;

        credientials = json.fromJsonString!Credentials;

        import std.algorithm;

        auto mapResult = credientials.map!((c) => c);
    };

    tests["variant enum"] = {
        Planet p = Planet.Earth;

        auto res = p.toJsonString.fromJsonString!Planet;

        assert(res.toJsonString == p.toJsonString);
    };

    tests["recursive variant"] = {
        import std.sumtype;
        import std.conv;

        auto v = RecursiveVariant(Int(43));
        auto r = RecordThatUsesRecursiveVariant(v, 42);
        auto vv = RecursiveVariant(Record(r));

        assert(vv.toJsonString == vv.toJsonString.fromJsonString!RecursiveVariant.toJsonString);
    };
}

void assertThrows(T)(T fn, bool writeMsg = false)
{
    try
    {
        fn();
        assert(false);
    }
    catch (Exception e)
    {
        if (writeMsg)
            writeln(e.msg);
        assert(true);
    }
}

int tryRunning(T)(T test, string name, bool verbose = false)
{
    try
    {
        test();
        if (verbose)
            writefln("\t✅ Test %s", name);

        return 1;
    }
    catch (Exception e)
    {
        writefln("\t❌ Test %s with: %s", name, e);
        return 0;
    }
}

int main()
{
    setupTests();
    writeln("Running tests...");

    int c = 0;
    foreach (key, value; tests)
        c += tryRunning(value, key, true);

    if (c != tests.length)
    {
        writefln("Failure, [%s/%s] tests passed", c, tests.length);
        return 1;
    }
    writefln("Tests successfully passed [%s/%s]", c, c);
    return 0;
}
