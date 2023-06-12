import everything;

import std.traits;
import std.stdio;
import std.conv;
import core.exception;

void function()[string] tests;
bool testFailed = false;

void setupTests()
{
    tests["simpleRecord"] = {
        auto record = IntFloatParametrizedRecord(32, [5.4, 3.3]);

        auto json = record.toJson();
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
        assert(str == p.toJsonString);
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

    tests["everything"] = {
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
        obj.kinds = [
            Kind(WOW()), Kind(Thing(99)), Kind(Amaze(["a", "b"])), Kind(Root_())
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
        obj.parametrized_tuple = KindParametrizedTuple(
            tuple(Kind(WOW()), Kind(WOW()), 100));

        auto jsonStr = obj.toJsonString;
        auto newObj = jsonStr.fromJsonString!Root;

        assert(obj == newObj);
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
        auto child1 = RecursiveClass(1, true, []);
        auto child2 = RecursiveClass(2, true, []);
        auto a_obj = RecursiveClass(0, false, [child1, child2]);

        assert (a_obj == a_obj.toJsonString.fromJsonString!RecursiveClass);
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
