import everything;

void testeasy()
{
    auto record = IntFloatParametrizedRecord(32, [5.4, 3.3]);

    auto json = record.toJson();
    auto recordFromJson = fromJson!IntFloatParametrizedRecord(json);

    assert(record == recordFromJson);
}

void recordMissingInt()
{
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
}

int main()
{
    testeasy();
    recordMissingInt();
    return 0;
}