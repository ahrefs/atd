#include "everything_atd.hpp"

#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/document.h> // Only for rapidjson::ParseResult
#include <rapidjson/error/en.h>

IntFloatParametrizedRecord IntFloatParametrizedRecord::from_json(const rapidjson::Document &doc)
{
    IntFloatParametrizedRecord record;
    if (doc.IsObject())
    {
        const rapidjson::Value &fieldA = doc["field_a"];
    
        record.field_a = _atd_read_int(doc["field_a"]);
        record.field_b = _atd_read_array<float, _atd_read_float>(doc["field_b"]);
    }

    return record;
}

IntFloatParametrizedRecord IntFloatParametrizedRecord::from_json_string(const std::string &json)
{
    rapidjson::Document doc;
    doc.Parse(json.c_str());

    if (doc.HasParseError())
    {
        throw AtdException("Error parsing JSON: " + std::string(rapidjson::GetParseError_En(doc.GetParseError())));
    }

    return from_json(doc);
}


std::string IntFloatParametrizedRecord::to_json_string() const
{
    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);

    writer.StartObject();

    writer.Key("field_a");
    _atd_write_int(field_a, writer);
    writer.Key("field_b");
    _atd_write_array<float, _atd_write_float>(field_b, writer);

    writer.EndObject();

    return buffer.GetString();
}


namespace
{
    void write(std::vector<int> obj, rapidjson::Writer<rapidjson::StringBuffer> &writer)
    {
        _atd_write_array<int, _atd_write_int>(obj, writer);
    }
}

std::string NestedNestedIntListRecord::to_json_string() const
{
    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);

    writer.StartObject();

    writer.Key("field_a");

    _atd_write_array<std::vector<int>, write>(field_a, writer);

    writer.EndObject();

    return buffer.GetString();
}

namespace __NNNIntListRecord
{
    typedef std::vector<std::vector<int>> T;

    void write_0(std::vector<int> obj, rapidjson::Writer<rapidjson::StringBuffer> &writer)
    {
        _atd_write_array<int, _atd_write_int>(obj, writer);
    }

    void write(T obj, rapidjson::Writer<rapidjson::StringBuffer> &writer)
    {
        _atd_write_array<std::vector<int>, write_0>(obj, writer);
    }

    std::vector<int> read_0(const rapidjson::Value &val)
    {
        return _atd_read_array<int, _atd_read_int>(val);
    }

    T read(const rapidjson::Value &val)
    {
        return _atd_read_array<std::vector<int>, read_0>(val);
    }
}

std::string NNNIntListRecord::to_json_string() const
{
    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);

    writer.StartObject();

    writer.Key("field_a");

    _atd_write_array<std::vector<std::vector<int>>, __NNNIntListRecord::write>(field_a, writer);

    writer.EndObject();

    return buffer.GetString();
}



NNNIntListRecord NNNIntListRecord::from_json(const rapidjson::Document &doc)
{
    NNNIntListRecord record;
    if (doc.IsObject())
    {
        const rapidjson::Value &fieldA = doc["field_a"];
    
        record.field_a = _atd_read_array<std::vector<std::vector<int>>, __NNNIntListRecord::read>(doc["field_a"]);
    }

    return record;
}

int main()
{
    std::string json = R"({"field_a":1134,"field_b":[1.1,2.2,7]})";

    try
    {
        IntFloatParametrizedRecord recordFromJson = IntFloatParametrizedRecord::from_json_string(json);
        std::cout << "IntFloatParametrizedRecord: " << IntFloatParametrizedRecord{123, {1.1, 2.2, 3.3}}.to_json_string() << std::endl;

        std::cout << "field_a: " << recordFromJson.field_a << std::endl;
        std::cout << "field_b: ";
        for (auto &val : recordFromJson.field_b)
        {
            std::cout << val << " ";
        }
        std::cout << std::endl;
    }
    catch(const std::exception& e)
    {
        std::cerr << e.what() << '\n';
    }

    return 0;
}