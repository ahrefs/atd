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
    namespace field_0
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
}

std::string NNNIntListRecord::to_json_string() const
{
    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);

    writer.StartObject();

    writer.Key("field_a");

    _atd_write_array<std::vector<std::vector<int>>, __NNNIntListRecord::field_0::write>(field_a, writer);

    writer.EndObject();

    return buffer.GetString();
}

NNNIntListRecord NNNIntListRecord::from_json(const rapidjson::Document &doc)
{
    NNNIntListRecord record;
    if (doc.IsObject())
    {
        const rapidjson::Value &fieldA = doc["field_a"];

        record.field_a = _atd_read_array<std::vector<std::vector<int>>, __NNNIntListRecord::field_0::read>(doc["field_a"]);
    }

    return record;
}

const rapidjson::Document doc_from_json(const std::string &json)
{
    rapidjson::Document doc;
    doc.Parse(json.c_str());

    if (doc.HasParseError())
    {
        throw AtdException("Error parsing JSON: " + std::string(rapidjson::GetParseError_En(doc.GetParseError())));
    }

    return doc;
}

int main()
{
    std::string NNNIntListRecord_json = R"({"field_a":[[[1,2,3],[4,4,6]],[[7,8,9],[10,11,12]]]})";

    NNNIntListRecord recordFromJson = NNNIntListRecord::from_json(doc_from_json(NNNIntListRecord_json));
    /// iterate through all vectors to print all values (3 level of nesting)
    for (auto &vec1 : recordFromJson.field_a)
    {
        for (auto &vec2 : vec1)
        {
            for (auto &val : vec2)
            {
                std::cout << val << " ";
            }
            std::cout << std::endl;
        }
    }

    std::cout << "NNNIntListRecord: " << NNNIntListRecord{{{{1, 2, 3}, {4, 5, 6}}, {{7, 8, 9}, {10, 18, 12}}}}.to_json_string() << std::endl;

    return 0;
}