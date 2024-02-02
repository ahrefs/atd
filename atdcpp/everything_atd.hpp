#pragma once

#include <stdexcept>
#include <string>
#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>
#include <iostream>
#include <stdexcept>
#include <vector>
#include <string>

class AtdException : public std::exception
{
public:
    AtdException(const std::string &message) : msg_(message) {}

    const char *what() const throw() override
    {
        return msg_.c_str();
    }

private:
    std::string msg_;
};

// Forward declaration for utility functions using snake case
template <typename T>
T read_from_json(const rapidjson::Value &val);

template <typename T>
rapidjson::Value write_to_json(const T &value, rapidjson::Document::AllocatorType &allocator);

// Reading an integer from JSON
int _atd_read_int(const rapidjson::Value &val)
{
    if (!val.IsInt())
    {
        throw AtdException("Expected an integer");
    }
    return val.GetInt();
}

// Reading a float from JSON
float _atd_read_float(const rapidjson::Value &val)
{
    if (val.IsInt())
    {
        return static_cast<float>(val.GetInt());
    }
    else if (val.IsUint())
    {
        return static_cast<float>(val.GetUint());
    }
    if (!val.IsFloat())
    {
        throw AtdException("Expected a float");
    }

    return val.GetFloat();
}

template <typename T, T (*read_func)(const rapidjson::Value &)>
std::vector<T> _atd_read_array(const rapidjson::Value &val)
{
    if (!val.IsArray())
    {
        throw std::runtime_error("Expected an array"); // Or your specific exception type
    }

    std::vector<T> result;
    for (rapidjson::SizeType i = 0; i < val.Size(); i++)
    {
        result.push_back(read_func(val[i]));
    }

    return result;
}

void _atd_write_int(int value, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.Int(value);
}

void _atd_write_float(float value, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.Double(value);
}

template <typename T, void (*write_func)(T, rapidjson::Writer<rapidjson::StringBuffer>&)>
void _atd_write_array(const std::vector<T>& values, rapidjson::Writer<rapidjson::StringBuffer>& writer)
{
    writer.StartArray();
    for (const auto& value : values)
    {
        write_func(value, writer);
    }
    writer.EndArray();
}


struct IntFloatParametrizedRecord
{
    int field_a;
    std::vector<float> field_b;

    std::string to_json_string() const;
    static IntFloatParametrizedRecord from_json(const rapidjson::Document &doc);
    static IntFloatParametrizedRecord from_json_string(const std::string &json);
};

struct NestedNestedIntListRecord
{
    std::vector<std::vector<int>> field_a;

    std::string to_json_string() const;
    static NestedNestedIntListRecord from_json(const rapidjson::Document &doc);
    static NestedNestedIntListRecord from_json_string(const std::string &json);
};

struct NNNIntListRecord
{
    std::vector<std::vector<std::vector<int>>> field_a;

    std::string to_json_string() const;
    static NNNIntListRecord from_json(const rapidjson::Document &doc);
    static NNNIntListRecord from_json_string(const std::string &json);
};