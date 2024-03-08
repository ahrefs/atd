#include "everything_atd.hpp"

#include <rapidjson/document.h>
#include <rapidjson/error/en.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/prettywriter.h>
#include <iostream>
#include <string>

const rapidjson::Document doc_from_json(const std::string &json)
{
    rapidjson::Document doc;
    doc.Parse(json.c_str());

    if (doc.HasParseError())
    {
    }

    return doc;
}

int main()
{
    using namespace atd;
    Root root;

    root.id = "id long";
    root.await = false;
    root.integer = 43;
    root.x___init__ = 3.14;
    root.float_with_auto_default = 90.03;
    root.float_with_default = 32.1;
    root.items = {{1, 2}, {-1, -2}};
    root.maybe = 422;
    root.extras = {34, 12};
    root.answer = 12;
    root.aliased = {55, 44};
    root.point = {4.4, 1.1};
    root.kind = Kind::Types::Root();
    root.kinds = {Kind::Types::Amaze({{"one", "two"}}), Kind::Types::Root(), Kind::Types::Root(), Kind::Types::Thing({1})};
    root.assoc1 = {{4.12, 1},{2.2, 2}};
    root.assoc2 = {{"first", 1}, {"second", 2}};
    root.assoc3 = {{1.1, 1}, {2.2, 2}};
    root.assoc4 = {{"firstt", 1}, {"secondd", 2}};
    root.nullables = {1, std::nullopt, 3};
    root.options = {1, 2, std::nullopt};
    root.parametrized_record = {2, {1.0, 1.1}};
    root.parametrized_tuple = {Kind::Types::Root(), Kind::Types::WOW(), 9};
    root.wrapped = 1;
    root.aaa = -90;
    root.item = 45;

    std::string json = root.to_json_string();

    // now you turn json into pretty json string
    rapidjson::Document doc;
    doc.Parse(json.c_str());
    rapidjson::StringBuffer buffer;
    rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    std::cout << "Root: " << buffer.GetString() << std::endl;

    std::cout << "Root: " << json << std::endl;
}