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
    // Credential credential = Credential::from_json(doc_from_json(R"({"name":"user","password":1234})"));
    // std::cout << "Credential: " << credential.to_json_string() << std::endl;

    // Credentials credentials = Credentials::from_json(doc_from_json(R"({"credentials":
    //     [{"name":"user1","password":1234},{"name":"user2","password":5678}]})"));

    // for (auto &credential : credentials.credentials)
    // {
    //     std::cout << "Credential: " << credential.to_json_string() << std::endl;
    // }

    // std::cout << "Credentials: " << credentials.to_json_string() << std::endl;

    // std::string NNNIntListRecord_json = R"({"field_a":[[[1,2,3],[4,4,6]],[[7,8,9],[10,11,12]]]})";

    // ThreeLevelNestedListRecord recordFromJson = ThreeLevelNestedListRecord::from_json(doc_from_json(NNNIntListRecord_json));

    // for (auto &vec1 : recordFromJson.field_a)
    // {
    //     std::cout << "vec1: " << std::endl;
    //     for (auto &vec2 : vec1)
    //     {
    //         std::cout << "vec2: " << std::endl;
    //         for (auto &val : vec2)
    //         {
    //             std::cout << val << " ";
    //         }
    //         std::cout << std::endl;
    //     }
    // }

    // std::cout << "ThreeLevelNestedListRecord: " << ThreeLevelNestedListRecord{{{{1, 2, 3}, {4, 5, 6}}, {{7, 8, 9}, {10, 18, 12}}}}.to_json_string() << std::endl;
    // std::cout << "Three..." << recordFromJson.to_json_string() << std::endl;


    // std::string RecursiveClass_json = R"({"id":1,"flag":true,"children":[{"id":2,"flag":false,"children":[]},{"id":3,"flag":true,"children":[{"id":4,"flag":false,"children":[]}]}]})";

    // RecursiveClass recordFromJson2 = RecursiveClass::from_json(doc_from_json(RecursiveClass_json));

    // std::cout << "RecursiveClass: " << recordFromJson2.to_json_string() << std::endl;


    // typedefs::Credentials2 credentials2 = Credentials2::from_json(doc_from_json(R"(
    //     [{"name":"user1","password":1234},{"name":"user2","password":5678}])"));

    // for (auto &credential : credentials2)
    // {
    //     std::cout << "Credential: " << credential.to_json_string() << std::endl;
    // }

    // std::cout << "Credentials: " << Credentials2::to_json_string(credentials2) << std::endl;


    // std::string pair_json = R"(["stringb", 1234])";
    // typedefs::Pair pair = Pair::from_json(doc_from_json(pair_json));

    // std::cout << "pair first: " << std::get<std::string>(pair) << std::endl;
    // std::cout << "pair second: " << std::get<int>(pair) << std::endl;

    // std::cout << "Pair: " << Pair::to_json_string(pair) << std::endl;
   
    // typedefs::Frozen frozen = Frozen::Types::A();
    // std::cout << "Frozen: " << Frozen::to_json_string(frozen) << std::endl;

    // std::string frozen_json = R"(["B", 1234])";
    // frozen = Frozen::from_json(doc_from_json(frozen_json));
    // std::cout << "Frozen: " << Frozen::to_json_string(frozen) << std::endl;

   // NNNIntListRecord recordFromJson = NNNIntListRecord::from_json(doc_from_json(NNNIntListRecord_json));
    // /// iterate through all vectors to print all values (3 level of nesting)
    // for (auto &vec1 : recordFromJson.field_a)
    // {
    //     for (auto &vec2 : vec1)
    //     {
    //         for (auto &val : vec2)
    //         {
    //             std::cout << val << " ";
    //         }
    //         std::cout << std::endl;
    //     }
    // }

    // std::cout << "NNNIntListRecord: " << NNNIntListRecord{{{{1, 2, 3}, {4, 5, 6}}, {{7, 8, 9}, {10, 18, 12}}}}.to_json_string() << std::endl;

    // return 0;
}