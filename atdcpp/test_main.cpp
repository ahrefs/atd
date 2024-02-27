#include "everything_atd.hpp"

#include <rapidjson/document.h>
#include <rapidjson/error/en.h>

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


    typedefs::Credentials2 credentials2 = Credentials2::from_json(doc_from_json(R"(
        [{"name":"user1","password":1234},{"name":"user2","password":5678}])"));

    for (auto &credential : credentials2)
    {
        std::cout << "Credential: " << credential.to_json_string() << std::endl;
    }

    std::cout << "Credentials: " << Credentials2::to_json_string(credentials2) << std::endl;


    std::string pair_json = R"(["stringb", 1234])";
    typedefs::Pair pair = Pair::from_json(doc_from_json(pair_json));

    std::cout << "pair first: " << std::get<std::string>(pair) << std::endl;
    std::cout << "pair second: " << std::get<int>(pair) << std::endl;

    std::cout << "Pair: " << Pair::to_json_string(pair) << std::endl;
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