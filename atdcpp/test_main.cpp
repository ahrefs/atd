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

    Credential credential = Credential::from_json(doc_from_json(R"({"name":"user","password":1234})"));
    std::cout << "Credential: " << credential.to_json_string() << std::endl;


    // std::string NNNIntListRecord_json = R"({"field_a":[[[1,2,3],[4,4,6]],[[7,8,9],[10,11,12]]]})";

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