#include <iostream>
#include <string>
#include <map>
#include <tuple>
#include <vector>
#include <stdexcept>
#include "everything_atd.hpp"

int main() {
    using namespace atd;
    
    std::map<std::string, std::function<void()>> tests;

    tests["simpleRecord"] = []() {
        IntFloatParametrizedRecord record{32, {5.4, 3.3}};

        std::string json = record.to_json_string();
        IntFloatParametrizedRecord recordFromJson = IntFloatParametrizedRecord::from_json_string(json);

        if (json == recordFromJson.to_json_string()) {
            std::cout << "Test passed: simpleRecord" << std::endl;
        } else {
            std::cout << "Test failed: simpleRecord" << std::endl;
        }
    };

    // Add the test for the Root object serialization and deserialization
    tests["rootObjectSerialization"] = []() {
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
        Root rootFromJson = Root::from_json_string(json);

        if (json == rootFromJson.to_json_string()) {
            std::cout << "Test passed: rootObjectSerialization" << std::endl;
        } else {
            std::cout << "Test failed: rootObjectSerialization" << std::endl;
        }
    };

    std::cout << "Running tests..." << std::endl;

    int passed = 0;
    for (const auto& test : tests) {
        try {
            test.second();
            passed++;
            std::cout << "✅ Test " << test.first << std::endl;
        } catch (const std::exception& e) {
            std::cout << "❌ Test " << test.first << " with: " << e.what() << std::endl;
        }
    }

    if (passed == tests.size()) {
        std::cout << "All tests passed" << std::endl;
    } else {
        std::cout << "Failure, " << passed << "/" << tests.size() << " tests passed" << std::endl;
    }

    return 0;
}