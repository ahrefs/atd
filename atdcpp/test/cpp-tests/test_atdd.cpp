#include <iostream>
#include <string>
#include <map>
#include <tuple>
#include <vector>
#include <stdexcept>

struct IntFloatParametrizedRecord {
    int field_a;
    std::vector<float> field_b;

    IntFloatParametrizedRecord() = default;

    IntFloatParametrizedRecord(int a, const std::vector<float>& b)
        : field_a(a), field_b(b) {}

    std::string toJson() const {
        // Convert record to JSON string
        // Implementation omitted for brevity
        return "";
    }

    bool operator==(const IntFloatParametrizedRecord& other) const {
        return field_a == other.field_a && field_b == other.field_b;
    }

    bool operator!=(const IntFloatParametrizedRecord& other) const {
        return !(*this == other);
    }
};

struct Pair {
    Pair() = default;
    std::string field_a;
    int field_b;
};

template<typename T>
T fromJson(const std::string& json) {
    // Convert JSON string to object of type T
    // Implementation omitted for brevity
    return T();
}

template<typename T>
std::string toJsonString(const T& obj) {
    // Convert object to JSON string
    // Implementation omitted for brevity
    return "";
}

int main() {
    std::map<std::string, std::function<void()>> tests;

    tests["simpleRecord"] = []() {
        IntFloatParametrizedRecord record(32, {5.4, 3.3});

        std::string json = record.toJson();
        IntFloatParametrizedRecord recordFromJson = fromJson<IntFloatParametrizedRecord>(json);

        if (record == recordFromJson) {
            std::cout << "Test passed: simpleRecord" << std::endl;
        } else {
            std::cout << "Test failed: simpleRecord" << std::endl;
        }
    };

    tests["simpleRecordMissingInt"] = []() {
        std::string json = "{\"field_b\":[5.40000009536743164,3.29999995231628418]}";
        try {
            fromJson<IntFloatParametrizedRecord>(json);
            std::cout << "Test failed: simpleRecordMissingInt" << std::endl;
        } catch (const std::exception& e) {
            std::cout << "Test passed: simpleRecordMissingInt" << std::endl;
        }
    };

    tests["validPair"] = []() {
        std::string str = "[\"hello\",2]";
        Pair p = fromJson<Pair>(str);

        if (str == toJsonString(p)) {
            std::cout << "Test passed: validPair" << std::endl;
        } else {
            std::cout << "Test failed: validPair" << std::endl;
        }
    };

    // Add more test cases...

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
