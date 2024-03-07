
// Generated by atdcpp from type definitions in everything.atd.
// This implements classes for the types defined in 'everything.atd', providing
// methods and functions to convert data from/to JSON.

// ############################################################################

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
#include <map>
#include <variant>


#include <stdint.h>
namespace atd {
// forward declarations
namespace RecursiveVariant::Types {
    struct Integer;
    struct Rec;
}
struct RecursiveRecord2;
struct RecursiveClass;
struct ThreeLevelNestedListRecord;
struct StructWithRecursiveVariant;
namespace Kind::Types {
    struct Root;
    struct Thing;
    struct WOW;
    struct Amaze;
}
struct IntFloatParametrizedRecord;
struct Root;
struct RequireField;
struct RecordWithWrappedType;
struct NullOpt;
namespace Frozen::Types {
    struct A;
    struct B;
}
struct EmptyRecord;
struct DefaultList;
struct Credential;
struct Credentials;


namespace typedefs {
    typedef RecursiveRecord2 RecursiveRecord2;
    typedef RecursiveClass RecursiveClass;
    typedef ThreeLevelNestedListRecord ThreeLevelNestedListRecord;
    typedef StructWithRecursiveVariant StructWithRecursiveVariant;
    typedef IntFloatParametrizedRecord IntFloatParametrizedRecord;
    typedef Root Root;
    typedef RequireField RequireField;
    typedef RecordWithWrappedType RecordWithWrappedType;
    typedef NullOpt NullOpt;
    typedef EmptyRecord EmptyRecord;
    typedef DefaultList DefaultList;
    typedef Credential Credential;
    typedef Credentials Credentials;

    typedef std::variant<atd::RecursiveVariant::Types::Integer, atd::RecursiveVariant::Types::Rec> RecursiveVariant;
    typedef std::variant<atd::Kind::Types::Root, atd::Kind::Types::Thing, atd::Kind::Types::WOW, atd::Kind::Types::Amaze> Kind;
    typedef std::variant<atd::Frozen::Types::A, atd::Frozen::Types::B> Frozen;

    typedef int St;
    typedef uint32_t Alias3;
    typedef typedefs::Alias3 AliasOfAliasNotWrapped;
    typedef typedefs::AliasOfAliasNotWrapped AliasOfAliasOfAlias;
    typedef std::vector<int> Alias;
    typedef std::tuple<typedefs::Kind, typedefs::Kind, int> KindParametrizedTuple;
    typedef uint32_t Password;
    typedef std::tuple<std::string, int> Pair;
    typedef std::vector<typedefs::Credential> Credentials2;
    typedef uint16_t AliasOfAlias;
    typedef std::vector<int> Alias2;
} // namespace typedefs

namespace RecursiveVariant {
    namespace Types {
        // Original type: recursive_variant = [ ... | Integer of ... | ... ]
        struct Integer
        {
            int value;
            static void to_json(const Integer &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);
        };
        // Original type: recursive_variant = [ ... | Rec of ... | ... ]
        struct Rec
        {
            std::shared_ptr<typedefs::RecursiveVariant> value;
            static void to_json(const Rec &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);
        };
    }

    typedefs::RecursiveVariant from_json(const rapidjson::Value &x);
    typedefs::RecursiveVariant from_json_string(const std::string &s);
    void to_json(const typedefs::RecursiveVariant &x, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::RecursiveVariant &x);
}


struct RecursiveRecord2 {
    int id;
    bool flag;
    std::shared_ptr<std::optional<typedefs::RecursiveRecord2>> children;

    static RecursiveRecord2 from_json(const rapidjson::Value & doc);
    static RecursiveRecord2 from_json_string(const std::string &s);
    static void to_json(const RecursiveRecord2 &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const RecursiveRecord2 &t);
    std::string to_json_string();
};


struct RecursiveClass {
    int id;
    bool flag;
    std::vector<typedefs::RecursiveClass> children;

    static RecursiveClass from_json(const rapidjson::Value & doc);
    static RecursiveClass from_json_string(const std::string &s);
    static void to_json(const RecursiveClass &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const RecursiveClass &t);
    std::string to_json_string();
};


struct ThreeLevelNestedListRecord {
    std::vector<std::vector<std::vector<int>>> field_a;

    static ThreeLevelNestedListRecord from_json(const rapidjson::Value & doc);
    static ThreeLevelNestedListRecord from_json_string(const std::string &s);
    static void to_json(const ThreeLevelNestedListRecord &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const ThreeLevelNestedListRecord &t);
    std::string to_json_string();
};


struct StructWithRecursiveVariant {
    typedefs::RecursiveVariant variant;

    static StructWithRecursiveVariant from_json(const rapidjson::Value & doc);
    static StructWithRecursiveVariant from_json_string(const std::string &s);
    static void to_json(const StructWithRecursiveVariant &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const StructWithRecursiveVariant &t);
    std::string to_json_string();
};


namespace St {
    typedefs::St from_json(const rapidjson::Value &doc);
    typedefs::St from_json_string(const std::string &s);
    void to_json(const typedefs::St &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::St &t);
}


namespace Kind {
    namespace Types {
        // Original type: kind = [ ... | Root | ... ]
        struct Root {
            static void to_json(const Root &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);
        };
        // Original type: kind = [ ... | Thing of ... | ... ]
        struct Thing
        {
            int value;
            static void to_json(const Thing &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);
        };
        // Original type: kind = [ ... | WOW | ... ]
        struct WOW {
            static void to_json(const WOW &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);
        };
        // Original type: kind = [ ... | Amaze of ... | ... ]
        struct Amaze
        {
            std::vector<std::string> value;
            static void to_json(const Amaze &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);
        };
    }

    typedefs::Kind from_json(const rapidjson::Value &x);
    typedefs::Kind from_json_string(const std::string &s);
    void to_json(const typedefs::Kind &x, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::Kind &x);
}


namespace Alias3 {
    typedefs::Alias3 from_json(const rapidjson::Value &doc);
    typedefs::Alias3 from_json_string(const std::string &s);
    void to_json(const typedefs::Alias3 &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::Alias3 &t);
}


namespace AliasOfAliasNotWrapped {
    typedefs::AliasOfAliasNotWrapped from_json(const rapidjson::Value &doc);
    typedefs::AliasOfAliasNotWrapped from_json_string(const std::string &s);
    void to_json(const typedefs::AliasOfAliasNotWrapped &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::AliasOfAliasNotWrapped &t);
}


namespace AliasOfAliasOfAlias {
    typedefs::AliasOfAliasOfAlias from_json(const rapidjson::Value &doc);
    typedefs::AliasOfAliasOfAlias from_json_string(const std::string &s);
    void to_json(const typedefs::AliasOfAliasOfAlias &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::AliasOfAliasOfAlias &t);
}


namespace Alias {
    typedefs::Alias from_json(const rapidjson::Value &doc);
    typedefs::Alias from_json_string(const std::string &s);
    void to_json(const typedefs::Alias &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::Alias &t);
}


namespace KindParametrizedTuple {
    typedefs::KindParametrizedTuple from_json(const rapidjson::Value &doc);
    typedefs::KindParametrizedTuple from_json_string(const std::string &s);
    void to_json(const typedefs::KindParametrizedTuple &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::KindParametrizedTuple &t);
}


struct IntFloatParametrizedRecord {
    int field_a;
    std::vector<float> field_b = {};

    static IntFloatParametrizedRecord from_json(const rapidjson::Value & doc);
    static IntFloatParametrizedRecord from_json_string(const std::string &s);
    static void to_json(const IntFloatParametrizedRecord &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const IntFloatParametrizedRecord &t);
    std::string to_json_string();
};


struct Root {
    std::string id;
    bool await;
    int integer;
    float x___init__;
    float float_with_auto_default = 0.0f;
    float float_with_default = 0.1f;
    std::vector<std::vector<int>> items;
    std::optional<int> maybe;
    std::vector<int> extras = {};
    int answer = 42;
    typedefs::Alias aliased;
    std::tuple<float, float> point;
    typedefs::Kind kind;
    std::vector<typedefs::Kind> kinds;
    std::vector<std::tuple<float, int>> assoc1;
    std::vector<std::tuple<std::string, int>> assoc2;
    std::map<float, int> assoc3;
    std::map<std::string, int> assoc4;
    std::vector<std::optional<int>> nullables;
    std::vector<std::optional<int>> options;
    typedefs::IntFloatParametrizedRecord parametrized_record;
    typedefs::KindParametrizedTuple parametrized_tuple;
    uint16_t wrapped;
    typedefs::AliasOfAliasOfAlias aaa;
    int item;

    static Root from_json(const rapidjson::Value & doc);
    static Root from_json_string(const std::string &s);
    static void to_json(const Root &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const Root &t);
    std::string to_json_string();
};


struct RequireField {
    std::string req;

    static RequireField from_json(const rapidjson::Value & doc);
    static RequireField from_json_string(const std::string &s);
    static void to_json(const RequireField &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const RequireField &t);
    std::string to_json_string();
};


struct RecordWithWrappedType {
    int item;

    static RecordWithWrappedType from_json(const rapidjson::Value & doc);
    static RecordWithWrappedType from_json_string(const std::string &s);
    static void to_json(const RecordWithWrappedType &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const RecordWithWrappedType &t);
    std::string to_json_string();
};


namespace Password {
    typedefs::Password from_json(const rapidjson::Value &doc);
    typedefs::Password from_json_string(const std::string &s);
    void to_json(const typedefs::Password &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::Password &t);
}


namespace Pair {
    typedefs::Pair from_json(const rapidjson::Value &doc);
    typedefs::Pair from_json_string(const std::string &s);
    void to_json(const typedefs::Pair &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::Pair &t);
}


struct NullOpt {
    int a;
    std::optional<int> b;
    std::optional<int> c;
    std::optional<int> f;
    std::optional<int> h = 3;
    std::optional<int> i = 3;

    static NullOpt from_json(const rapidjson::Value & doc);
    static NullOpt from_json_string(const std::string &s);
    static void to_json(const NullOpt &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const NullOpt &t);
    std::string to_json_string();
};


namespace Frozen {
    namespace Types {
        // Original type: frozen = [ ... | A | ... ]
        struct A {
            static void to_json(const A &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);
        };
        // Original type: frozen = [ ... | B of ... | ... ]
        struct B
        {
            int value;
            static void to_json(const B &e, rapidjson::Writer<rapidjson::StringBuffer> &writer);
        };
    }

    typedefs::Frozen from_json(const rapidjson::Value &x);
    typedefs::Frozen from_json_string(const std::string &s);
    void to_json(const typedefs::Frozen &x, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::Frozen &x);
}


struct EmptyRecord {

    static EmptyRecord from_json(const rapidjson::Value & doc);
    static EmptyRecord from_json_string(const std::string &s);
    static void to_json(const EmptyRecord &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const EmptyRecord &t);
    std::string to_json_string();
};


struct DefaultList {
    std::vector<int> items = {};

    static DefaultList from_json(const rapidjson::Value & doc);
    static DefaultList from_json_string(const std::string &s);
    static void to_json(const DefaultList &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const DefaultList &t);
    std::string to_json_string();
};


struct Credential {
    std::string name;
    int password;

    static Credential from_json(const rapidjson::Value & doc);
    static Credential from_json_string(const std::string &s);
    static void to_json(const Credential &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const Credential &t);
    std::string to_json_string();
};


namespace Credentials2 {
    typedefs::Credentials2 from_json(const rapidjson::Value &doc);
    typedefs::Credentials2 from_json_string(const std::string &s);
    void to_json(const typedefs::Credentials2 &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::Credentials2 &t);
}


struct Credentials {
    std::vector<typedefs::Credential> credentials;

    static Credentials from_json(const rapidjson::Value & doc);
    static Credentials from_json_string(const std::string &s);
    static void to_json(const Credentials &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    static std::string to_json_string(const Credentials &t);
    std::string to_json_string();
};


namespace AliasOfAlias {
    typedefs::AliasOfAlias from_json(const rapidjson::Value &doc);
    typedefs::AliasOfAlias from_json_string(const std::string &s);
    void to_json(const typedefs::AliasOfAlias &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::AliasOfAlias &t);
}


namespace Alias2 {
    typedefs::Alias2 from_json(const rapidjson::Value &doc);
    typedefs::Alias2 from_json_string(const std::string &s);
    void to_json(const typedefs::Alias2 &t, rapidjson::Writer<rapidjson::StringBuffer> &writer);
    std::string to_json_string(const typedefs::Alias2 &t);
}
} // namespace atd
