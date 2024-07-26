# json_cpp11
A tiny JSON for any embedded platform supports C++11, providing JSON parsing and serialization.

The core object provided by the library is t2::json. A json object represents any JSON
value: null, bool, number (int64_t-negative, uint64_t-positive or double), string (std::string), array (std::vector), or
object (std::map).

Json objects act like values. They can be assigned, copied, moved, compared for equality or
order, and so on. There are also helper methods json::dump, to serialize a Json to a string, and
json::parse (static) to parse a std::string as a Json object.

It's easy to make a JSON object with C++11's new initializer syntax (explicit constructors by sonarlint):

    t2::json obj = t2::json::object {
        { "key1", t2::json{"value1"} },
        { "key2", t2::json{false} },
        { "key3", t2::json{Json::array { 1, 2, 3 }} },
    };
    std::string str = obj.dump();

It's easy to parse JSON:

    t2::json obj;
    std::string err;
    std::string str = R"(
        {
            "name": "John Doe",
            "age": 30,
            "city": "New York",
            "isStudent": false,
            "temperature": -10,
            "grades": [95, 89, 78.05],
            "address": {
                "street": "123 Main St",
                "zipCode": "10001"
            }
        }
    )";

    std::tie(obj, err) = t2::json::parse(str);
    assert(err.empty());

    assert(obj["name"].is_string());
    assert(obj["name"].to_string() == "John Doe");

    assert(obj["age"].is_number());
    assert(obj["age"].to_uint() == 30);

    assert(obj["city"].is_string());
    assert(obj["city"].to_string() == "New York");

    assert(obj["isStudent"].is_bool());
    assert(obj["isStudent"].to_bool() == false);

    assert(obj["temperature"].is_number());
    assert(obj["temperature"].to_int() == -10);

    assert(obj["grades"].is_array());
    auto grades = obj["grades"].to_array();
    assert(grades.size() == 3);
    assert(grades[0].is_number());
    assert(grades[0].to_uint() == 95);
    assert(grades[1].is_number());
    assert(grades[1].to_uint() == 89);
    assert(grades[2].is_number());
    double f64 = 78.05;
    assert(grades[2].to_float() == f64);

    assert(obj["address"].is_object());
    auto obj_2 = obj["address"].to_object();
    assert(obj_2["street"].is_string());
    assert(obj_2["street"].to_string() == "123 Main St");
    assert(obj_2["zipCode"].is_string());
    assert(obj_2["zipCode"].to_string() == "10001");