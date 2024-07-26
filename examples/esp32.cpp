//
// Created by Trung Tran on 1/12/2024.
//

#include <Arduino.h>
#include <cassert>
#include "json.hpp"

void setup() {
    Serial.bein(115200);

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

    obj.dump();
}

void loop() {

}