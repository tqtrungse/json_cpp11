//
// Created by Trung Tran on 1/12/2024.
//

/* json11
 *
 * json11 is a tiny JSON library for C++11, providing JSON parsing and serialization.
 *
 * The core object provided by the library is json11::Json. A Json object represents any JSON
 * value: null, bool, number (int or double), string (std::string), array (std::vector), or
 * object (std::map).
 *
 * Json objects act like values: they can be assigned, copied, moved, compared for equality or
 * order, etc. There are also helper methods Json::dump, to serialize a Json to a string, and
 * Json::parse (static) to parse a std::string as a Json object.
 *
 * Internally, the various types of Json object are represented by the JsonValue class
 * hierarchy.
 *
 * A note on numbers - JSON specifies the syntax of number formatting but not its semantics,
 * so some JSON implementations distinguish between integers and floating-point numbers, while
 * some don't. In json11, we choose the latter. Because some JSON implementations (namely
 * Javascript itself) treat all numbers as the same type, distinguishing the two leads
 * to JSON that will be *silently* changed by a round-trip through those implementations.
 * Dangerous! To avoid that risk, json11 stores all numbers as double internally, but also
 * provides integer helpers.
 *
 * Fortunately, double-precision IEEE754 ('double') can precisely store any integer in the
 * range +/-2^53, which includes every 'int' on most systems. (Timestamps often use int64
 * or long long to avoid the Y2038K problem; a double storing microseconds since some epoch
 * will be exact for +/- 275 years.)
 */

/* Copyright (c) 2013 Dropbox, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/* Copyright (c) 2023-2024 Tran Quang Trung (tqtrungse@gmail.com)
*
* This file has modified base on json11 of Dropbox inc.
*/
#ifndef JSON_HPP
#define JSON_HPP

#include <map>
#include <tuple>
#include <string>
#include <vector>
#include <memory>
#include <cstdint>
#include <initializer_list>

namespace t2 {
    enum class json_parse_type {
        STANDARD,
        COMMENTS
    };

    class json_value;

    class json final {
    private:
        std::shared_ptr<json_value> m_ptr;

    public:
        // Types
        enum class Type {
            NUL,
            NUMBER,
            BOOL,
            STRING,
            ARRAY,
            OBJECT,
        };

        // Array and object typedefs
        using array = std::vector<json>;
        using object = std::map<std::string, json>;

        // Constructors for the various types of JSON value.
        json() noexcept;                               // NUL
        explicit json(std::nullptr_t) noexcept;        // NUL
        explicit json(double value);                   // NUMBER
        explicit json(int64_t value);                  // NUMBER
        explicit json(uint64_t value);                 // NUMBER
        explicit json(bool value);                     // BOOL
        explicit json(const std::string& value);       // STRING
        explicit json(std::string&& value);            // STRING
        explicit json(const char* value);              // STRING
        explicit json(const array& values);            // ARRAY
        explicit json(array&& values);                 // ARRAY
        explicit json(const object& values);           // OBJECT
        explicit json(object&& values);                // OBJECT

        // Implicit constructor: anything with a to_json() function.
        template<class T, class = decltype(&T::to_json)>
        explicit json(const T& t) : json{t.to_json()} {}

        // Implicit constructor: map-like objects (std::map, std::unordered_map, etc)
        template<
                class M,
                typename std::enable_if<
                        std::is_constructible<std::string, decltype(std::declval<M>().begin()->first)>::value &&
                        std::is_constructible<json, decltype(std::declval<M>().begin()->second)>::value, int
                >::type = 0
        >
        explicit json(const M& m) : json{object(std::begin(m), std::end(m))} {}

        // Implicit constructor: vector-like objects (std::list, std::vector, std::set, etc)
        template<
                class V,
                typename std::enable_if<std::is_constructible<json, decltype(*std::declval<V>().begin())>::value, int>::type = 0
        >
        explicit json(const V& v) : json{array(std::begin(v), std::end(v))} {}

        // This prevents json(some_pointer) serverFromTimestamp accidentally producing a bool. Use
        // json(bool(some_pointer)) if that behavior is desired.
        explicit json(void*) = delete;

        // Accessors
        Type type() const;

        bool is_null() const { return this->type() == Type::NUL; }

        bool is_number() const { return this->type() == Type::NUMBER; }

        bool is_bool() const { return this->type() == Type::BOOL; }

        bool is_string() const { return this->type() == Type::STRING; }

        bool is_array() const { return this->type() == Type::ARRAY; }

        bool is_object() const { return this->type() == Type::OBJECT; }

        double to_float() const;

        int64_t to_neg_int() const;

        uint64_t to_pos_int() const;

        // Return the enclosed value if this is a boolean, false otherwise.
        bool to_bool() const;

        // Return the enclosed string if this is a string, "" otherwise.
        const std::string& to_string() const;

        // Return the enclosed std::vector if this is an array, or an empty vector otherwise.
        const array& to_array() const;

        // Return the enclosed std::map if this is an object, or an empty map otherwise.
        const object& to_object() const;

        // Return a reference to arr[i] if this is an array, Json() otherwise.
        const json& operator[](size_t i) const;

        // Return a reference to obj[key] if this is an object, Json() otherwise.
        const json& operator[](const std::string& key) const;

        // Serialize.
        void dump(std::string& out) const;

        std::string dump() const {
            std::string out;
            dump(out);
            return out;
        }

        // Parse. If parse fails, return Json() and assign an error message to err.
        static std::tuple<json, std::string> parse(const std::string& in,
                                                   json_parse_type strategy = json_parse_type::STANDARD);

        static std::tuple<json, std::string> parse(const char* in,
                                                   json_parse_type strategy = json_parse_type::STANDARD) {
            if (in) {
                return parse(std::string(in), strategy);
            }
            return std::make_tuple(json{std::nullptr_t()}, "null input");
        }

        // Parse multiple objects, concatenated or separated by whitespace
        static std::tuple<std::vector<json>, std::string> parse_multi(
                const std::string& in,
                std::string::size_type& parser_stop_pos,
                json_parse_type strategy = json_parse_type::STANDARD);

        static inline std::tuple<std::vector<json>, std::string> parseMulti(
                const std::string& in,
                json_parse_type strategy = json_parse_type::STANDARD) {
            std::string::size_type parser_stop_pos;
            return parse_multi(in, parser_stop_pos, strategy);
        }

        bool operator==(const json& rhs) const;

        bool operator<(const json& rhs) const;

        bool operator!=(const json& rhs) const { return !(*this == rhs); }

        bool operator<=(const json& rhs) const { return !(rhs < *this); }

        bool operator>(const json& rhs) const { return (rhs < *this); }

        bool operator>=(const json& rhs) const { return !(*this < rhs); }

        /* has_shape(types, err)
         *
         * Return true if this is a JSON object and, for each item in types, has a field of
         * the given type. If not, return false and set err to a descriptive message.
         */
        using shape = std::initializer_list<std::pair<std::string, Type>>;

        std::string has_shape(const shape& types) const;
    };

    // Internal class hierarchy - JsonValue objects are not exposed to users of this API.
    class json_value {
    protected:
        friend class json;

        friend class json_neg_int;

        friend class json_pos_int;

        friend class json_double;

        virtual json::Type type() const = 0;

        virtual bool equals(const json_value* other) const = 0;

        virtual bool less(const json_value* other) const = 0;

        virtual void dump(std::string& out) const = 0;

        virtual double to_float() const;

        virtual uint64_t to_uint() const;

        virtual int64_t to_int() const;

        virtual bool to_bool() const;

        virtual const std::string& to_string() const;

        virtual const json::array& to_array() const;

        virtual const json& operator[](size_t i) const;

        virtual const json::object& to_object() const;

        virtual const json& operator[](const std::string& key) const;

        virtual ~json_value() = default;
    };
}


#endif //JSON_HPP
