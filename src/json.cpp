//
// Created by Trung Tran on 1/12/2024.
//

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

#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <limits>

#include "json.hpp"

namespace t2 {
    static const int max_depth = 200;

    /* Helper for representing null - just a do-nothing struct, plus comparison
     * operators so the helpers in json_value work. We can't use nullptr_t because
     * it may not be orderable.
     */
    struct null_struct {
        bool operator==(null_struct) const {
            return true;
        }

        bool operator<(null_struct) const {
            return false;
        }
    };

    /* * * * * * * * * * * * * * * * * * * *
     * Serialization
     */

    static void do_dump(null_struct, std::string& out) {
        out += "null";
    }

    static void do_dump(double value, std::string& out) {
        if (std::isfinite(value)) {
            std::string buf(32, '\0');
            snprintf(&buf[0], buf.size(), "%.17g", value);
            out += buf;
        } else {
            out += "null";
        }
    }

    static void do_dump(int64_t value, std::string& out) {
        std::string buf(32, '\0');
        snprintf(&buf[0], buf.size(), "%lld", value);

        size_t nullCharPos = buf.find('\0');
        if (nullCharPos != std::string::npos) {
            buf.erase(nullCharPos);
        }
        out += buf;
    }

    static void do_dump(uint64_t value, std::string& out) {
        std::string buf(32, '\0');
        snprintf(&buf[0], buf.size(), "%llu", value);

        size_t nullCharPos = buf.find('\0');
        if (nullCharPos != std::string::npos) {
            buf.erase(nullCharPos);
        }
        out += buf;
    }

    static void do_dump(bool value, std::string& out) {
        out += value ? "true" : "false";
    }

    static void do_dump(const std::string& value, std::string& out) {
        out += '"';
        for (size_t i = 0; i < value.length();) {
            const char ch = value[i];
            if (ch == '\\') {
                out += R"(\\)";
                i += 1;
            } else if (ch == '"') {
                out += R"(\")";
                i += 1;
            } else if (ch == '\b') {
                out += "\\b";
                i += 1;
            } else if (ch == '\f') {
                out += "\\f";
                i += 1;
            } else if (ch == '\n') {
                out += "\\n";
                i += 1;
            } else if (ch == '\r') {
                out += "\\r";
                i += 1;
            } else if (ch == '\t') {
                out += "\\t";
                i += 1;
            } else if (static_cast<uint8_t>(ch) <= 0x1f) {
                std::string buf(8, '\0');
                snprintf(&buf[0], buf.size(), "\\u%04x", ch);
                out += buf;
                i += 1;
            } else if (static_cast<uint8_t>(ch) == 0xe2 &&
                       static_cast<uint8_t>(value[i + 1]) == 0x80 &&
                       static_cast<uint8_t>(value[i + 2]) == 0xa8) {
                out += "\\u2028";
                i += 2;
            } else if (static_cast<uint8_t>(ch) == 0xe2 &&
                       static_cast<uint8_t>(value[i + 1]) == 0x80 &&
                       static_cast<uint8_t>(value[i + 2]) == 0xa9) {
                out += "\\u2029";
                i += 2;
            } else {
                out += ch;
                i += 1;
            }
        }
        out += '"';
    }

    static void do_dump(const json::array& values, std::string& out) {
        bool first = true;
        out += "[";
        for (const auto& value: values) {
            if (!first) {
                out += ", ";
            }
            value.dump(out);
            first = false;
        }
        out += "]";
    }

    static void do_dump(const json::object& values, std::string& out) {
        bool first = true;
        out += "{";
        for (const auto& kv: values) {
            if (!first) {
                out += ", ";
            }
            do_dump(kv.first, out);
            out += ": ";
            kv.second.dump(out);
            first = false;
        }
        out += "}";
    }

    void json::dump(std::string& out) const {
        this->m_ptr->dump(out);
    }

    /* * * * * * * * * * * * * * * * * * * *
     * json_val wrappers
     */

    template<json::Type tag, typename T>
    class json_val : public json_value {
    protected:
        const T m_value;

        // Constructors
        explicit json_val(const T& value)
                : m_value{value} {}

        explicit json_val(T&& value)
                : m_value{std::move(value)} {}

        // Get type tag
        json::Type type() const override {
            return tag;
        }

        // Comparisons
        bool equals(const json_value* other) const override {
            return this->m_value == static_cast<const json_val<tag, T>*>(other)->m_value;
        }

        bool less(const json_value* other) const override {
            return this->m_value < static_cast<const json_val<tag, T>*>(other)->m_value;
        }

        void dump(std::string& out) const override {
            do_dump(this->m_value, out);
        }
    };

    class json_double final : public json_val<json::Type::NUMBER, double> {
    private:
        double to_float() const override {
            return this->m_value;
        }

        int64_t to_int() const override {
            return static_cast<int64_t>(this->m_value);
        }

        uint64_t to_uint() const override {
            return static_cast<uint64_t>(this->m_value);
        }

        bool equals(const json_value* other) const override {
            return this->m_value == other->to_float();
        }

        bool less(const json_value* other) const override {
            return this->m_value < other->to_float();
        }

    public:
        explicit json_double(double value)
                : json_val{value} {}
    };

    class json_neg_int final : public json_val<json::Type::NUMBER, int64_t> {
    private:
        double to_float() const override {
            return static_cast<double>(this->m_value);
        }

        int64_t to_int() const override {
            return this->m_value;
        }

        uint64_t to_uint() const override {
            return this->m_value;
        }

        bool equals(const json_value* other) const override {
            return this->m_value == other->to_int();
        }

        bool less(const json_value* other) const override {
            return this->m_value < other->to_int();
        }

    public:
        explicit json_neg_int(int64_t value)
                : json_val{value} {}
    };

    class json_pos_int final : public json_val<json::Type::NUMBER, uint64_t> {
    private:
        double to_float() const override {
            return static_cast<double>(this->m_value);
        }

        uint64_t to_uint() const override {
            return this->m_value;
        }

        bool equals(const json_value* other) const override {
            return this->m_value == other->to_uint();
        }

        bool less(const json_value* other) const override {
            return this->m_value < other->to_uint();
        }

    public:
        explicit json_pos_int(uint64_t value)
                : json_val{value} {}
    };

    class json_boolean final : public json_val<json::Type::BOOL, bool> {
    private:
        bool to_bool() const override {
            return this->m_value;
        }

    public:
        explicit json_boolean(bool value)
                : json_val{value} {}
    };

    class json_string final : public json_val<json::Type::STRING, std::string> {
    private:
        const std::string& to_string() const override {
            return this->m_value;
        }

    public:
        explicit json_string(const std::string& value)
                : json_val{value} {}

        explicit json_string(std::string&& value)
                : json_val{std::move(value)} {}
    };

    class json_array final : public json_val<json::Type::ARRAY, json::array> {
    private:
        const json::array& to_array() const override {
            return this->m_value;
        }

        const json& operator[](size_t i) const override;

    public:
        explicit json_array(const json::array& value)
                : json_val{value} {}

        explicit json_array(json::array&& value)
                : json_val{std::move(value)} {}
    };

    class json_object final : public json_val<json::Type::OBJECT, json::object> {
    private:
        const json::object& to_object() const override {
            return this->m_value;
        }

        const json& operator[](const std::string& key) const override;

    public:
        explicit json_object(const json::object& value)
                : json_val{value} {}

        explicit json_object(json::object&& value)
                : json_val{std::move(value)} {}
    };

    class json_null final : public json_val<json::Type::NUL, null_struct> {
    public:
        json_null()
                : json_val({}) {}
    };

    /* * * * * * * * * * * * * * * * * * * *
     * Static globals - static-init-safe
     */
    struct statics_ {
        const std::shared_ptr<json_value> null = std::make_shared<json_null>();
        const std::shared_ptr<json_value> t = std::make_shared<json_boolean>(true);
        const std::shared_ptr<json_value> f = std::make_shared<json_boolean>(false);
        const std::string empty_string;
        const std::vector<json> empty_vector;
        const std::map<std::string, json> empty_map;

        statics_() = default;
    };

    static const statics_& statics() {
        static const statics_ s{};
        return s;
    }

    static const json& static_null() {
        // This has to be separate, not in Statics, because Json() accesses statics().null.
        static const json json_null;
        return json_null;
    }

    /* * * * * * * * * * * * * * * * * * * *
     * Constructors
     */

    json::json() noexcept
            : m_ptr{statics().null} {}

    json::json(std::nullptr_t) noexcept
            : m_ptr{statics().null} {}

    json::json(double value)
            : m_ptr{std::make_shared<json_double>(value)} {}

    json::json(int64_t value)
            : m_ptr{std::make_shared<json_neg_int>(value)} {}

    json::json(uint64_t value)
            : m_ptr{std::make_shared<json_pos_int>(value)} {}

    json::json(bool value)
            : m_ptr{value ? statics().t : statics().f} {}

    json::json(const std::string& value)
            : m_ptr{std::make_shared<json_string>(value)} {}

    json::json(std::string&& value)
            : m_ptr{std::make_shared<json_string>(std::move(value))} {}

    json::json(const char* value)
            : m_ptr{std::make_shared<json_string>(value)} {}

    json::json(const json::array& values)
            : m_ptr{std::make_shared<json_array>(values)} {}

    json::json(json::array&& values)
            : m_ptr{std::make_shared<json_array>(std::move(values))} {}

    json::json(const json::object& values)
            : m_ptr{std::make_shared<json_object>(values)} {}

    json::json(json::object&& values)
            : m_ptr{std::make_shared<json_object>(std::move(values))} {}

    /* * * * * * * * * * * * * * * * * * * *
     * Accessors
     */

    json::Type json::type() const {
        return this->m_ptr->type();
    }

    double json::to_float() const {
        return this->m_ptr->to_float();
    }

    int64_t json::to_neg_int() const {
        return this->m_ptr->to_int();
    }

    uint64_t json::to_pos_int() const {
        return this->m_ptr->to_uint();
    }

    bool json::to_bool() const {
        return this->m_ptr->to_bool();
    }

    const std::string& json::to_string() const {
        return this->m_ptr->to_string();
    }

    const std::vector<json>& json::to_array() const {
        return this->m_ptr->to_array();
    }

    const std::map<std::string, json>& json::to_object() const {
        return this->m_ptr->to_object();
    }

    const json& json::operator[](size_t i) const {
        return (*(this->m_ptr))[i];
    }

    const json& json::operator[](const std::string& key) const {
        return (*(this->m_ptr))[key];
    }

    double json_value::to_float() const {
        return 0;
    }

    int64_t json_value::to_int() const {
        return 0;
    }

    uint64_t json_value::to_uint() const {
        return 0;
    }

    bool json_value::to_bool() const {
        return false;
    }

    const std::string& json_value::to_string() const {
        return statics().empty_string;
    }

    const std::vector<json>& json_value::to_array() const {
        return statics().empty_vector;
    }

    const std::map<std::string, json>& json_value::to_object() const {
        return statics().empty_map;
    }

    const json& json_value::operator[](size_t) const {
        return static_null();
    }

    const json& json_value::operator[](const std::string&) const {
        return static_null();
    }

    const json& json_object::operator[](const std::string& key) const {
        auto iter = this->m_value.find(key);
        return (iter == this->m_value.end()) ?
               static_null() :
               iter->second;
    }

    const json& json_array::operator[](size_t i) const {
        if (i >= this->m_value.size()) {
            return static_null();
        }
        return this->m_value[i];
    }

    /* * * * * * * * * * * * * * * * * * * *
     * Comparison
     */

    bool json::operator==(const json& other) const {
        if (this->m_ptr == other.m_ptr) {
            return true;
        }
        if (this->m_ptr->type() != other.m_ptr->type()) {
            return false;
        }
        return this->m_ptr->equals(other.m_ptr.get());
    }

    bool json::operator<(const json& other) const {
        if (this->m_ptr == other.m_ptr) {
            return false;
        }
        if (this->m_ptr->type() != other.m_ptr->type()) {
            return this->m_ptr->type() < other.m_ptr->type();
        }
        return this->m_ptr->less(other.m_ptr.get());
    }

    /* * * * * * * * * * * * * * * * * * * *
     * Parsing
     */

    /* esc(c)
     *
     * Format char c suitable for printing in an error message.
     */
    static inline std::string esc(char c) {
        std::string buf(12, '\0');
        if (static_cast<uint8_t>(c) >= 0x20 &&
            static_cast<uint8_t>(c) <= 0x7f) {
            snprintf(&buf[0], buf.size(), "'%c' (%d)", c, c);
        } else {
            snprintf(&buf[0], buf.size(), "(%d)", c);
        }
        return buf;
    }

    static inline bool in_range(long x, long lower, long upper) {
        return (x >= lower && x <= upper);
    }

    namespace {
        /* json_parser
         *
         * Object that tracks all state of an in-progress parse.
         */
        struct json_parser final {

            /* State
             */
            const std::string& str;
            size_t i;
            std::string& err;
            bool failed;
            const json_parse_type strategy;

            /* fail(msg, err_ret = json())
             *
             * Mark this parse as failed.
             */
            json fail(std::string&& msg) {
                return fail(std::move(msg), json{});
            }

            template<typename T>
            T fail(std::string&& msg, const T err_ret) {
                if (!this->failed) {
                    this->err = std::move(msg);
                }
                this->failed = true;
                return std::move(err_ret);
            }

            /* consume_whitespace()
             *
             * Advance until the current character is non-whitespace.
             */
            void consume_whitespace() {
                while (this->str[i] == ' ' ||
                       this->str[i] == '\r' ||
                       this->str[i] == '\n' ||
                       this->str[i] == '\t') {
                    i++;
                }
            }

            /* consume_comment()
             *
             * Advance comments (c-style inline and multiline).
             */
            bool consume_comment() {
                if (this->str[i] != '/') {
                    return false;
                }

                this->i++;
                if (this->i == this->str.size()) {
                    return this->fail("unexpected end of input after start of comment", false);
                }

                if (this->str[this->i] == '/') { // inline comment
                    this->i++;
                    // advance until next line, or end of input
                    while (this->i < this->str.size() &&
                           this->str[this->i] != '\n') {
                        this->i++;
                    }
                    return true;
                }

                if (this->str[this->i] == '*') { // multiline comment
                    this->i++;
                    if (this->i > this->str.size() - 2) {
                        return this->fail("unexpected end of input inside multi-line comment", false);
                    }

                    // advance until closing tokens
                    while (!(this->str[this->i] == '*' &&
                             this->str[this->i + 1] == '/')) {
                        this->i++;
                        if (this->i > this->str.size() - 2) {
                            return this->fail("unexpected end of input inside multi-line comment", false);
                        }
                    }
                    this->i += 2;
                    return true;
                }
                return this->fail("malformed comment", false);
            }

            /* consume_garbage()
             *
             * Advance until the current character is non-whitespace and non-comment.
             */
            void consume_garbage() {
                consume_whitespace();
                if (this->strategy == json_parse_type::COMMENTS) {
                    bool comment_found{false};
                    do {
                        comment_found = consume_comment();
                        if (this->failed) return;
                        consume_whitespace();
                    } while (comment_found);
                }
            }

            /* get_next_token()
             *
             * Return the next non-whitespace character. If the end of the input is reached,
             * flag an error and return 0.
             */
            char get_next_token() {
                consume_garbage();
                if (this->failed) {
                    return static_cast<char>(0);
                }
                if (this->i == this->str.size()) {
                    return this->fail("unexpected end of input", static_cast<char>(0));
                }
                this->i++;
                return str[this->i - 1];
            }

            /* encode_utf8(pt, out)
             *
             * Encode pt as UTF-8 and add it to out.
             */
            static void encode_utf8(long pt, std::string& out) {
                if (pt < 0) {
                    return;
                }

                if (pt < 0x80) {
                    out += static_cast<char>(pt);
                } else if (pt < 0x800) {
                    out += static_cast<char>((pt >> 6) | 0xC0);
                    out += static_cast<char>((pt & 0x3F) | 0x80);
                } else if (pt < 0x10000) {
                    out += static_cast<char>((pt >> 12) | 0xE0);
                    out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
                    out += static_cast<char>((pt & 0x3F) | 0x80);
                } else {
                    out += static_cast<char>((pt >> 18) | 0xF0);
                    out += static_cast<char>(((pt >> 12) & 0x3F) | 0x80);
                    out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
                    out += static_cast<char>((pt & 0x3F) | 0x80);
                }
            }

            /* parse_string()
             *
             * Parse a string, starting at the current position.
             */
            std::string parse_string() {
                std::string out;
                long lastEscapedCodepoint = -1;
                while (true) {
                    if (this->i == this->str.size()) {
                        return this->fail("unexpected end of input in string", "");
                    }

                    char ch = this->str[this->i];
                    this->i++;

                    if (ch == '"') {
                        encode_utf8(lastEscapedCodepoint, out);
                        return out;
                    }

                    if (in_range(ch, 0, 0x1f)) {
                        return this->fail("unescaped in string" + esc(ch), "");
                    }

                    // The usual case: non-escaped characters
                    if (ch != '\\') {
                        encode_utf8(lastEscapedCodepoint, out);
                        lastEscapedCodepoint = -1;
                        out += ch;
                        continue;
                    }

                    // Handle escapes
                    if (this->i == this->str.size()) {
                        return this->fail("unexpected end of input in string", "");
                    }

                    ch = this->str[this->i];
                    this->i++;

                    if (ch != 'u') {
                        encode_utf8(lastEscapedCodepoint, out);
                        lastEscapedCodepoint = -1;

                        if (ch == 'b') {
                            out += '\b';
                        } else if (ch == 'f') {
                            out += '\f';
                        } else if (ch == 'n') {
                            out += '\n';
                        } else if (ch == 'r') {
                            out += '\r';
                        } else if (ch == 't') {
                            out += '\t';
                        } else if (ch == '"' ||
                                   ch == '\\' ||
                                   ch == '/') {
                            out += ch;
                        } else {
                            return this->fail("invalid escape character " + esc(ch), "");
                        }
                        continue;
                    }

                    // Extract 4-byte escape sequence
                    std::string esc = this->str.substr(this->i, 4);
                    // Explicitly check length of the substring. The following loop
                    // relies on std::string returning the terminating NUL when
                    // accessing str[length]. Checking here reduces brittleness.
                    if (esc.length() < 4) {
                        return this->fail("bad \\u escape: " + esc, "");
                    }
                    for (size_t j = 0; j < 4; j++) {
                        if (!in_range(esc[j], 'a', 'f') &&
                            !in_range(esc[j], 'A', 'F') &&
                            !in_range(esc[j], '0', '9')) {
                            return this->fail("bad \\u escape: " + esc, "");
                        }
                    }

                    long codePoint = strtol(esc.data(), nullptr, 16);

                    // JSON specifies that characters outside the BMP shall be encoded as a pair
                    // of 4-hex-digit \u escapes encoding their surrogate pair components. Check
                    // whether we're in the middle of such a beast: the previous codePoint was an
                    // escaped lead (high) surrogate, and this is a trail (low) surrogate.
                    if (in_range(lastEscapedCodepoint, 0xD800, 0xDBFF) &&
                        in_range(codePoint, 0xDC00, 0xDFFF)) {
                        // Reassemble the two surrogate pairs into one astral-plane character, per
                        // the UTF-16 algorithm.
                        encode_utf8((((lastEscapedCodepoint - 0xD800) << 10) | (codePoint - 0xDC00)) + 0x10000, out);
                        lastEscapedCodepoint = -1;
                    } else {
                        encode_utf8(lastEscapedCodepoint, out);
                        lastEscapedCodepoint = codePoint;
                    }
                    this->i += 4;
                }
            }

            /* parse_number()
             *
             */
            json parse_number() {
                size_t startPos = this->i;

                bool isNegative{false};
                if (this->str[this->i] == '-') {
                    isNegative = true;
                    this->i++;
                }

                // Integer part
                if (this->str[this->i] == '0') {
                    this->i++;
                    if (in_range(this->str[this->i], '0', '9')) {
                        return this->fail("leading 0s not permitted in numbers");
                    }
                } else if (in_range(this->str[this->i], '1', '9')) {
                    this->i++;
                    while (in_range(this->str[this->i], '0', '9')) {
                        this->i++;
                    }
                } else {
                    return this->fail("invalid " +  esc(this->str[this->i]) + " in toNumber\"");
                }

                if (this->str[this->i] != '.' &&
                    this->str[this->i] != 'e' &&
                    this->str[this->i] != 'E') {
                    if (isNegative) {
                        if ((this->i - startPos) <= static_cast<size_t>(std::numeric_limits<int64_t>::digits10)) {
                            return json{static_cast<int64_t>(strtoll(this->str.c_str() + startPos, nullptr, 10))};
                        }
                    } else if ((this->i - startPos) <= static_cast<size_t>(std::numeric_limits<uint64_t>::digits10)) {
                        return json{static_cast<uint64_t>(strtoull(this->str.c_str() + startPos, nullptr, 10))};
                    }
                }

                // Decimal part
                if (this->str[this->i] == '.') {
                    this->i++;
                    if (!in_range(this->str[this->i], '0', '9')) {
                        return this->fail("at least one digit required in fractional part");
                    }

                    while (in_range(this->str[this->i], '0', '9')) {
                        this->i++;
                    }
                }

                // Exponent part
                if (this->str[this->i] == 'e' ||
                    this->str[this->i] == 'E') {
                    this->i++;

                    if (this->str[this->i] == '+' ||
                        this->str[this->i] == '-') {
                        this->i++;
                    }

                    if (!in_range(this->str[this->i], '0', '9')) {
                        return this->fail("at least one digit required in exponent");
                    }

                    while (in_range(this->str[this->i], '0', '9')) {
                        this->i++;
                    }
                }
                return json{strtod(this->str.c_str() + startPos, nullptr)};
            }

            /* expect(str, res)
             *
             * Expect that 'str' starts at the character that was just read. If it does, advance
             * the input and return res. If not, flag an error.
             */
            json expect(const std::string& expected, json res) {
                assert(this->i != 0);
                this->i--;
                if (this->str.compare(this->i, expected.length(), expected) == 0) {
                    this->i += expected.length();
                    return res;
                } else {
                    return this->fail("parse error: expected " + expected + ", got " + this->str.substr(i, expected.length()));
                }
            }

            /* parse_json()
             *
             * Parse a JSON object.
             */
            json parse_json(int depth) {
                if (depth > max_depth) {
                    return this->fail("exceeded maximum nesting depth");
                }

                char ch = get_next_token();
                if (this->failed) {
                    return {};
                }

                if (ch == '-' ||
                    (ch >= '0' && ch <= '9')) {
                    this->i--;
                    return this->parse_number();
                }

                if (ch == 't') {
                    return this->expect("true", json{true});
                }

                if (ch == 'f') {
                    return this->expect("false", json{false});
                }

                if (ch == 'n') {
                    return this->expect("null", json{});
                }

                if (ch == '"') {
                    return json{this->parse_string()};
                }

                if (ch == '{') {
                    std::map<std::string, json> data;
                    ch = this->get_next_token();
                    if (ch == '}') {
                        return json{data};
                    }

                    while (true) {
                        if (ch != '"') {
                            return this->fail("expected '\"' in object, got " + esc(ch));
                        }

                        std::string key = this->parse_string();
                        if (this->failed) {
                            return {};
                        }

                        ch = this->get_next_token();
                        if (ch != ':') {
                            return this->fail("expected ':' in object, got " + esc(ch));
                        }

                        data[std::move(key)] = this->parse_json(depth + 1);
                        if (this->failed) {
                            return {};
                        }

                        ch = this->get_next_token();
                        if (ch == '}') {
                            break;
                        }
                        if (ch != ',') {
                            return this->fail("expected ',' in object, got " + esc(ch));
                        }

                        ch = this->get_next_token();
                    }
                    return json{data};
                }

                if (ch == '[') {
                    std::vector<json> data;
                    ch = this->get_next_token();
                    if (ch == ']') {
                        return json{data};
                    }

                    while (true) {
                        this->i--;
                        data.push_back(this->parse_json(depth + 1));
                        if (this->failed) {
                            return {};
                        }

                        ch = this->get_next_token();
                        if (ch == ']') {
                            break;
                        }
                        if (ch != ',') {
                            return this->fail("expected ',' in list, got " + esc(ch));
                        }

                        ch = this->get_next_token();
                        (void) ch;
                    }
                    return json{data};
                }

                return this->fail("expected value, got " + esc(ch));
            }
        };
    }

    std::tuple<json, std::string> json::parse(const std::string& in, json_parse_type strategy) {
        std::string err;
        json_parser parser{in, 0, err, false, strategy};
        json result = parser.parse_json(0);

        // Check for any trailing garbage
        parser.consume_garbage();
        if (parser.failed) {
            return std::make_tuple(json{}, std::move(err));
        }
        if (parser.i != in.size()) {
            std::make_tuple(
                    parser.fail("unexpected trailing " + esc(in[parser.i])),
                    std::move(err)
            );
        }
        return std::make_tuple(result, std::move(err));
    }

    // Documented in json.hpp
    std::tuple<std::vector<json>, std::string> json::parse_multi(const std::string& in,
                                                                 std::string::size_type& parser_stop_pos,
                                                                 json_parse_type strategy) {
        std::string err;
        json_parser parser{in, 0, err, false, strategy};
        parser_stop_pos = 0;
        std::vector<json> json_vec;
        while (parser.i != in.size() && !parser.failed) {
            json_vec.push_back(parser.parse_json(0));
            if (!parser.failed) {
                // Check for another object
                parser.consume_garbage();
            }
            if (parser.failed) {
                break;
            }
            parser_stop_pos = parser.i;
        }
        return std::make_tuple(json_vec, std::move(err));
    }

    /* * * * * * * * * * * * * * * * * * * *
     * Shape-checking
     */

    std::string json::has_shape(const shape& types) const {
        if (!is_object()) {
            return "expected JSON object, got " + dump();
        }

        const auto& obj_items = to_object();
        for (auto& item: types) {
            const auto it = obj_items.find(item.first);
            if (it == obj_items.cend() ||
                it->second.type() != item.second) {
                return "bad type for " + item.first + " in " + dump();
            }
        }
        return "";
    }
}