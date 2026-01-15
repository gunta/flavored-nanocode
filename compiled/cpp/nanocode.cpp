// nanocode - minimal claude code alternative (C++)
// g++ -std=c++17 -o nanocode nanocode.cpp -lcurl && ./nanocode
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <curl/curl.h>
#include <cstdlib>
#include <regex>

#define R "\033[0m"
#define B "\033[1m"
#define D "\033[2m"
#define C "\033[36m"
#define G "\033[32m"
#define BL "\033[34m"

std::string KEY, MODEL, API;
std::string msgs = "[]";

size_t write_cb(void* p, size_t s, size_t n, std::string* out) {
    out->append((char*)p, s * n);
    return s * n;
}

std::string run_tool(const std::string& name, const std::string& input) {
    std::regex path_re(R"("path"\s*:\s*"([^"]*)")");
    std::regex cmd_re(R"("cmd"\s*:\s*"([^"]*)")");
    std::regex pat_re(R"("pat"\s*:\s*"([^"]*)")");
    std::regex old_re(R"("old"\s*:\s*"([^"]*)")");
    std::regex new_re(R"("new"\s*:\s*"([^"]*)")");
    std::regex content_re(R"("content"\s*:\s*"([^"]*)")");
    std::regex off_re(R"("offset"\s*:\s*(\d+))");
    std::regex lim_re(R"("limit"\s*:\s*(\d+))");
    std::smatch m;
    
    if (name == "read" && std::regex_search(input, m, path_re)) {
        std::ifstream f(m[1]);
        std::string result, line;
        int off = std::regex_search(input, m, off_re) ? std::stoi(m[1]) : 0;
        int lim = std::regex_search(input, m, lim_re) ? std::stoi(m[1]) : 0;
        int line_no = 0, emitted = 0;
        while (std::getline(f, line) && (lim == 0 || emitted < lim)) {
            if (line_no++ < off) continue;
            result += std::to_string(line_no) + "| " + line + "\n";
            emitted++;
        }
        return result;
    }
    if (name == "write" && std::regex_search(input, m, path_re) && std::regex_search(input, m, content_re)) {
        std::string path = m[1];
        std::regex_search(input, m, content_re);
        std::string content = m[1];
        std::ofstream f(path); f << content; f.close(); return "ok";
    }
    if (name == "edit" && std::regex_search(input, m, path_re) && std::regex_search(input, m, old_re) && std::regex_search(input, m, new_re)) {
        std::string path = m[1], oldv = m[1], newv;
        std::regex_search(input, m, old_re); oldv = m[1];
        std::regex_search(input, m, new_re); newv = m[1];
        bool all = input.find("\"all\":true") != std::string::npos;
        std::ifstream f(path); if (!f) return "error: file not found";
        std::stringstream buf; buf << f.rdbuf(); std::string txt = buf.str(); f.close();
        size_t first = txt.find(oldv);
        if (first == std::string::npos) return "error: old_string not found";
        if (!all && txt.find(oldv, first + oldv.size()) != std::string::npos) return "error: old_string appears multiple times, use all=true";
        if (all) {
            size_t pos = 0;
            while ((pos = txt.find(oldv, pos)) != std::string::npos) { txt.replace(pos, oldv.size(), newv); pos += newv.size(); }
        } else {
            txt.replace(first, oldv.size(), newv);
        }
        std::ofstream o(path); o << txt; o.close();
        return "ok";
    }
    if (name == "bash" && std::regex_search(input, m, cmd_re)) {
        char buf[256];
        std::string result;
        FILE* fp = popen(m[1].str().c_str(), "r");
        while (fp && fgets(buf, sizeof(buf), fp)) result += buf;
        if (fp) pclose(fp);
        return result;
    }
    if (name == "glob" && std::regex_search(input, m, pat_re)) {
        char buf[256];
        std::string result;
        std::string cmd = "find . -name '" + m[1].str() + "' | head -50";
        FILE* fp = popen(cmd.c_str(), "r");
        while (fp && fgets(buf, sizeof(buf), fp)) result += buf;
        if (fp) pclose(fp);
        return result;
    }
    if (name == "grep" && std::regex_search(input, m, pat_re)) {
        char buf[256];
        std::string result;
        std::string cmd = "grep -R \"" + m[1].str() + "\" . -n 2>/dev/null | head -50";
        FILE* fp = popen(cmd.c_str(), "r");
        while (fp && fgets(buf, sizeof(buf), fp)) result += buf;
        if (fp) pclose(fp);
        return result.empty() ? "none" : result;
    }
    return "ok";
}

std::string api_call() {
    std::string body = R"({"model":")" + MODEL + R"(","max_tokens":4096,"system":"Concise assistant","messages":)" + msgs + R"(,)"
        R"("tools":[)"
        R"({"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer"},"limit":{"type":"integer"}},"required":["path"]}},)"
        R"({"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},)"
        R"({"name":"edit","description":"Replace","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"},"all":{"type":"boolean"}},"required":["path","old","new"]}},)"
        R"({"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},)"
        R"({"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},)"
        R"({"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}}]})";
    
    CURL* curl = curl_easy_init();
    std::string resp;
    struct curl_slist* hdrs = nullptr;
    hdrs = curl_slist_append(hdrs, "Content-Type: application/json");
    hdrs = curl_slist_append(hdrs, "anthropic-version: 2023-06-01");
    if (const char* ok = std::getenv("OPENROUTER_API_KEY")) {
        hdrs = curl_slist_append(hdrs, ("Authorization: Bearer " + std::string(ok)).c_str());
    } else {
        hdrs = curl_slist_append(hdrs, ("x-api-key: " + KEY).c_str());
    }
    
    curl_easy_setopt(curl, CURLOPT_URL, API.c_str());
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, hdrs);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_cb);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_perform(curl);
    curl_easy_cleanup(curl);
    curl_slist_free_all(hdrs);
    return resp;
}

int main() {
    const char* or_key = std::getenv("OPENROUTER_API_KEY");
    const char* an_key = std::getenv("ANTHROPIC_API_KEY");
    KEY = or_key ? or_key : (an_key ? an_key : "");
    API = or_key ? "https://openrouter.ai/api/v1/messages" : "https://api.anthropic.com/v1/messages";
    MODEL = std::getenv("MODEL") ?: (or_key ? "anthropic/claude-opus-4-5" : "claude-opus-4-5");
    std::cout << B "nanocode" R " | " D << MODEL << R "\n\n";
    
    std::string input;
    while (true) {
        std::cout << B BL "❯" R " " << std::flush;
        if (!std::getline(std::cin, input)) break;
        while (!input.empty() && (input.back() == ' ' || input.back() == '\n')) input.pop_back();
        if (input.empty()) continue;
        if (input == "/q") break;
        if (input == "/c") { msgs = "[]"; std::cout << G "⏺ Cleared" R "\n"; continue; }
        
        // Escape input for JSON
        std::string escaped;
        for (char c : input) {
            if (c == '"') escaped += "\\\"";
            else if (c == '\\') escaped += "\\\\";
            else if (c == '\n') escaped += "\\n";
            else escaped += c;
        }
        msgs = msgs.substr(0, msgs.length()-1) + R"({"role":"user","content":")" + escaped + R"("}])";
        
        std::string resp = api_call();
        
        // Extract text
        std::regex text_re(R"("type"\s*:\s*"text"[^}]*"text"\s*:\s*"([^"]*)")");
        std::smatch m;
        if (std::regex_search(resp, m, text_re))
            std::cout << "\n" C "⏺" R " " << m[1] << "\n";
        
        // Extract tool use
        std::regex tool_re(R"("type"\s*:\s*"tool_use"[^}]*"name"\s*:\s*"(\w+)"[^}]*"input"\s*:\s*\{([^}]*)\})");
        if (std::regex_search(resp, m, tool_re)) {
            std::string name = m[1], inp = "{" + m[2].str() + "}";
            std::cout << "\n" G "⏺ " << name << R "\n";
            std::string result = run_tool(name, inp);
            std::cout << "  " D "⎿ " << result.substr(0, result.find('\n')) << R "\n";
        }
        std::cout << "\n";
    }
    return 0;
}
