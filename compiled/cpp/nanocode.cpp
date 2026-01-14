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

std::string KEY, MODEL;
std::string msgs = "[]";

size_t write_cb(void* p, size_t s, size_t n, std::string* out) {
    out->append((char*)p, s * n);
    return s * n;
}

std::string run_tool(const std::string& name, const std::string& input) {
    std::regex path_re(R"("path"\s*:\s*"([^"]*)")");
    std::regex cmd_re(R"("cmd"\s*:\s*"([^"]*)")");
    std::regex pat_re(R"("pat"\s*:\s*"([^"]*)")");
    std::smatch m;
    
    if (name == "read" && std::regex_search(input, m, path_re)) {
        std::ifstream f(m[1]);
        std::string result, line;
        for (int i = 1; std::getline(f, line) && i <= 50; ++i)
            result += std::to_string(i) + "| " + line + "\n";
        return result;
    }
    if (name == "bash" && std::regex_search(input, m, cmd_re)) {
        char buf[256];
        std::string result;
        FILE* fp = popen(m[1].str().c_str(), "r");
        while (fgets(buf, sizeof(buf), fp)) result += buf;
        pclose(fp);
        return result;
    }
    if (name == "glob" && std::regex_search(input, m, pat_re)) {
        char buf[256];
        std::string result;
        std::string cmd = "find . -name '" + m[1].str() + "' | head -50";
        FILE* fp = popen(cmd.c_str(), "r");
        while (fgets(buf, sizeof(buf), fp)) result += buf;
        pclose(fp);
        return result;
    }
    return "ok";
}

std::string api_call() {
    std::string body = R"({"model":")" + MODEL + R"(","max_tokens":4096,"system":"Concise assistant","messages":)" + msgs + R"(,)"
        R"("tools":[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},)"
        R"({"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},)"
        R"({"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}}]})";
    
    CURL* curl = curl_easy_init();
    std::string resp;
    struct curl_slist* hdrs = nullptr;
    hdrs = curl_slist_append(hdrs, "Content-Type: application/json");
    hdrs = curl_slist_append(hdrs, "anthropic-version: 2023-06-01");
    hdrs = curl_slist_append(hdrs, ("x-api-key: " + KEY).c_str());
    
    curl_easy_setopt(curl, CURLOPT_URL, "https://api.anthropic.com/v1/messages");
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
    KEY = std::getenv("ANTHROPIC_API_KEY") ?: "";
    MODEL = std::getenv("MODEL") ?: "claude-sonnet-4-20250514";
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
