// nanocode - minimal claude code alternative (C)
// gcc -o nanocode nanocode.c -lcurl && ./nanocode
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

#define R "\033[0m"
#define B "\033[1m"
#define D "\033[2m"
#define C "\033[36m"
#define G "\033[32m"
#define BL "\033[34m"

char *KEY, *MODEL, *API_URL;
char msgs[65536] = "[]";

typedef struct { char *data; size_t len; } Buf;

size_t write_cb(void *p, size_t s, size_t n, Buf *b) {
    size_t sz = s * n;
    b->data = realloc(b->data, b->len + sz + 1);
    memcpy(b->data + b->len, p, sz);
    b->len += sz;
    b->data[b->len] = 0;
    return sz;
}

char *run_tool(const char *name, const char *input) {
    static char result[4096];
    char cmd[1024], path[256], *p;
    
    if (strcmp(name, "read") == 0) {
        sscanf(strstr(input, "path") + 8, "%255[^\"]", path);
        FILE *f = fopen(path, "r");
        if (!f) return "error: file not found";
        result[0] = 0; int i = 1; char line[256];
        while (fgets(line, sizeof(line), f) && strlen(result) < 3800)
            sprintf(result + strlen(result), "%d| %s", i++, line);
        fclose(f); return result;
    }
    if (strcmp(name, "write") == 0) {
        sscanf(strstr(input, "path") + 8, "%255[^\"]", path);
        char *content = strstr(input, "content");
        if (content) {
            sscanf(content + 10, "%1000[^\"]", cmd);
            FILE *f = fopen(path, "w"); if (!f) return "error: cannot open file";
            fputs(cmd, f); fclose(f); return "ok";
        }
    }
    if (strcmp(name, "bash") == 0) {
        if ((p = strstr(input, "cmd"))) {
            sscanf(p + 6, "%1000[^\"]", cmd);
            FILE *fp = popen(cmd, "r");
            result[0] = 0;
            while (fgets(result + strlen(result), 256, fp) && strlen(result) < 3800);
            pclose(fp); return result;
        }
    }
    if (strcmp(name, "edit") == 0) {
        sscanf(strstr(input, "path") + 8, "%255[^\"]", path);
        char old[256], new[256];
        sscanf(strstr(input, "old") + 7, "%255[^\"]", old);
        sscanf(strstr(input, "new") + 7, "%255[^\"]", new);
        FILE *f = fopen(path, "r"); if (!f) return "error: file not found";
        char *buf = NULL; size_t len = 0; int c;
        while ((c = fgetc(f)) != EOF) { buf = realloc(buf, len + 2); buf[len++] = c; }
        buf[len] = 0; fclose(f);
        char *pos = strstr(buf, old); if (!pos) { free(buf); return "error: old_string not found"; }
        char *next = strstr(pos + strlen(old), old);
        if (next && !strstr(input, "\"all\":true")) { free(buf); return "error: old_string appears multiple times, use all=true"; }
        size_t outlen = len + strlen(new) - strlen(old) + 1;
        char *out = calloc(outlen, 1);
        char *cursor = buf;
        if (strstr(input, "\"all\":true")) {
            while ((pos = strstr(cursor, old))) {
                strncat(out, cursor, pos - cursor);
                strcat(out, new);
                cursor = pos + strlen(old);
            }
            strcat(out, cursor);
        } else {
            pos = strstr(cursor, old);
            strncat(out, cursor, pos - cursor);
            strcat(out, new);
            strcat(out, pos + strlen(old));
        }
        f = fopen(path, "w"); fwrite(out, 1, strlen(out), f); fclose(f);
        free(buf); free(out); return "ok";
    }
    if (strcmp(name, "glob") == 0) {
        sscanf(strstr(input, "pat") + 7, "%255[^\"]", path);
        sprintf(cmd, "find . -name '%s' 2>/dev/null | head -50", path);
        FILE *fp = popen(cmd, "r"); result[0] = 0;
        while (fgets(result + strlen(result), 256, fp)); pclose(fp); return result;
    }
    if (strcmp(name, "grep") == 0) {
        sscanf(strstr(input, "pat") + 7, "%255[^\"]", path); // reuse buffer
        sprintf(cmd, "grep -R \"%s\" . -n 2>/dev/null | head -50", path);
        FILE *fp = popen(cmd, "r"); result[0] = 0;
        while (fgets(result + strlen(result), 256, fp) && strlen(result) < 3800);
        pclose(fp); return result[0] ? result : "none";
    }
    return "ok";
}

char *api_call() {
    static char body[65536];
    snprintf(body, sizeof(body),
        "{\"model\":\"%s\",\"max_tokens\":4096,\"system\":\"Concise assistant\",\"messages\":%s,"
        "\"tools\":["
          "{\"name\":\"read\",\"description\":\"Read\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}},"
          "{\"name\":\"write\",\"description\":\"Write\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"content\":{\"type\":\"string\"}},\"required\":[\"path\",\"content\"]}},"
          "{\"name\":\"edit\",\"description\":\"Replace\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"old\":{\"type\":\"string\"},\"new\":{\"type\":\"string\"},\"all\":{\"type\":\"boolean\"}},\"required\":[\"path\",\"old\",\"new\"]}},"
          "{\"name\":\"bash\",\"description\":\"Run\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"cmd\":{\"type\":\"string\"}},\"required\":[\"cmd\"]}},"
          "{\"name\":\"glob\",\"description\":\"Find\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}},"
          "{\"name\":\"grep\",\"description\":\"Search\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}}]}", MODEL, msgs);
    
    CURL *curl = curl_easy_init();
    Buf resp = {0};
    struct curl_slist *hdrs = NULL;
    hdrs = curl_slist_append(hdrs, "Content-Type: application/json");
    hdrs = curl_slist_append(hdrs, "anthropic-version: 2023-06-01");
    char auth[128]; snprintf(auth, 128, "x-api-key: %s", KEY);
    hdrs = curl_slist_append(hdrs, auth);
    
    curl_easy_setopt(curl, CURLOPT_URL, API_URL);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, hdrs);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_cb);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_perform(curl);
    curl_easy_cleanup(curl);
    curl_slist_free_all(hdrs);
    return resp.data;
}

int main() {
    KEY = getenv("ANTHROPIC_API_KEY");
    MODEL = getenv("MODEL"); if (!MODEL) MODEL = "claude-sonnet-4-20250514";
    API_URL = getenv("API_URL"); if (!API_URL) API_URL = "https://api.anthropic.com/v1/messages";
    printf(B "nanocode" R " | " D "%s" R "\n\n", MODEL);
    
    char input[1024];
    while (1) {
        printf(B BL "❯" R " ");
        if (!fgets(input, sizeof(input), stdin)) break;
        input[strcspn(input, "\n")] = 0;
        if (!input[0]) continue;
        if (strcmp(input, "/q") == 0) break;
        if (strcmp(input, "/c") == 0) { strcpy(msgs, "[]"); printf(G "⏺ Cleared" R "\n"); continue; }
        
        char tmp[65536];
        snprintf(tmp, sizeof(tmp), "%.*s{\"role\":\"user\",\"content\":\"%s\"}]", (int)(strlen(msgs)-1), msgs, input);
        strcpy(msgs, tmp);
        
        char *resp = api_call();
        char *txt = strstr(resp, "\"text\":\"");
        if (txt) {
            txt += 8; char *end = strstr(txt, "\",\""); if (end) *end = 0;
            printf("\n" C "⏺" R " %s\n\n", txt);
        }
        char *tool = strstr(resp, "\"tool_use\"");
        if (tool) {
            char *nm = strstr(tool, "\"name\":\"") + 8;
            char name[32]; sscanf(nm, "%31[^\"]", name);
            printf("\n" G "⏺ %s" R "\n", name);
            char *inp = strstr(tool, "\"input\":{");
            char *result = run_tool(name, inp);
            printf("  " D "⎿ %.60s" R "\n\n", result);
        }
        free(resp);
    }
    return 0;
}
