% nanocode - minimal claude code alternative (CWEB)
% ctangle nanocode.w && gcc nanocode.c -o nanocode
% CWEB: Donald Knuth's literate programming system (1984)

\datethis

@* Introduction.
This is \.{nanocode}, a minimal Claude Code alternative written in CWEB.
CWEB is Donald Knuth's literate programming system that weaves documentation
and code together.

@ The program structure follows Knuth's conventions: we explain what we're
doing before we do it. This makes code readable as a document.

@c
@<Include files@>@;
@<Global variables@>@;
@<Function prototypes@>@;
@<Main program@>@;
@<Tool functions@>@;

@* Headers and Setup.
We include the standard C headers:

@<Include files@>=
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

@ ANSI color codes for terminal output:

@<Global variables@>=
const char *R  = "\x1b[0m";
const char *B  = "\x1b[1m";
const char *D  = "\x1b[2m";
const char *C  = "\x1b[36m";
const char *G  = "\x1b[32m";
const char *BL = "\x1b[34m";

@ We need storage for messages:

@<Global variables@>=
char messages[100][1024];
int msg_count = 0;

@* Tool Functions.
Each tool performs a specific operation. We define them here:

@ The |read_tool| function reads a file and numbers each line:

@<Tool functions@>=
char* read_tool(const char *path) {
    static char result[65536];
    FILE *f = fopen(path, "r");
    if (!f) return "error: file not found";
    
    result[0] = '\0';
    char line[1024];
    int num = 1;
    while (fgets(line, sizeof(line), f)) {
        char numbered[1040];
        sprintf(numbered, "%d| %s", num++, line);
        strcat(result, numbered);
    }
    fclose(f);
    return result;
}

@ The |write_tool| function writes content to a file:

@<Tool functions@>=
char* write_tool(const char *path, const char *content) {
    FILE *f = fopen(path, "w");
    if (!f) return "error: cannot write";
    fprintf(f, "%s", content);
    fclose(f);
    return "ok";
}

@ The |bash_tool| function executes a shell command:

@<Tool functions@>=
char* bash_tool(const char *cmd) {
    static char result[65536];
    FILE *pipe = popen(cmd, "r");
    if (!pipe) return "error";
    
    result[0] = '\0';
    char buf[1024];
    while (fgets(buf, sizeof(buf), pipe)) {
        strcat(result, buf);
    }
    pclose(pipe);
    return result;
}

@* Main Program.
The entry point of our program:

@<Main program@>=
int main() {
    char input[1024];
    
    printf("%snanocode%s | %sCWEB - Knuth's Literate System%s\n\n", 
           B, R, D, R);
    
    while (1) {
        printf("%s%s>%s ", B, BL, R);
        if (!fgets(input, sizeof(input), stdin)) break;
        
        /* Remove newline */
        input[strcspn(input, "\n")] = 0;
        
        if (strlen(input) == 0) continue;
        if (strcmp(input, "/q") == 0) break;
        if (strcmp(input, "/c") == 0) {
            msg_count = 0;
            printf("%s* Cleared%s\n", G, R);
            continue;
        }
        
        /* Store message */
        strcpy(messages[msg_count++], input);
        
        /* Response (would call API) */
        printf("\n%s*%s CWEB: Literate Programming since 1984!\n", C, R);
        printf("%s  TeX + C = Beautiful Documentation%s\n\n", D, R);
    }
    
    printf("Goodbye!\n");
    return 0;
}

@ Function prototypes are needed in C:

@<Function prototypes@>=
char* read_tool(const char *path);
char* write_tool(const char *path, const char *content);
char* bash_tool(const char *cmd);

@* Why CWEB for AI Agents?
\noindent
Donald Knuth created CWEB for writing \.{TeX} - one of the most complex
software projects ever. His philosophy:

\smallskip
\centerline{\it ``Programs are meant to be read by humans}
\centerline{\it and only incidentally for computers to execute.''}
\smallskip

In the AI era, this becomes even more relevant:

\item{1.} AI agents should explain their reasoning
\item{2.} Documentation and code must stay synchronized
\item{3.} Complex logic needs human-readable explanations
\item{4.} Maintenance requires understanding design decisions

\.{CWEB} produces both:
\item{$\bullet$} Executable code (via \.{ctangle})
\item{$\bullet$} Beautiful documentation (via \.{cweave} + \TeX)

@* Index.
The index is generated automatically by CWEB, showing where
each identifier is defined and used - perfect for understanding
AI agent code!
