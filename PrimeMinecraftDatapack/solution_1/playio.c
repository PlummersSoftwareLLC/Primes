#define _GNU_SOURCE
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <inttypes.h>
#include <time.h>
#include <string.h>
#include <errno.h>

#define PIPE_READ   0
#define PIPE_WRITE  1

int hascmdparam(const char *str);
char *getcmdparam(char *str);
char *clipnewline(char *str);
void runscript(int childStdinFD, int childStdoutFD);

const char newline = '\n';

int main(int argc, const char* argv[]) {
    int stdinPipe[2];
    if (pipe(stdinPipe) < 0) {
        perror("allocating pipe for child input redirect");
        return errno;
    }

    int stdoutPipe[2];
    if (pipe(stdoutPipe) < 0) {
        close(stdinPipe[PIPE_READ]);
        close(stdinPipe[PIPE_WRITE]);
        perror("allocating pipe for child output redirect");
        return errno;
    }

    int childPid = fork();
    if (childPid == 0) {
        // child continues here

        // redirect stdin
        if (dup2(stdinPipe[PIPE_READ], STDIN_FILENO) == -1) {
            perror("redirecting child stdin");
            return errno;
        }

        // redirect stdout
        if (dup2(stdoutPipe[PIPE_WRITE], STDOUT_FILENO) == -1) {
            perror("redirecting child stdout");
            return errno;
        }

        // redirect stderr
        if (dup2(stdoutPipe[PIPE_WRITE], STDERR_FILENO) == -1) {
            perror("redirecting child stderr");
            return errno;
        }

        // all these are for use by parent only
        close(stdinPipe[PIPE_READ]);
        close(stdinPipe[PIPE_WRITE]);
        close(stdoutPipe[PIPE_READ]);
        close(stdoutPipe[PIPE_WRITE]);

        // command line options denote program to run, and its command line options
        char *childArgs[argc];

        for (int i = 1; i < argc; i++) {
            int argLen = strlen(argv[i]);

            childArgs[i - 1] = malloc(argLen + 1);
            strncpy(childArgs[i - 1], argv[i], argLen);
            childArgs[i - 1][argLen] = 0; 
        }

        childArgs[argc - 1] = NULL;

        return execvp(argv[1], childArgs);
    }
    else if (childPid > 0) {
        // parent continues here

        // close unused file descriptors, these are for child only
        close(stdinPipe[PIPE_READ]);
        close(stdoutPipe[PIPE_WRITE]);

        runscript(stdinPipe[PIPE_WRITE], stdoutPipe[PIPE_READ]);

        close(stdinPipe[PIPE_WRITE]);
        close(stdoutPipe[PIPE_READ]);
    }
    else {
        // failed to create child
        close(stdinPipe[PIPE_READ]);
        close(stdinPipe[PIPE_WRITE]);
        close(stdoutPipe[PIPE_READ]);
        close(stdoutPipe[PIPE_WRITE]);
        return errno;
    }

    return 0;
}

void runscript(int childStdinFD, int childStdoutFD) {
    char *stdinLine = NULL;
    size_t stdinBufLen = 0;
    ssize_t stdinReadLen = -1;

    char *childLine = NULL;
    size_t childBufLen = 0;
    ssize_t childReadLen = -1;

    FILE *childStdout = fdopen(childStdoutFD, "r");

    while((stdinReadLen = getline(&stdinLine, &stdinBufLen, stdin)) != -1) {
        if (stdinReadLen == 0)
            continue;

        switch(stdinLine[0]) {

            // output timestamp (μs since epoch)
            case 't': {
                struct timespec tms;

                if (timespec_get(&tms, TIME_UTC)) {
                    int64_t micros = tms.tv_sec * 1000000;
                    micros += tms.tv_nsec / 1000;

                    if (tms.tv_nsec % 1000 >= 500)
                        ++micros;

                    const char *param = hascmdparam(stdinLine) ? getcmdparam(stdinLine) : "";

                    printf("%s%"PRId64"\n", param, micros);
                }
            }
            break;

            // read line(s) from child stdout, optionally until a specific string is found
            case 'f': {
                const char *param = hascmdparam(stdinLine) ? getcmdparam(stdinLine) : NULL;

                while((childReadLen = getline(&childLine, &childBufLen, childStdout)) != -1) {
                    // try to find the search string, if we have one
                    if (param == NULL || strstr(childLine, param) != NULL)
                        break;
                }
            }
            break;

            // output string or last line read from child stdout
            case 'o': {
                if (hascmdparam(stdinLine)) {
                    printf("%s\n", getcmdparam(stdinLine));
                    break;
                }
                
                if (childReadLen == -1)
                    break;

                printf("%s\n", clipnewline(childLine));
            }
            break;

            // pause for a specified number of seconds; fractional numbers are supported
            case 'p': {
                if (!hascmdparam(stdinLine))
                    return;

                double duration = atof(getcmdparam(stdinLine));
                struct timespec tms = {
                    .tv_sec = (time_t)duration,
                    .tv_nsec = (long)((duration - (long)duration) * 1000000000)
                };

                nanosleep(&tms, NULL);
            }
            break;

            // write (optional) string to child stdin
            case 'w': {
                if (hascmdparam(stdinLine)) {
                    const char *param = getcmdparam(stdinLine);
                    write(childStdinFD, param, strlen(param));
                }

                write(childStdinFD, &newline, 1);
            }
            break;

            default:
            break;
        }
    } // end of while

    // read and discard any remaining output from child
    while(getline(&childLine, &childBufLen, childStdout) != -1);

    free(stdinLine);
    free(childLine);
}

inline int hascmdparam(const char *str) {
    size_t strLen = strlen(str);
    return strLen > 3 || (strLen == 3 && str[2] != newline);
}

inline char *getcmdparam(char *str) {
    return clipnewline(&str[2]);
}

inline char *clipnewline(char *str) {
    size_t strLen = strlen(str);

    if (strLen == 0)
        return str;

    if (str[strLen - 1] == newline)
        str[strLen - 1] = 0;

    return str;
}