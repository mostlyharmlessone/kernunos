//have to call with call LogC("text here"//c_null_char) from Fortran
// from c++ call with LogC("text here")

#include "logc.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <string.h>
#endif

bool LogCreated = false;
void LogC(const char *Message) { FILE *file;
    char *path;
#ifdef _WIN32
   path = getenv("USERPROFILE");
#else
    path = getenv("USER");
#endif
    if ( path == NULL )
    {
        path = ".";
        fprintf(stdout,"getenv() failed to get user path, attempting to write to local directory\n");
    }
    size_t lenp = strlen(path);
    size_t len = strlen(LOGFILE);
#ifdef _WIN32
    char pathandfile[len+lenp+2];
    snprintf(pathandfile, sizeof(pathandfile), "%s%s%s", path, "\\", LOGFILE);
#else
#ifdef __APPLE__
    char pathandfile[len+lenp+9];
    snprintf(pathandfile, sizeof(pathandfile), "%s%s%s%s", "/Users/" ,path, "/", LOGFILE);
#else
    char pathandfile[len+lenp+8];
    snprintf(pathandfile, sizeof(pathandfile), "%s%s%s%s", "/home/" ,path, "/", LOGFILE);
#endif
#endif
    if (!LogCreated) { file = fopen(pathandfile, "w");
        LogCreated = true; }
    else file = fopen(pathandfile, "a");
    if (file == NULL) { if (LogCreated) LogCreated = false; return ; }
    else {
        time_t timeStamp;
        time(&timeStamp);
        fprintf(file, "%.24s%s%s", ctime(&timeStamp)," ",Message);  //the .24sis the first 24 characters of the data and time, omitting the CR/LF etc.
        fprintf(file,"\n");
        fclose(file);
        return;
    }
}
