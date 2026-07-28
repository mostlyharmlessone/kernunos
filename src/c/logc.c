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
        perror("getenv() error"); return ;
    }
    size_t lenp = strlen(path);
    size_t len = strlen(LOGFILE);
    char pathandfile[len+lenp+2];
#ifdef _WIN32
    snprintf(pathandfile, sizeof(pathandfile), "%s%s%s", path, "\\", LOGFILE);
#else
    snprintf(pathandfile, sizeof(pathandfile), "%s%s%s", path,"/",LOGFILE);
#endif
    if (!LogCreated) { file = fopen(pathandfile, "w");
        LogCreated = true; }
    else file = fopen(pathandfile, "a");
    if (file == NULL) { if (LogCreated) LogCreated = false; return ; }
    else {
        time_t timeStamp;
        time(&timeStamp);
        fprintf(file, "%s", ctime(&timeStamp));
        fprintf(file, "%s", " ");
        fprintf(file, "%s", Message);
        fprintf(file,"\n");
        fclose(file);
        return;
    }
}
