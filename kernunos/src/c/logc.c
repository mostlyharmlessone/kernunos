//have to call with call LogC("text here"//c_null_char) from Fortran
// from c++ call with LogC("text here")

#include "logc.h"
#include <bits/types/FILE.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>

bool LogCreated = false;
void LogC(const char *Message) { FILE *file;
    if (!LogCreated) { file = fopen(LOGFILE, "w");
        LogCreated = true; }
    else file = fopen(LOGFILE, "a");
    if (file == NULL) { if (LogCreated) LogCreated = false; return ; }
    else {
        fprintf(file, "%s", Message);
        fprintf(file,"\n");
        fclose(file);
        return;
    }
}
