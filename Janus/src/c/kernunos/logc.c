#include "logc.h"
#include "counter.h"
#include <bits/types/FILE.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>

bool LogCreated = false;
void LogC(const char *Message, int *inc) { FILE *file;
    if (!LogCreated) { file = fopen(LOGFILE, "w");
        LogCreated = true; }
    else file = fopen(LOGFILE, "a");
    if (file == NULL) { if (LogCreated) LogCreated = false; return ; }
    else {

    //    fprintf(file,"%d",(int)*inc);
    //    fprintf(file,"\n");

        if ((int)*inc == 0) {
        fprintf(file, "%s", Message);
        fprintf(file,"\n");
        fclose(file);}
        else {
        counter=counter+(int)*inc;}
    }
        return;
    }
