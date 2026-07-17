//https://stackoverflow.com/questions/72566680/count-occurrences-of-character-in-file-c

#include "charcount.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>

int charcount(const char *filename) {
//    LogC("Open a file for counting periods");
//    LogC(filename);
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
     LogC("Failure to open file for counting periods");
     LogC(filename);
     perror("fopen");
     return(-1);
    }
    else {
//    LogC("Success");
    int count = 0;
    int c;
    while ((c = getc(fp)) != EOF) {
        count += (c == '.');
    }
    fclose(fp);
    return(count);
}
}
