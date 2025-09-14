//https://stackoverflow.com/questions/72566680/count-occurrences-of-character-in-file-c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

void charcount(int* count, const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
    }
    count = 0;
    int c;
    while ((c = getc(fp)) != EOF) {
        count += (c == '.');
    }
    fclose(fp);
}
