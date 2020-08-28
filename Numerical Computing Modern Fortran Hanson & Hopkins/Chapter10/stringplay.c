#include<stdio.h>
#include<string.h>

void copy_string(char cstring[], char fstring[], int *len, int* ftoc);

void c_print(char cstring[]){
  printf ("String printed from C: %s\n",cstring);
}

void c_setstring(char cstring[]){
  (void) strcpy(cstring, "C set me!");
}
