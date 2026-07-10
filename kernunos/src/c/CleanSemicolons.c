/* CleanSemicolons.c */
/* changes to ; (3B) to , (2C) */
#include <stdio.h>
int main(int argc, char *argv[]){
	FILE *fptr1;
	FILE *fptr2;
	char ch;
	if(argc !=3)
        { printf("usage: CleanSemicolons filename1 filename2 \n"); return(1);}
	if( (fptr1 = fopen(argv[1],"rb")) == NULL)
        { printf("can't open file %s \n",argv[1]); return(2);};
        if( (fptr2 = fopen(argv[2],"wb")) == NULL)
        { printf("can't open file %s \n",argv[2]); return(3);};
	while( (ch=getc(fptr1)) != EOF ) {
		if(ch == 0x3B) {putc(0x2C,fptr2);};
                if(ch != 0x3B) {putc(ch,fptr2);};
	};
	fclose(fptr1);
	fclose(fptr2);
        printf("Wrote file %s \n",argv[2]);
        return(0);
	
}

