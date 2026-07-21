/* needs c_null_char terminated from Fortran with no extra whitespace */
/* CleanSemicolons_C.c */
/* changes to ; (3B) to , (2C) */
#include <stdio.h>
int CleanSemicolons_C(const char *iname, const char *oname)
{
	FILE *fptr1;
	FILE *fptr2;
	char ch;
	if( (fptr1 = fopen(iname,"rb")) == NULL)
        { printf("can't open file %s \n",iname); return(2);};
        if( (fptr2 = fopen(oname,"wb")) == NULL)
        { printf("can't open file %s \n",oname); fclose(fptr1); return(3);};
	while( (ch=getc(fptr1)) != EOF ) {
		if(ch == 0x3B) {putc(0x2C,fptr2);};
                if(ch != 0x3B) {putc(ch,fptr2);};
	};
	fclose(fptr1);
	fclose(fptr2);
        printf("Wrote file %s \n",oname);
        return(0);
	
}

