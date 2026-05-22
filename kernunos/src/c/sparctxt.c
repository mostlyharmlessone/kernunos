/* sparctxt.c */
/* changes DOS CR/LF to SPARC LF */
#include <stdio.h>
main(argc,argv)
int argc;
char *argv[];
{
	FILE *fptr1;
	FILE *fptr2;
	char ch;
	if(argc !=3)
	{ printf("usage: sparctxt filename1 filename2"); exit();}
	if( (fptr1 = fopen(argv[1],"rb")) == NULL)
	{ printf("can't open file %s.",argv[1]); exit();};
	fptr2 = fopen(argv[2],"wb");
	while( (ch=getc(fptr1)) != EOF )
	{
		if(ch != 13) {putc(ch,fptr2);};
	}
	fclose(fptr1);
	fclose(fptr2);
	
}
