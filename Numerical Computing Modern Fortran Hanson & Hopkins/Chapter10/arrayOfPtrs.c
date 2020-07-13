#include <stdlib.h>
#include <stdio.h>

#define LDA 5
#define N 2

void PtrArray(double *a[], int *m, int *n);

int main()
{
  int i, j, m, n;
  int sizes[2];
  double rval;
  double *a[2];

/* Set up two vectors of data */
/* First of length 3 containing 1.0, 2.0, 3.0 */
/* Second of length 5 containing 9.0 (1.0) 13.0 */

  a[0] = (double *)malloc(3*sizeof(double));
  sizes[0]=3;
  a[1] = (double *)malloc(5*sizeof(double));
  sizes[1]=5;

  rval = 0.0;
  for (i=0; i<3; i++){
    rval += 1.0;
    a[0][i] = rval;
  }
  for (i=0; i<5; i++){
    rval += 1.0;
    a[1][i] = rval + 5.0;
  }

  m = 2;

/* Print out the vectors using the Fortran routine */
  (void) PtrArray(a,&m,sizes);

}
