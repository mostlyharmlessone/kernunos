#include <stdio.h>

#define LDA 5
#define LDB 5
#define LDC 5

//!!snippet cdgemm.c
void C_dgemm(char* transa, char* transb, int* m, int* n,  int* k,
	     double* alpha, double a[][LDA], int* lda, double b[][LDB],
	     int* ldb, double* beta, double c[][LDC], int* ldc);
//!!end snippet cdgemm.c

int main() {
  int m, n, k, lda, ldb, ldc;
  int i, j;
  double  alpha, beta;
  double  rval;
  double  a[2][LDA], b[2][LDB], c[5][LDC];
  char transa, transb;

  double answer[5][5] = 
     {653.0,760.0,871.0,982.0,1093.0,
     892.0,1057.0,1212.0,1371.0,1530.0,
     1177.0,1398.0,1623.0,1838.0,2057.0,
     1504.0,1797.0,2090.0,2387.0,2674.0,
     1873.0,2250.0,2627.0,3004.0,3385.0};

  m = 5;
  k = 2;
  n = 5;
  alpha = 3.0;
  beta = 2.0;
  lda = LDA;
  ldb = LDB;
  ldc = LDC;
  transa = 'n';
  transb = 'T';

  rval = 0.0;
  for (j=0; j<2; j++) {
    for (i=0; i<5; i++){
      rval += 1.0;
      a[j][i] = rval;
      b[j][i] = rval * rval;
    }
  }
  for (i=0; i<5; i++){
    c[i][i] = (double) (i+1);
  }

  for (i=0; i<4; i++){
    for (j=i+1; j<5; j++) {
      c[i][j] = c[j][i] = (double) (i-1);
    }
  }

  (void) C_dgemm(&transa, &transb, &m, &n, &k, &alpha, a, &lda, b,
		 &ldb, &beta, c, &ldc);

  int ndiffs = 0;
  for (i=0; i<5; i++){
    for (j=0; j<5; j++){
      if (c[i][j] != answer[i][j]) {
	ndiffs++;
	printf ("Error: c[%d, %d] = %f answer[%d, %d] = %f\n",
		i, j, c[i][j],i, j, answer[i][j]);
      }
    }
  }

  if (ndiffs == 0) {
    printf ("dgemm test passed\n");
  }
  else {
    printf ("dgemm test failed\n");
  }

}
