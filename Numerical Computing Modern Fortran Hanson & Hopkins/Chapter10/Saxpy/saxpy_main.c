#include <stdio.h>

void C_saxpy(int *n, float *sa, float sx[], int *incx,
             float sy[], int *incy);
void C_val_saxpy(int n, float sa, float sx[], int incx,
             float sy[], int incy);

int main() {
  int incx, incy, n, i;
  float sa, sx[5], sy[5];
  int ndiffs;

  float answer[5] = {6.0, 9.0, 12.0, -1.0, -2.0};

  n=5;
  incx=2;
  incy=1;
  sa = (float) 2.0;

  for (i=0; i<5; i++){
    sx[i] = (float)(i+2);
    sy[i]=(float)(2-i);
  }

  (void) C_saxpy(&n, &sa, sx, &incx, sy, &incy);

  ndiffs = 0;
  for (i=0; i<5; i++){
    if (sy[i] != answer[i]) {
      ndiffs++;
      printf ("sy[%d] = %f answer[%d] = %f\n", i, sy[i], i, answer[i]);
    }
  }

  if (ndiffs == 0) {
    printf ("Test C_saxpy passed\n");
  }
  else {
    printf ("Test C_saxpy failed\n");
  }


  for (i=0; i<5; i++){
    sx[i] = (float)(i+2);
    sy[i]=(float)(2-i);
  }

  (void) C_val_saxpy(n, sa, sx, incx, sy, incy);

  ndiffs = 0;
  for (i=0; i<5; i++){
    if (sy[i] != answer[i]) {
      ndiffs++;
      printf ("sy[%d] = %f answer[%d] = %f\n", i, sy[i], i, answer[i]);
    }
  }


  if (ndiffs == 0) {
    printf ("Test C_val_saxpy passed\n");
  }
  else {
    printf ("Test C_val_saxpy failed\n");
  }



}
