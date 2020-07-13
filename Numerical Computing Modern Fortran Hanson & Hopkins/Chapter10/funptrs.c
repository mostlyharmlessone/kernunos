/*!!snippet funptrs.c*/
#include <stdio.h>

float g (float*);
float h (float*);
float c_sam (float(*fun)(float*), float*);

int main()
{
  float (*fp)(float*);
  float x, y;

  fp = g;
  x = (float) 5.0;

  if (c_sam(fp, &x) == (float) 25.0) {
    printf("Test passed for function g\n");
  }
  else {
    printf("Test failed for function g: expected %f got %f\n", 
       (float) 25.0, c_sam(fp, &x));  
  }

  fp = h;
  x = (float) 6.0;

  if (c_sam(fp, &x) == (float) 216.0) {
    printf("Test passed for function h\n");
  }
  else {
    printf("Test failed for function h: expected %f got %f\n", 
       (float) 216.0, c_sam(fp, &x));  
  }

}

float g(float* x)
{
  return ((*x)*(*x));
}

float h(float* x)
{
  return ((*x)*(*x)*(*x));
}

/*!!end snippet funptrs.c*/
