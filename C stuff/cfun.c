#include <stdio.h>
#include <gsl/gsl_sf_bessel.h>

int cfun_(int *ip, double *xp)
{
int i = *ip;
double x = *xp;
printf("This is in C function...\n");
printf("i = %d, x = %g\n", i, x);
//   double x = 5.0;
double y = gsl_sf_bessel_J0 (x);
printf ("J0(%g) =%.18e\n", x, y);

return 0;
}


