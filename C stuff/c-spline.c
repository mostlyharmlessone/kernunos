
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_spline.h>

int cfun_(double *xp[], double *yp[], int *np)

{
   int i;
   int n = *np;
   double x[] = *xp[];
   double y[] = *yp[];
   double xi, yi;

   printf ("#m=0,S=17\n");

   for (i = 0; i < n; i++)
    {
        printf ("%g %g\n", x[i], y[i]);
     }

   printf ("#m=1,S=0\n");

  {
     gsl_interp_accel *acc
        = gsl_interp_accel_alloc ();
     gsl_spline *spline
        = gsl_spline_alloc (gsl_interp_cspline, n);

     gsl_spline_init (spline, x, y, n);

//     for (xi = x[0]; xi < x[n-1]; xi += 0.01)
//       {
//         yi = gsl_spline_eval (spline, xi, acc);
//          printf ("%g %g\n", xi, yi);
//       }

     gsl_spline_free (spline);
     gsl_interp_accel_free (acc);
  }
   return 0;
}

