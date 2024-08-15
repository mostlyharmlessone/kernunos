//from Fortran call with Ccounter(10) or Ccounter(inc) where inc is a c_int
// from C++ call with Ccounter(&inc) where inc is an integer

#include "counter.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

void Ccounter(int *inc, const char *iname) {
        counter=(int)*inc;
        if (counter == 100) {int wrote=gnuplot_zern(iname);
            if (wrote == 0) {
                fprintf(stdin,"gnuplot call succeeded\n");
                LogC("gnuplot call succeeded\n");}
            else {
                fprintf(stdin,"gnuplot call failed!\n");
                LogC("gnuplot call failed!\n");}
        }
        return;
    }
