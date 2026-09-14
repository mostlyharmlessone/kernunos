//from Fortran call with Ccounter(10) or Ccounter(inc) where inc is a c_int
// from C++ call with Ccounter(&inc) where inc is an integer

#include "counter.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

void Ccounter(int *inc) {
        counter=(int)*inc;
        return;
    }
