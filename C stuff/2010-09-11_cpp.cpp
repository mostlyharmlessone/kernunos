#include <stdio.h>

extern "C" struct kwaves_type
{
    double x, y;
} kwaves;

extern "C" void c_routine (
                        int int_arg,
                        char* input_text,
                        char* output_text
                        )

{
    sprintf(output_text,"%s%i:%lf",input_text,int_arg, kwaves.x);
}
