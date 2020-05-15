#include <stdio.h>
#include <time.h>
#include <math.h>

/* Floating point nanoseconds per second */
#define NANO_PER_SEC 1000000000.0

void speedtst0_(void);
void speedtst1_(void);
void speedtst2_(void);
void speedtst3_(void);

int main(void)
{
    struct timespec start, end;
    double start_sec, end_sec, elapsed_sec;
    clock_gettime(CLOCK_REALTIME, &start);

    // Your code here
    speedtst0_();
   
    clock_gettime(CLOCK_REALTIME, &end);
    start_sec = start.tv_sec + start.tv_nsec / NANO_PER_SEC;
    end_sec = end.tv_sec + end.tv_nsec / NANO_PER_SEC;
    elapsed_sec = end_sec - start_sec;
    printf("speedtst1 took %.3f seconds\n", elapsed_sec);

    clock_gettime(CLOCK_REALTIME, &start);
    // Your code here
    speedtst1_();
   
    clock_gettime(CLOCK_REALTIME, &end);
    start_sec = start.tv_sec + start.tv_nsec / NANO_PER_SEC;
    end_sec = end.tv_sec + end.tv_nsec / NANO_PER_SEC;
    elapsed_sec = end_sec - start_sec;
    printf("speedtst1 took %.3f seconds\n", elapsed_sec);

    clock_gettime(CLOCK_REALTIME, &start);
    // Your code here
    speedtst2_();
   
    clock_gettime(CLOCK_REALTIME, &end);
    start_sec = start.tv_sec + start.tv_nsec / NANO_PER_SEC;
    end_sec = end.tv_sec + end.tv_nsec / NANO_PER_SEC;
    elapsed_sec = end_sec - start_sec;
    printf("speedtst2 took %.3f seconds\n", elapsed_sec);

    clock_gettime(CLOCK_REALTIME, &start);
    // Your code here
    speedtst3_();
   
    clock_gettime(CLOCK_REALTIME, &end);
    start_sec = start.tv_sec + start.tv_nsec / NANO_PER_SEC;
    end_sec = end.tv_sec + end.tv_nsec / NANO_PER_SEC;
    elapsed_sec = end_sec - start_sec;
    printf("speedtst3 took %.3f seconds\n", elapsed_sec);

    return 0;
}
   
