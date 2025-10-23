/*
    so that math.m gets included for C ?
    target_link_libraries(<my program> m) 
    gcc -o sphere sphere.c -lm
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h> 

// Function to convert binary string to integer
long long binaryToDecimalInt(char *binary) {
    long long decimal = 0;
    int power = 0;
    int i;
    for (i = strlen(binary) - 1; i >= 0; i--) {
        if (binary[i] == '1') {
            decimal += pow(2, power);
        }
        power++;
    }
    return decimal;
}

// Function to convert binary fractional part to decimal
double binaryFractionToDecimal(char *binaryFraction) {
    double decimal = 0.0;
    int power = -1;
    int i;
    for (i = 0; i < strlen(binaryFraction); i++) {
        if (binaryFraction[i] == '1') {
            decimal += pow(2, power);
        }
        power--;
    }
    return decimal;
}

int main() {
    char binaryFloatStr[64]; // Example: "0_10000001_01000000000000000000000" (sign_exponent_mantissa)

// ~2.9612
// IEEE 754 binary32 aka single precision https://en.wikipedia.org/wiki/Single-precision_floating-point_format
//  40 3d 84 4d == 0100 0000 0011 1101 1000 0100 0100 1101 = 0_10000000_01111011000010001001101 bias 127
/*
  from PR:
  4C    A6   93
  0100 1100 1010 0110 1001 0011
  0_10011001_010011010010011
*/

// MRK II NXP 24 bit  https://stackoverflow.com/questions/26930117/float24-24-bit-floating-point-to-hex
// possibly 
 /* ByteValue[0] : mantissa bits m7..m0
    ByteValue[1] : sign bit, mantissa bits m14..m8
    ByteValue[2] : exponent e7..e0 (signed, 2s complement repres.)  https://en.wikipedia.org/wiki/Two%27s_complement (bias = 1 ?)
    Actual value is
    f = (-1)^s * ( 2^(-1) + m14 * 2^(-2) + ... m0 * 2^(-16) ) * 2^e */
/* so..
   e7..e0     02 = 0000 0010
   s m14.. m8 3d = 0 011 1101
   m7..m0     84 = 1000 0100    
  formula implies bias = 1 
  IEEE-> 0_00000010_011110110000100 bias 1
  
  from PR: possibly 3.x
  possibly unsigned floats
  4C    A6   93
  0100 1100 1010 0110 1001 0011
  1_01001100_010011010010011
  4C    A7   29    
  4C    A7   67    
  4C    A7   A6

  two signed floats; PR
  2C 23 22 E  2C 32 65 F
  2c 22 22    2c 22 22
  0.01        0.1043

  two signed floats; PE
  D 2C 27 43  E 3C 49 53
  
  possibly unsigned PE possibly 2.x
  4C    22   4B    
  3C    BB   AA   
  3C    BB   73

from RA which should all be around 8.x if this is the model eye

AC
24
88
E    

AC    
24    
57    
E

AC
24 
34
E

from ED  which starts 0.2x to 4.x

2C
45
99
E
2C
82
75
E

going to

6C
82
6A

E
6C
99
92

E

6C
AA
25
E

This is possibly a calibration data set

subtraction, maybe 2c 22 22 is a registration zero based on an actual measurement, then interpret as BCD ignoring the C which is for +, haha!
https://en.wikipedia.org/wiki/Binary-coded_decimal; the C is "Signed leading separate" (COBOL)

  2C 45 99
  2c 22 22
  00 23 77 -> 0.2377

  6c aa 25
  2c 22 22
  40 88 03 -> 4.8803

  ac 24 34
  2c 22 22
  80 02 12  -> 8.0212

 PR
  4C A6 93
  2C 22 22
  20 84 71 -> 2.8471

 PE possibly 2.x
  4C 22 4B 
  2C 22 22
  20 00 29-> 2.0029   
 
  3C BB AA
  2C 22 22
  10 99 88 -> 1.9988

  2C 22 22 seems to be zero 0010 1100 0010 0010 0010 0010
  or could be -10000000000.0000 based on HT*.*
  cannot assume first bit24 is sign bit because in RA first hex is A so first bit is 1
  cannot assume 9th bit bit 15 is sign bit because frequently 3rd hex is A so bit is 1  (not MBF or ByteValue[1])
  0xC is positive in Packed BCD but nothing else fits that format
  the five other hex digits do not have a strictly monotonic relation to the numbers represented in RA but perhaps this is a calibration sphere with constant diameter + noise
  the other digits do not exceed B? no CDEF? (need this for insane BCD subtraction scheme)
  0010 1100 0010 0010 0010 0010

*/
// or  bias 3 with IEEE 754 and truncated mantissa
// 02 3d 84 = 0000 0010 0011 1101 1000 0100 = 0_00000100_011110110000100 bias 3

// possibly truncated 32 MBF see also https://en.wikipedia.org/wiki/Microsoft_Binary_Format#32-bit_MBF
/*Bit 31...24 (8 bit) 	 => bias = 128
  Bit 23      (1 bit)
  Bit 22...0  (23 bit) 
  xxxxxxxx 	s 	mmmmmmm mmmmmmmm mmmmmmmm 
  example:  sqrt(2) 81h, 35h, 04h, F3h
  0100 0001 0011 0101 0000 0100 1111 0011
  IEEE-> 0_01000001_01101010000010011110011 bias 65
  example:  2*pi 83h, 49h, 0Fh, DBh
  0100 0011 0100 1001 0000 1111 1101 1011
  IEEE->0_01000011_10010010000111111011011 bias 65
  truncated mantissa 0_01000011_100100100001111 bias 65
  example:  1.0 81h, 00h, 00h, 00h
  1000 0001 0000 0000 0000 0000 0000 0000
  IEEE->0_10000001_000 00000000000000000000 bias 129
  "The exponent is biased by 127. There is an assumed 1 bit before the radix point (so the assumed mantissa 
   is 1.ffff… where f's are the fraction bits) […] Microsoft Binary Format (single precision) […] The exponent is biased by 128. 
   There is an assumed 1 bit after the radix point (so the assumed mantissa is 0.1ffff… where f's are the fraction bits) […] 
   the IEEE mantissa is twice the MBF mantissa. […] to convert from MBF to IEEE single […] subtract 2 from the exponent (one for 
   the bias change, one for the mantissa factor), and then rearrange the sign and exponent bits. The fraction does not change. 
   To convert from IEEE single to MBF, […] add 2 to the exponent (one for the bias change, one for the mantissa factor), and 
   then rearrange the sign and exponent bits. The fraction does not change. […]"
*/

    printf("Enter binary floating-point number (e.g., 0_10000001_01000000000000000000000): ");
    scanf("%s", binaryFloatStr);

    char signBit = binaryFloatStr[0];
    char exponentStr[9];
    char mantissaStr[24];

    // Extract exponent and mantissa strings
    strncpy(exponentStr, binaryFloatStr + 2, 8);
    exponentStr[8] = '\0';
    strncpy(mantissaStr, binaryFloatStr + 11, 23);
    mantissaStr[23] = '\0';

    // Convert exponent to decimal
    long long exponentDecimal = binaryToDecimalInt(exponentStr);
    int biasedExponent = (int)exponentDecimal;
//    int actualExponent = biasedExponent - 127; // For single-precision (32float)
     int actualExponent = biasedExponent - 3; // For single-precision (24float) keeping same 3d 84 in example but changing 40 to 02
//    int actualExponent = biasedExponent - 1; // For single-precision (24 float) using NXP formula
//    int actualExponent = biasedExponent - 129; // For single-precision MBF (32float)
//    int actualExponent = biasedExponent - 152; // For single-precision (32float)

    // Calculate mantissa value (implicit leading 1 for normalized numbers)
    double mantissaValue = 1.0 + binaryFractionToDecimal(mantissaStr);

    // Calculate the final decimal value
    double decimalValue = mantissaValue * pow(2, actualExponent);

    // Apply sign
    if (signBit == '1') {
        decimalValue = -decimalValue;
    }

    printf("Decimal equivalent: %lf\n", decimalValue);

    return 0;
}