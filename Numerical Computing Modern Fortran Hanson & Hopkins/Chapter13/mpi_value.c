#include "mpi.h"
#include "string.h"
#include <stdio.h>

/* This is a function called by Fortran function MPI_VALUE().
   It provides the implementation's enumeration of MPI parameters
   such as MPI_COMM_WORLD, MPI_DOUBLE, etc. */
int MPI_Value(const char* name)
{
  if(strcmp(name,"MPI_IDENT")==0)              return (int) MPI_IDENT;
  if(strcmp(name,"MPI_CONGRUENT")==0)          return (int) MPI_CONGRUENT;
  if(strcmp(name,"MPI_SIMILAR")==0)            return (int) MPI_SIMILAR;
  if(strcmp(name,"MPI_UNEQUAL")==0)            return (int) MPI_UNEQUAL;
  if(strcmp(name,"MPI_SUCCESS")==0)            return (int) MPI_SUCCESS;

  if(strcmp(name,"MPI_CHAR")==0)               return (int) MPI_CHAR;
  if(strcmp(name,"MPI_UNSIGNED_CHAR")==0)      return (int) MPI_UNSIGNED_CHAR;
  if(strcmp(name,"MPI_BYTE")==0)               return (int) MPI_BYTE;
  if(strcmp(name,"MPI_SHORT")==0)              return (int) MPI_SHORT;
  if(strcmp(name,"MPI_UNSIGNED_SHORT")==0)     return (int) MPI_UNSIGNED_SHORT;

  if(strcmp(name,"MPI_INT")==0)                return (int) MPI_INT;
  if(strcmp(name,"MPI_UNSIGNED")==0)           return (int) MPI_UNSIGNED;
  if(strcmp(name,"MPI_LONG")==0)               return (int) MPI_LONG;
  if(strcmp(name,"MPI_UNSIGNED_LONG")==0)      return (int) MPI_UNSIGNED_LONG;
  if(strcmp(name,"MPI_FLOAT")==0)              return (int) MPI_FLOAT;

  if(strcmp(name,"MPI_DOUBLE")==0)             return (int) MPI_DOUBLE;
  if(strcmp(name,"MPI_LONG_DOUBLE")==0)        return (int) MPI_LONG_DOUBLE;
  if(strcmp(name,"MPI_LONG_LONG_INT")==0)      return (int) MPI_LONG_LONG_INT;
  if(strcmp(name,"MPI_UNSIGNED_LONG_LONG")==0) return (int) MPI_UNSIGNED_LONG_LONG;
  if(strcmp(name,"MPI_LONG_LONG")==0)          return (int) MPI_LONG_LONG;

  if(strcmp(name,"MPI_PACKED")==0)             return (int) MPI_PACKED;
  if(strcmp(name,"MPI_LB")==0)                 return (int) MPI_LB;
  if(strcmp(name,"MPI_UB")==0)                 return (int) MPI_UB;
  if(strcmp(name,"MPI_FLOAT_INT")==0)          return (int) MPI_FLOAT_INT;
  if(strcmp(name,"MPI_DOUBLE_INT")==0)         return (int) MPI_DOUBLE_INT;

  if(strcmp(name,"MPI_LONG_INT")==0)           return (int) MPI_LONG_INT;
  if(strcmp(name,"MPI_SHORT_INT")==0)          return (int) MPI_SHORT_INT;
  if(strcmp(name,"MPI_2INT")==0)               return (int) MPI_2INT;
  if(strcmp(name,"MPI_LONG_DOUBLE_INT")==0)    return (int) MPI_LONG_DOUBLE_INT;
  if(strcmp(name,"MPI_COMPLEX")==0)            return (int) MPI_COMPLEX;

  if(strcmp(name,"MPI_DOUBLE_COMPLEX")==0)     return (int) MPI_DOUBLE_COMPLEX;
  if(strcmp(name,"MPI_LOGICAL")==0)            return (int) MPI_LOGICAL;
  if(strcmp(name,"MPI_REAL")==0)               return (int) MPI_REAL;
  if(strcmp(name,"MPI_DOUBLE_PRECISION")==0)   return (int) MPI_DOUBLE_PRECISION;
  if(strcmp(name,"MPI_2INTEGER")==0)           return (int) MPI_2INTEGER;

  if(strcmp(name,"MPI_2REAL")==0)              return (int) MPI_2REAL;
  if(strcmp(name,"MPI_2DOUBLE_PRECISION")==0)  return (int) MPI_2DOUBLE_PRECISION;
  if(strcmp(name,"MPI_CHARACTER")==0)          return (int) MPI_CHARACTER;
  if(strcmp(name,"MPI_INTEGER")==0)            return (int) MPI_INTEGER;
  if(strcmp(name,"MPI_STATUS_IGNORE")==0)      return (int) MPI_STATUS_IGNORE;

  if(strcmp(name,"MPI_STATUSES_IGNORE")==0)    return (int) MPI_STATUSES_IGNORE;
  if(strcmp(name,"MPI_MAX_PROCESSOR_NAME")==0) return (int) MPI_MAX_PROCESSOR_NAME;
  if(strcmp(name,"MPI_COMM_WORLD")==0)         return (int) MPI_COMM_WORLD;
  if(strcmp(name,"MPI_COMM_SELF")==0)          return (int) MPI_COMM_SELF;
  if(strcmp(name,"MPI_VERSION")==0)            return (int) MPI_VERSION;

  if(strcmp(name,"MPI_UNDEFINED")==0)          return (int) MPI_UNDEFINED;
  if(strcmp(name,"MPI_MAX")==0)                return (int) MPI_MAX;
  if(strcmp(name,"MPI_MIN")==0)                return (int) MPI_MIN;
  if(strcmp(name,"MPI_SUM")==0)                 return (int) MPI_SUM;
  if(strcmp(name,"MPI_PROD")==0)                return (int) MPI_PROD;

  if(strcmp(name,"MPI_LAND")==0)                return (int) MPI_LAND;
  if(strcmp(name,"MPI_BAND")==0)                return (int) MPI_BAND;
  if(strcmp(name,"MPI_LOR")==0)                 return (int) MPI_LOR;
  if(strcmp(name,"MPI_BOR")==0)                 return (int) MPI_BOR;
  if(strcmp(name,"MPI_LXOR")==0)                return (int) MPI_LXOR;

  if(strcmp(name,"MPI_BXOR")==0)                return (int) MPI_BXOR;
  if(strcmp(name,"MPI_MINLOC")==0)              return (int) MPI_MINLOC;
  if(strcmp(name,"MPI_MAXLOC")==0)              return (int) MPI_MAXLOC;
  if(strcmp(name,"MPI_STATUS_SIZE")==0)         return (int) sizeof(MPI_Status)/sizeof(int);
//This conditional can be made TRUE with a full MPI2 header, mpi.h.

#ifdef MPI2ALL
  if(strcmp(name,"MPI_INTEGER16")==0)          return (int) MPI_INTEGER16;
  if(strcmp(name,"MPI_REAL16")==0)             return (int) MPI_REAL16;
  if(strcmp(name,"MPI_COMPLEX32")==0)          return (int) MPI_COMPLEX32;
  if(strcmp(name,"MPI_SIGNED_CHAR")==0)        return (int) MPI_SIGNED_CHAR;
  if(strcmp(name,"MPI_WCHAR")==0)              return (int) MPI_WCHAR;

  if(strcmp(name,"MPI_REAL4")==0)              return (int) MPI_REAL4;
  if(strcmp(name,"MPI_REAL8")==0)              return (int) MPI_REAL8;
  if(strcmp(name,"MPI_COMPLEX8")==0)           return (int) MPI_COMPLEX8;
  if(strcmp(name,"MPI_COMPLEX16")==0)          return (int) MPI_COMPLEX16;
  if(strcmp(name,"MPI_INTEGER1")==0)           return (int) MPI_INTEGER1;

  if(strcmp(name,"MPI_INTEGER2")==0)           return (int) MPI_INTEGER2;
  if(strcmp(name,"MPI_INTEGER4")==0)           return (int) MPI_INTEGER4;
  if(strcmp(name,"MPI_INTEGER8")==0)           return (int) MPI_INTEGER8;
  if(strcmp(name,"MPI_TYPECLASS_REAL")==0)     return (int) MPI_TYPECLASS_REAL;
  if(strcmp(name,"MPI_TYPECLASS_INTEGER")==0)  return (int) MPI_TYPECLASS_INTEGER;

  if(strcmp(name,"MPI_TYPECLASS_COMPLEX")==0)  return (int) MPI_TYPECLASS_COMPLEX;
  if(strcmp(name,"MPI_REPLACE")==0)            return (int) MPI_REPLACE;
  if(strcmp(name,"MPI_IN_PLACE")==0)           return (int) MPI_IN_PLACE;

#endif
  fprintf(stdout, "String parameter not identified; returning\n" );
  fprintf(stdout, "Unidentified string = %s\n",name);
  return (int) MPI_UNDEFINED; //FOUND NO MATCH - ERROR 
}
/*
typedef int MPI_Datatype;
#define MPI_CHAR           ((MPI_Datatype)0x4c000101)
#define MPI_SIGNED_CHAR    ((MPI_Datatype)0x4c000118)
#define MPI_UNSIGNED_CHAR  ((MPI_Datatype)0x4c000102)
#define MPI_BYTE           ((MPI_Datatype)0x4c00010d)
#define MPI_WCHAR          ((MPI_Datatype)0x4c00020e)
#define MPI_SHORT          ((MPI_Datatype)0x4c000203)
#define MPI_UNSIGNED_SHORT ((MPI_Datatype)0x4c000204)
#define MPI_INT            ((MPI_Datatype)0x4c000405)
#define MPI_UNSIGNED       ((MPI_Datatype)0x4c000406)
#define MPI_LONG           ((MPI_Datatype)0x4c000407)
#define MPI_UNSIGNED_LONG  ((MPI_Datatype)0x4c000408)
#define MPI_FLOAT          ((MPI_Datatype)0x4c00040a)
#define MPI_DOUBLE         ((MPI_Datatype)0x4c00080b)
#define MPI_LONG_DOUBLE    ((MPI_Datatype)0x4c00080c)
#define MPI_LONG_LONG_INT  ((MPI_Datatype)0x4c000809)
#define MPI_UNSIGNED_LONG_LONG ((MPI_Datatype)0x4c000819)
#define MPI_LONG_LONG      MPI_LONG_LONG_INT

#define MPI_PACKED         ((MPI_Datatype)0x4c00010f)
#define MPI_LB             ((MPI_Datatype)0x4c000010)
#define MPI_UB             ((MPI_Datatype)0x4c000011)
*/
/*
#define MPI_MAX     (MPI_Op)(0x58000001)
#define MPI_MIN     (MPI_Op)(0x58000002)
#define MPI_SUM     (MPI_Op)(0x58000003)
#define MPI_PROD    (MPI_Op)(0x58000004)
#define MPI_LAND    (MPI_Op)(0x58000005)
#define MPI_BAND    (MPI_Op)(0x58000006)
#define MPI_LOR     (MPI_Op)(0x58000007)
#define MPI_BOR     (MPI_Op)(0x58000008)
#define MPI_LXOR    (MPI_Op)(0x58000009)
#define MPI_BXOR    (MPI_Op)(0x5800000a)
#define MPI_MINLOC  (MPI_Op)(0x5800000b)
#define MPI_MAXLOC  (MPI_Op)(0x5800000c)

#define MPI_REPLACE (MPI_Op)(0x5800000d)
*/
/*
#define MPI_FLOAT_INT         ((MPI_Datatype)0x8c000000)
#define MPI_DOUBLE_INT        ((MPI_Datatype)0x8c000001)
#define MPI_LONG_INT          ((MPI_Datatype)0x8c000002)
#define MPI_SHORT_INT         ((MPI_Datatype)0x8c000003)
#define MPI_2INT              ((MPI_Datatype)0x4c000816)
#define MPI_LONG_DOUBLE_INT   ((MPI_Datatype)0x8c000004)

// /* Fortran types 
#define MPI_COMPLEX           ((MPI_Datatype)0x4c00081e)
#define MPI_DOUBLE_COMPLEX    ((MPI_Datatype)0x4c001022)
#define MPI_LOGICAL           ((MPI_Datatype)0x4c00041d)
#define MPI_REAL              ((MPI_Datatype)0x4c00041c)
#define MPI_DOUBLE_PRECISION  ((MPI_Datatype)0x4c00081f)
#define MPI_INTEGER           ((MPI_Datatype)0x4c00041b)
#define MPI_2INTEGER          ((MPI_Datatype)0x4c000820)
#define MPI_2COMPLEX          ((MPI_Datatype)0x4c001024)
#define MPI_2DOUBLE_COMPLEX   ((MPI_Datatype)0x4c002025)
#define MPI_2REAL             ((MPI_Datatype)0x4c000821)
#define MPI_2DOUBLE_PRECISION ((MPI_Datatype)0x4c001023)
#define MPI_CHARACTER         ((MPI_Datatype)0x4c00011a)

// /* Size-specific types (see MPI-2, 10.2.5) 
#define MPI_REAL4             ((MPI_Datatype)0x4c000427)
#define MPI_REAL8             ((MPI_Datatype)0x4c000829)
#define MPI_REAL16            ((MPI_Datatype)0x4c00102b) // problem
#define MPI_COMPLEX8          ((MPI_Datatype)0x4c000828)
#define MPI_COMPLEX16         ((MPI_Datatype)0x4c00102a) 
#define MPI_COMPLEX32         ((MPI_Datatype)0x4c00202c) // problem
#define MPI_INTEGER1          ((MPI_Datatype)0x4c00012d)
#define MPI_INTEGER2          ((MPI_Datatype)0x4c00022f)
#define MPI_INTEGER4          ((MPI_Datatype)0x4c000430)
#define MPI_INTEGER8          ((MPI_Datatype)0x4c000831)
#define MPI_INTEGER16         ((MPI_Datatype)0x4c001032) // problem

// /* typeclasses 
#define MPI_TYPECLASS_REAL 1
#define MPI_TYPECLASS_INTEGER 2
#define MPI_TYPECLASS_COMPLEX 3
*/
