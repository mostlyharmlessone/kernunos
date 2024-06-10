--- slu_ddefs.h	2013-11-17 10:51:28.000000000 -0600
+++ slu_ddefs.h	2021-03-27 11:34:54.774587599 -0500
@@ -120,7 +120,7 @@
 dgssvx(superlu_options_t *, SuperMatrix *, int *, int *, int *,
        char *, double *, double *, SuperMatrix *, SuperMatrix *,
        void *, int, SuperMatrix *, SuperMatrix *,
-       double *, double *, double *, double *,
+       double *, double *, double *, double *, GlobalLU_t *,
        mem_usage_t *, SuperLUStat_t *, int *);
     /* ILU */
 extern void
@@ -159,7 +159,7 @@
 extern void    dallocateA (int, int, double **, int **, int **);
 extern void    dgstrf (superlu_options_t*, SuperMatrix*,
                        int, int, int*, void *, int, int *, int *, 
-                       SuperMatrix *, SuperMatrix *, SuperLUStat_t*, int *);
+                       SuperMatrix *, SuperMatrix *, GlobalLU_t *, SuperLUStat_t*, int *);
 extern int     dsnode_dfs (const int, const int, const int *, const int *,
 			     const int *, int *, int *, GlobalLU_t *);
 extern int     dsnode_bmod (const int, const int, const int, double *,
