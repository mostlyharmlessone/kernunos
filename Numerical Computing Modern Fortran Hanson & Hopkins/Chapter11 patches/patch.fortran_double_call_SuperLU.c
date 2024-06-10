--- fortran_double_call_SuperLU.c	2013-11-17 10:51:28.000000000 -0600
+++ fortran_double_call_SuperLU.c	2021-03-27 11:39:59.597963956 -0500
@@ -49,6 +49,8 @@
 	int *etree;  /* column elimination tree */
 	SCformat *Lstore;
 	NCformat *Ustore;
+        GlobalLU_t Glu; /* facilitate multiple factorizations with 
+                           SamePattern_SameRowPerm                  */	
 	int      i, panel_size, permc_spec, relax;
 
 	//trans_t  trans;
@@ -93,7 +95,7 @@
 		relax = sp_ienv(2);
 
 		dgstrf(options, &AC, relax, panel_size, 
-			etree, NULL, 0, perm_c, perm_r, L, U, &stat, info);
+			etree, NULL, 0, perm_c, perm_r, L, U,&Glu, &stat, info);
 
 		/*dgstrf (superlu_options_t *options, SuperMatrix *A,
 		int relax, int panel_size, int *etree, void *work, int lwork,
