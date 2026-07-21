
/*
* Call from Fortran to SuperLU, double precision.
* This function is similar to the distributed code 
* c_fortran_dgssv.c except that additional parameters
* are passed for more flexibility.  On the Fortran
* side the ISO C bindings are used at the interface.
*/

#include <stdint.h>  //need this for uintptr_t under linux
#include "slu_ddefs.h"

/* Kind of integer to hold a pointer.  Use 'long int'
so it works on 64-bit systems. On the Fortran side
this is declared as TYPE(C_PTR).*/
//typedef long int fptr;  /* 64 bit */
/* changed for Windows build*/
typedef uintptr_t fptr;  /* 64 bit */

typedef struct {
	SuperMatrix *L;
	SuperMatrix *U;
	int *perm_c;
	int *perm_r;
} factors_t;

void FACTOR_with_SuperLU(int *iopt, int *n, int *nnz, 
	double *values, int *rowind, int *colptr,
	superlu_options_t *options,
	double *drop_tol, fptr *f_factors, int *info)

{
	/* 
	* This routine is called from Fortran.
	*
	* iopt (input) int
	*      Specifies the operation:
	*      = 1, performs LU decomposition for the first time
	*      = 3, free all the storage in the end
	*
	* f_factors (input/output) fptr* 
	*      If iopt == 1, it is an output and contains the pointer pointing to
	*                    the structure of the factored matrices.
	*      Otherwise, it is an input.  It is used with iopt=2 or 3
	*      for further solve steps or freeing storage.
	*/

	SuperMatrix A, AC, B;
	SuperMatrix *L, *U;
	int *perm_r; /* row permutations from partial pivoting */
	int *perm_c; /* column permutation vector */
	int *etree;  /* column elimination tree */
	SCformat *Lstore;
	NCformat *Ustore;
        GlobalLU_t Glu; /* facilitate multiple factorizations with 
                           SamePattern_SameRowPerm                  */	
	int      i, panel_size, permc_spec, relax;

	//trans_t  trans;
	mem_usage_t   mem_usage;
	SuperLUStat_t stat;
	factors_t *LUfactors;

	/* The options are passed in from the Fortran interface. */

	if ( *iopt == 1 ) { /* LU decomposition */

		/* Initialize the statistics variables. */
		StatInit(&stat);

		/* Adjust to 0-based indexing.  This is input data
		but is changed to 1-based indexing after the factorization.*/
		for (i = 0; i < *nnz; ++i) --rowind[i];
		for (i = 0; i <= *n; ++i) --colptr[i];

		dCreate_CompCol_Matrix(&A, *n, *n, *nnz, values, rowind, colptr,
			SLU_NC, SLU_D, SLU_GE);
		L = (SuperMatrix *) SUPERLU_MALLOC( sizeof(SuperMatrix) );
		U = (SuperMatrix *) SUPERLU_MALLOC( sizeof(SuperMatrix) );
		if ( !(perm_r = intMalloc(*n)) ) ABORT("Malloc fails for perm_r[].");
		if ( !(perm_c = intMalloc(*n)) ) ABORT("Malloc fails for perm_c[].");
		if ( !(etree = intMalloc(*n)) ) ABORT("Malloc fails for etree[].");
		options->ILU_DropTol=*drop_tol;
		///*
		//* Get column permutation vector perm_c[], according to permc_spec:
		//*   permc_spec = 0: natural ordering 
		//*   permc_spec = 1: minimum degree on structure of A'*A
		//*   permc_spec = 2: minimum degree on structure of A'+A
		//*   permc_spec = 3: approximate minimum degree for unsymmetric matrices
		//*/    	
		permc_spec = options->ColPerm;
		get_perm_c(permc_spec, &A, perm_c);


		sp_preorder(options, &A, perm_c, etree, &AC);

		panel_size = sp_ienv(1);
		relax = sp_ienv(2);

		dgstrf(options, &AC, relax, panel_size, 
			etree, NULL, 0, perm_c, perm_r, L, U,&Glu, &stat, info);

		/*dgstrf (superlu_options_t *options, SuperMatrix *A,
		int relax, int panel_size, int *etree, void *work, int lwork,
		int *perm_c, int *perm_r, SuperMatrix *L, SuperMatrix *U,
		SuperLUStat_t *stat, int *info)	*/

		Lstore = (SCformat *) L->Store;
		Ustore = (NCformat *) U->Store;
		if(options->PrintStat){				
			dQuerySpace(L, U, &mem_usage);
                        // Call to a Fortran subroutine that prints the data.
			// No call is made if options->PrintStat==0, the default.
			fortran_print(info,n,&Lstore->nnz,&Ustore->nnz,&mem_usage);
		}

		/* Restore Fortran input to 1-based indexing */
		for (i = 0; i < *nnz; ++i) ++rowind[i];
		for (i = 0; i <= *n; ++i) ++colptr[i];

		// Save the LU factors and permutations in the factors handle 
		LUfactors = (factors_t*) SUPERLU_MALLOC(sizeof(factors_t));
		LUfactors->L = L;
		LUfactors->U = U;
		LUfactors->perm_c = perm_c;
		LUfactors->perm_r = perm_r;
		*f_factors = (fptr) LUfactors;

		// Free un-wanted storage 
		SUPERLU_FREE(etree);
		Destroy_SuperMatrix_Store(&A);
		Destroy_CompCol_Permuted(&AC);
		StatFree(&stat);

	} 
}

void SOLVE_with_SuperLU(int *iopt, int *n, 			
	double *b, int *ldb, int *nrhs, 
	superlu_options_t *options, trans_t *trans,
	fptr *f_factors, int *info)

{
	/* 
	* This routine is called from Fortran.
	*
	* iopt (input) int
	*      Specifies the operation:
	*      = 2, performs triangular solve
	*
	* f_factors (input/output) fptr* 
	*      If iopt == 1, it is an output and contains the pointer pointing to
	*                    the structure of the factored matrices.
	*      Otherwise, it is an input.  It is used with iopt=2 or 3
	*      for further solve steps or freeing storage.
	*/

	SuperMatrix A, AC, B;
	SuperMatrix *L, *U;
	int *perm_r; /* row permutations from partial pivoting */
	int *perm_c; /* column permutation vector */
	int *etree;  /* column elimination tree */
	SCformat *Lstore;
	NCformat *Ustore;
	int      i, panel_size, permc_spec, relax;

	//trans_t  trans;
	mem_usage_t   mem_usage;
	SuperLUStat_t stat;
	factors_t *LUfactors;

	// The options are passed in from the Fortran interface. 


	if ( *iopt == 2 ) 

	{ // Triangular solve step or back substiution
		// Initialize the statistics variables. 
		StatInit(&stat);

		// Extract the LU factors in the f_factors handle 
		LUfactors = (factors_t*) *f_factors;
		L = LUfactors->L;
		U = LUfactors->U;
		perm_c = LUfactors->perm_c;
		perm_r = LUfactors->perm_r;

		dCreate_Dense_Matrix(&B, *n, *nrhs, b, *ldb, SLU_DN, SLU_D, SLU_GE);
		// Solve the system A*X=B or A^T*X = B, overwriting B with X. 
		dgstrs (*trans, L, U, perm_c, perm_r, &B, &stat, info);

		/*dgstrs (trans_t trans, SuperMatrix *L, SuperMatrix *U,
		int *perm_c, int *perm_r, SuperMatrix *B,
		SuperLUStat_t *stat, int *info)*/

		Destroy_SuperMatrix_Store(&B);
		StatFree(&stat);
	} 	
}
void CLEAR_SuperLU(int *iopt, superlu_options_t *options,
	fptr *f_factors, int *info)

{
	/* 
	* This routine is called from Fortran.
	*
	* iopt (input) int
	*      Specifies the operation:
	*      = 3, free all the storage in the end
	*
	* f_factors (input/output) fptr* 
	*      If iopt == 1, it is an output and contains the pointer pointing to
	*                    the structure of the factored matrices.
	*      Otherwise, it is an input.  It is used with iopt=2 or 3
	*      for further solve steps or freeing storage.
	*/

	factors_t *LUfactors;


	if( *iopt == 3)
		// Once a non-singular factorization is available, options->Fact == 1.
		// The next test does not free storage if it was never allocated.

	{
		if(options->Fact == 0) {
			*info=-1; // No storage need be released.
			return;
		}
		// Free the LU factors in the factors handle 
		LUfactors = (factors_t*) *f_factors;
		SUPERLU_FREE (LUfactors->perm_r);
		SUPERLU_FREE (LUfactors->perm_c);
		Destroy_SuperNode_Matrix(LUfactors->L);
		Destroy_CompCol_Matrix(LUfactors->U);
		SUPERLU_FREE (LUfactors->L);
		SUPERLU_FREE (LUfactors->U);
		SUPERLU_FREE (LUfactors);
		*info=0; // Flag that all storage released.
	} 
}

