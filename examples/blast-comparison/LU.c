#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double LU_num_flops(int N);
void LU_copy_matrix(int M, int N, double **lu, double **A);
int LU_factor(int M, int N, double **A, int *pivot);
void init_arrF2(double **a, int i, int j, int m, int n);
double** new_Array2D_double(int M, int N);

void error() {
ERROR: goto ERROR;
} 

int dim = 10;

void main()
{
  int jp = 1;
  double t = 0.0;
  double **A;
  if ((A = new_Array2D_double(dim, dim)) == 0) exit(1); 

  int pivot[10];
  init_arrF2(A,0,0,dim-1,dim-1);

  printf("before LU: %1.0f-%1.0f-..\n",A[0][0],A[0][1]);
  LU_factor(dim,dim,A,pivot);
  printf("after LU: %1.0f-%1.0f-..\n",A[0][0],A[0][1]);
}


double** new_Array2D_double(int M, int N)
{
    int i=0;
    int failed = 0;

    double **A = (double**) malloc(sizeof(double*)*M);
    if (A == 0)
        return 0;

    for (i=0; i<M; i++)
    {
        A[i] = (double*) malloc(N * sizeof(double));
        if (A[i] == 0)
        {
            failed = 1;
            break;
        }
    }

    /* if we didn't successfully allocate all rows of A      */
    /* clean up any allocated memory (i.e. go back and free  */
    /* previous rows) and return NULL                        */

    if (failed)
    {
        i--;
        for (; i<=0; i--)
            free(A[i]);
        free(A);
        return 0;
    }
    else
        return A;
} 

void init_arrF2(double **a, int i, int j, int m, int n) {
  while (i<=n) {
    while (j<=m) {
      a[i][j]=random();
      j=j+1;
    }
    j=0;
    i=i+1;
  }
}

double LU_num_flops(int N)
{
        /* rougly 2/3*N^3 */

    double Nd = (double) N;

    return (2.0 * Nd *Nd *Nd/ 3.0);
}


void LU_copy_matrix(int M, int N, double **lu, double **A)
{
    int i;
    int j;

    for (i=0; i<M; i++)
        for (j=0; j<N; j++) {
if (i<0 || i>=dim) { error(); };
if (j<0 || j>=dim) { error(); };
if (i<0 || i>=dim) { error(); };
if (j<0 || j>=dim) { error(); };
            lu[i][j] = A[i][j];
          }
}


int LU_factor(int M, int N, double **A,  int *pivot)
{
 

    int minMN =  M < N ? M : N;
    int j=0;

    for (j=0; j<minMN; j++)
    {
        /* find pivot in column j and  test for singularity. */

        int jp=j;
        int i;
        
if (j<0 || j>=dim) { error(); };
if (j<0 || j>=dim) { error(); };
        double t = fabs(A[j][j]);
        for (i=j+1; i<M; i++)
        {
if (i<0 || i>=dim) { error(); };
if (j<0 || j>=dim) { error(); };
            double ab = fabs(A[i][j]);
            if ( ab > t)
            {
                jp = i;
                t = ab;
            }
        }
        
if (j<0 || j>=dim) { error(); };
        pivot[j] = jp;

        /* jp now has the index of maximum element  */
        /* of column j, below the diagonal          */

if (jp<0 || jp>=dim) { error(); };
if (j<0 || j>=dim) { error(); };
        if ( A[jp][j] == 0 )                 
            return 1;       /* factorization failed because of zero pivot */


        if (jp != j)
        {
            /* swap rows j and jp */
if (j<0 || j>=dim) { error(); };
           double *tA = A[j];
if (j<0 || j>=dim) { error(); };
if (jp<0 || jp>=dim) { error(); };
            A[j] = A[jp];
if (jp<0 || jp>=dim) { error(); };
            A[jp] = tA;
        }

        if (j<M-1)                /* compute elements j+1:M of jth column  */
        {
            /* note A(j,j), was A(jp,p) previously which was */
            /* guarranteed not to be zero (Label #1)         */

if (j<0 || j>=dim) { error(); };
if (j<0 || j>=dim) { error(); };
            double recp =  1.0 / A[j][j];
            int k;
            for (k=j+1; k<M; k++){
if (k<0 || k>=dim) { error(); };
if (j<0 || j>=dim) { error(); };
                A[k][j] *= recp;
              }
        }


        if (j < minMN-1)
        {
            /* rank-1 update to trailing submatrix:   E = E - x*y; */
            /* E is the region A(j+1:M, j+1:N) */
            /* x is the column vector A(j+1:M,j) */
            /* y is row vector A(j,j+1:N)        */

            int ii;
            for (ii=j+1; ii<M; ii++)
            {
if (ii<0 || ii>=dim) { error(); };
                double *Aii = A[ii];
if (j<0 || j>=dim) { error(); };
                double *Aj = A[j];
if (j<0 || j>=dim) { error(); };
                double AiiJ = Aii[j];
                int jj;
                for (jj=j+1; jj<N; jj++){
if (jj<0 || jj>=dim) { error(); };
if (jj<0 || jj>=dim) { error(); };
                  Aii[jj] -= AiiJ * Aj[jj];
                }

            }
        }
    }

    return 0;
}

