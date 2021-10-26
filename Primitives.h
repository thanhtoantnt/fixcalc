#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0
//Following typedef must be commented-out for compilation with g++
typedef int bool;

typedef struct arr{
  int len;
  int *arr;
} arr;

typedef struct arrF{
  int len;
  float *arr;
} arrF;

typedef struct arr2{
  int len1;
  int len2;
  int *arr;
} arr2; 

typedef struct arrF2{
  int len1;
  int len2;
  float *arr;
} arrF2; 

//-----Array Int-------------------
void _initArr(arr* A,int init, int size);
void printArr(arr A);
arr newArr(int size,int init);
int len(arr A,int dim);
int sub(arr A,int i);
void assign(arr A,int i, int v);
int runtimeL(int i);
int runtimeH(int i,arr A);

//-----Array Float-----------------
void _initArrF(arrF* A,float init, int size);
void printArrF(arrF A);
int lenF(arrF A,int dim);
float subF(arrF A,int i);
void assignF(arrF A,int i, float v);
int runtimeLF(int i);
int runtimeHF(int i,arrF A);

//-----Array2 Int-------------------
void _initArr2(arr2* A,int init, int sizex, int sizey);
void printArr2(arr2 A);
int len2(arr2 A,int dim);
int sub2(arr2 A, int i, int j);
void assign2(arr2 A, int i, int j, int v);

//-----Array2 Float-----------------
void _initArrF2(arrF2* A,float init, int sizex, int sizey);
void printArrF2(arrF2 A);
int lenF2(arrF2 A,int dim);
float subF2(arrF2 A, int i, int j);
void assignF2(arrF2 A, int i, int j, float v);
int runtimeL1F2(int i);
int runtimeL2F2(int j);
int runtimeH1F2(int i,arrF2 A);
int runtimeH2F2(int j,arrF2 A);

//-----Boolean operators-----------
bool boolOr(bool a,bool b);
bool boolAnd(bool a,bool b);

//-----Arithmetic Int-Int----------
int plus(int a,int b);
int minus(int a,int b);
int mul(int a,int b);
int mul2(int a);
int div2(int a);
int divide(int a,int b);
bool gt(int a,int b);
bool gte(int a,int b);
bool eq(int a,int b);
bool lt(int a,int b);
bool lte(int a,int b);

//-----Arithmetic Int-Float--------
float minusIF(int a,float b);
float mulIF(int a,float b);
float divIF(int a,float b);

//-----Arithmetic Float-Int--------
float plusFI(float a,int b);
float mulFI(float a,int b);
float divFI(float a,int b);

//-----Arithmetic Float-Float------
float plusFF(float a,float b);
float minusFF(float a,float b);
float mulFF(float a,float b);
float divFF(float a,float b);
bool gtFF(float a,float b);
bool gteFF(float a,float b);
bool eqFF(float a,float b);
bool ltFF(float a,float b);
bool lteFF(float a,float b);

//-----Miscellaneous---------------
int myrandom(int max);
float myrandomF(float max);
int min(int a,int b);
int minIMP(int a,int b);
void print(int no);
void printF(float no);

//Bsearch
void init_arrF(arrF a,int i,int j);
int Bsearch(arrF a,float k);
int lookup(arrF a,float k,int i,int j);
int getmid(int i,int j);

//Bubblesort
void bubble_sort(arrF a,int n);
void doi(arrF a,int i,int n);
void doj(arrF a,int i,int j,int n);

//Dotprod
float dotprod(arrF v1,arrF v2);
float loopDP(int i,int n,arrF v1,arrF v2,float sum);

//Quicksort
int changeM(arrF a,int m,int i,int h,float v);

//Sentinel
int initsearch(arrF a,float k);
int sent(arrF a,int i,float k);

//Sumvec
float sum(arrF a,int i,int j);
float sumRec(arrF a,int i,int j);

//LU
void loop(int j,int minMN,int M,int N,arrF2 A,arr pivot);
int loopi_jp(int i,int j,int M,arrF2 A,float t,int r);
void array2D_swaprow(int jp,int j,int M,arrF2 A);
void loopii(int ii,int j,int M,int N,arrF2 A);
void loopk(int k,int M,int j,float recp,arrF2 A);


//SOR
void init_arrF2(arrF2 a,int i,int j,int m,int n);
void SOR_execute(int M,int N,float omega,arrF2 G,int num_iterations);

//linpack
void loop_main1_i (int i, int n, arrF x, arrF b);
void loop_main2_i (int i, int n, arrF b);
float matgen(arrF a,int n,arrF b);
float loop_j_0(int j, int n, arrF a, int init, float norma);
float loop_i_0(int j, int i, int n, arrF a, int init, float norma);
void loop_j_1(int j, int n, arrF a, arrF b);
void loop_i_1(int j, int i, int n, arrF a, arrF b);
void init_arrFF(arrF a, int i, int j, float val);
int dgefa(arrF a, int i1, int n, arr ipvt);
int loop_dgefa1_k(int k, int n, arrF a, int i1, arr ipvt);
float loop_dgefa_k(int k, int n, int l, arrF a, int i1);
void loop_dgefa2_j(int j, int k, int n, int l, arrF a, int i1);
void daxpy (int n, float da, arrF dx, int i1, int incx, arrF dy, int i2, int incy);
void loop_daxpy2_i (int i, int n, float da, arrF dx , int i1, arrF dy, int i2);
void dscal(int n, float da, arrF dx, int i1, int incx);
void loop_dscal1_i(int i, int nincx, float da, arrF dx, int i1,int incx);
void loop_dscal2_i(int i, int n, float da, arrF dx, int i1);
int idamax(int n, arrF dx, int i1, int incx);
int loop_idamax1_i(int i, int n, arrF dx, int i1, float dmax, int itemp);
int loop_idamax2_i(int i, int n, arrF dx, int i1, float dmax,int ix, int incx, int itemp);
void dgesl(arrF a, int n, arr ipvt, arrF b, int job);
void loop_dgesl1_k (int k, int n, arr ipvt, arrF a, arrF b);
void loop_dgesl1_kb (int kb, int n, arrF a, arrF b);
void loop_dgesl2_k (int k, int n, arrF a, arrF b);
void loop_dgesl2_kb (int kb, int n, arr ipvt, arrF a, arrF b);
float ddot(int n,arrF dx, int i1, int incx,  arrF dy, int i2, int incy);
float loop_ddot2_i (int i, int n, float dtemp, arrF dx, int i1, arrF dy, int i2);
void dmxpy(int n1, arrF y, int n2, arrF x,arrF m);
void loop_dmxpy1_i (int i, int n1, int j, 
		arrF y, arrF x, arrF m);
void loop_dmxpy2_i (int i, int n1, int j, 
		arrF y, arrF x, arrF m);
void loop_dmxpy3_i (int i, int n1, int j, 
		arrF y, arrF x, arrF m);
void loop_dmxpy4_i (int i, int n1, int j,  
		arrF y, arrF x, arrF m);
float loop7 (int n, int i, int j, arrF x, arrF m, float s);
void dmxpy_mainloop1 (int j, int n1, int n2, arrF y, arrF x, arrF m);
void dmxpy_mainloop2(int i, int j, int n1, arrF y, arrF x, arrF m);
float loop15 (int n, int i, int j, arrF x, arrF m, float s);




