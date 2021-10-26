#include "Primitives.h"


long array_accesses = 0;
long array_accessesF = 0;
long array_accesses2 = 0;
long array_accessesF2 = 0;
long runtime_checks = 0;
long runtime_checksF = 0;
long runtime_checksF2 = 0;
long runtime_checksPost = 0;

//-----Array Int-------------------
void _initArr(arr* A,int init, int size){
  int i=0;
  A->len=size;
  A->arr=(int *)malloc(size*sizeof(int));
  for (;i<A->len;i++)
    A->arr[i]=init;
}

void printArr(arr A){
  int i=0;
  for (;i<A.len;i++)
    printf("%d\t",A.arr[i]);
  printf("\n");
}

arr newArr(int size,int init){
  int i=0;
  arr A;
  A.len=size;
  A.arr=(int *)malloc(size*sizeof(int));
  for (;i<A.len;i++)
    A.arr[i]=init;\
  return A;
}

int len(arr A,int dim){
  array_accesses+=1;
  return A.len;
}

int sub(arr A,int i){
  array_accesses+=2;
  return A.arr[i];
}

void assign(arr A,int i, int v){
  array_accesses+=2;
  A.arr[i]=v;
  return;
}

int runtimeL(int i){
//(0<=i)
  runtime_checks++;
  return (0<=i);
}

int runtimeH(int i, arr A){
//lt(i,(l_:len(A,1)
  runtime_checks++;
  return (i<A.len);
}

int runtimePost(){
  runtime_checksPost++;
  return 1;
}

//-----Array Float-----------------
void _initArrF(arrF* A,float init, int size){
  int i=0;
  A->len=size;
  A->arr=(float *)malloc(size*sizeof(float));
  for (;i<A->len;i++)
    A->arr[i]=init;
}

void printArrF(arrF A){
  int i=0;
  for (;i<A.len;i++)
    printf("%.2f\t",A.arr[i]);
  printf("\n");
}

int lenF(arrF A,int dim){
  array_accessesF+=1;
  return A.len;
}

float subF(arrF A,int i){
  array_accessesF+=2;
  return A.arr[i];
}

void assignF(arrF A,int i, float v){
  array_accessesF+=2;
  A.arr[i]=v;
  return;
}

int runtimeLF(int i){
//(0<=i)
  runtime_checksF++;
  return (0<=i);
}

int runtimeHF(int i, arrF A){
//lt(i,(l_:lenF(A,1)
  runtime_checksF++;
  return (i<A.len);
}

//-----Array2 Int-------------------
void _initArr2(arr2* A,int init, int size1, int size2){
  int i=0;int noElem=size1*size2;
  A->len1 = size1; A->len2 = size2;
  A->arr=(int *)malloc(noElem*sizeof(int));
  for (;i<noElem;i++){
    A->arr[i]=init;
  }
}

void printArr2(arr2 A){
  int i=0,j=0;
  for (i=0;i<A.len1;i++){
    printf("Line %d:",i);
    for (j=0;j<A.len2;j++)
      printf("%d\t",A.arr[A.len1*i+j]);
    printf("\n");
  }
  printf("\n");
} 

int len2(arr2 A,int dim){
  array_accesses2+=2;
  if (dim==1) return A.len1;
  if (dim==2) return A.len2;
  return (-1);
}

int sub2(arr2 A, int i, int j){
  array_accesses2+=4;
  int indx = A.len1*i+j;
  return A.arr[indx];
}

void assign2(arr2 A, int i, int j, int v){
  array_accesses2+=4;
  int indx = A.len1*i+j;
  A.arr[indx]=v;
  return;
}  

//-----Array2 Float-----------------
void _initArrF2(arrF2* A,float init, int size1, int size2){
  int i=0;int noElem=size1*size2;
  A->len1 = size1; A->len2 = size2;
  A->arr=(float *)malloc(noElem*sizeof(float));
  for (;i<noElem;i++){
    A->arr[i]=init;
  }
}

void printArrF2(arrF2 A){
  int i=0,j=0;
  for (i=0;i<A.len1;i++){
    printf("Line %d:",i);
    for (j=0;j<A.len2;j++)
      printf("%.2f\t",A.arr[A.len1*i+j]);
    printf("\n");
  }
  printf("\n");
} 

int lenF2(arrF2 A,int dim){
  array_accessesF2+=2;
  if (dim==1) return A.len1;
  if (dim==2) return A.len2;
  return (-1);
}

float subF2(arrF2 A, int i, int j){
  array_accessesF2+=4;
  int indx = A.len1*i+j;
  return A.arr[indx];
}

void assignF2(arrF2 A, int i, int j, float v){
  array_accessesF2+=4;
  int indx = A.len1*i+j;
  A.arr[indx]=v;
  return;
}  

int runtimeL1F2(int i){
//(0<=i)
  runtime_checksF2++;
  return (0<=i);
}

int runtimeL2F2(int j){
//(0<=j)
  runtime_checksF2++;
  return (0<=j);
}

int runtimeH1F2(int i,arrF2 A){
//lt(i,(l_:lenF2(A,1)
  runtime_checksF2++;
  return (i<A.len1);
}

int runtimeH2F2(int j,arrF2 A){
//lt(j,(l_:lenF2(A,2)
  runtime_checksF2++;
  return (j<A.len2);
}

//-----Boolean operators-----------
bool or(bool a,bool b){
  return (a || b);
}

bool and(bool a,bool b){
  return (a && b);
}


//-----Arithmetic Int-Int----------
int plus(int a,int b){
  return (a+b);
}

int minus(int a,int b){
  return (a-b);
}

int mul(int a,int b){
  return (a*b);
}

int mul2(int a){
  return a*2;
}

int mul201(int a){
  return a*201;
}

int mul100(int a){
  return a*100;
}

int div2(int a){
  return a/2;
}

int div2Even(int a){
  return a/2;
}

int divide(int a,int b){ //the name must differ from div provided by stdlib.h
  return a/b;
}

int mod(int a, int b){
  return a % b;
}

int min(int a,int b){
  return (a>b?a:b);
}

int max(int a,int b){
  return (a<b?a:b);
}

int minIMP(int a,int b){
  return (a>b?a:b);
}

int maxIMP(int a,int b){
  return (a<b?a:b);
}

bool gt(int a,int b){
  return (a>b);
}

bool gte(int a,int b){
  return (a>=b);
}

bool eq(int a,int b){
  return (a==b);
}

bool lt(int a,int b){
  return (a<b);
}

bool lte(int a,int b){
  return (a<=b);
}

//-----Arithmetic Int-Float--------
float plusIF(int a,float b){
  return a+b;
}

float minusIF(int a,float b){
  return ((float) a) - b;
}

float mulIF(int a,float b){
  return ((float) a) * b;
}

float divIF(int a,float b){
  return ((float) a)/b;
}

bool gtIF(int a,float b){
  return (((float)a)>b);
}

bool gteIF(int a,float b){
  return (((float) a)>=b);
}

bool eqIF(int a,float b){
  return (((float)a)==b);
}

bool ltIF(int a,float b){
  return (((float)a)<b);
}

bool lteIF(int a,float b){
  return (((float)a)<=b);
}

//-----Arithmetic Float-Int--------
float plusFI(float a,int b){
  return a+b;
}
float minusFI(float a,int b){
  return (a - (float)b);
}

float mulFI(float a,int b){
  return a*b;
}

float divFI(float a,int b){
  return a/((float) b);
}

bool gtFI(float a,int b){
  return (a>((float)b));
}

bool gteFI(float a,int b){
  return (a>=((float)b));
}

bool eqFI(float a,int b){
  return (a==((float)b));
}

bool ltFI(float a,int b){
  return (a<((float)b));
}

bool lteFI(float a,int b){
  return (a<=((float)b));
}

//-----Arithmetic Float-Float------
float plusFF(float a,float b){
  return (a+b);
}

float minusFF(float a,float b){
  return (a-b);
}

float mulFF(float a,float b){
  return (a*b);
}

float divFF(float a,float b){
  return (a/b);
}

bool gtFF(float a,float b){
  return (a>b);
}

bool gteFF(float a,float b){
  return (a>=b);
}

bool eqFF(float a,float b){
  return (a==b);
}

bool ltFF(float a,float b){
  return (a<b);
}

bool lteFF(float a,float b){
  return (a<=b);
}

//-----Miscellaneous---------------
int myrandom(int max){
  float maxF=max;
  int j=(int) (maxF*rand()/(RAND_MAX+1.0));
  return j;
}

float myrandomF(float maxF){
  float j = (maxF*rand()/(RAND_MAX+1.0));
  return j;
}

//float mysin(float a){
//  return sin(a);
//}

float pi () {
  return 3.1415926535897932;
}

void print(int no){
  printf("%d\n",no);
}

void printF(float no){
  printf("%.2f\n",no);
}

void printStats(){
  printf("No. of integer array accesses: %d\n",array_accesses);
  printf("No. of float array accesses: %d\n",array_accessesF);
  printf("No. of 2D integer array accesses: %d\n",array_accesses2);
  printf("No. of 2D float array accesses: %d\n",array_accessesF2);
  printf("No. of integer runtime checks: %d\n",runtime_checks);
  printf("No. of float runtime checks: %d\n",runtime_checksF);
  printf("No. of 2D float runtime checks: %d\n",runtime_checksF2);
  printf("No. of postcondition runtime checks: %d\n",runtime_checksPost);
}

int randInt(){
  return rand();
}

int even(int x) {
  if (x%2 == 0) { return 1; }
  else { return 0; }
}

int odd(int x) {
  if (x%2 != 0) { return 1; }
  else { return 0; }
}
