#include "Primitives.h"

void main() {
 int dim = 4;
 arr A; _initArr(&A,0,dim);
  int res = 0;
   {
  int v_31 = 0;
  int v_35 = 1;
   int v_33 = minus(dim,v_35);
   init_arr(A,v_31,v_33);
  };
  printArr(A);
   {
  int v_37 = 0;
  int v_41 = 1;
   int v_39 = minus(dim,v_41);
   res=sum(A,v_37,v_39);
  };
  return print(res);
 }

void init_arr(arr a,int i,int j) {
 bool v_51 = gt(i,j);
 if (v_51) {
   return ;
   } else {
    {
   int v_45 = 10;
   int v_43 = myrandom(v_45);
    assign(a,i,v_43);
   };
    {
   int v_49 = 1;
   int v_47 = plus(i,v_49);
    return init_arr(a,v_47,j);
   };
   };
 }

int sum(arr a,int i,int j) {
 bool v_61 = gt(i,j);
 if (v_61) {
   return 0;
   } else {
   int v_53 = sub(a,i);
   int v_59 = 1;
    int v_57 = plus(i,v_59);
    int v_55 = sum(a,v_57,j);
    return plus(v_53,v_55);
   };
 }

