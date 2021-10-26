void error() {
ERROR: goto ERROR;
} 

int dim=10;

void main()
{
  float A[10];
  A[4]=3.0;
  initsearch(A,3.0);
}

int initsearch(float a[], float k){
  if (0<0 || 0>=dim) { error(); };
  a[0]=k;
  int i=dim-1;
  while (a[i]!=k) {
    i--; 
 	  if (i<0 || i>=dim) { error(); };
  }
}
/*
Int initsearch(Float[Int] a,Float k) {
   assignF(a,0,k);
   sent(a,lenF(a,1)-1,k)
}

Int sent(Float[Int] a, Int i, Float k) {
  Float v := subF(a,i);
  if (eqFF(v,k)) then { i }
  else { sent(a,i-1,k) }
}
*/