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
  a[0]=k;
  int i=dim+1;
  while (a[i]!=k) {
    i--; 
 	  if (i<0 || i>=dim) { error(); };
  }
}
