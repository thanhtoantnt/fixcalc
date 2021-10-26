void error() {
ERROR: goto ERROR;
} 

void main() {
  int dim=10;
  int A[10];
  int i=0;
  int j=dim+1;
  while (i<=j) {
 	  if (i<0 || i>=dim) { error(); };
 	  A[i]=0;
 	  i++;
  }
}

