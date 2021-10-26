void error() {
ERROR: goto ERROR;
} 

int dim=10;
void main() {
  int A[10];
  initarr(A,0,dim-1); // safe
//  initarr(A,0,dim);   // unsafe
}

void initarr(int A[], int i, int j) {
  if (i<=j) {
 	  if (i<0 || i>=dim) { error(); };
 	  A[i]=0;
    initarr(A,i+1,j);
  }  
}
