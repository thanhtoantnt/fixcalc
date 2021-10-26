void bubble_sort (float a[], int n);
void doi(float a[], int i, int n);
void doj(float a[], int i, int j, int n);
void swap(float a[], int i, int j);

void error() {
ERROR: goto ERROR;
} 

int dim = 10;

int main()
{
  float A[10]={9,8,7,6,5,4,3,2,1,0};
  printf("unsorted vector: %1.0f-%1.0f-..\n",A[0],A[1]); 
  bubble_sort(A,dim);
  printf("sorted vector: %1.0f-%1.0f-..\n",A[0],A[1]); 
}

void bubble_sort (float a[], int n) { 
  int i,j;
  for (i=0;i<n-1;) {
    for (j=0;j<n-1-i;) {
      if (j+1<0 || j+1>=dim) { error(); };
      if (j<0 || j>=dim) { error(); };
      if (a[j+1] < a[j])
        { swap(a,j,j+1); }
      j=j+1;
    }
    i=i+1;
  }
}

void swap(float a[], int i, int j)
{ 
  if (i<0 || i>=dim) { error(); };
  float tmp1 = a[i];
  if (j<0 || j>=dim) { error(); };
  float tmp2 = a[j];
  if (i<0 || i>=dim) { error(); };
  a[i] = tmp2;
  if (j<0 || j>=dim) { error(); };
  a[j] = tmp1;
}


