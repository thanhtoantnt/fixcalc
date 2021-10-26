int bsearch(float A[], float value);

void error() {
ERROR: goto ERROR;
} 

int dim=10;

int main() {
  float A[10]={0,1,2,3,4,5,6,7,8,9};
  int res=bsearch(A, 8);
  printf("position of 8 is %d\n",res);
}


int bsearch(float A[], float value) {
   int low = 0;
//   int high = dim - 1; // correct
   int high = dim + 1; // error: detected by bsearch
   while (low <= high) {
       int mid = (low + high) / 2;
       if (mid<0 || mid>=dim) { error(); };
       if (A[mid] > value) {
           high = mid - 1;
       } else if (A[mid] < value) {
                low = mid + 1;
              } else {
                return mid;
              }
   }
   return -1;
}

