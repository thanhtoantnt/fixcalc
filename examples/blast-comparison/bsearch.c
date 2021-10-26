int bsearch(float A[], float value);

void error() {
ERROR: goto ERROR;
} 

int dim=10;
int mid, low, high;

int main() {
  float A[10]={0,1,2,3,4,5,6,7,8,9};
  int res=bsearch(A, 8);
  printf("position of 8 is %d\n",res);
}

int bsearch(float A[], float value) {
   low = 0;
   high = dim - 1;
   while (low <= high) {
       mid = (low + high) / 2;
// difference-constraints cannot encode this multiple-variables constraint
{__blockattribute__((assume(AND(mid+mid<=low+high,low+high<=mid+mid+1))))} 
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
