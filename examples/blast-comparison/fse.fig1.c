void error() {
ERROR: goto ERROR;
} 

void main(int a) {
  int i=0;
  int c=0;
  while (i<1000) {
    c=c+1;
    i=i+1;
  }
  if (a<=0) { error(); };
}