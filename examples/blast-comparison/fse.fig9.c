void error() {
ERROR: goto ERROR;
} 

void main() {
  int x=0;
  int y=0;
  while (y>=0) {
    y=y+x;
  }
  error();
}
