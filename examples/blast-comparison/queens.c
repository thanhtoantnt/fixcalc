void queens (int board[],int size) ;
void loopQ(int i, int size, int board[]);

void error() {
//printf("error");
ERROR: goto ERROR;
} 

int dim = 8;

int main(){
  int a[8]={0,1,2,3,4,5,6,7};
  queens(a,dim);
}

void queens (int board[],int size) {
  return loopQ (0, size, board);
}

void loopQ(int i, int size, int board[]){
  if (i<size) {
    if (i<0 || i>=dim) { error(); };
    int next = 1 + board[i];
    if (next > size) { 
       if (i<0 || i>=dim) { error(); };
        board[i] = 0;
        if (i == 0) { return; }
        else { return loopQ (i-1, size, board); }
    } else {
        if (i<0 || i>=dim) { error(); };
        board[i] = next;
        if (test(0, i, next, board)==1) {
            if (i+1 == size) {
                //printArr(board);
                return loopQ (i, size, board);
            } else { return loopQ (i+1, size, board); }
        } else { return loopQ (i, size, board); }
    }
  } else { return; }
  
}

int test(int j, int i, int qi, int board[])
{ 
  if (j >= i) { return 1; }
  else {
    if (j<0 || j>=dim) { error(); };
    int qj = board[j];
    if (qi = qj) { return 1; }
    else {
      if ((abs (qi - qj)) == (i - j)) { return 0; }
      else { 
        return test (j+1, i, qi, board);
      }
    }
  }
}
