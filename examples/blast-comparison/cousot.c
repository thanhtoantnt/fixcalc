int x,y;
int __BLAST_NONDET;

void main(){
  x = 0;
  y = 0;

  if (__BLAST_NONDET){
    x++;
    y++;
  }

  if (x>0){
    x--;
    y--;
  }

  if (y != 0){ ERROR: goto ERROR;};
}




