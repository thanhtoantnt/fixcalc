void foo(ref int x) 
requires true
ensures (x<=0 & x'=x | x>0 & x'=0);
{
  if (x<=0) { return; }
  else {
    x=x-1;
    foo(x);
  }
}


// example from Figure 2 of Gulwani-PLDI'08
void PV2()
{
  int x=0;
  int y=0;
  while (y>=0) 
  requires (0<=x & x<=51 & x=y | 51<=x & x+y=102)
  ensures x'+y'=102 & (x>=51 & x+y=102 | 0<=x & x<=51 & x=y);
  {
    if (x<=50) { y=y+1; }
    else { y=y-1; }
    x=x+1;
  }
  assert x'+y'=102;
}


