#lang processing

int fib(int n) {
  if ( n == 1 || n == 0)
    return 1;
  else
    return n * fib(n - 1); 
}
                   
void draw(){
  int x = fib(10); 
  println(x);
}

