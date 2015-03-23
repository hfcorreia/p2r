#lang processing

int x = 2;
int y = 3;

int x(int x){
  return x;
}

int x(int x, int y){
  return x + y;
}



void setup(){
   int xy = x(x) + x(x,y);
   println(xy);
}