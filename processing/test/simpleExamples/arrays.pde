#lang processing

int[] x = new int[10];

void setup(){
  for ( int i = 0; i < x.length ; i++ ) {
    x[i] = i % 2;
  }

  println(x);
}
