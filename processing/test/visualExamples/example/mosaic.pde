#lang processing

require "fib.rkt";
require "draw.rkt";

void echo(int n, float x, float y, float z, float ang, float r) {
  if ( n == 1) {
    fullArc(x, y, z, r, ang, HALF_PI);
  }
  else {
    fullArc(x, y, z, r / fib(n), ang, HALF_PI);
    echo(n-1, x, y, z, ang, r);
  }
}

void mosaic(float x, float y, float z, float l) {
  int iter = 10;
  echo(iter, x, y, z, 0.0, l);
  echo(iter, x + l, y, z, HALF_PI, l);
  echo(iter, x + l, y + l, z, PI, l);
  echo(iter, x, y + l, z, 3/2 * PI, l);
  frame(x, y, y, l);
}

void setup() {
  backend(autocad);
}

void draw() {
  for(int i = 0; i < 3;  i++) {
    for (int j = 0; j < 3; j++) {
      mosaic(i*100,j*100,0,100);
    }
  }
}
