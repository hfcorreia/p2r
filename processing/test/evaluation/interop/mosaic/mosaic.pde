#lang processing

require "fib.rkt";
require "draw.rkt";

float height = 20;

void echo(int n, Object pos, float ang, float r) {
  if ( n == 1) {
    fullArc( pos, r, ang, HALF_PI, height);
  }
  else {
    float fib = fib(n);
    fullArc( pos, r / fib, ang, HALF_PI, height);
    echo(n-1, pos, ang, r);
  }
}

void mosaic(float l) {
  int max = 3;
  for(int i = 0; i < max;  i++) {
    for (int j = 0; j < max; j++) {
      echo(10, xyz(i*100.0, j*100.0, 0.0), 0.0, l);
      echo(10, xyz(i*100.0 + l, j*100.0, 0.0), HALF_PI, l);
      echo(10, xyz(i*100.0 + l, j*100.0 + l, 0.0), PI, l);
      echo(10, xyz(i*100.0 , j*100.0 + l, 0.0), 3/2* PI, l);
    }
  }

  frame(xyz(0,0,0), max * l, height);
}

void setup() {
  backend(autocad);
}

void draw() {
  mosaic(100);
}

void renderMosaic(){
  renderSize(1920,1080);
  renderDir("C:\\Users\\hugo\\render");
  render("mosaico",xyz (148.616 ,-51.5337, 251.441), xyz( 149.301, 107.593 ,34.156), 20.0);
}
