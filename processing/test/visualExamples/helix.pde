#lang processing

float r =  15;
float height = 2;

void helix(float z, float ang) {
  float x1 = r * cos(ang), y1 = r * sin(ang);
  float x2 = r * cos( PI + ang), y2 = r * sin(PI + ang);

  sphere( x1, y1,  z, 2);
  cylinder( x1, y1,  z, 0.5, x2, y2, z);
  sphere( x2, y2,  z, 2);

  if( ang > 0) helix(z + height, ang - PI/8);
}

void setup() {
  backend(autocad);
}

void draw(){
  helix(0, 4 * PI);
}
