#lang processing


void setup(){
     backend(autocad);
}

void draw(){

   Object[] points1 = new Object[10];
   Object[] points2 = new Object[10];
   
   float p = -PI;
   
   for(int i = 0; i < 10; i++, p += PI / 10) {
      points1[i] = xyz(0.0, p, sin( 1.5 * p));
      points2[i] = xyz(5.0, p, cos( 1/2 * p));
   }

   loft(spline(points1), spline(points2));
}