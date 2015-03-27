#lang processing
int x = 2;

float x(float x) {
   return x;
}

float x(int x, int y) {
   return x(x) + x(y);
}
        
float x(int x) {
   return x;
}
                 