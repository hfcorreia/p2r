#lang processing

void setup(){
     backend(autocad);
}

void draw(){
    pyramidFrustum(6, xyz(0,0,0), 15, 0, xyz(0,0,4), 10);
    pyramidFrustum(6, xyz(0,0,5), 10, 0, xyz(0,0,9), 5);
    pyramid(6, xyz(0,0,10), 5, 0, xyz(0,0,14));
}
