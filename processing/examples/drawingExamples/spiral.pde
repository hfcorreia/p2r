#lang processing
// http://www.rupert.id.au/tutorials/processing/examples/spiral_v2/index.php

void setup() {
    size(600, 600);
    noFill();
    smooth();
}

// Global Variables
float x = 0;
float y = 0;
float deg = 1;
float st = 1;
int count = 0;

void draw() {
    strokeWeight(st);
    translate(width/2, height/2);  // translate to center of screen
    rotate(deg);

    point(x, y);
    x = x + 0.1;
    y = y + 0.1;
    deg = deg + 1;
    st = st + 0.007;
    count ++;

    if (count > 2000) {noLoop();}
}
