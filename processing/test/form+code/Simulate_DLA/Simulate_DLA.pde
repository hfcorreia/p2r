#lang processing
/**
 * Simulate: Diffusion-Limited Aggregation
 * from Form+Code in Design, Art, and Architecture 
 * by Casey Reas, Chandler McWilliams, and LUST
 * Princeton Architectural Press, 2010
 * ISBN 9781568989372
 * 
 * This code was written for Processing 1.2+
 * Get Processing at http://www.processing.org/download
 */

// this number might need to be smaller for some computers
int particleCount = 20000;
Particle[] particles = new Particle[particleCount];
boolean[] field;

void setup() {
  size(1024, 700, P2D);
  
  // create an array that stores the position of our particles
  field = new boolean[width * height];

  // add seed in the center
  int fcenterX = width / 2;
  int fcenterY = height / 2;
  field[fcenterX + fcenterY * width] = true;
  
  // make particles
  for(int i=0; i<particleCount; i++) {
    particles[i] = new Particle();
  }
}


void draw() {
  background(255);
  loadPixels();
  for(int i=0; i<particleCount; i++) {
    particles[i].update();
    if (particles[i].stuck) {
      pixels[particles[i].y * width + particles[i].x] = color(0);
    }
  }
  updatePixels();
}
