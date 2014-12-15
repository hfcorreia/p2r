#lang processing
/**
 * Transform: Slit-Scan
 * from Form+Code in Design, Art, and Architecture 
 * by Casey Reas, Chandler McWilliams, and LUST
 * Princeton Architectural Press, 2010
 * ISBN 9781568989372
 * 
 * This code was written for Processing 1.2+
 * Get Processing at http://www.processing.org/download
 */


import processing.video.*;

Movie myVideo;
int video_width     = 160;
int video_height    = 120;
int video_slice_x   = video_width/2;
int window_width    = 1000;
int window_height   = video_height;
int draw_position_x = 0; 
boolean newFrame  = false;

void setup() {
  myVideo = new Movie(this, "station.mov");
  size(window_width, window_height, P2D);
  background(0);
  myVideo.loop();
}

void movieEvent(Movie myMovie) {
  myMovie.read();
  newFrame = true;
}

void draw() {
  if (newFrame) {
    loadPixels();
    for (int y=0; y<window_height; y++){
      int setPixelIndex = y*window_width + draw_position_x;
      int getPixelIndex = y*video_width  + video_slice_x;
      pixels[setPixelIndex] = myVideo.pixels[getPixelIndex];
    }
    updatePixels();
    
    draw_position_x++;
    if (draw_position_x >= window_width) {
      exit();
    }
    newFrame = false;
  }
}
