#lang processing
#lang processing
// Only calculate the color once
int wh = width*height;
color c = 0;//color(102);
loadPixels();
for (int index = 0; index < wh; index++) {
  pixels[index] = c;
}
updatePixels();
