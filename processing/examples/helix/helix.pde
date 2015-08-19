#lang processing

float r =  15;
float height = 2;

void helix(float z, float ang) {
  float x1 = r * cos(ang), y1 = r * sin(ang);
  float x2 = r * cos(PI + ang), y2 = r * sin(PI + ang);

  sphere( xyz(x1, y1, z), 2);
  cylinder( xyz(x1, y1, z), 0.5, xyz(x2, y2, z));
  sphere( xyz(x2, y2, z), 2);

  if( ang > 0) helix(z + height, ang - PI/8);
}

void setup() {
  backend(autocad);
}

void draw(){
  helix(0, 4 * PI);
}

void renderHelix(){
 helix(0, 4 * PI);
 renderSize(1920,1080);
 renderDir("C:\\Users\\hugo\\render");
 render("helix", xyz( -88.3167, -26.9472, 37.9823), xyz(-14.8119, -1.83835, 32.9565), 20.0);
 
}