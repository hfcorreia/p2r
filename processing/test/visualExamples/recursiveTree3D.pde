#lang processing

float f = 0.7;
float narrow = 0.55;

void tree(float x, float y, float z, float len, float lat, float coLat, float radius) {
  float x2 = x + len * cos(lat) * sin(coLat);
  float y2 = y + len * sin(lat) * cos(coLat);
  float z2 = z + len * cos(coLat);
  float narrowRadius = radius * narrow;

  cone(x, y, z, radius , x2, y2, z2, narrowRadius);
 
  if (len < 7) {
    centeredBox(x2,y2,z2,6,6,1);   
  }
  else {
    int a1 = lat + PI/4;;
    int a2 = PI/4;

    tree(x2, y2, z2, f * len, a1, a2, narrowRadius);
    tree(x2, y2, z2, f * len, (a1 + PI * 1/2), a2, narrowRadius);
    tree(x2, y2, z2, f * len, (a1 + PI ), a2, narrowRadius);
    tree(x2, y2, z2, f * len, (a1 + PI * 3/2), a2, narrowRadius);
  }               
}

void setup(){
    backend(autocad);
    tree(0,0,0, 20, 0, 0, 3);
}
