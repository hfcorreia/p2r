#lang processing

float f = 0.7;
float narrow = 0.55;

void tree(float x, float y,float z, float len, float lat, float coLat, float radius) {
  float x2 = x + len * cos(lat) * sin(coLat);
  float y2 = y + len * sin(lat) * cos(coLat);
  float z2 = z + len * cos(coLat);
  float narrowRadius = radius * narrow;

  coneFrustum( xyz(x, y, z), radius , xyz(x2, y2, z2), narrowRadius);
 
  if (len < 7) {
    box(xyz(x2-3,y2-3,z2-0.5),6,6,1);   
  }
  else {
    float a1 = lat + PI/4;;
    float a2 = PI/4;

    tree(x2, y2, z2, f * len, a1, a2, narrowRadius);
    tree(x2, y2, z2, f * len, (a1 + PI * 1/2), a2, narrowRadius);
    tree(x2, y2, z2, f * len, (a1 + PI ), a2, narrowRadius);
    tree(x2, y2, z2, f * len, (a1 + PI * 3/2), a2, narrowRadius);
  }               
}

void treeAutocad(){
                  backend(autocad);
                  tree(0,0,0, 20, 0, 0, 3);
                  renderSize(320,240);
                  renderDir("C:\\Users\\hugo\\render");
                  view( xyz(   36.7448, -40.1265 ,33.4099), xyz( -8.07395, 9.04725, 22.5821), 20.0);
}

void treeRhino(){
                  backend(rhino5);
                  tree(0,0,0, 20, 0, 0, 3);
                  renderSize(320,240);
                  renderDir("C:\\Users\\hugo\\render");
                  view( xyz(  29.363617503490328, -51.34296632572393, 31.829149017676848), xyz( -2.2819458822936696 ,4.820529099041082, 21.272334752699052), 19.999999999999993);
}