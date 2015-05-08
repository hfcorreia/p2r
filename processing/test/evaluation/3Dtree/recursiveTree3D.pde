#lang processing

void tree(float x, float y,float z, float l, float a1, float a2, float r) {
  float x2 = x + l * cos(a1) * sin(a2),
        y2 = y + l * sin(a1) * cos(a2),
        z2 = z + l * cos(a2),
        r2 = r * 0.55;

  coneFrustum( xyz(x, y, z), r , xyz(x2, y2, z2), r2);

  if (l < 7) {
    box(xyz(x2-3,y2-3,z2-0.5),6,6,1);
  }  else {
    tree(x2, y2, z2, l * 0.7, a1 + PI * 1/4, PI/4, r2);
    tree(x2, y2, z2, l * 0.7, a1 + PI * 3/4, PI/4, r2);
    tree(x2, y2, z2, l * 0.7, a1 + PI * 5/4, PI/4, r2);
    tree(x2, y2, z2, l * 0.7, a1 + PI * 7/4, PI/4, r2);
  }
}

void setup(){
  //  backend(rhinocerous);
  //  tree(0,0,0, 20, 0, 0, 3);
}


void treeAutocad(){
  backend(autocad);
  tree(0,0,0, 20, 0, 0, 3);
  renderSize(320,240);
  renderDir("C:\\Users\\hugo\\render");
  view( xyz(   36.7448, -40.1265 ,33.4099), xyz( -8.07395, 9.04725, 22.5821), 20.0);
}

void treeRhino(){
  backend(rhinocerous);
  tree(0,0,0, 20, 0, 0, 3);
  renderSize(320,240);
  renderDir("C:\\Users\\hugo\\render");
  view( xyz(  29.363617503490328, -51.34296632572393, 31.829149017676848), xyz( -2.2819458822936696 ,4.820529099041082, 21.272334752699052), 19.999999999999993);
}
