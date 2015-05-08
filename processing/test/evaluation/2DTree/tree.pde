float fmax = 0.79, fmin = 0.69;
float da = PI/6, db = PI/5; 

void tree(float x, float y, float len, float ang) {
  float x2 = x - len * cos(ang);
  float y2 = y - len * sin(ang);

  line( x, y, x2, y2);
  
  if (len < 10) 
    ellipse( x2, y2, 0.6, 0.6);
  else {
    tree(x2, y2, random(fmin,fmax) * len, ang + da);
    tree(x2, y2, random(fmin,fmax) * len, ang - db);
  }
                  
}

void setup(){
    size(1024,768);
    background(255);
    translate(width/2, height);
    scale(2);
    tree(0,0,100,PI/2);
}

void mousePressed() {
  save("2D-tree-p5.png");
}
