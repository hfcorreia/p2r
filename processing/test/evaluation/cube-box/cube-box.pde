#lang processing

float size = 18.0;
float off = 1.0;
float reduction = size/3;

Object box;

void renderBox(){
  renderSize(1920,1080);
  render("box-with-holes", xyz( 179.896, -102.22, 63.0956), xyz( 26.2572, 72.1579, 44.5973), 20.0);
}
void setup(){
     backend(autocad);  
}

void draw(){
  layer("Concrete");
  box = box(xyz(0,0,0), 100, 100, 100);
    
  layer("Glass");
  perfurate();
  
  layer("Concrete");
  frame(5);
 
  subtraction(box, box(xyz(1,1,1),98,98,98));
}

void perfurate() {
      for(int i = 0; i < 5; i++) {
         for(int j = 0; j < 5; j++) {
              // x
              genX((i * 20), (j * 20));
              // y
              genY((i * 20), (j * 20));  
              // z
              genZ((i * 20), (j * 20));  
              }
         }
}


void genX(float y, float z){
    if(random(500) > 200) {
       box =  subtraction (box, box(xyz(-size/2,y + off,z + off), size, size, size));
       box(xyz(-(reduction + 0.2) ,y,z), reduction, size+2*off, size+2*off);
    }
    if(random(2000) > 1200) {
       box =  subtraction (box, box(xyz(100 - size/2,y + off,z + off), size, size, size));
       box(xyz( 100 + 0.2 ,y,z), reduction, size+2*off, size+2*off);          
    }
}

void genY(float x, float z){
    if(random(900) > 700) {
       box = subtraction (box, box(xyz(x + off, -size/2, z + off), size, size, size));
       box(xyz( x, -(reduction + 0.2),z), size+2*off, reduction, size+2*off);
    }
    if(random(10000) > 5500) {
       box = subtraction (box, box(xyz(x + off , 100 - size/2, z + off), size, size, size));
       box(xyz( x, 100 + 0.2 ,z), size+2*off, reduction, size+2*off);      
    }
}

void genZ(float x, float y){
    if(random(1000) > 800) {
       box = subtraction (box, box(xyz(x + off ,y + off,-size/2), size, size, size));
       box(xyz( x, y,-(reduction + 0.2)), size+2*off, size+2*off, reduction);
    }
    if(random(600) > 550) {
       box = subtraction (box, box(xyz(x + off ,y + off, 100 - size/2), size, size, size));
       box(xyz( x, y, 100 + 0.2), size+2*off, size+2*off, reduction);      
    }
}
                               

void frame(int size){
  box(xyz(0,0,0), size, size, 100);
  box(xyz(0,0,0), size, 100, size);
  box(xyz(0,0,0), 100, size, size);
  
  box(xyz(100-size,0,0), size, size, 100);
  box(xyz(100-size,0,0), size, 100, size);
  
  box(xyz(0,100-size,0), size, size, 100);
  box(xyz(0,100-size,0), 100, size, size);
  
  box(xyz(100-size,100-size,0), size, size, 100);
  
  box(xyz(0,0,100-size), 100, size, size);
  box(xyz(0,0,100-size), size, 100, size);
  
  box(xyz(100-size,0,100-size), size, 100, size);
  box(xyz(0,100-size,100-size), 100, size, size);
}