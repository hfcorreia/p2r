#lang processing

Foo[] x = new Foo[10];

void setup(){
   

   for ( int i = 0; i < 10 ; i++ ) 
   {
      int z = 10;
      x[i] = new Foo();
      z -= x[i].x = 20;
      
      println(x[i].x);
      println(z);
   }

}
          

class Foo {
       int x;
       int y;
       
       void display(int x, int y) {
              println(x,y);
       }
}