#lang processing

class Foo {
       int x=0;
       int y=1;
       
       void display(int a, int b) {
              println(a+x,b+y);
       }
}

Foo[] x = new Foo[10];

Foo z = new Foo();

void setup(){
   int zx = x.length;
   println(x.length);

   for ( int i = 0; i < 10 ; i++ ) 
   {  
     x[i] = new Foo();  
     x[i].display(0,0);
   }
}
          

