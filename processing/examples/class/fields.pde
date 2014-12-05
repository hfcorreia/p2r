#lang processing


class Foo {
    int x = 1;
    int y = 2;
    
    void sum(){
      return x + y;
    }
}


void setup(){
  Foo foo = new Foo();

  int x;
  x = 2;
  foo.x = 20;

  println(foo.y + foo.x); // this is ok
  println(x);

  println(foo.y); // this is ok

  println(foo.sum());
  
  int[] array1 = new int[10];
  int[] array2 = new int[] { 1,2,3};
  
  println(array1[0]);
  println(array2[2]);
  
}