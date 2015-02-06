#lang processing

void foo() {
  if ( true )
    println("foo",true);
}

void foo2(int x){
  if ( x == 0 )
    println("foo2", true);
  else
    println("foo2",false);
}

void foo3(int x){
  if ( x == 0 ) {
    println("foo3", true);
  }
}

void foo4(int x){
  if ( x == 0 ) {
    println("foo4",true);
  }
  else {
    println("foo4",false);
  }
}

void setup(){
  foo();
  foo2(0);
  foo2(1);
  foo3(0);
  foo4(0);
  foo4(1);
}
