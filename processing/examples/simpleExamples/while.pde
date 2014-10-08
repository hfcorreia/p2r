#lang processing

void foo() {
  boolean i = false;

  while ( i == false) {
    println("just once");
    i = true;
  }

  do { 
    println("still once");
  } while( i == true);
}

foo();
