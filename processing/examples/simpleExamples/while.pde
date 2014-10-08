#lang processing

void foo() {
  int i = 0;

  while (i < 10 ) {
    if ( i == 5)  {
      i = i + 1; 
      continue;
    }
    else 
      println(i);

    i = i + 1; 
  }
}

foo();
