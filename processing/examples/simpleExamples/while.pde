#lang processing

void setup() {
  int i = 0;

  while ( i < 10) {
    if ( i == 5 ) {
      i += 1;
      continue;
    }
    else if ( i == 8)
      break;
    else
      println(i);

    i += 1;
  }

  do {
   println("once");
    } while( i == 0 );

}
