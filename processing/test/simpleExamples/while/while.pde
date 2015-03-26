#lang processing

int i = 0;

while( i < 10 ){
  println(i);
  i++;
}

while(true){
  if(i == 20)
    break;
  println(i++);
}

while(i < 30){
  if(i == 25) {
    println("-");
    i++;
    continue;
  }
  println(i++);
}
