#lang processing

class Foo {
    int y = 20;
        
    void Foo() {
       println("Building Foo");
    }

    void setY(int val) {
       y = val;
    }

    void printValue(){
        println(y);
    }
}

int x = 2;
;

{

            }

void setup(){
Foo foo = new Foo(), foo2 = new Foo();
foo.setY(10);
foo.printValue();
}

void draw(){
            
}


