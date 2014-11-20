#lang processing

class Foo {
    int y = 20;

    void Foo() {
        println("Building Foo");
    }

    void setY(int value) {
        y = value;
    }

    void printValue(){
        println(y);
    }
}


new Foo();
//foo.setY(10);
//foo.printValue();

