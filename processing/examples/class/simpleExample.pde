#lang processing

class Foo {
    int y = 20;
    int x;

    void Foo() {
        println("Building Foo");
    }

    void setY(int z) {
        y = z;
    }

    void printValue(){
        println(y);
    }
}


Foo foo = new Foo(), foo2 = new Foo();
foo.setY(10);
foo.printValue();

