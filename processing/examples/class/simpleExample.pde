#lang processing

class Foo {
    int y = 20;
    int x;

    void Foo() {
       println("Building Foo");
    }

    void setY(int z,int w) {
        y = z;
        x = w;
    }

    void printValue(){
        println(y);
    }
}


Foo foo = new Foo(), foo2 = new Foo();
//println();
//println(2);
//foo.setY(10);
foo.printValue();




