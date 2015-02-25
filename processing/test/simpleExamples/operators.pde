#lang processing

println("Priting Binary Operations");
int x1 = 1+2;
int x2 = 2-1;
int x3 = 2*2;
int x4 = 2/2;
int x5 = 3%2;
int x6 = 3&2;
int x7 = 3|2;
int x8 = 3^2;
int x9 = 3<<2;
int x10 = 3>>2;
int x11 = 3>>>2;
boolean b1 = false&&true;
boolean b2 = false||true;

println("Priting Unary Operations");
int u1 = +(-1);
int u2 = +1;
int u3 = -1;
int u4 = ~1;
boolean u5 = !false;

println(";;;;;;;;;;;;");
int x = 20;
println("eq 20",x++); //20
println("eq 21",x); //21

x = 20;
println("eq 20",x--); //20
println("eq 19",x); //19

x = 20;
println("eq 21",++x); //21
println("eq 21",x); //21

x = 20;
println("eq 19",--x); //19
println("eq 19",x); //19
println(";;;;;;;;;;;;");

println("Relational Operations");
println(1<2);
println(1>2);
println(1<=2);
println(2>=2);

println(1==2);
println(1!=2);
