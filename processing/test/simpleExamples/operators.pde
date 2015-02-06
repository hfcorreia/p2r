#lang processing

println("Priting Binary Operations");
println(1+2);
println(2-1);
println(2*2);
println(2/2);
println(3%2);
println(3&2);
println(3|2);
println(3^2);
println(3<<2);
println(3>>2);
println(3>>>2);
println(false&&true);
println(false||true);

println("Priting Unary Operations");
println(+(-1));
println(+1);
println(-1);
println(~1);
println(!false);

println(";;;;;;;;;;;;");
int x = 20;
println(x++); //20
println(x); //21

x = 20;
println(x--); //20
println(x); //19

x = 20;
println(++x); //21
println(x); //21

x = 20;
println(--x); //19
println(x); //19
println(";;;;;;;;;;;;");

println("Relational Operations");
println(1<2);
println(1>2);
println(1<=2);
println(2>=2);

println(1==2);
println(1!=2);
