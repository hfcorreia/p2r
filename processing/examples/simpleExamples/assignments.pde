#lang processing

int y;
int t;
int x=1;


y = x; // 1

y *= 2; // 2

x = t = y;
println(x);
println(y);
println(t);
