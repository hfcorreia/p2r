#lang processing
PVector x = new PVector();
PVector y = new PVector(1, 2, 3);
float[] array = new float[] { 
  10, 20, 30
};

println("SET"); // SET - no static
println("x:", x);
x.set(-1, -1, -1);
println("x:", x);
x.set(-2, -2);
println("x:", x);
x.set(y);
println("x:", x);
x.set(array);
println("x:", x);

println("Random2D"); // Random2D - only static
println("x:", PVector.random2D()); 
println("x:", x.random2D());
println("x:", x);
println("y:", PVector.random2D(y)); //side effect
println("y:", y);
