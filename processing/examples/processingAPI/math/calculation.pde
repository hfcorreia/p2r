#lang processing

println(abs(-2) == abs(2)); //true
println(ceil(2.5) == 3.0); //true
println(floor(2.5) == 2.0); //true
println(constrain(10,20,30) == 20);
println(constrain(20,10,30) == 20);
println(constrain(50,10,30) == 30);
println(dist(4,0,0,0) == 4);
println(dist(4,0,4,0) == 0);
println(dist(4,0,0,0,0,0) == 4);
println(log(exp(1))== 1.0);

int[] list = new int[10];
list[3]=4;
list[5]=2;
println(max(list)==4);

println(max(1,3,4,5)==5);


println(min(list)==0);
println(min(1,3,4,5)==1);

println(pow(2,2)==4);
println(sq(2)==4);

println(sqrt(4.)==2.);

for(int i =0 ; i < 20; i++){
  
        println(randomGaussian());
}
