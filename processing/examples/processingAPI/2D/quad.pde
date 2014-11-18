#lang processing

backend(tikz);
quad(0,0,100,100,100,200,0,200);
quad(0,0,-100,100,-100,200,0,200);

generateTikz("quad");
