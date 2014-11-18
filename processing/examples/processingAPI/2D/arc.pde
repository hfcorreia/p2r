#lang processing

backend(tikz);
arc(0,0,100,100,pi,pi+pi/2);
arc(0,0,90,90,0,pi+pi/2);
generateTikz("arc");
