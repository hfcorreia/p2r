#lang processing

require (planet aml/rosetta:1:=50);
require "tikz.rkt";

backend(tikz);

// create a circle at poit 1,2 with radius 2
circle( xy(1,2), 2);
generateTikz("circle");

