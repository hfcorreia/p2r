#lang processing

require "tikz.rkt";
require (planet aml/rosetta);

backend(tikz);

circle();
generateTikz("circle");

