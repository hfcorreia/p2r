#lang processing

require (rename-in pict [cloud xx] [circle b]);
require (prefix-in p- pict);
require "foo.rkt";

int x = 2;
foo(x);
