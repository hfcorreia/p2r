P2R
===

P2R (Processing to Racket) is an implementation of the Processing programming language for Racket that allows architects and designers to use Processing with their favourite CAD application.

## Installation:
Before installing P2R, you will need to have [Racket](http://www.racket-lang.org/).

You can install P2R from DrRacket's Install Package dialog box with `git://github.com/hfcorreia/p2r` as the package source.
Alternatively, you can install it with the `raco` tool by running:

> `raco pkg install -n git://github.com/hfcorreia/p2r`

## Updating:

You can update the currently installed version with DrRacket's Package Manager, or alternatively with the following raco command:

> `raco pkg update p2r`

## Features:

At the moment P2R implements Processing's basic primitive types and control flow structures. P2R uses the [Rosetta](http://planet.racket-lang.org/display.ss?package=rosetta.plt&owner=aml) library to have a rendering environment, allowing to use Processing with CAD applications (such as AutoCAD ou Rhinoceros 3D). Many of Rosetta drawing operations and capabilities were mapped to Processing.

## Use:

To use P2R with Racket, simply replace `#lang racket` with `#lang processing`.

## Examples

Check the examples for some ideas of how to use P2R.

## To Do:

  * Processing's object system and exceptions
  * Map Processing's runtime and primitives to Racket's

## Known Issue
Importing Rosetta in our implementation generates a `cannot instantiate 'racket/gui/base' a second time in the same process` warning which breaks DrRacket arrow/binding traking.

## Acknowledgments

This work was partially supported by national funds through Fundação para a Ciência e a Tecnologia (FCT) with reference UID/CEC/50021/2013, and by the Rosetta project under contract PTDC/ATP-AQI/5224/2012.
