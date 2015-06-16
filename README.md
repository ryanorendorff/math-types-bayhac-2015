The Math of Types Presentation for BayHac 2015
==============================================

Presentation on the math behind types, including derivatives for the Bay
Area Haskell Users Group on 13 June, 2015.

Included is a nix shell file to make it convenient to get into the correct
environment to both compile and run the included code.


Compiling
---------

To compile the presentation into a PDF, run

    make pdf

This will require custom fonts to be installed. If you do not have these
fonts, edit the Makefile to remove the fonts header added to the `tex`
target pandoc call.
