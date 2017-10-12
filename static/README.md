What is this?
=============

we use the "unihex" format which consists of 16-bit unicode codepoint in big-endian + 16 or 32 bytes bitmaps for (8x16) and (16x16) resolution, respectively.

Since unihex is not very format these are decoded from hex and unneeded : and newlines are stripped, see "compile_unihex.py" for details.

The steps to produce bitmapfont.ml are:

1) Compile unihex bitmap fonts to more efficient binary format:
python compile_unihex.py ascii.unihex single-chars.bitmap wide-chars.bitmap

2) Put this into an ocaml module:
ocaml-crunch -e bitmap -m plain -o bitmapfont.ml ./

Additional notes
================

ttf2bdf    - archive.org
bdftopcf   - xfonts-utils
pcf2unihex - unifont-bin

https://github.com/jirutka/otf2bdf
https://fontforge.github.io/pcf-format.html

