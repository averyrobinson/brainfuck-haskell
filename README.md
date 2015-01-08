Brainfuck Interpreter
=====================

This is a Brainfuck interpreter written in Haskell. It uses Haskell's
laziness to make Brainfuck's byte array infinitely long in both
directions.

I have included two example Brainfuck programs, `helloworld.bf`,
which I got from the Wikipedia page on Brainfuck, and `rot13.bf`,
which I got from [here](https://github.com/bonomat/rot13-brainfuck).

Usage
-----

    make
    ./brainfuck [source file]
