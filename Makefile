all:
	ghc Brainfuck.hs -o brainfuck

clean:
	rm brainfuck Brainfuck.hi Brainfuck.o
