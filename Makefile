FLAGS=-Wall -Werror -fno-warn-name-shadowing

tensor:
	ghc ${FLAGS} Main.hs
	./Main > test.svg

nowarn:
	ghc Main.hs
	./Main > test.svg

clean:
	rm -f *.hi
	rm -f *.o
	rm -f Main
