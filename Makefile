FLAGS=-Wall -Werror -fno-warn-name-shadowing
EXTRAFLAGS=-XTypeFamilies -XFlexibleInstances

main: Main.hs
	ghc ${FLAGS} ${EXTRAFLAGS} Main.hs
	./Main > test.svg

nowarn:
	ghc Main.hs
	./Main > test.svg

test: Test_Tensor Test_TensorField
	./Test_Tensor
	./Test_TensorField

Test_Tensor:
	ghc ${FLAGS} Test_Tensor.hs

Test_TensorField:
	ghc ${FLAGS} Test_TensorField.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f Main Test_Tensor Test_TensorField
