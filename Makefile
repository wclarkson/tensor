FLAGS=-Wall -Werror -fno-warn-name-shadowing
EXTRAFLAGS=-XTypeFamilies -XFlexibleInstances -XDeriveGeneric -fno-warn-name-shadowing

main: Main.hs
	ghc ${FLAGS} ${EXTRAFLAGS} Main.hs -o tensor

nowarn:
	ghc ${EXTRAFLAGS} Main.hs -o tensor

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

sad:
	@echo ":("
