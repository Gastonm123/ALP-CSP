all: build

build: $(HS_FILES) .FORCE
	cabal build

test: build .FORCE
	cabal run CSP-stack-exe -- examples/Especificacion.csp -a
	cabal run CSP-stack-exe -- examples/Fibonacci.csp -a
	cabal run CSP-stack-exe -- examples/HelloWorld.csp -a
	cabal run CSP-stack-exe -- examples/LinkedList.csp -a

HS_FILES := $(wildcard src/*.hs)

.FORCE: