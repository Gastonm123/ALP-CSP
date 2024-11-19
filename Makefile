all: build

build: $(HS_FILES)
	cabal build

test: build .FORCE
	cabal run CSP-stack-exe -- examples/Especificacion.csp -a
	cabal run CSP-stack-exe -- examples/Fibonacci.csp -a
	cabal run CSP-stack-exe -- examples/HelloWorld.csp -a
	cabal run CSP-stack-exe -- examples/LinkedList.csp -a
	cabal run CSP-stack-exe -- examples/SesionWeb.csp -a
	cabal run CSP-stack-exe -- examples/Supermercado.csp -a
	cabal run CSP-stack-exe -- examples/Defectuoso.csp -a
	cabal run CSP-stack-exe -- examples/Interruptor.csp -a

HS_FILES := $(wildcard src/*.hs)

.FORCE: