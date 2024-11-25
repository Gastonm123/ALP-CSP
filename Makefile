all: build

build: $(HS_FILES)
	stack build

test: build .FORCE
	stack run CSP-stack-exe -- examples/Especificacion.csp -a
	stack run CSP-stack-exe -- examples/Fibonacci.csp -a
	stack run CSP-stack-exe -- examples/HelloWorld.csp -a
	stack run CSP-stack-exe -- examples/LinkedList.csp -a
	stack run CSP-stack-exe -- examples/SesionWeb.csp -a
	stack run CSP-stack-exe -- examples/Supermercado.csp -a
	stack run CSP-stack-exe -- examples/Defectuoso.csp -a
	stack run CSP-stack-exe -- examples/Interruptor.csp -a

HS_FILES := $(wildcard src/*.hs)

.FORCE: