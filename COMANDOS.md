## Stack o Cabal
Se puede usar Cabal o Stack de manera intercambiable aunque Stack es mas
confiable y mas configurable por medio del `stack.yaml`

Para stack:
```
stack build
stack run -- examples/Especificacion.csp
```

Para cabal
```
cabal build
cabal run CSP-stack-exe -- examples/Especificacion.csp
```

Stack, por defecto, no soporta el campo setup-tools para instalar happy de forma
automatica. Vamos a tener que instalarlo manualmente. Suerte!

## Otros
### Para aplicar cambios de package.yaml
```
hpack
```
Si algo se rompe intentar `stack build` y debugear los errores de dependencias
antes de compilar de nuevo3

### Para ejecutar el proyecto
Agregar o quitar la opcion `-d` para ver informacion de debug.
```
cabal run CSP-stack-exe -- archivo.csp
stack run -- archivo.csp
```

Para ver la sintaxis interpretada por el parser agregar la bandera -a
```
cabal run CSP-stack-exe -- examples/Especificacion.csp -a
stack run -- examples/Especificacion.csp -a
```

### Para testear el parser
```
make test
```