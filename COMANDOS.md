### Para aplicar cambios de package.yaml
```
hpack
```

### Para ejecutar el proyecto
Agregar o quitar la opcion `-d` para ver informacion de debug.
```
cabal run CSP-stack-exe -- archivo.csp
```

Para ver la sintaxis interpretada por el parser agregar la bandera -a
```
cabal run CSP-stack-exe -- examples/Especificacion.csp -a
```

### Para testear el parser
```
make test
```