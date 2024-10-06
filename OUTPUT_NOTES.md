# Object file API
## Exposed functions
```c
size_t urcl_main();
```
- Return value: number of instructions executed

## Required external functions
```c
size_t urcl_in(size_t port);
void urcl_out(size_t port, size_t data);
```

## Optional external functions
```c
// only used if CLI option `--bounds-safety` is used
void memory_oob(size_t addr);
```
