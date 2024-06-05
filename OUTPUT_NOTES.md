# Object file API
## Exposed functions
```
uint32_t urcl_main();
```
- Return value: number of instructions executed

## Required external functions
```
uint32_t urcl_in(uint32_t port);
void urcl_out(uint32_t port, uint32_t data);
```
