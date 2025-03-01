typedef unsigned _BitInt(URCL_BITS) urcl_t;
typedef signed _BitInt(URCL_BITS) surcl_t;

extern size_t urcl_main();
void urcl_in(urcl_t *ret, uint8_t port);
void urcl_out(uint8_t port, urcl_t *data);
