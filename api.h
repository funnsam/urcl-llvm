typedef unsigned _BitInt(URCL_BITS) urcl_t;
typedef signed _BitInt(URCL_BITS) surcl_t;

#if URCL_BITS >= 128
typedef _Float128 urcl_float_t;
#elif URCL_BITS >= 64
typedef _Float64 urcl_float_t;
#elif URCL_BITS >= 32
typedef _Float32 urcl_float_t;
#elif URCL_BITS >= 16
typedef _Float16 urcl_float_t;
#endif

extern size_t urcl_main();
void urcl_in(urcl_t *ret, uint8_t port);
void urcl_out(uint8_t port, urcl_t *data);
