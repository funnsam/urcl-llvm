macro_rules! impl_to_bits {
    ($ty:tt $sty:tt $bits:tt $max_exp:literal $min_exp:literal $mantissa:literal $exp:literal $inf:literal) => {
        pub mod $ty {
            use dashu::{Decimal, base::Sign};

            pub fn from_dec(dec: Decimal) -> $ty {
                let r = dec.with_base_and_precision::<2>($mantissa).value();
                let repr = r.repr();

                if repr.digits_ub() == 0 {
                    // number == 0
                    return 0; // ieee f128 0.0 is just 0 in bits
                }

                if repr.is_infinite() || repr.exponent() >= $max_exp {
                    sign_ty($inf, repr.sign())
                } else if repr.exponent() < $min_exp - $mantissa {
                    sign_ty(0, repr.sign())
                } else {
                    encode(
                        repr.significand().try_into().unwrap(),
                        repr.exponent() as i16,
                    )
                }
            }

            fn sign_ty(f: $ty, sign: Sign) -> $ty {
                (f & (((1 as $ty) << ($bits - 2)) - 1))
                    | (((sign == Sign::Negative) as $ty) << ($bits - 1))
            }

            fn encode(mantissa: $sty, exponent: i16) -> $ty {
                if mantissa == 0 {
                    return 0;
                }

                // clear sign
                let sign = Sign::from(mantissa < 0);
                let mut mantissa = mantissa.unsigned_abs();

                let zeros = mantissa.leading_zeros();
                let top_bit = (u64::BITS - zeros) as i16 + exponent;

                if top_bit > $max_exp {
                    return sign_ty($inf, sign);
                } else if top_bit < -1022 - 52 {
                    return sign_ty(0, sign);
                }

                let bits; // bit representation
                if top_bit <= $min_exp - $mantissa {
                    // subnormal float

                    // first remove the exponent
                    let shift = exponent - $min_exp + $mantissa;
                    if shift >= 0 {
                        mantissa <<= shift as u32;
                    } else {
                        mantissa >>= (-shift) as u32;
                    }

                    // then compose the bit representation of this float
                    bits = ((sign as $ty) << ($bits - 1)) | mantissa;
                } else {
                    // normal float
                    // first normalize the mantissa (and remove the top bit)
                    if mantissa == 1 {
                        mantissa = 0; // shl will overflow
                    } else {
                        mantissa <<= zeros + 1;
                    }

                    // then calculate the exponent (bias = MAX_EXP - 1)
                    let exponent =
                        (exponent + ($max_exp - 1) + $bits as i16) as $ty - zeros as $ty - 1;

                    // then compose the bit representation of this float
                    bits = ((sign as $ty) << ($bits - 1))
                        | (exponent << ($mantissa - 1))
                        | (mantissa >> ($exp + 1));
                };

                bits
            }
        }
    };
}

impl_to_bits!(u128 i128 128 16384 -16381 113 15 0x7fff0000000000000000000000000000_u128);
impl_to_bits!(u16 i16 16 16 -13 11 5 0x7c00_u16);
