/*
 * Ported from Java to ECMAScript by Christian Lawson-Perfect, 2022.
 */
/*
 * Copyright (C) 2015 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/*
 * The above license covers additions and changes by AOSP authors.
 * The original code is licensed as follows:
 */
//
// Copyright (c) 1999, Silicon Graphics, Inc. -- ALL RIGHTS RESERVED
//
// Permission is granted free of charge to copy, modify, use and distribute
// this software  provided you include the entirety of this notice in all
// copies made.
//
// THIS SOFTWARE IS PROVIDED ON AN AS IS BASIS, WITHOUT WARRANTY OF ANY
// KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION,
// WARRANTIES THAT THE SUBJECT SOFTWARE IS FREE OF DEFECTS, MERCHANTABLE, FIT
// FOR A PARTICULAR PURPOSE OR NON-INFRINGING.   SGI ASSUMES NO RISK AS TO THE
// QUALITY AND PERFORMANCE OF THE SOFTWARE.   SHOULD THE SOFTWARE PROVE
// DEFECTIVE IN ANY RESPECT, SGI ASSUMES NO COST OR LIABILITY FOR ANY
// SERVICING, REPAIR OR CORRECTION.  THIS DISCLAIMER OF WARRANTY CONSTITUTES
// AN ESSENTIAL PART OF THIS LICENSE. NO USE OF ANY SUBJECT SOFTWARE IS
// AUTHORIZED HEREUNDER EXCEPT UNDER THIS DISCLAIMER.
//
// UNDER NO CIRCUMSTANCES AND UNDER NO LEGAL THEORY, WHETHER TORT (INCLUDING,
// WITHOUT LIMITATION, NEGLIGENCE OR STRICT LIABILITY), CONTRACT, OR
// OTHERWISE, SHALL SGI BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL,
// INCIDENTAL, OR CONSEQUENTIAL DAMAGES OF ANY CHARACTER WITH RESPECT TO THE
// SOFTWARE INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF GOODWILL, WORK
// STOPPAGE, LOSS OF DATA, COMPUTER FAILURE OR MALFUNCTION, OR ANY AND ALL
// OTHER COMMERCIAL DAMAGES OR LOSSES, EVEN IF SGI SHALL HAVE BEEN INFORMED OF
// THE POSSIBILITY OF SUCH DAMAGES.  THIS LIMITATION OF LIABILITY SHALL NOT
// APPLY TO LIABILITY RESULTING FROM SGI's NEGLIGENCE TO THE EXTENT APPLICABLE
// LAW PROHIBITS SUCH LIMITATION.  SOME JURISDICTIONS DO NOT ALLOW THE
// EXCLUSION OR LIMITATION OF INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THAT
// EXCLUSION AND LIMITATION MAY NOT APPLY TO YOU.
//
// These license terms shall be governed by and construed in accordance with
// the laws of the United States and the State of California as applied to
// agreements entered into and to be performed entirely within California
// between California residents.  Any litigation relating to these license
// terms shall be subject to the exclusive jurisdiction of the Federal Courts
// of the Northern District of California (or, absent subject matter
// jurisdiction in such courts, the courts of the State of California), with
// venue lying exclusively in Santa Clara County, California.
// Copyright (c) 2001-2004, Hewlett-Packard Development Company, L.P.
//
// Permission is granted free of charge to copy, modify, use and distribute
// this software  provided you include the entirety of this notice in all
// copies made.
//
// THIS SOFTWARE IS PROVIDED ON AN AS IS BASIS, WITHOUT WARRANTY OF ANY
// KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION,
// WARRANTIES THAT THE SUBJECT SOFTWARE IS FREE OF DEFECTS, MERCHANTABLE, FIT
// FOR A PARTICULAR PURPOSE OR NON-INFRINGING.   HEWLETT-PACKARD ASSUMES
// NO RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE.
// SHOULD THE SOFTWARE PROVE DEFECTIVE IN ANY RESPECT,
// HEWLETT-PACKARD ASSUMES NO COST OR LIABILITY FOR ANY
// SERVICING, REPAIR OR CORRECTION.  THIS DISCLAIMER OF WARRANTY CONSTITUTES
// AN ESSENTIAL PART OF THIS LICENSE. NO USE OF ANY SUBJECT SOFTWARE IS
// AUTHORIZED HEREUNDER EXCEPT UNDER THIS DISCLAIMER.
//
// UNDER NO CIRCUMSTANCES AND UNDER NO LEGAL THEORY, WHETHER TORT (INCLUDING,
// WITHOUT LIMITATION, NEGLIGENCE OR STRICT LIABILITY), CONTRACT, OR
// OTHERWISE, SHALL HEWLETT-PACKARD BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL,
// INCIDENTAL, OR CONSEQUENTIAL DAMAGES OF ANY CHARACTER WITH RESPECT TO THE
// SOFTWARE INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF GOODWILL, WORK
// STOPPAGE, LOSS OF DATA, COMPUTER FAILURE OR MALFUNCTION, OR ANY AND ALL
// OTHER COMMERCIAL DAMAGES OR LOSSES, EVEN IF HEWLETT-PACKARD SHALL
// HAVE BEEN INFORMED OF THE POSSIBILITY OF SUCH DAMAGES.
// THIS LIMITATION OF LIABILITY SHALL NOT APPLY TO LIABILITY RESULTING
// FROM HEWLETT-PACKARD's NEGLIGENCE TO THE EXTENT APPLICABLE
// LAW PROHIBITS SUCH LIMITATION.  SOME JURISDICTIONS DO NOT ALLOW THE
// EXCLUSION OR LIMITATION OF INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THAT
// EXCLUSION AND LIMITATION MAY NOT APPLY TO YOU.
//
/**
/** Return the number of bits in the binary representation of `n`.
 *
 * @private
 * @param {BigInt} n
 * @returns {integer}
 */
function bitLength(n) {
    return n.toString(2).length;
}

const bigIntMath = {
    abs: function(n) {
        return n < 0n ? -n : n;
    },
    signum: function(n) {
        return n > 0n ? 1 : n < 0n ? -1 : 0;
    }
}

/**
 * Constructive real numbers, also known as recursive, or computable reals.
 * Each recursive real number is represented as an object that provides an
 * approximation function for the real number.
 * The approximation function guarantees that the generated approximation
 * is accurate to the specified precision.
 * Arithmetic operations on constructive reals produce new such objects;
 * they typically do not perform any real computation.
 * In this sense, arithmetic computations are exact: They produce
 * a description which describes the exact answer, and can be used to
 * later approximate it to arbitrary precision.
 * 
 * When approximations are generated, e.g. for output, they are
 * accurate to the requested precision; no cumulative rounding errors
 * are visible.
 * In order to achieve this precision, the approximation function will often
 * need to approximate subexpressions to greater precision than was originally
 * demanded.  Thus the approximation of a constructive real number
 * generated through a complex sequence of operations may eventually require
 * evaluation to very high precision.  This usually makes such computations
 * prohibitively expensive for large numerical problems.
 * But it is perfectly appropriate for use in a desk calculator,
 * for small numerical problems, for the evaluation of expressions
 * computated by a symbolic algebra system, for testing of accuracy claims
 * for floating point code on small inputs, or the like.
 * 
 * We expect that the vast majority of uses will ignore the particular
 * implementation, and the member functons `approximate`
 * and `get_appr`.  Such applications will treat `CReal` as
 * a conventional numerical type, with an interface modelled on
 * `java.math.BigInteger`.  No subclasses of `CReal`
 * will be explicitly mentioned by such a program.
 * 
 * All standard arithmetic operations, as well as a few algebraic
 * and transcendal functions are provided.  Constructive reals are
 * immutable; thus all of these operations return a new constructive real.
 * 
 * A few uses will require explicit construction of approximation functions.
 * The requires the construction of a subclass of `CReal` with
 * an overridden `approximate` function.  Note that `approximate`
 * should only be defined, but never called.  `get_appr`
 * provides the same functionality, but adds the caching necessary to obtain
 * reasonable performance.
 * 
 * Any operation may also throw an exception if the precision request generated
 * during any subcalculation overflows a 28-bit integer.  (This should be 
 * extremely unlikely, except as an outcome of a division by zero, or other 
 * erroneous computation.)
 */
class CReal {
    min_prec = null;
    max_appr = null;
    appr_valid = false;

    approximate(precision) {
        throw new Error("approximate not implemented");
    }

    /**
     * @private
     * @param {integer} n
     * @returns {integer}
     */
    static bound_log2(n) {
        const abs_n = Math.abs(n);
        return Math.ceil(Math.log(abs_n+1)/Math.log(2.0));
    }

    /** Throw an error if the requested precision is outside what can be safely represented with an integer.
     * @private
     */
    static check_prec(n) {
        const high = n >> 28;
        const high_shifted = n >> 29;
        if( 0 != (high ^ high_shifted) ) {
            throw(new Error("precision overflow"));
        }
    }

    /** Multiply k by 2**n.
     *
     * @param {BigInt} k
     * @param {integer} n
     * @returns {BigInt}
     * @private
     */
    static shift(k, n) {
        if(n == 0) {
            return k;
        } else if(n < 0) {
            return k >> BigInt(-n);
        } else {
            return k << BigInt(n);
        }
    }

    /* Multiply k by 2**n, rounding result.
     * 
     * @param {BigInt} k
     * @param {integer} n
     * @returns {BigInt}
     * @private
     */
    static scale(k, n) {
        if(n >= 0) {
            return k << BigInt(n);
        } else {
            const adj_k = CReal.shift(k, n+1) + 1n;
            return adj_k / 2n;
        }
    }

    /** Returns `value / 2 ** precision` rounded to an integer.
     * The error in the result is strictly less than 1.
     * Produces the same answer as `approximate`, but uses and maintains a cached approximation.
     * Normally not overridden, and called only from `approximate` methods in subclasses. 
     * Not needed if the provided operations on constructive reals suffice.
     *
     * @param {integer} precision
     * @returns {BigInt}
     */
    get_appr(precision) {
        CReal.check_prec(precision);
        if(this.appr_valid && precision >= this.min_prec) {
            return CReal.scale(this.max_appr, this.min_prec - precision);
        } else {
            const result = this.approximate(precision);
            this.min_prec = precision;
            this.max_appr = result;
            this.appr_valid = true;
            return result;
        }
    }

    /** Return the position of the most significant digit (msd).
     * If `x.msd() == n` then `2**(n-1) < abs(x) < 2**(n+1)`
     * This initial version assumes that `max_appr` is valid and sufficiently removed from zero that the msd is determined.
     *
     * @returns {integer}
     * @private
     */
    known_msd() {
        let length;
        if(this.max_appr >= 0) {
            length = bitLength(this.max_appr);
        } else {
            length = bitLength(-this.max_appr);
        }
        return this.min_prec + length - 1;
    }

    /** Return ths position of the most significant digit (msd) - this version may return `Number.MIN_SAFE_INTEGER` if the correct answer is `< n`.
     *
     * If `n` is not given, it uses `iter_msd` to eventually return a correct answer, unless the constructive real is zero, when it either loops forever or throws a precision overflow error.
     * 
     * @param {integer} n
     * @returns {integer}
     */
    msd(n) {
        if(n===undefined) {
            return this.iter_msd(Number.MIN_SAFE_INTEGER);
        }
        if(!this.appr_valid || this.max_appr <= 1n && this.max_appr >= -1n) {
            this.get_appr(n-1);
            if(bigIntMath.abs(this.max_appr) <= 1n) {
                // msg could still be arbitrarily far to the right.
                return Number.MIN_SAFE_INTEGER;
            }
        }
        return this.known_msd();
    }

    /** Functionally equivalent to `msd`, but iteratively evaluates to higher precision.
     *
     * @param {integer} n
     * @returns {integer}
     * @private
     */
    iter_msd(n) {
        let prec = 0;
        for(let prec = 0; prec > n+30; prec = (prec*3)/2 - 16) {
            let msd = this.msd(prec);
            if(msd != Number.MIN_SAFE_INTEGER) {
                return msd;
            }
            CReal.check_prec(prec);
        }
        return this.msd(n);
    }

    /** A helper function for `CReal.toString`.
     * Generate a string containing n zeros.
     *
     * @param {integer} n
     * @returns {string}
     * @private
     */
    static zeros(n) {
        let a = '';
        for(let i=0;i<n;i++) {
            a += '0';
        }
        return a;
    }

    /** Natural logarithm.
     * @returns {CReal}
     * @private
     */
    simple_ln() {
        return new prescaled_ln_CReal(this.subtract(CReal.ONE));
    }

    /** Atan of integer reciprocal: atan(1/n).
     *
     * @param {integer} n
     * @returns {CReal}
     */
    static atan_reciprocal(n) {
        return new integral_atan_CReal(n);
    }

    /**
    * Return `0` if `x = y` to within the indicated tolerance,
    * `-1` if `x < y`, and `+1` if `x > y`.  If `x` and `y` are indeed
    * equal, it is guaranteed that `0` will be returned.  If
    * they differ by less than the tolerance, anything
    * may happen.  The tolerance allowed is
    * the maximum of `(abs(this)+abs(x))*(2**r)` and `2**a`
    *
    * @param {CReal} x - The other constructive real.
    * @param {integer} r - Relative tolerance in bits.
    * @param {integer} a - Absolute tolerance in bits.
    */
    compareTo(x) {
        let r;
        let a;

        if(arguments.length==3) {
            r = arguments[1];
            a = arguments[2];
            const this_msd = this.iter_msd(a);
            const x_msd = x.iter_msd(this_msd > a ? this_msd : a);
            const max_msd = (x_msd > this_msd ? x_msd : this_msd);
            if(max_msd === Number.MIN_SAFE_INTEGER) {
                return 0;
            }
            CReal.check_prec(r);
            const rel = max_msd + r;
            const abs_prec = rel > a ? rel : a;
            return this.compareTo(x, abs_prec);
        } else if(arguments.length==2) {
            a = arguments[1];
            const needed_prec = a - 1;
            const this_appr = this.get_appr(needed_prec);
            const x_appr = x.get_appr(needed_prec);
            if(this_appr > (x_appr+1n)) {
                return 1;
            } else if(this_appr < x_appr - 1n){ 
                return -1;
            } else {
                return 0;
            }
        } else {
            for(a = -20; ; a *= 2) {
                CReal.check_prec(a);
                let result = CReal.compareTo(x,a);
                if(result != 0) {
                    return result;
                }
            }
        }
    }

    /** Return `-1` if negative, `+1` if positive.
     * Should be called only if `this != 0`.
     * In the 0 case, this will not terminate correctly; typically it will run until it exhausts memory.
     * If the two constructive reals may be equal, the one or two argument version of signum should be used.
     *
     * @param {integer} [a]
     * @returns {integer}
     */
    signum(a) {
        if(a===undefined) {
            for(a = -20; ; a *= 2) {
                CReal.check_prec(a);
                const result = this.signum(a);
                if(result != 0) {
                    return result;
                }
            }
        } else {
            if(this.appr_valid) {
                if(this.max_appr != 0n) {
                    return this.max_appr > 0 ? 1 : -1;
                }
            }
            const needed_prec = a - 1;
            const this_appr = this.get_appr(needed_prec);
            return this_appr.signum();
        }
    }

    /** Return a textual representation accurate to `n` places to the right of the decimal point.
     * 
     * @param {integer} [n=10] - must be non-negative.
     * @param {integer} [radix=10] - between 2 and 16, inclusive.
     * @example
     * // returns "3.141"
     * CReal.PI.toString(3)
     * @returns {string}
     */
    toString(n, radix) {
        if(n === undefined) {
            n = 10;
        }
        if(radix === undefined) {
            radix = 10;
        }
        let scaled_CReal;
        if(radix == 16) {
            scaled_CReal = this.shiftLeft(4*n);
        } else {
            const scale_factor = BigInt(radix)**BigInt(n);
            scaled_CReal = this.multiply(new int_CReal(scale_factor));
        }
        const scaled_int = scaled_CReal.get_appr(0);
        let scaled_string = bigIntMath.abs(scaled_int).toString(radix);
        let result;
        if(n == 0) {
            result = scaled_string;
        } else {
            let len = scaled_string.length;
            if(len <= n) {
                const z = CReal.zeros(n+1-len);
                scaled_string = z + scaled_string;
                len = n + 1;
            }
            const whole = scaled_string.slice(0,len-n);
            const fraction = scaled_string.slice(len-n);
            result = whole + "." + fraction;
        }
        if(scaled_int < 0n) {
            result = "-" + result;
        }
        return result;
    }

    /** A representation of a number in scientific notation.
     *
     * @typedef StringFloatRep
     * @private
     * @property {integer} sign
     * @property {string} mantissa
     * @property {integer} radix
     * @property {integer} exponent
     */

    /** Return a textual scientific notation representation accurate to `n` places to the right of the decimal point.
     * `n` must be non-negative. 
     * A value smaller than `radix**-m` may be displayed as 0.
     * The `mantissa` component of the result is either "0" or exactly `n` digits long. 
     * The `sign` component is zero exactly when the mantissa is "0".
     *
     * @param {integer} n - Number of digits (>0) included to the right of the decimal point.
     * @param {integer} radix - The base (between 2 and 16) for the resulting representation.
     * @param {integer} m - The precision used to distinguish the number from zero. Expressed as a power of `radix`.
     * @returns {StringFloatRep}
     */
    toStringFloatRep(n, radix, m) {
        if(n <= 0) {
            throw new Error("Bad precision argument");
        }
        const log2_radix = Math.log(radix) / Math.log(2);
        const big_radix = BigInt(radix);
        const msd_prec = Math.round(log2_radix * m);
        if(msd_prec > Number.MAX_SAFE_INTEGER || msd_prec < Number.MIN_SAFE_INTEGER) {
            throw new Error("precision overflow");
        }
        CReal.check_prec(msd_prec);
        const msd = this.iter_msd(msd_prec-2);
        if(msd == Number.MIN_SAFE_INTEGER) {
            return {
                sign: 0,
                mantissa: "0",
                radix: radix,
                exponent: 0
            };
        }
        let exponent = Math.ceil(msd/log2_radix);
        const scale_exp = exponent - n;
        let scale;
        if(scale_exp > 0) {
            scale = CReal.valueOf(big_radix ** BigInt(scale_exp)).inverse();
        } else {
            scale = CReal.valueOf(big_radix ** BigInt(-scale_exp));
        }
        let scaled_res = this.multiply(scale);
        let scaled_int = scaled_res.get_appr(0);
        let sign = bigIntMath.signum(scaled_int);
        let scaled_string = bigIntMath.abs(scaled_int).toString(radix);
        while(scaled_string.length<n) {
            // Exponent was too large. Adjust.
            scaled_res = scaled_res.multiply(CReal.valueOf(big_radix));
            exponent -= 1;
            scaled_int = scaled_res.get_appr(0);
            sign = bigIntMath.signum(scaled_int);
            scaled_string = bigIntMath.abs(scaled_int).toString(radix);
        }
        if(scaled_string.length > n) {
            // Exponent was too small. Adjust by truncating.
            exponent += (scaled_string.length - n);
            scaled_string = scaled_string.slice(0,n);
        }
        return {
            sign,
            mantissa: scaled_string,
            radix,
            exponent
        };
    }

    /** Return a BigInt which differs by less than one from the constructive real.
     *
     * @returns {BigInt}
     */
    BigIntValue() {
        return this.get_appr(0);
    }

    /** Return an integer which differs by less than one from the constructive real.
     * Behaviour on overflow is undefined.
     *
     * @returns {integer}
     */
    intValue() {
        return Number(this.BigIntValue());
    }

    /** Return a float which differs by less than one in the least represented bit from the constructive real.
     *  (We're in fact closer to round-to-nearest than that, but we can't and don't promise correct rounding.)
     *
     *  @returns {number}
     */
    floatValue() {
        const my_msd = this.iter_msd(-1080);
        if(my_msd == Number.MIN_SAFE_INTEGER) {
            return 0;
        }
        const needed_prec = my_msd - 60;
        let scaled_int = Number(this.get_appr(needed_prec));
        return Number(scaled_int) * (2**needed_prec)

        /** Original from CReal.java - I'm not sure whether this could work with JS numbers.
        /*
        const may_underflow = needed_prec < -1000;
        const exp_adj = may_underflow ? needed_prec + 96 : needed_prec;
        const orig_exp = (scaled_int >> 52) & 0x7ff;
        // Original unbiased exponent is > 50. Exp_adj > -1050.
        // Thus the sum must be > the smallest representable exponent
        // of -1023.
        if(orig_exp + exp_adj >= 0x7ff) {
            // Exponent overflowed.
            if(scaled_int < 0) {
                return -Infinity;
            } else {
                return Infinity;
            }
        }
        scaled_int += exp_adj << 52;
        const result = scaled_int;
        if(may_underflow) {
            const two48 = 1 << 48;
            return result/two48/two48;
        } else {
            return result;
        }
        */
    }

    /** Add two constructive reals.
     *
     * @param {CReal} x
     * @returns {CReal}
     */
    add(x) {
        return new add_CReal(this, x);
    }

    /** Multiply a constructive real by `2 ** n`.
     * 
     * @param {integer} n
     * @returns {CReal}
     */
    shiftLeft(n) {
        CReal.check_prec(n);
        return new shifted_CReal(this, n);
    }

    /** Multiply a constructive real by `2 ** -n`.
     *
     * @param {integer} n
     * @returns {CReal}
     */
    shiftRight(n) {
        CReal.check_prec(n);
        return new shifted_CReal(this, -n);
    }

    /** Produce a constructive real equivalent to the original, assuming the original was an integer.
     * Undefined results if the original was not an integer.
     * Prevents evaluation of digits to the right of the decimal point, and may thus improve performance.
     *
     * @returns {CReal}
     */
    assumeInt() {
        return new assumed_int_CReal(this);
    }

    /** The additive inverse of a constructive real.
     *
     * @returns {CReal}
     */
    negate() {
        return new neg_CReal(this);
    }

    /** The difference between two constructive reals.
     *
     * @param {CReal} x
     * @returns {CReal}
     */
    subtract(x) {
        return new add_CReal(this, x.negate());
    }

    /** The produce of two constructive reals.
     *
     * @param {CReal} x
     * @returns {CReal}
     */
    multiply(x) {
        return new mult_CReal(this, x);
    }

    /** The multiplicative inverse of a constructive real.
     *  `x.inverse()` is equivalent to `CReal.valueOf(1).divide(x)`.
     *
     *  @returns {CReal}
     */
    inverse() {
        return new inv_CReal(this);
    }

    /** The quotient of two constructive reals.
     * 
     * @param {CReal} x
     * @returns {CReal}
     */
    divide(x) {
        return new mult_CReal(this, x.inverse());
    }

    /** The real number `x` if `this < 0`, or `y` otherwise.
     *  Requires `x = y` if `this = 0`.
     *  Since comparisons may diverge, this is often a useful alternative to conditionals.
     *
     *  @param {CReal} x
     *  @param {CReal} y
     *  @returns {CReal}
     */
    select(x, y) {
        return new select_CReal(this, x, y);
    }

    /** The maximum of two constructive reals.
     *
     * @param {CReal} x
     * @returns {CReal}
     */
    max(x) {
        return this.subtract(x).select(x, this);
    }

    /** The minimum of two constructive reals.
     *
     * @param {CReal} x
     * @returns {CReal}
     */
    min(x) {
        return this.subtract(x).select(this, x);
    }

    /** The absolute value of a constructive real.
     *  Note that this cannot be written as a conditional.
     *
     *  @returns {CReal}
     */
    abs() {
        return this.select(this.negate(), this);
    }

    /** The exponential function, that is `e**this`.
     *
     * @returns {CReal}
     */
    exp() {
        const low_prec = -10;
        const rough_appr = this.get_appr(low_prec);
        // Handle negative arguments directly; negating and computing inverse
        // can be very expensive.
        if(rough_appr > 2n || rough_appr < -2n) {
            const square_root = this.shiftRight(1).exp();
            return square_root.multiply(square_root);
        } else {
            return new prescaled_exp_CReal(this);
        }
    }

    /** To the power of x.
     *
     * @param {CReal} x
     * @returns {CReal}
     */
    pow(x) {
        return x.multiply(this.ln()).exp();
    }

    /** The trigonometric cosine function.
     *
     * @returns {CReal}
     */
    cos() {
        const halfpi_multiples = this.divide(CReal.PI).get_appr(-1);
        const abs_halfpi_multiples = bigIntMath.abs(halfpi_multiples);
        if(abs_halfpi_multiples >= 2n) {
            // Subtract multiples of PI
            const pi_multiples = CReal.scale(halfpi_multiples, -1)
            const adjustment = CReal.PI.multiply(CReal.valueOf(pi_multiples));
            if((pi_multiples & 1n) != 0n) {
                return this.subtract(adjustment).cos().negate();
            } else {
                return this.subtract(adjustment).cos();
            }
        } else if(bigIntMath.abs(this.get_appr(-1)) >= 2n) {
            // Scale further with double angle formula
            const cos_half = this.shiftRight(1).cos();
            return cos_half.multiply(cos_half).shiftLeft(1).subtract(CReal.ONE);
        } else {
            return new prescaled_cos_CReal(this);
        }
    }

    /** The trigonometric sine function.
     * 
     * @returns {CReal}
     */
    sin() {
        return CReal.half_pi.subtract(this).cos();
    }

    /** The trigonometric arc (inverse) sine function.
     *
     * @returns {CReal}
     */
    asin() {
        const rough_appr = this.get_appr(-10);
        if(rough_appr > 750n) { // 1/sqrt(2) + a bit
            const new_arg = CReal.ONE.subtract(this.multiply(this)).sqrt();
            return new_arg.acos();
        } else if(rough_appr < -750n) {
            return this.negate().asin().negate();
        } else {
            return new prescaled_asin_CReal(this);
        }
    }

    /** The trigonometric arc (incerse) cosine function.
     *
     * @returns {CReal}
     */
    acos() {
        return CReal.half_pi.subtract(this.asin());
    }

    /** The natural (base e) logarithm.
     *
     * @returns {CReal}
     */
    ln() {
        const low_prec = -4;
        const rough_appr = this.get_appr(low_prec); // In sixteenths
        if(rough_appr < 0n) {
            throw new Error("ln(negative)");
        }
        if(rough_appr <= CReal.low_ln_limit) {
            return this.inverse().ln().negate();
        }
        if(rough_appr >= CReal.high_ln_limit) {
            if(rough_appr <= CReal.scaled_4) {
                const quarter = this.sqrt().sqrt().ln();
                return quarter.shiftLeft(2);
            } else {
                const extra_bits = bitLength(rough_appr) - 3;
                const scaled_result = this.shiftRight(extra_bits).ln();
                return scaled_result.add(CReal.valueOf(extra_bits).multiply(CReal.ln2));
            }
        }
        return this.simple_ln();
    }

    /** The square root of a constructive real.
     *
     * @returns {CReal}
     */
    sqrt() {
        return new sqrt_CReal(this);
    }
}

/** A specialization of CReal for cases in which approximate() calls
 *  to increase evaluation precision are somewhat expensive.
 *  If we need to (re)evaluate, we speculatively evaluate to slightly
 *  higher precision, miminimizing reevaluations.
 *  Note that this requires any arguments to be evaluated to higher
 *  precision than absolutely necessary.  It can thus potentially
 *  result in lots of wasted effort, and should be used judiciously.
 *  This assumes that the order of magnitude of the number is roughly one.
 *
 *  @private
 */
class slow_CReal  extends CReal {
    static max_prec = -64;
    static prec_incr = 32;

    get_appr(precision) {
        CReal.check_prec(precision);
        if(this.appr_valid && precision >= this.min_prec) {
            return CReal.scale(this.max_appr, this.min_prec - precision);
        } else {
            const eval_prec = precision >= this.max_prec ? this.max_prec : (precision - slow_CReal.prec_incr + 1) & ~(slow_CReal.prec_incr - 1);
            const result = this.approximate(eval_prec);
            this.min_prec = eval_prec;
            this.max_appr = result;
            this.appr_valid = true;
            return CReal.scale(result, eval_prec - precision);
        }
    }   
}

/** Representation of an integer constant.
 *
 *  @private
 */
class int_CReal extends CReal {
    /**
     * @param {BigInt} n
     */
    constructor(n) {
        super();
        this.value = n;
    }

    approximate(p) {
        return CReal.scale(this.value, -p);
    }
}

/** Representation of a number that may not have been completely
 *  evaluated, but is assumed to be an integer.  Hence we never
 *  evaluate beyond the decimal point.
 *
 *  @private
 */
class assumed_int_CReal extends CReal {
    /* @param {CReal} x
     */
    constructor(x) {
        super();
        this.value = x;
    }

    approximate(p) {
        if(p >= 0) {
            return this.value.get_appr(p);
        } else {
            return CReal.scale(this.value.get_appr(0), -p);
        }
    }
}

/** Representation of the sum of two constructive reals.
 * 
 *  @private
 */
class add_CReal extends CReal {
    /**
     * @param {CReal} x
     * @param {CReal} y
     */
    constructor(x, y) {
        super();
        this.op1 = x;
        this.op2 = y;
    }

    approximate(p) {
        return CReal.scale(this.op1.get_appr(p-2) + this.op2.get_appr(p-2), -2);
    }
}

/** Representation of a CReal multiplied by 2**n.
 *
 *  @private
 */
class shifted_CReal extends CReal {
    /**
     * @param {CReal} x
     * @param {integer} n
     */
    constructor(x, n) {
        super();
        this.op = x;
        this.count = n;
    }

    approximate(p) {
        return this.op.get_appr(p - this.count);
    }
}

/** Representation of the negation of a CReal.
 *
 *  @private
 */
class neg_CReal extends CReal {
    /**
     * @param {CReal} x
     */
    constructor(x) {
        super();
        this.op = x;
    }

    approximate(p) {
        return -this.op.get_appr(p);
    }
}

/** Representation of:
 *      op1     if selector < 0
 *      op2     if selector >= 0
 *  Assumes x = y if s = 0.
 *
 *  @private
 */
class select_CReal extends CReal {
    /**
     * @param {CReal} s
     * @param {CReal} x
     * @param {CReal} y
     */
    constructor(s, x, y) {
        super();
        this.selector = s;
        this.selector_sign = bigIntMath.signum(this.selector.get_appr(-20));
        this.op1 = x;
        this.op2 = y;
    }

    approximate(p) {
        if(this.selector_sign < 0) { 
            return this.op1.get_appr(p);
        } else if(this.selector_sign > 0) {
            return this.op2.get_appr(p);
        }
        const op1_appr = this.op1.get_appr(p-1);
        const op2_appr = this.op2.get_appr(p-1);
        const diff = bigIntMath.abs(op1_appr - op2_appr);
        if(diff <= 1n) {
            return CReal.scale(op1_appr, -1);
        }
        /** op1 and op2 are different; selector != 0.
         * Safe to get sign of selector.
         */
        if(this.selector.signum() < 0) {
            this.selector_sign = -1;
            return CReal.scale(op1_appr, -1);
        } else {
            this.selector_sign = 0;
            return CReal.scale(op2_appr, -1);
        }
    }
}

/** Representation of the product of two constructive reals. Private.
 *
 *  @private
 */
class mult_CReal extends CReal {
    /**
     * @param {CReal} x
     * @param {CReal} y
     */
    constructor(x, y) {
        super();
        this.op1 = x;
        this.op2 = y;
    }

    approximate(p) {
        const half_prec = (p >> 1) - 1;
        let msd_op1 = this.op1.msd(half_prec);
        let msd_op2;
        if(msd_op1 == Number.MIN_SAFE_INTEGER) {
            msd_op2 = this.op2.msd(half_prec);
            if(msd_op2 == Number.MIN_SAFE_INTEGER) {
                // Product is small enough that zero will do as an approximation.
                return 0n;
            } else {
                // Swap them, so the larger operand (in absolute value) is first.
                let tmp = this.op1;
                this.op1 = this.op2;
                this.op2 = tmp;
                msd_op1 = msd_op2;
            }
        }
        // msd_op1 is valid at this point.
        
        // Precision needed for op2.
        // The appr. error is multiplied by at most
        // 2 ** (msd_op1 + 1)
        // Thus each approximation contributes 1/4 ulp
        // to the rounding error, and the final rounding adds
        // another 1/2 ulp.
        const prec2 = p - msd_op1 - 3;

        const appr2 = this.op2.get_appr(prec2);
        if(appr2 == 0n) {
            return 0n;
        }
        msd_op2 = this.op2.known_msd();
        const prec1 = p - msd_op2 - 3;
        const appr1 = this.op1.get_appr(prec1);
        const scale_digits = prec1 + prec2 - p;
        return CReal.scale(appr1 * appr2, scale_digits);
    }
}

/** Representation of the multiplicative inverse of a constructive real.
 * Should use Newton iteration to refine estimates.
 *
 *  @private
 */
class inv_CReal extends CReal {
    /**
     * @param {CReal} x
     */
    constructor(x) {
        super();
        this.op = x;
    }

    approximate(p) {
        const msd = this.op.msd();
        const inv_msd = 1 - msd;
        const digits_needed = inv_msd - p + 3;
                                // Number of SIGNIFICANT digits needed for
                                // argument, excl. msd position, which may
                                // be fictitious, since msd routine can be
                                // off by 1.  Roughly 1 extra digit is
                                // needed since the relative error is the
                                // same in the argument and result, but
                                // this isn't quite the same as the number
                                // of significant digits.  Another digit
                                // is needed to compensate for slop in the
                                // calculation.
                                // One further bit is required, since the
                                // final rounding introduces a 0.5 ulp
                                // error.
        const prec_needed = msd - digits_needed;
        const log_scale_factor = -p - prec_needed;
        if(log_scale_factor < 0) {
            return 0n;
        }
        const dividend = 1n << BigInt(log_scale_factor);
        const scaled_divisor = this.op.get_appr(prec_needed);
        const abs_scaled_divisor = bigIntMath.abs(scaled_divisor);
        const adj_dividend = dividend + (abs_scaled_divisor >> 1n);
        const result = adj_dividend / abs_scaled_divisor;
        if(scaled_divisor < 0) {
            return -result;
        } else {
            return result;
        }
    }
}

/** Representation of the exponential of a constructive real.
 *  Uses a Taylor series expansion.  Assumes |x| < 1/2.
 *  Note: this is known to be a bad algorithm for
 *  floating point.  Unfortunately, other alternatives
 *  appear to require precomputed information.
 *
 *  @private
 */
class prescaled_exp_CReal extends CReal {
    /**
     * @param {CReal} x
     */
    constructor(x) {
        super();
        this.op = x;
    }

    approximate(p) {
        if(p >= 1) {
            return 0n;
        }
        const iterations_needed = Math.floor(-p/2 + 2);
        const calc_precision = p - CReal.bound_log2(2*iterations_needed);
        const op_prec = p - 3;
        const op_appr = this.op.get_appr(op_prec);
        const scaled_1 = 1n << (-BigInt(calc_precision));
        let current_term = scaled_1;
        let current_sum = scaled_1;
        let n = 0n;
        const max_trunc_error = 1n << BigInt(p - 4 - calc_precision);
        while(bigIntMath.abs(current_term) >= max_trunc_error) {
            n += 1n;
            current_term = CReal.scale(current_term * op_appr, op_prec) / n;
            current_sum += current_term;
        }
        return CReal.scale(current_sum, calc_precision - p);
    }
}

/** Representation of the cosine of a constructive real.
 *  Uses a Taylor series expansion.
 *  Assumex |x| < 1.
 *
 *  @private
 */
class prescaled_cos_CReal extends slow_CReal {
    /**
     * @param {CReal} x
     */
    constructor(x) {
        super();
        this.op = x;
    }

    approximate(p) {
        if(p >= 1) {
            return 0n;
        }
        const iterations_needed = Math.floor(-p/2 + 4);
            // conservative estimate > 0.
            //  Claim: each intermediate term is accurate
            //  to 2*2^calc_precision.
            //  Total rounding error in series computation is
            //  2*iterations_needed*2^calc_precision,
            //  exclusive of error in op.
        const calc_precision = p - CReal.bound_log2(2*iterations_needed) - 4;
        const op_prec = p - 2;
        const op_appr = this.op.get_appr(op_prec);
        const max_trunc_error = 1n << BigInt(p - 4 - calc_precision);
        let n = 0n;
        let current_term = 1n << BigInt(-calc_precision);
        let current_sum = current_term;
        while(bigIntMath.abs(current_term) >= max_trunc_error) {
            n += 2n;
            current_term = CReal.scale(current_term * op_appr, op_prec);
            current_term = CReal.scale(current_term * op_appr, op_prec);
            const divisor = -n * (n-1n);
            current_term = current_term / divisor;
            current_sum = current_sum + current_term;
        }
        return CReal.scale(current_sum, calc_precision - p);
    }
}

/** The constructive real atan(1/n), where n is a small integer > base.
 *  This gives a simple and moderately fast way to compute PI.
 *
 *  @private
 */
class integral_atan_CReal extends slow_CReal {
    /**
     * @param {integer} x
     */
    constructor(x) {
        super();
        this.op = x;
    }

    approximate(p) {
        if(p >= 1) {
            return 0n;
        }
        const iterations_needed = Math.floor(-p/2 + 2);
          //  Claim: each intermediate term is accurate
          //  to 2*base^calc_precision.
          //  Total rounding error in series computation is
          //  2*iterations_needed*base^calc_precision,
          //  exclusive of error in op.
        const calc_precision = p - CReal.bound_log2(2*iterations_needed) - 2;
          // Error in argument results in error of < 3/8 ulp.
          // Cumulative arithmetic rounding error is < 1/4 ulp.
          // Series truncation error < 1/4 ulp.
          // Final rounding error is <= 1/2 ulp.
          // Thus final error is < 1 ulp.
        const scaled_1 = 1n << BigInt(-calc_precision);
        const big_op = BigInt(this.op);
        const big_op_squared = big_op * big_op;
        const op_inverse = scaled_1 / big_op;
        let current_power = op_inverse;
        let current_term = op_inverse;
        let current_sum = op_inverse;
        let current_sign = 1n;
        let n = 1n;
        let max_trunc_error = 1n << BigInt(p - 2 - calc_precision);
        while(bigIntMath.abs(current_term) >= max_trunc_error) {
            n += 2n;
            current_power = current_power / big_op_squared;
            current_sign = -current_sign;
            current_term = current_power / (current_sign*n);
            current_sum += current_term;
        }
        return CReal.scale(current_sum, calc_precision - p);
    }
}

/** Representation for ln(1 + op)
 *
 *  @private
 */
class prescaled_ln_CReal extends slow_CReal {
    /**
     * @param {CReal} x
     */
    constructor(x) {
        super();
        this.op = x;
    }

    /** Compute an approximation of ln(1+x) to precision
     *  prec. This assumes |x| < 1/2.
     *  It uses a Taylor series expansion.
     *  Unfortunately there appears to be no way to take
     *  advantage of old information.
     *  Note: this is known to be a bad algorithm for
     *  floating point.  Unfortunately, other alternatives
     *  appear to require precomputed tabular information.
     */
    approximate(p) {
        if(p >= 0) {
            return 0n;
        }
        const iterations_needed = -p;
          //  Claim: each intermediate term is accurate
          //  to 2*2^calc_precision.  Total error is
          //  2*iterations_needed*2^calc_precision
          //  exclusive of error in op.
        const calc_precision = p - CReal.bound_log2(2*iterations_needed);
        const op_prec = p - 3;
        const op_appr = this.op.get_appr(op_prec);
        let x_nth = CReal.scale(op_appr, op_prec - calc_precision);
        let current_term = x_nth;
        let current_sum = current_term;
        let n = 1n;
        let current_sign = 1n;
        const max_trunc_error = 1n << BigInt(p - 4 - calc_precision);
        while(bigIntMath.abs(current_term) >= max_trunc_error) {
            n += 1n;
            current_sign = -current_sign;
            x_nth = CReal.scale(x_nth * op_appr, op_prec);
            current_term = x_nth / (n*current_sign);
            current_sum += current_term;
        }
        return CReal.scale(current_sum, calc_precision - p);
    }
}

/** Representation of the arcsine of a constructive real.
 *  Uses a Taylor series expansion. Assumes |x| < (1/2)^(1/3).
 *
 *  @private
 */
class prescaled_asin_CReal extends slow_CReal {
    /**
     * @param {CReal} x
     */
    constructor(x) {
        super();
        this.op = x;
    }

    approximate(p) {
        /** The Taylor series is the sum of x^(2n+1) * (2n)!/(4^n n!^2 (2n+1))
         *  Note that (2n)!/(4^n n!^2) is always less than one.
         *  (The denominator is effectively 2n*2n*(2n-2)*(2n-2)*...*2*2
         *  which is clearly > (2n)!)
         *  Thus all terms are bounded by x^(2n+1).
         *  Unfortunately, there's no easy way to prescale the argument
         *  to less than 1/sqrt(2), and we can only approximate that.
         *  Thus the worst case iteration count is fairly high.
         *  But it doesn't make much difference.
         */
        if(p >= 2) {
            return 0n;
        }
        const iterations_needed = -3 * p / 2 + 4;
                                // conservative estimate > 0.
                                // Follows from assumed bound on x and
                                // the fact that only every other Taylor
                                // Series term is present.
          //  Claim: each intermediate term is accurate
          //  to 2*2^calc_precision.
          //  Total rounding error in series computation is
          //  2*iterations_needed*2^calc_precision,
          //  exclusive of error in op.
        const calc_precision = p - CReal.bound_log2(2*iterations_needed);
        const op_prec = p - 3;
        const op_appr = this.op.get_appr(op_prec);
          // Error in argument results in error of < 1/4 ulp.
          // (Derivative is bounded by 2 in the specified range and we use
          // 3 extra digits.)
          // Ignoring the argument error, each term has an error of
          // < 3ulps relative to calc_precision, which is more precise than p.
          // Cumulative arithmetic rounding error is < 3/16 ulp (relative to p).
          // Series truncation error < 2/16 ulp.  (Each computed term
          // is at most 2/3 of last one, so some of remaining series <
          // 3/2 * current term.)
          // Final rounding error is <= 1/2 ulp.
          // Thus final error is < 1 ulp (relative to p).
        const max_last_term = 1n << BigInt(p - 4 - calc_precision);
        let exp = 1n;
        let current_term = op_appr << BigInt(op_prec - calc_precision);
        let current_sum = current_term;
        let current_factor = current_term;
                            // Current scaled Taylor series term
                            // before division by the exponent.
                            // Accurate to 3 ulp at calc_precision.
        while(bigIntMath.abs(current_term) >= max_last_term) {
            exp += 2n;
            // current_factor = current_factor * op * op * (exp-1) * (exp-2) /
            // (exp-1) * (exp-1), with the two exp-1 factors cancelling,
            // giving
            // current_factor = current_factor * op * op * (exp-2) / (exp-1)
            // Thus the error any in the previous term is multiplied by
            // op^2, adding an error of < (1/2)^(2/3) < 2/3 the original
            // error.
            current_factor = current_factor * (exp - 2n);
            current_factor = CReal.scale(current_factor * op_appr, op_prec + 2);
                // Carry 2 extra bits of precision forward; thus
                // this effectively introduces 1/8 ulp error.
            current_factor *= op_appr;
            const divisor = exp - 1n;
            current_factor /= divisor;
                // Another 1/4 ulp error here.
            current_factor = CReal.scale(current_factor, op_prec - 2);
                // Remove extra 2 bits.  1/2 ulp rounding error.
            // Current_factor has original 3 ulp rounding error, which we
            // reduced by 1, plus < 1 ulp new rounding error.
            current_term = current_factor / exp;
                // Contributes 1 ulp error to sum plus at most 3 ulp
                // from current_factor.
            current_sum += current_term;
        }
        return CReal.scale(current_sum, calc_precision - p);
    }
}

/** Representation of the square root of a constructive real.
 *
 *  @private
 */
class sqrt_CReal extends CReal {
    /**  Explicitly provide an initial approximation.
     *  Useful for arithmetic geometric mean algorithms, where we've previously
     *  computed a very similar square root.
     *
     *  @param {CReal} x
     *  @param {int} min_p
     *  @param {BigInt} max_a
     */
    constructor(x, min_p, max_a) {
        super();
        this.op = x;
        if(min_p !== undefined) {
            this.min_prec = min_p;
            this.max_appr = max_a;
            this.appr_valid = true;
        }
    }

    static fp_prec = 50;
        // Conservative estimate of number of
        // significant bits in double precision
        // computation.
    static fp_op_prec = 60;

    approximate(p) {
        const max_op_prec_needed = 2*p - 1;
        const msd = this.op.iter_msd(max_op_prec_needed);
        if(msd <= max_op_prec_needed) {
            return 0n;
        }
        const result_msd = Math.floor(msd/2);   // ± 1
        const result_digits = result_msd - p;   // ± 2
        if(result_digits > sqrt_CReal.fp_prec) {
            // Compute less precise approximation and use a Newton iter.
            const appr_digits = Math.floor(result_digits/2) + 6;
            const appr_prec = result_msd - appr_digits;
            const prod_prec = 2*appr_prec;
            // First compute the argument to maximal precision, so we don't end up
            // reevaluating it incrementally.
            const op_appr = this.op.get_appr(prod_prec);
            const last_appr = this.get_appr(appr_prec);
            // Compute (last_appr * last_appr + op_appr) / last_appr / 2
            // while adjusting the scaling to make everything work
            const prod_prec_scaled_numerator = last_appr * last_appr + op_appr;
            const scaled_numerator = CReal.scale(prod_prec_scaled_numerator, appr_prec - p);
            const shifted_result = scaled_numerator / last_appr;
            return (shifted_result + 1n) >> 1n;
        } else {
            // Use a double precision floating point approximation.
            // Make sure all precisions are even
            const op_prec = (msd - sqrt_CReal.fp_op_prec) & ~1;
            const working_prec = op_prec - sqrt_CReal.fp_op_prec;
            const scaled_bi_appr = this.op.get_appr(op_prec) << BigInt(sqrt_CReal.fp_op_prec);
            const scaled_appr = Number(scaled_bi_appr);
            if(scaled_appr < 0) {
                throw new Error("sqrt(negative)");
            }
            const scaled_fp_sqrt = Math.sqrt(scaled_appr);
            const scaled_sqrt = BigInt(scaled_fp_sqrt);
            const shift_count = Math.floor(working_prec/2) - p;
            return CReal.shift(scaled_sqrt, shift_count);
        }
    }
}


/** Return the constructive real number corresponding to the given textual representation.
 *
 * @private
 * @param {string} s
 */
CReal.valueOf_string = function(s) {
    s = s.trim();
    const len = s.length;
    let start_pos = 0;
    let point_pos;
    let fraction;
    point_pos = s.indexOf('.');
    if(point_pos == -1) {
        point_pos = len;
        fraction = '0';
    } else {
        fraction = s.slice(point_pos+1);
    }
    const whole = s.slice(0,point_pos);
    const scaled_result = BigInt(whole+fraction);
    const divisor = 10n ** BigInt(fraction.length);
    return CReal.valueOf(scaled_result).divide(CReal.valueOf(divisor));
}

/** Construct a CReal from a number or string.
 *
 * @param {number|string} n
 * @returns {CReal}
 */
CReal.valueOf = function(n) {
    if(typeof n == 'number' && isNaN(n)) {
        throw(new Error("NaN argument"));
    }
    if(n == Infinity || n == -Infinity) {
        throw(new Error("Infinite argument"));
    }

    if(typeof n == 'bigint') {
        return new int_CReal(n);
    } else if(typeof n == 'string' || n%1 != 0) {
        return CReal.valueOf_string(n+'');
    } else {
        return new int_CReal(BigInt(n));
    }

    /** Copied from the Java BigInteger version - is this needed in JS?
     */
    /*
    const negative = n < 0;
    const bits = Math.abs(n);
    let mantissa = bits & 0xfffffffffffff;
    const biased_exp = bits >> 52;
    const exp = biased_exp - 1075;
    if(biased_exp != 0) {
        mantissa += (1 << 52);
    } else {
        mantissa <<= 1;
    }
    let result = CReal.valueOf(BigInt(mantissa)).shiftLeft(exp);
    if(negative) {
        result = result.negate();
    }
    return result;
    */
}

/** Zero.
 * @type {CReal}
 */
CReal.ZERO = CReal.valueOf(0);

/** One.
 * @type {CReal}
 */
CReal.ONE = CReal.valueOf(1);

/** Natural log of 2.
 *
 * `ln(2) = 7*ln(10/9) - 2*ln(25/24) + 3*ln(81/80)`.
 *
 * @type {CReal}
 */
CReal.ln2 = 
    CReal.valueOf(7)
        .multiply(CReal.valueOf(10).divide(CReal.valueOf(9)).simple_ln())
        .subtract(CReal.valueOf(2).multiply(CReal.valueOf(25).divide(CReal.valueOf(24)).simple_ln()))
        .add(CReal.valueOf(3).multiply(CReal.valueOf(81).divide(CReal.valueOf(80)).simple_ln()))
;

CReal.four = CReal.valueOf(4);


/** The constant PI, computed using the Gauss-Legendre alternating
 *  arithmetic-geometric mean algorithm:
 *       a[0] = 1
 *       b[0] = 1/sqrt(2)
 *       t[0] = 1/4
 *       p[0] = 1
 * 
 *       a[n+1] = (a[n] + b[n])/2        (arithmetic mean, between 0.8 and 1)
 *       b[n+1] = sqrt(a[n] * b[n])      (geometric mean, between 0.7 and 1)
 *       t[n+1] = t[n] - (2^n)(a[n]-a[n+1])^2,  (always between 0.2 and 0.25)
 * 
 *       pi is then approximated as (a[n+1]+b[n+1])^2 / 4*t[n+1].
 *
 *  @private
 */
class gl_pi_CReal extends slow_CReal {
    // In addition to the best approximation kept by the CReal base class, we keep
    // the entire sequence b[n], to the extent we've needed it so far.  Each
    // reevaluation leads to slightly different sqrt arguments, but the
    // previous result can be used to avoid repeating low precision Newton
    // iterations for the sqrt approximation.
    constructor() {
        super();
        this.b_prec = [null];
        this.b_val = [null];
    }

    static TOLERANCE = 4n;
    static SQRT_HALF = new sqrt_CReal(CReal.ONE.shiftRight(1));

    approximate(p) {
        const b_prec = this.b_prec;
        const b_val = this.b_val;

        // Rough approximations are easy.
        if(p >= 0) {
            return CReal.scale(3n, -p);
        }
        // We need roughly log2(p) iterations.  Each iteration should
        // contribute no more than 2 ulps to the error in the corresponding
        // term (a[n], b[n], or t[n]).  Thus 2log2(n) bits plus a few for the
        // final calulation and rounding suffice.
        const extra_eval_prec = Math.ceil(Math.log(-p) / Math.log(2)) + 10;
        // All our terms are implicitly scaled by eval_prec.
        const eval_prec = p - extra_eval_prec;
        let a = 1n << BigInt(-eval_prec);
        let b = gl_pi_CReal.SQRT_HALF.get_appr(eval_prec);
        let t = 1n << BigInt(-eval_prec - 2);
        let n = 0;
        while(a - b - gl_pi_CReal.TOLERANCE > 0) {
            // Current values correspond to n, next_ values to n + 1
            // b_prec.size() == b_val.size() >= n + 1
            const next_a = (a + b) >> 1n;
            let next_b;
            const a_diff = a - next_a;
            const b_prod = (a * b) >> BigInt(-eval_prec);
            // We compute square root approximations using a nested
            // temporary CReal computation, to avoid implementing BigInteger
            // square roots separately.
            const b_prod_as_CReal = CReal.valueOf(b_prod).shiftRight(-eval_prec);
            if(b_prec.length == n + 1) {
                // Add an n+1st slot.
                // Take care to make this exception-safe; b_prec and b_val
                // must remain consistent, even if we are interrupted, or run
                // out of memory. It's OK to just push on b_prec in that case.
                const next_b_as_CReal = b_prod_as_CReal.sqrt();
                next_b = next_b_as_CReal.get_appr(eval_prec);
                const scaled_next_b = CReal.scale(next_b, -extra_eval_prec);
                b_prec.push(p);
                b_val.push(scaled_next_b);
            } else {
                // Reuse previous approximation to reduce sqrt iterations,
                // hopefully to one.
                const next_b_as_CReal = new sqrt_CReal(b_prod_as_CReal, b_prec[n+1], b_val[n+1]);
                next_b = next_b_as_CReal.get_appr(eval_prec);
                // We assume that set() doesn't throw for any reason.
                b_prec[n+1] = p;
                b_val[n+1] = CReal.scale(next_b, -extra_eval_prec);
            }
            // b_prec.length == b_val.length >= n + 2
            const next_t = t - ((a_diff*a_diff) << BigInt(n + eval_prec));
            a = next_a;
            b = next_b;
            t = next_t;
            n += 1;
        }
        const sum = a + b;
        const result = (sum * sum / t) >> 2n;
        return CReal.scale(result, -extra_eval_prec);
    }
}

/** The ratio of a circle's circumference to its diameter.
 * @type {CReal}
 */
CReal.PI = new gl_pi_CReal();

/** The base of the natural logarithm.
 * @type {CReal}
 */
CReal.E = CReal.ONE.exp();

CReal.half_pi = CReal.PI.shiftRight(1);

CReal.low_ln_limit = 8n;   // sixteenths, i.e. 1/2
CReal.high_ln_limit = 16n + 8n;    // 1.5
CReal.scaled_4 = 4n * 16n;

export default CReal;
