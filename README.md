# CReal.js

Constructive real numbers for JavaScript.

Ported from Hans Boehm's Java implementation to JavaScript by Christian Lawson-Perfect.

Each recursive real number is represented as an object that provides an approximation function for the real number.
The approximation function guarantees that the generated approximation is accurate to the specified precision.
Arithmetic operations on constructive reals produce new such objects; they typically do not perform any real computation.
In this sense, arithmetic computations are exact: They produce a description which describes the exact answer, and can be used to later approximate it to arbitrary precision.

When approximations are generated, e.g. for output, they are accurate to the requested precision; no cumulative rounding errors are visible.
In order to achieve this precision, the approximation function will often need to approximate subexpressions to greater precision than was originally demanded.
Thus the approximation of a constructive real number generated through a complex sequence of operations may eventually require evaluation to very high precision.
This usually makes such computations prohibitively expensive for large numerical problems.
But it is perfectly appropriate for use in a desk calculator, for small numerical problems, for the evaluation of expressions computated by a symbolic algebra system, for testing of accuracy claims for floating point code on small inputs, or the like.

## Usage

`creal.js` is an ECMAScript module.
It exposes an object with a `valueOf` method to create a CReal from an ES number or string, and some constants.


```
import CReal from 'creal.js';

const one_seventh = CReal.valueOf(7).inverse();
console.log(one_seventh.toString(20));
```

See [docs.html](docs.html) for full documentation.
