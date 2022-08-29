---
title: "Special Functions"
linkTitle: "Special Functions"
weight: 10
date: 2021-03-07
description: >
  Implemented in Common Lisp
---

The library assumes working with 64 bit double-floats. It will
probably work with single-float as well. Whilst we would prefer to
implement the complex domain, the majority of the sources do
not. Tabled below are the special function implementations and their
source. This library has a focus on high accuracy double-float
calculations using the latest algorithms.

| function | source
| -------- | ----
| erf          |  libm
| erfc         |  libm
| inverse-erf  |  Boost
| inverse-erfc |  Boost
| log-gamma    |  libm
| gamma        |  Cephes
| incomplete-gamma | Boost


## Error rates

The following table shows the peak and mean errors using Boost test
data. Tests run on MS Windows 10 with SBCL 2.0.10. Boost results taken
from the Boost [error function,
](https://www.boost.org/doc/libs/1_69_0/libs/math/doc/html/math_toolkit/sf_erf/error_function.html)[inverse
error
function](https://www.boost.org/doc/libs/1_68_0/libs/math/doc/html/math_toolkit/sf_erf/error_inv.html)
and
[log-gamma](https://www.boost.org/doc/libs/1_74_0/libs/math/doc/html/math_toolkit/sf_gamma/lgamma.html)
pages.

### erf

| Data Set          | Boost (MS C++)                | Special-Functions                |
|------------------ |------------------------------ | -------------------------------- |
| erf small values  | Max = 0.841ε (Mean = 0.0687ε) | Max = 6.10e-5ε (Mean = 4.58e-7ε) |
| erf medium values | Max = 1ε (Mean = 0.119ε)      | Max = 1ε (Mean = 0.003ε)         |
| erf large values  | Max = 0ε (Mean = 0ε)          | N/A erf range 0 < x < 6          |



### erfc

| Data Set           | Boost (MS C++)              | Special-Functions                  |
| ---                | ---                         | ---                                |
| erfc small values  | Max = 0ε (Mean = 0)         | Max = 1ε (Mean = 0.00667ε)         |
| erfc medium values | Max = 1.65ε (Mean = 0.373ε) | Max = 1.71ε (Mean = 0.182ε)        |
| erfc large values  | Max = 1.14ε (Mean = 0.248ε) | Max = 2.31e-15ε (Mean = 8.86e-18ε) |


### inverse-erf/c

| Data Set     | Boost (MS C++)              | Special-Functions        |
| ---          | ---                         | ---                      |
| inverse-erf  | Max = 1.09ε (Mean = 0.502ε) | Max = 2ε (Mean = 0.434ε) |
| inverse-erfc | Max = 1ε (Mean = 0.491ε)    | Max = 2ε (Mean = 0.425ε) |

### log-gamma

| Data Set | Boost (MS C++) | Special-Functions |
| ---      | ---            | ---               |
| factorials | Max = 0.914ε (Mean = 0.175ε) | Max = 2.10ε (Mean = 0.569ε)      |
| near 0     | Max = 0.964ε (Mean = 0.462ε) | Max = 1.93ε (Mean = 0.662ε)      |
| near 1     | Max = 0.867ε (Mean = 0.468ε) | Max = 0.50ε (Mean = 0.0183ε)     |
| near 2     | Max = 0.591ε (Mean = 0.159ε) | Max = 0.0156ε (Mean = 3.83d-4ε)  |
| near -10   | Max = 4.22ε (Mean = 1.33ε)   | Max = 4.83d+5ε (Mean = 3.06d+4ε) |
| near -55   | Max = 0.821ε (Mean = 0.419ε) | Max = 8.16d+4ε (Mean = 4.53d+3ε) |


The results for log gamma are good near 1 and 2, bettering those of
Boost, however are worse (relatively speaking) at values of `x > 8`. I
don't have an explanation for this, since the libm values match Boost
more closely. For example:

```lisp
(spfn:log-gamma -9.99999237060546875d0) = -3.3208925610275326d0
(libm:lgamma    -9.99999237060546875d0) = -3.3208925610151265d0
Boost test answer                         -3.320892561015125097640948165422843317137
```

libm:lgamma provides an additional 4 digits of accuracy over
spfn:log-gamma when compared to the Boost test answer, despite using
identical computations. log-gamma is still within 12 digits of agreement
though, and likely good enough for most uses.

### gamma

| Data Set   | Boost (MS C++)              | Special-Functions             |
| ---        | ---                         | ---                           |
| factorials | Max = 1.85ε (Mean = 0.491ε) | Max = 3.79ε (Mean = 0.949ε)   |
| near 0     | Max = 1.96ε (Mean = 0.684ε) | Max = 2.26ε (Mean = 0.56ε)    |
| near 1     | Max = 2ε (Mean = 0.865ε)    | Max = 2.26ε (Mean = 0.858ε)   |
| near 2     | Max = 2ε (Mean = 0.995ε)    | Max = 2ε (Mean = 0.559ε)      |
| near -10   | Max = 1.73ε (Mean = 0.729ε) | Max = 0.125ε (Mean = 0.0043ε) |
| near -55   | Max = 1.8ε (Mean = 0.817ε)  | Max = 0ε (Mean = 0ε)          |

### incomplete-gamma
See [boost incomplete gamma
documentation](https://www.boost.org/doc/libs/1_80_0/libs/math/doc/html/math_toolkit/sf_gamma/igamma.html)
for notes and error rates.

#### lower
| Data Set                 | Boost (MS C++)              | Special-Functions            |
|--------------------------|-----------------------------|------------------------------|
| small values             | Max = 1.54ε (Mean = 0.439ε) | Max = 3.00ε (Mean = 0.516ε)  |
| medium values            | Max = 35.1ε (Mean = 6.98ε)  | Max = 10.00ε (Mean = 0.294ε) |
| large values             | Max = 243ε (Mean = 20.2ε)   | Max = 20ε (Mean = 0.613ε)    |
| integer and half-integer | Max = 13ε (Mean = 2.97ε)    | Max = 3ε (Mean = 0.189ε)     |

#### upper
| Data Set                 | Boost (MS C++)             | Special-Functions           |
|--------------------------|----------------------------|-----------------------------|
| small values             | Max = 2.26ε (Mean = 0.74ε) | Max = 2.23ε (Mean = 0.511ε) |
| medium values            | Max = 23.7ε (Mean = 4ε)    | Max = 9.00ε (Mean = 0.266ε) |
| large values             | Max = 469ε (Mean = 31.5ε)  | Max = 20.5ε (Mean = 0.621ε) |
| integer and half-integer | Max = 8.72ε (Mean = 1.48ε) | Max = 4.00ε (Mean = 0.174ε) |





## NaN and Infinity

The lisp specification mentions neither NaN nor infinity, so any proper
treatment of these is going to be either implementation specific or
using a third party library.

We are using the
[float-features](https://github.com/Shinmera/float-features)
library. There is also some support for infinity in the extended-reals
package of
[numerical-utilities](https://github.com/Common-Lisp-Statistics/numerical-utilities),
but it is not comprehensive. Openlibm and Cephes have definitions, but
we don't want to introduce a large dependency just to get these
definitions.

## Test data

The test data is based on Boost test data. You can run all the tests
using the ASDF test op:

```lisp
(asdf:test-system :special-functions)
```

By default the test summary values (the same as in Boost) are printed
after each test, along with the key epsilon values.
