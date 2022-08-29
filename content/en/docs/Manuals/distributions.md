---
title: "Distributions"
date: 2022-8-28
weight: 3
latex: true
vega: true
description: >
  Working with statistical distributions
---

## Overview

The Distributions package provides a collection of probabilistic distributions and related functions such as:

- Sampling from distributions
- Moments (e.g mean, variance, skewness, and kurtosis), entropy, and other properties
- Probability density/mass functions (pdf) and their logarithm (logpdf)
- Moment-generating functions and characteristic functions
- Maximum likelihood estimation
- Distribution composition and derived distributions

## Getting Started

Load the distributions system with `(ql:quickload :distributions)` and generate a sequence of 1000 samples drawn from the standard normal distribution:

```lisp
(defparameter *rn-samples*
              (nu:generate-sequence '(vector double-float)
			            			 1000
				                     #'distributions:draw-standard-normal))
```
and plot a histogram of the counts:

```lisp
(plot:plot
   (vega:defplot normal
       `(:mark :bar
	     :data (:x ,*rn-samples*)
	     :encoding (:x (:bin (:step 0.5)
	                    :field x)
		            :y (:aggregate :count)))))
```

{{< vega id="standard-normal" spec="/plots/standard-normal.vl.json" >}}

It looks like there's an outlier at 5, but basically you can see it's centered around 0.

To create a parameterised distribution, pass the parameters when you create the object.  In the following example we create a distribution with a mean of 2 and variance of 1 and plot it:

```lisp
(defparameter rn2 (distributions:r-normal 2 1))
(let* ((seq (nu:generate-sequence '(vector double-float) 10000 (lambda () (distributions:draw rn2)))))
  (plot:plot
   (vega:defplot normal-2-1
       `(:mark :bar
	 :data (:x ,seq)
	 :encoding (:x (:bin (:step 0.5)
			:field x)
		    :y (:aggregate :count))))))
```
{{< vega id="normal-2-1" spec="/plots/normal-2-1.vl.json" >}}

Now that we have the distribution as an object, we can obtain `pdf`, `cdf`, `mean` and other parameters for it:

```
LS-USER> (mean rn2)
2.0d0
LS-USER> (pdf rn2 1.75)
0.38666811680284924d0
LS-USER> (cdf rn2 1.75)
0.4012936743170763d0
```

## Gamma

In probability theory and statistics, the gamma distribution is a two-parameter family of continuous probability distributions. The exponential distribution, Erlang distribution, and chi-square distribution are special cases of the gamma distribution. There are two different parametrization in common use:

- With a shape parameter k and a scale parameter θ.
- With a shape parameter α = k and an inverse scale parameter β = 1/θ, called a rate parameter.

In each of these forms, both parameters are positive real numbers.

The parametrization with k and θ appears to be more common in econometrics and certain other applied fields, where for example the gamma distribution is frequently used to model waiting times.

The parametrization with α and β is more common in Bayesian statistics, where the gamma distribution is used as a conjugate prior distribution for various types of inverse scale (rate) parameters, such as the λ of an exponential distribution or a Poisson distribution.

When the shape parameter has an integer value, the distribution is the Erlang distribution.  Since this can be produced by ensuring that the shape parameter has an integer value > 0, the Erlang distribution is not separately implemented.

### PDF

The probability density function parameterized by shape-scale is:

$f(x;k,\theta )={\frac {x^{k-1}e^{-x/\theta }}{\theta ^{k}\Gamma (k)}}\quad {\text{ for }}x&gt;0{\text{ and }}k,\theta &gt;0$,

and by shape-rate:

$f(x;\alpha ,\beta )={\frac {x^{\alpha -1}e^{-\beta x}\beta ^{\alpha }}{\Gamma (\alpha )}}\quad {\text{ for }}x&gt;0\quad \alpha ,\beta &gt;0$

### CDF

The cumulative distribution function characterized by shape and scale (k and θ) is:

$F(x;k,\theta )=\int _{0}^{x}f(u;k,\theta )\,du={\frac {\gamma \left(k,{\frac {x}{\theta }}\right)}{\Gamma (k)}}$

where $\gamma \left(k,{\frac {x}{\theta }}\right)$ is the lower-incomplete-gamma function.

Characterized by α and β (shape and rate):

$F(x;\alpha ,\beta )=\int _{0}^{x}f(u;\alpha ,\beta )\,du={\frac {\gamma (\alpha ,\beta x)}{\Gamma (\alpha )}}$

where $\gamma (\alpha ,\beta x)$ is the lower incomplete gamma function.

### Usage

Python, and Boost use shape & scale for parameterization.  Both forms of parametrization are common.

Lisp-Stat and R use shape and rate for the default parametrization.  However, since Lisp-Stat's *implementation* is based on Boost (because of the restrictive license of R), we perform the conversion $\theta=\frac{1}{\beta}$ internally.

### Implementation notes

In the following table k is the shape parameter of the distribution, θ is its scale parameter, x is the random variate, p is the probability and q is (- 1 p).  The implementation functions are in the [special-functions](https://github.com/Lisp-Stat/special-functions) system.

| Function            | Implementation                             |
|---------------------|--------------------------------------------|
| PDF                 | (/ (gamma-p-derivative k (/ x θ)) θ)       |
| CDF                 | (incomplete-gamma k (/ x θ))               |
| CDF complement      | (upper-incomplete-gamma k (/ x θ))         |
| quantile            | (* θ (inverse-incomplete-gamma k p))       |
| quantile complement | (* θ (upper-inverse-incomplete-gamma k p)) |
| mean                | kθ                                         |
| variance            | kθ<sup>2</sup>                             |
| mode                | (* (1- k) θ), k>1                          |
| skewness            | (/ 2 (sqrt k))                             |
| kurtosis            | (+ 3 (/ 6 k))                              |
| kurtosis excess     | (/ 6 k)                                    |


### Example
On average, a train arrives at a station once per 15 minutes (θ=.25). What is the probability one arrives α=10 in less than x=3 hours?

In this example we have:
```
alpha = 10
theta = 15/60
x = 3
```
To compute the exact answer:
```
(distributions:cdf-gamma 3d0 10d0 :scale 15/60)
;=> 0.7576078383294877d0
```

<!-- TODO: Figure out how to the example here in Lisp-Stat: https://rpubs.com/mpfoley73/459051

### References

[Boost implementation of Gamma](https://www.boost.org/doc/libs/1_80_0/libs/math/doc/html/math_toolkit/dist_ref/dists/gamma_dist.html)
[Gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution) (Wikipedia)




