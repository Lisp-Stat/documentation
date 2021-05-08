---
title: "Code Repository"
linkTitle: "XLisp-Stat"
weight: 10
description: >
  Collection of XLisp and Common Lisp statistical routines
---

Below is a partial list of the consoidated XLispStat packages from
UCLA and CMU repositories.  There is a great deal more XLispStat code
available that was not submitted to these archives, and a search for
an algorithm or technique that includes the term "xlispstat" will
often turn up interesting results.

## Artificial Intelligence

### Genetic Programming

[Cerebrum](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/genetic/cerebrum/)
: A Framework for the Genetic Programming of Neural Networks. Peter
  Dudey. No license specified.<br/>
   [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/genetic/cerebrum/cerebrum.doc)]

[GAL](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/genetic/gal/)
: Functions useful for experimentation in Genetic Algorithms. It is
  hopefully compatible with Lucid Common Lisp (also known as Sun
  Common Lisp).  The implementation is a "standard" GA, similar to
  Grefenstette's work.  Baker's SUS selection algorithm is employed, 2
  point crossover is maintained at 60%, and mutation is very low.
  Selection is based on proportional fitness. This GA uses
  generations.  It is also important to note that this GA maximizes.
  William M. Spears. "Permission is hereby granted to copy all or any
  part of this program for free distribution, however this header is
  required on all copies."

[mGA](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/genetic/mGA/)
: A Common Lisp Implementation of a Messy Genetic Algorithm. No
  license specified. <br/>
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/genetic/mGA/messyGA_in_C.pdf),
  [errata](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/genetic/mGA/errata.pdf)]


### Machine Learning

[Machine Learning](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/machine-learning/ml-progs/)
: Common Lisp files for various standard inductive learning algorithms
  that all use the same basic data format and same interface.  It also
  includes automatic testing software for running learning curves that
  compare multiple systems and utilities for plotting and
  statistically evaluating the results. Included are:

- AQ:		Early DNF learner.
- Backprop: 	The standard multi-layer neural-net learning method.
- Bayes Indp:     Simple naive or "idiot's" Bayesian classifier.
- Cobweb:   	A probabilistic clustering system.
- Foil:           A first-order Horn-clause learner (Prolog and Lisp versions).
- ID3:            Decision tree learner with a number of features.
- KNN:   		K nearest neighbor (instance-based) algorithm.
- Perceptron:	Early one-layer neural-net algorithm.
- PFOIL:          Propositional version of FOIL for learning DNF.
- PFOIL-CNF:      Propositional version of FOIL for learning CNF.

Raymond J. Mooney. "This program may be freely copied, used, or
modified provided that this copyright notice is included in each copy
of this code and parts thereof."


### Neural Networks

[QuickProp](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/neural/quickprop/)
: Common Lisp implementation of "Quickprop", a variation on
  back-propagation. For a description of the Quickprop algorithm, see
  [Faster-Learning Variations on Back-Propagation: An Empirical
  Study](ce.sharif.edu/courses/84-85/2/ce667/resources/root/Seminar_no_3/qp_tr.pdf)
  by Scott E. Fahlman in Proceedings of the 1988 Connectionist Models
  Summer School, Morgan-Kaufmann, 1988. Scott E. Fahlman. Public
  domain. <br/>
  [[README](https://github.com/Lisp-Stat/xls-archive/tree/master/ai/neural/quickprop/paper.pdf)]

## Fun & Games

[Towers of Hanoi](https://github.com/Lisp-Stat/xls-archive/tree/master/fun-and-games/)
: Tower of Hanoi plus the Queens program explained in Winston and
  Horn. No license specified.

## Mathematics

[Combinatorial](https://github.com/Lisp-Stat/xls-archive/tree/master/mathematics/combinatorial)
: Various combinatorial functions for XLispStat. There are other
  Common Lisp libraries for this, for example
  [cl-permutation](https://github.com/stylewarning/cl-permutation). It's
  worth searching for something in Quicklisp too. No license
  specified.

[functions](https://github.com/Lisp-Stat/xls-archive/tree/master/mathematics/functions/clmath/)
: Bessel, beta, erf, gamma and horner implementations. Gerald
  Roylance. License restricted to non-commercial use only.

[integrate](https://github.com/Lisp-Stat/xls-archive/tree/master/mathematics/integrate/)
: gauss-hermite.lsp is by Jan de Leeuw.

	runge.lsp and integr.lsp are from Gerald Roylance 1982 CLMATH
    package. integr.lsp has Simpson's rule and the trapezoid
    rule. runge.lsp integrates runge-kutta differential equations by
    various methods.

	Roylance code is non-commercial use only. Jan de Leeuw's code has
    no license specified.

[lsqpack](https://github.com/Lisp-Stat/xls-archive/tree/master/mathematics/lsqpack/)
: This directory contains the code from the Lawson and Hanson book,
  [Solving Least Squares
  Problems](https://www.amazon.com/Solving-Squares-Problems-Classics-Mathematics/dp/0898713560),
  translated with f2cl, tweaked for Xlisp-Stat by Jan de Leeuw. No
  license specified. <br/>

[nswc](https://github.com/Lisp-Stat/xls-archive/tree/master/mathematics/nswc/)
: This is an f2cl translation, very incomplete, of the NSWC
  mathematics library. The fortran, plus a great manual, is [available
  on github](https://github.com/jacobwilliams/nswc).  The report is
  NSWCDD/TR-92/425, by Alfred H. Morris, Jr. dated January 1993. No
  license specified, but this code is commonly considered public
  domain.

[Numerical Recipes](https://github.com/Lisp-Stat/xls-archive/tree/master/mathematics/numrecipes/)
: Code from Numerical Recipes in FORTRAN, first edition, translated
  with Waikato's f2cl and tweaked for Xlisp-Stat by Jan de Leeuw. No
  license specified.

[optimization](https://github.com/Lisp-Stat/xls-archive/tree/master/mathematics/optimization/)
: Code for annealing, simplex and other optimization problems. Various
  licenses. These days, better implementations are available, for
  example the
  [linear-programming](https://github.com/neil-lindquist/linear-programming)
  library.

## Statistics

### Algorithms

* [AS 190](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/algorithms/AS-190/) Probabilities and Upper Quantiles for the Studentized Range.
* [AS 226](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/algorithms/AS-226/) Computing Noncentral Beta Probabilities
* [AS 241](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/algorithms/AS-241/) The Percentage Points of the Normal Distribution
* [AS 243](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/algorithms/AS-243/) Cumulative Distribution Function of the  Non-Central T Distribution
* [TOMS 744](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/algorithms/TOMS-744/) A stochastic algorithm for global optimization with constraints

AS algorithms: B. Narasimhan (naras@euler.bd.psu.edu) "You can freely use and
distribute this code provided you don't remove this notice. NO
WARRANTIES, EXPLICIT or IMPLIED"

TOMS:  F. Michael Rabinowitz. No license specified.

### Categorical

[glim](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/categorical/glim/)
: Glim extension for log-linear models. Jan de Leeuw. No license specified.

[IPF](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/categorical/ipf/)
: Fits Goodman's RC model to the array X. Also included is a set of
  functions for APL like array operations. The four basic APL
  operators (see, for example, Garry Helzel, An Encyclopedia of APL,
  2e edition, 1989, I-APL, 6611 Linville Drive, Weed., CA) are
  inner-product, outer-product, reduce, and scan. They can be used to
  produce new binary and unary functions from existing ones.
  Unknown author. No license specified.

[latent-class](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/categorical/latent-class/latent-class.lsp)
: One file with the function latent-class. Unknown author. No license specified.

[max](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/categorical/max/max.lsp)
: Functions to do quantization and cluster analysis in the empirical
  case. Jan de Leeuw. No license specified.

[write-profiles](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/categorical/write-profiles/write-profile.lsp)
: A function. The argument is a list of lists of strings. Each element
  of the list corresponds with a variable, the elements of the list
  corresponding with a variable are the labels of that variable, which
  are either strings or characters or numbers or symbols.  The program
  returns a matrix of strings coding all the profiles. Unknown
  author. License not specified.

### Distributions

The [distributions
repository](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/distributions/)
contains single file implementations of:

[density demo](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/distributions/densdemo.lsp)
: Demonstrations of plots of density and probability
  functions. Requires XLispStat graphics. Jan de Leeuw. No license specified.

[noncentral t-distribution](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/distributions/noncentral.lsp)
: noncentral-t distribution by Russ Lenth, based on Applied Statistics Algorithm AS 243. No license specified.

[probability-functions](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/distributions/probability-functions.lsp)
: A compilation of probability densities, cumulative distribution
functions, and their inverses (quantile functions), by Jan de
Leeuw. No license specified.

[power](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/distributions/power.lsp)
: This appears to test the powers of various distribution
  functions. Unknown author. No license specified.

[weibull-mle](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/distributions/weibull-mle.lsp)
: Maximum likelihood estimation of Weibull parameters. M. Ennis. No license specified.


### Classroom Statistics

The systems in the
[introstat](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/introstat/)
directory are meant to be used in teaching situations.  For the most
part they use XLispStat's graphical system to introduce students to
statistical concepts.  They are generally simple in nature from a the
perspective of a statistical practitioner.

[ElToY](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/introstat/eltoy/)
: ElToY is a collection of three program written in
  XLISP-STAT. Dist-toy displays a univarate distribution dynamically
  linked to its parameters. CLT-toy provides an illustration of the
  central limit theorem for univariate distributions. ElToY provides a
  mechanism for displaying the prior and posterior distributions for a
  conjugate family dynamically linked so that changes to the prior
  affect the posterior and visa versa. Russell Almond
  <almond@stat.washington.edu>. GPL v2.



### Multivariate

[Dendro](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/multivariate/cluster/dendro/)
: Denrdo is for producing dendrograms for agglomerative cluster in
  XLISP-STAT.


### Plotting

[Boxplot Matrix](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/plotting/boxmat)
: Graphical Display of Analysis of Variance with the Boxplot Matrix.
  Extension of the standard oneway boxplot to cross-classified data
  with multiple observations per cell. Richard M. Heiberger
  <rmh@astro.ocis.temple.edu> No license specified.<br/>
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/plotting/boxmat/b5.pdf)]

[Dynamic Graphics and Regression Diagnostics](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/plotting/dyndiag)
: Contains methods for regression diagnostics using dynamic graphics,
  including all the methods discussed in Cook and Weisberg (1989)
  Technometrics, 277-312.  Includes documentation written in
  LaTeX. <sandy@umnstat.stat.umn.edu> No license specified.<br/>
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/plotting/dyndiag/doc.pdf)}

[FEDF](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/plotting/FEDF/)
: Flipped Empirical Distribution Function. Parallel-FEDF,
FEDF-ScatterPlot, FEDF-StarPlot written in XLISP-STAT. These plots are
suggested for exploring multidimensional data suggested in "Journal of
Computational and Graphical Statistics", Vol. 4, No. 4, pp.335-343.
97/07/18. Lee, Kyungmi & Huh, Moon Yul <myhuh@yurim.skku.ac.kr> No
license specified.

[PDFPlot](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/plotting/PDFPlot/)
: PDF graphics output from XlispStat PDFPlot is a XlispStat class to
generate PDF files from LispStat plot objects. Steven D. Majewski
<sdm7g@virginia.edu>. No license specified.

[RXridge](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/plotting/RXridge/)
: RXridge.LSP adds shrinkage regression calculation and graphical
  ridge "trace" display functionality to the XLisp-Stat, ver2.1
  release 3+ implementation of LISP-STAT. Bob Obenchain. No license
  specified.


### Regression

[Bayes-Linear](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/bayes-linear/)
: BAYES-LIN is an extension of the XLISP-STAT object-oriented
  statistical computing environment, which adds to XLISP-STAT some
  object prototypes appropriate for carrying out local computation via
  message-passing between clique-tree nodes of Bayes linear belief
  networks. Darren J. Wilkinson. No license specified.
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/bayes-linear/bayeslin.pdf)]

[Bayesian Poisson Regression](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/bpois/)
: Bayesian Poisson Regression using the Gibbs Sampler Sensitivity
  Analysis through Dynamic Graphics.  A set of programs that allow you
  to do Bayesian sensitivity analysis dynamically for a variety of
  models.  B. Narasimhan (naras@stat.fsu.edu) License restricted to
  non-commercial use only.<br/>
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/bpois.pdf)]

[Binary regression](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/binary/)
: Smooth and parametric binary regression code. Unknown author. License not specified.

[Cost of Data Analysis](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/cda)
: A regression analysis usually consists of several stages such as
  variable selection, transformation and residual diagnosis.
  Inference is often made from the selected model without regard to
  the model selection methods that proceeded it. This can result in
  overoptimistic and biased inferences.  We first characterize data
  analytic actions as functions acting on regression models.  We
  investigate the extent of the problem and test bootstrap, jacknife
  and sample splitting methods for ameliorating it. We also
  demonstrate an interactive XLISP-STAT system for assessing the cost
  of the data analysis while it is taking place.  Julian
  J. Faraway. BSD license.<br/>
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/cda/cda.pdf)]

[Gee](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/gee/)
: Lisp-Stat code for generalised estimating equation models. Thomas
  Lumley <thomas@biostat.washington.edu>. GPL v2. <br/>
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/gee/gee.1.0.pdf)]

[GLIM](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/glim/)
: Functions and prototypes for fitting generalized linear
  models. Contributed by Luke Tierney <luke@umnstat.stat.umn.edu>. No
  license specified.<br/>
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/glim/glim.pdf)]

[GLMER](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/glmer)
: A function to estimate coefficients and dispersions in a generalized
  linear model with random effects. Guanghan Liu
  <gliu@math.ucla.edu>. No license specified.

[Hasse](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/hasse/)
: Implements Taylor & Hilton's rules for balanced ANOVA designs and
  draws the Hasse diagram of nesting
  relationships. Philip Iversen <piversen@iastate.edu>. License restricted to
  non-commercial use only.

[monotone](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/monotone/)
: Implementation of an algorithm to project on the intersection of r
   closed convex sets. Further details and references are in Mathar,
   Cyclic Projections in Data Analysis, Operations Research
   Proceedings 1988, Spinger, 1989. Jan de Leeuw. No license
   specified.

[OIRS](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/oirs/)
: Order and Influence in Regression Strategy.  The methods (tactics)
  of regression data analysis such as variable selection,
  transformation and outlier detection are characterised as functions
  acting on regression models and returning regression models.  The
  ordering of the tactics, that is the strategy, is studied.  A method
  for the generation of acceptable models supported by the choice of
  regression data analysis methods is described with a view to
  determining if two capable statisticians may reasonably hold
  differing views on the same data.  Optimal strategies are
  considered.  The idea of influential points is extended from
  estimation to the model building process itself both quantitatively
  and qualitatively.  The methods described are not intended for the
  entirely automatic analysis of data, rather to assist the
  statistician in examining regression data at a strategic level.
  Julian J. Faraway <julian@stat.lsa.umich.edu>. BSD license.

[oneway](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/oneway/)
: Additions to Tierney's one way ANOVA. B. Narasimhan
  <naras@euler.bd.psu.edu>. No license specified.

[Regstrat](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/regstrat/)
: A XLispStat tool to investigate order in Regression Strategy
  particularly for finding and examining the models found by changing
  the ordering of the actions in a regression analysis. Julian Faraway
  <julian@stat.lsa.umich.edu>. License restricted to non-commercial use
  only.

[Simsel](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/regression/simsel/)
: XLISP-STAT software to perform Bayesian Predictive Simultaneous
  Variable and Transformation Selection for regression.  A
  criterion-based model selection algorithm. Jennifer A. Hoeting
  <jah@stat.colostate.edu>. License restricted to non-commercial use
  only.

### Robust

There are three robust systems in the
[robust](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/robust/)
directory:

[robust regression](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/robust/regression)
: This is the Xlisp-Stat version of
  [ROSEPACK](https://www.tandfonline.com/doi/abs/10.1080/03610927708827533),
  the robust regression package developed by Holland, Welsch, and
  Klema around 1975. See Holland and Welsch, Commun. Statist. A6,
  1977, 813-827. See also the Xlisp-Stat book, pages 173-177, for an
  alternative approach. Jan de Leeuw. No license specified.

There is also robust statistical code for 
[location](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/robust/location)
and
[scale](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/robust/scale).



### Simulation

The [simulation](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/simulation/)
directory contains [bootstrapping
methods](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/simulation/bootstrap/),
[variable
imputation](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/simulation/impute/),
[jackknife
resampling](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/simulation/jackknife/),
[monte-carlo
simulations](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/simulation/monte-carlo/)
and a [general purpose
simulator](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/simulation/simulator). There
is also the [discrete finite state markov
chains](https://github.com/Lisp-Stat/xls-archive/tree/master/statisticstemporal/markov/) in
the [temporal
directory](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/temporal/).


### Smoothers

[kernel density estimators](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/smoothers/density/)
: KDEs based on [Wand](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/smoothers/density/mvkde/README), [CFFI based KDEs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/smoothers/density/naras-kde/README) by B. Narasimhan, and [graphical univariate density estimation](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/smoothers/density/udina/README).

[spline](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/smoothers/spline/)
: Regularized bivariate splines with smoothing and tension according
  to Mitasova and Mitas. Cubic splines according to Green and
  Silverman. Jan de Leeuw. No license specified.

[super-smoother](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/smoothers/supersmoother/)
: The super smoothing algorithm, originally implemented in FORTRAN by
  Jerome Friedman of Stanford University, is a method by which a
  smooth curve may be fitted to a two-dimensional array of points. Its
  implementation is presented here in the XLISP-STAT language. Jason
  Bond. No license specified.<br/>
  [[DOCS](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/smoothers/supersmoother/super-smoother.pdf)]

[Variable Bandwidth](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/smoothers/variable-bandwidth/)
:  XLispStat code to facilitate interactive bandwidth choice for
estimator (3.14), page 44 in Bagkavos (2003), "BIAS REDUCTION IN
NONPARAMETRIC HAZARD RATE ESTIMATION". No license specified.


### Spatial

[livemap](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/spatial/livemap/)
: LiveMap is a tool for exploratory spatial data analysis. Dr. Chris Brunsdon. No license specified. <br/>
  [[DOCS](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/spatial/livemap/introlm.pdf)]

[variograms](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/spatial/variograms/)
: Produces variograms using algorithms from C.V. Deutsch and
  A.G. Journel, "GSLIB: Geostatistical Software Library and User's
  Guide, Oxford University Press, New York, 1992. Stanley
  S. Bentow. No license specified. <br/>
  [[DOCS](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/spatial/variograms/variogram.pdf)]



### Temporal

[Exploratory survival analysis](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/temporal/exp-surv)
: A set of XLISP-STAT routines for the interactive, dynamic,
   exploratory analysis of survival data. E.  Neely Atkinson
   (neely@odin.mda.uth.tmc.edu) "This software may be freely redistributed." <br/>
   [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/temporal/exp-surv/expsurv.pdf)]

[Markov](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/temporal/markov/)
: Simulate some Markov chains in Xlisp-Stat.  Complete documentation and examples are included.  B. Narasimhan (naras@sci234e.mrs.umn.edu). GPL.<br/>
  [[Docs](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/temporal/markov/doc/gambler.pdf)]

[SAPA](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/temporal/sapa4xl/)
: Sapaclisp is a collection of Common Lisp functions that can be used to
carry out many of the computations described in the SAPA book:

   Donald B. Percival and Andrew T. Walden, "Spectral Analysis for
   Physical Applications: Multitaper and Conventional Univariate
   Techniques", Cambridge University Press, Cambridge, England, 1993.

The SAPA book uses a number of time series as examples of various
spectral analysis techniques.

{{< alert title="Note" >}}This archive contains SAPA converted to XLispStat. A [Common
Lisp version](ftp://lib.stat.cmu.edu/sapaclisp/everything) can be
obtained from the CMU archive.{{< /alert >}}

From the description:

Sapaclisp features functions for converting to/from decibels, the
Fortran sign function, log of the gamma function, manipulating
polynomials, root finding, simple numerical integration, matrix
functions, Cholesky and modified Gram-Schmidt (i.e., Q-R) matrix
decompositions, sample means and variances, sample medians,
computation of quantiles from various distributions, linear least
squares, discrete Fourier transform, fast Fourier transform, chirp
transform, low-pass filters, high-pass filters, band-pass filters,
sample autocovariance sequence, autoregressive spectral estimates,
least squares, forward/backward least squares, Burg's algorithm, the
Yule-Walker method, periodogram, direct spectral estimates, lag window
spectral estimates, WOSA spectral estimates, sample cepstrum, time
series bandwidth, cumulative periodogram test statistic for white
noise, and Fisher's g statistic.

License: "Use and copying of this software and preparation of
derivative works based upon this software are permitted.  Any
distribution of this software or derivative works must comply with all
applicable United States export control laws."

[Times](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/temporal/times/)
:  XLispstat functions for time series analysis, data editing, data
   selection, and other statistical operations.  W. Hatch
   (bts!bill@uunet.uu.net). Public Domain.


### Tests

The [tests
directory](https://github.com/Lisp-Stat/xls-archive/tree/master/statistics/tests)
contains code to do one-sample and two-sample Kolmogorov-Smirnov test
(with no estimated parameters) and code to do Mann-Whitney and
Wilcoxon rank signed rank tests.

## Training & Documentation

[ENAR Short Course](https://github.com/Lisp-Stat/xls-archive/tree/master/training/short-course/)
: This directory contains slides and examples used in a shortcourse on
  Lisp-Stat presented at the 1992 ENAR meetings in Cincinnati, 22
  March 1992.

[ASA Course](https://github.com/Lisp-Stat/xls-archive/tree/master/training/asa-course.pdf)
: Material from an ASA course given in 1992.

[Tech Report](https://github.com/Lisp-Stat/xls-archive/tree/master/training/tech-report.pdf)
: A 106 page mini manual on XLispStat.

## Utilities

The majority of the files in the [utilities
directory](https://github.com/Lisp-Stat/xls-archive/tree/master/utilities/)
are specific to XLISP-STAT and unlikely to be useful. In most cases
better alternatives now exist for Common Lisp. A few that may be worth
investigating have been noted below.

### Filters

[XLisp-S](https://github.com/Lisp-Stat/xls-archive/tree/master/utilities/filters/xlisp-to-s/)
: A series of routines to allow users of Xlisp or LispStat to
  interactively transfer data to and access functions in New S. Steve
  McKinney <kilroy@biostat.washington.edu>. License restricted to
  non-commercial use only.


### I/O

[formatted-input](https://github.com/Lisp-Stat/xls-archive/tree/master/utilities/IO/formatted-input)
: A set of xlisp functions that can be used to read ascii files into
  lists of lists, using formatted input. The main function is
  `read-file`, which has as arguments a filename and a fortran type
  format string (with f, i, x, t, and a formats) Jan Deleeuw
  <deleeuw@laplace.sscnet.ucla.edu> "THIS SOFTWARE CAN BE FREELY
  DISTRIBUTED, USED, AND MODIFIED."

### Memoization

[automatic memoization](https://github.com/Lisp-Stat/xls-archive/tree/master/utilities/memoization/hall/)
: As the name suggests. Marty Hall
  <hall@aplcenmp.apl.jhu.edu>. "Permission is granted for any use or
  modification of this code provided this notice is retained."<br/>
  [[OVERVIEW](https://github.com/Lisp-Stat/xls-archive/tree/master/utilities/memoization/hall/Memoization-Overview.text)]

