---
title: "Statistics"
linkTitle: "Statistics"
weight: 3
date: 2021-04-27
description: >
  Examples of statistical analysis
---

These notebooks describe how to undertake statistical analyses introduced as examples in the Ninth Edition of _Introduction to the Practices of Statistics_ (2017) by Moore, McCabe and Craig.  The notebooks are organised in the same manner as the chapters of the book.  The data comes from the site [IPS9 in R](https://nhorton.people.amherst.edu/ips9/) by Nicholas Horton.

To run the notebooks you will have to install a third-party library,
[common-lisp-jupyter](https://github.com/yitzchak/common-lisp-jupyter).  See the [cl-jupyter installation page](https://yitzchak.github.io/common-lisp-jupyter/install.html) for how to perform the installation.

After installing `cl-jupyter`, clone the IPS repository into your `~/common-lisp/` directory.

{{< alert title="Note" >}}Be careful when upgrading `common-lisp-jupyter`.  Breaking changes are often introduced without warning.  If you experience problems, use cl-jupyter revision `b1021ab` by using the [get checkout command](https://www.git-tower.com/learn/git/faq/git-checkout-commits/).
{{< /alert >}}


## Looking at data

[Chapter 1 &ndash; Distributions](https://github.com/Lisp-Stat/IPS/blob/master/notebooks/Part%20I/01%20-%20Distributions.ipynb)
	: Exploratory data analysis using plots and numbers

