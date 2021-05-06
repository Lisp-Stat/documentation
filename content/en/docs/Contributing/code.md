---
title: "Contributing Code"
linkTitle: "Code"
weight: 10
description: >
  How to contribute code to Lisp-Stat
---

First, ensure you have signed a [contributor license
agreement](/docs/contributing/#contributor-license-agreement). Then
follow these steps for contributing to Lisp-Stat:

- Step 1: [Get source code](#get-source-code).
- Step 2: Get approval and [modify the source code](#modify-the-source).
- Step 3: Get your [code reviewed](#code-review) and committed to the project.

You may also be interested in the [additional
information](#additional-info) at the end of this document.

### Get source code

First you need the Lisp-Stat source code. The core systems are found
on the [Lisp-Stat github](https://github.com/Lisp-Stat) page. For the
individual systems, just check out the one you are interested in. For
the entire Lisp-Stat system, at a minimum you will need:

- [data-frame](https://github.com/Lisp-Stat/data-frame)
- [dfio](https://github.com/Lisp-Stat/dfio)
- [special-functions](https://github.com/Lisp-Stat/special-functions)
- [numerical-utilities](https://github.com/Lisp-Stat/numerical-utilities)
- [documentation](https://github.com/Lisp-Stat/documentation)
- [distributions](https://github.com/Lisp-Stat/distributions)
- [lisp-stat](https://github.com/Lisp-Stat/lisp-stat)

Other dependencies will be pulled in by Quicklisp.

Development occurs on the "master" branch. To get all the repos, you
can use the following command in the directory you want to be your top
level dev space:


```shell
git clone https://github.com/Lisp-Stat/data-frame.git && \
git clone https://github.com/Lisp-Stat/dfio.git && \
git clone https://github.com/Lisp-Stat/special-functions.git && \
git clone https://github.com/Lisp-Stat/numerical-utilities.git && \
git clone https://github.com/Lisp-Stat/documentation.git && \
git clone https://github.com/Lisp-Stat/lisp-stat.git && \
git clone https://github.com/Lisp-Stat/plot.git && \
git clone https://github.com/Lisp-Stat/select.git && \
git clone https://github.com/Lisp-Stat/array-operations.git
```

### Modify the source

Before you start, send a message to the [Lisp-Stat mailing
list](https://groups.google.com/g/lisp-stat) or file an issue on
Github describing your proposed changes.  Doing this helps to verify
that your changes will work with what others are doing and have
planned for the project.  Importantly, there may be some existing code
or design work for you to leverage that is not yet published, and we'd
hate to see work duplicated unnecessarily.

Be patient, it may take folks a while to understand your
requirements. For large systems or design changes, a design document
is preferred. For small changes, issues and the mailing list are fine.


Once your suggested changes are agreed, you can modify the source code
and add some features using your favorite IDE.

The following sections provide tips for working on the project:

#### Coding Convention

Please consider the following before submitting a pull request:

- Code should be formatted according to the [Google Common Lisp Style Guide](https://google.github.io/styleguide/lispguide.xml)
- All code should include unit tests. Currently we use [fiveam](https://github.com/lispci/fiveam) as the test framework for new projects, but are looking at [Parachute](https://github.com/Shinmera/parachute) and [Rove](https://github.com/fukamachi/rove) as more extensible alternatives.
- Contributions should pass existing unit tests
- New unit tests should be provided to demonstrate bugs and fixes
- [Indentation in Common Lisp](https://dept-info.labri.fr/~idurand/enseignement/lst-info/PFS/Common/Strandh-Tutorial/indentation.html) is important for readability. Contributions should adhere to these guidelines. For the most part, a properly configured Emacs will do this automatically.

### Code review

Github includes [code review
tools](https://github.com/features/code-review/) that can be used as
part of a pull request. We recommend using a [triangular
workflow](https://gist.github.com/anjohnson/8994c95ab2a06f7d2339) and
feature/bug branches in your own repository to work from. Once you
submit a pull request, one of the committers will review it and
possibly request modifications.

As a contributor you should organise
([squash](https://www.git-tower.com/learn/git/faq/git-squash/)) your
git commits to make them understandable to reviewers:

* Combine WIP and other small commits together.
* Address multiple issues, for smaller bug fixes or enhancements, with a single commit.
* Use separate commits to allow efficient review, separating out formatting changes or simple refactoring from core changes or additions.
* Rebase this chain of commits on top of the current master
* Write a [good git commit message](https://chris.beams.io/posts/git-commit/)

Once all the comments in the review have been addressed, a Lisp-Stat committer  completes the following steps to commit the patch:
* If the master branch has moved forward since the review, rebase the branch from the pull request on the latest master and re-run tests.
* If all tests pass, the committer amends the last commit message in the series to include “this closes #1234”. This can be done with interactive rebase. When on the branch issue: `git rebase -i HEAD^`
  * Change where it says “pick” on the line with the last commit, replacing it with “r” or “reword”. It replays the commit giving you the opportunity the change the commit message.
  * The committer pushes the commit(s) to the github repo
  * The committer resolves the issue with a message like `"Fixed in <Git commit SHA>"`.

### Additional Info

#### Where to start?

If you are new to statistics or Lisp, documentation updates are always
a good place to start. You will become familiar with the workflow,
learn how the code functions and generally become better acquainted
with how Lisp-Stat operates. Besides, any contribution will require
documentation updates, so it's good to learn this system first.

If you are coming from an existing statistical environment, consider
porting a XLispStat package that you find useful to Lisp-Stat.  Use
the [XLS](https://github.com/Lisp-Stat/XLS) compatibility layer to
help.  If there is a function missing in XLS, raise an issue and we'll
create it. Some XLispStat code to browse:

* [XLispStat archive](https://github.com/Lisp-Stat/xls-archive)
* [XLispStat source](https://github.com/jhbadger/xlispstat)
* The following are built on XLispStat:
  * [ARC](http://www.stat.umn.edu/arc/) Applied Regression
  * [ViSta](http://www.visualstats.org/) Visual Statistics

Keep in mind that some of these rely on the XLispStat graphics
system, which was native to the platform. LISP-STAT uses Vega for
visualizations, so there isn't a direct mapping. Non-graphical code
should be a straight forward port.

You could also look at [CRAN](https://cran.r-project.org/), which
contains thousands of high-quality packages.

For specific ideas that would help, see the
[ideas](/docs/contributing/ideas) page.

#### Issue Guidelines

Please comment on issues in github, making your concerns known. Please
also vote for issues that are a high priority for you.

Please refrain from editing descriptions and comments if possible, as
edits spam the mailing list and clutter the audit trails, which is
otherwise very useful. Instead, preview descriptions and comments
using the preview button (on the right) before posting them. Keep
descriptions brief and save more elaborate proposals for comments,
since descriptions are included in GitHub automatically sent
messages. If you change your mind, note this in a new comment, rather
than editing an older comment. The issue should preserve this history
of the discussion.

