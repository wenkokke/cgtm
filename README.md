# CG Turing Machines

[![Build Status](https://travis-ci.org/pepijnkokke/cgtm.svg?branch=master)](https://travis-ci.org/pepijnkokke/cgtm)

This is a small library which generates VISL CG-3 code from Turing machine specifications, proving that VISL CG-3[^subset] is Turing-complete.[^other]

[^subset]: More specifically, the subset which only uses a single `SECTION`, `ADDCOHORT` and `REMCOHORT`.
[^other]: The other direction -- the fact that VISL CG-3 grammars can be run on a Turing machine -- is adequately proven by the VISL CG-3 implementation.
