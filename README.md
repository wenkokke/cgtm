# CG Turing Machines

[![Build Status](https://travis-ci.org/pepijnkokke/cgtm.svg?branch=master)](https://travis-ci.org/pepijnkokke/cgtm)

This is a small library which generates VISL CG-3 code from Turing machine specifications, proving that VISL CG-3<a name="1" href="#ad1"><sup>1</sup></a> is Turing-complete<a name="2" href="#ad2"><sup>2</sup></a>.

---

[<a name="ad1" href="#1">1</a>]: 
More specifically, the subset which only uses a single `SECTION`, `ADDCOHORT` and `REMCOHORT`.

[<a name="ad2" href="#2">2</a>]: 
The other direction—the fact that VISL CG-3 grammars can be run on a Turing machine—is probably adequately proven by the VISL CG-3 implementation.
