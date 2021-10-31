# Plans for kelp
## features
- combinators (SKI calculus)
  - I - identity \(Ix = x\)
  - K - \(Kxy = x\)
  - S - substitution \(Sxyz = xz(yz)\)
- simple composition of functions (similiar to apl)
  - this should be relatively simple for programmer as it follows the same rules as LISP itself, but more complex
- scan and reduce functions (similiar to apl)
  - scan function array -> [x_1, f(x_2, x_1), f(x_3, f(x_2, x_1))...]
  - reduce(or fold) function array -> f(x_n, f(x_{n-1}, f(x_{n-2}, ...)))
  - maybe allow for tree folds left folds and folds with initial value, as well for the scan (or allow zero cost reversal of arrays)
- infinite sequences and sequences with defining functions
  - for example one should be able to create a sequence of sums 1..n like this create_sequence n -> ((n^2 + n) / n), without it evaluating in point and actually calculating it on demand (maybe even allow for iterative calculations, ie. something that cannot be manually done)
- data structures (similiar to javascript stuctures, ie. no classes, more dynamic easy to type) 
## to be decided
- interpreted or compiled (probably byte compilation to allow reflexivity, agar for the rescue)
- linked to rust libraries? i guess, rust has great crates and it'll be nice to implement in