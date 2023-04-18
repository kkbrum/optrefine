# optrefine 1.0.0

* Added a couple more possible elements to the `options` argument to the `refine` function. These are `minsplit` for the minimum number of treated or control units to allow in a split stratum and `threads` for the number of threads to use in the optimization.
* Changed the value of `details$valueLP` to `NA` if the integer program was solved instead of giving the same value as `details$valueIP`.

# optrefine 1.0.0

* Added a `NEWS.md` file to track changes to the package.
