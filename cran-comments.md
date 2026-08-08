## R CMD check results

0 errors | 0 warnings | 3 notes

All three notes are local to the check machine (macOS, R 4.4.0):

* The maintainer note that accompanies every submission, together with
  "Availability using Additional_repositories specification: ? ?" for
  <https://ropensci.r-universe.dev>. That repository hosts 'srr', which is used
  only to generate rOpenSci standards documentation and is listed in Suggests;
  the check machine could not reach the repository to verify it.
* "checking for future file timestamps ... unable to verify current time",
  which is the time server being unreachable.
* "checking HTML version of manual", from an HTML Tidy build dating to 2006
  that does not recognise the `<main>` element emitted by current R.

## Submission Summary

This is a minor release for 'bridgr'. It contains two user-visible breaking
changes, both correcting methods that did not honour their documented
contract:

* `summary()` on an `"mf_model"` object now returns a `"summary.mf_model"`
  object, following the convention of `summary.lm()`, instead of printing and
  returning the model unchanged. The printed report is unchanged. The returned
  object exposes the summary quantities programmatically, including a standard
  `coefficients` matrix, so `coef(summary(model))` works as it does for `lm()`.

* Objects returned by `forecast()` no longer inherit from the `forecast`
  package's `"forecast"` class. The inheritance was not honoured: `plot()` and
  `autoplot()` failed on the result and `accuracy()` returned misleading
  values, because target frequencies supported by this package include daily
  and weekly series that `stats::ts()` cannot represent exactly. `plot()` and
  `autoplot()` methods are now provided directly for the returned class, and
  the new `as.forecast()` converts to a genuine `"forecast"` object where the
  target frequency has an exact `ts` representation.

The release also adds accessor methods (`indicators()`, `weights()`,
`aggregation_parameters()`, `variable.names()`, `model.frame()`) so that
results can be extracted without reaching into the object's internal
structure, and speeds up the full-system block bootstrap.

Reverse dependencies: none.
