## CRAN Submission - FuzzyClass 0.2.0

This is a minor release update (0.2.0).

### Summary of changes:
- Fixed a class/label misalignment that could mislabel predictions when the training data were not sorted by class.
- Estimation aligned with the literature: exact Gamma maximum likelihood, unbounded Beta shapes, Geometric pmf consistent with its estimator, Binomial `n >= max(x)`, class-wise sample sizes and nested alpha-cuts in the fuzzy-parameter classifiers.
- Added new fuzzy number ordering method `metd = 5` in `GauNBFuzzyParam`, `ExpNBFuzzyParam`, and `PoiNBFuzzyParam`.
- Vectorized predictions and fuzzy-number ranking (about 100x faster).
- Fixed discrete variables check (`verifyNumbersFunction`) to ensure compatibility with R 4.2+.
- Removed unused dependencies (`dplyr`, `tibble`, `tidyr`, `rlang`, `purrr`, `parallel`, `foreach`, `doParallel`).

### Test environments:
- Ubuntu 22.04.5 LTS, R 4.6.1

### R CMD check results:
- 0 ERRORs | 0 WARNINGs | 0 NOTEs (locally only a NOTE about the missing 'tidy' HTML validation tool)

