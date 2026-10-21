## CRAN Submission - FuzzyClass 0.3.0

This is a minor release update (0.3.0).

### Summary of changes:
- All classifiers are now implemented as 'R6' classes (new imports: 'R6' and 'cli'); the functional interface and the predictions are unchanged.
- Training reports its progress with a message, which can be silenced with `verbose = FALSE` or `options(FuzzyClass.verbose = FALSE)`.
- New 'parsnip' model `fuzzy_naive_bayes()` with one engine per classifier, so the classifiers can be used and tuned within 'tidymodels'. The tidymodels packages are only suggested.
- Requires R >= 4.1.0.

### Test environments:
- Ubuntu 22.04.5 LTS, R 4.6.1

### R CMD check results:
- 0 ERRORs | 0 WARNINGs | 1 NOTE (locally only, about the missing 'tidy' HTML validation tool)
