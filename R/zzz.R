.onLoad <- function(libname, pkgname) {
  # Make `fuzzy_naive_bayes()` available to tidymodels: register it now if
  # parsnip is already loaded, or as soon as parsnip gets loaded.
  if (isNamespaceLoaded("parsnip")) {
    register_fuzzy_naive_bayes()
  }
  setHook(packageEvent("parsnip", "onLoad"), function(...) register_fuzzy_naive_bayes())
  invisible()
}
