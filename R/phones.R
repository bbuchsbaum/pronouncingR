
#' @export
phones_for_word <- function(word) {
  sapply(word, function(x) pronouncing$phones_for_word(x))
}

#' @export
syllable_count <- function(pvec) {
  sapply(pvec, function(x) pronouncing$syllable_count(x))
}

#' @export
stresses <- function(pvec) {
  sapply(pvec, function(x) pronouncing$stresses(x))
}

#' @export
rhymes <- function(word) {
  sapply(word, function(x) pronouncing$rhymes(x))
}

#' @export
install_pronouncing <- function() {
  reticulate::py_install("pronouncing", pip=TRUE)
}

#' @export
install_pincelate <- function() {
  reticulate::py_install("pincelate", pip=TRUE)
}

#' @export
install_tensorflow <- function() {
  reticulate::py_install("tensorflow", pip=TRUE)
}
