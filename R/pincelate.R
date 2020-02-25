

#' @export
soundout <- function(word) {
  sapply(word, function(x) pin$soundout(x), simplify=FALSE)
}

#' @export
manipulate <- function(word, letters=list(e=0), features=list('alv'=0)) {
  sapply(word, function(x) pin$manipulate(x, letters=letters, features=features))
}

#' @export
phonemefeatures <- function(word) {
  sapply(word, function(x) pin$phonemefeatures(x), simplify=FALSE)
}

#' @export
phonemestate <- function(word) {
  sapply(word, function(x) pin$phonemestate(x), simplify=FALSE)
}

#' @export
spellstate <- function(pstate) {
  pin$spellstate(pstate)
}