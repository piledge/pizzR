list.do <- function(.data, fun, ...) {
  do.call(what = fun, args = as.list(.data), ...)
}

list.rbind <- function(.data) {
  list.do(.data, "rbind")
}

list.cbind <- function(.data) {
  list.do(.data, "cbind")
}
