#' @noRd
make_list_names <- function(x, pre = "X", sep = "")
{
  stopifnot(is.list(x))
  n <- length(x)
  name <- names(x)
  if(!is.null(name) && all(name != "" & !is.na(name)))
    return(x)
  if(is.null(x)) {
    names(x) <- paste0(pre, sep, seq_len(n))
  }
  if(all(name == "" | is.na(name))) {
    names(x) <- paste0(pre, sep, seq_len(n))
  } else {
    idx <- name == "" | is.na(name)
    name[idx] <- paste0(pre, sep, sum(idx))
    names(x) <- make.unique(name)
  }
  return(x)
}

#' @noRd
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

#' @noRd
sig_mark <- function(p.value,
                     sig.level = c(0.05, 0.01, 0.001),
                     mark = c("*", "**", "***")) {
  if(!is.numeric(p.value))
    p.value <- as.numeric(p.value)
  ord <- order(sig.level)
  sig.level <- sig.level[ord]
  mark <- mark[ord]
  brks <- c(0, sig.level, 1)
  lbs <- c(mark, "")
  pp <- cut(p.value, breaks = brks, labels = lbs, include.lowest = FALSE, right = TRUE)
  ifelse(p.value == 0, mark[1], as.character(pp))
}

#' @noRd
format_number <- function(x, digits = 2, nsmall = 2) {
  if(!is.numeric(x))
    stop("`x` must be a numeric vector.", call. = FALSE)
  x <- round(x, digits = digits)
  format(x, nsmall = nsmall)
}

#' @noRd
ggname <- function (prefix, grob)
{
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#' @noRd
`%||%` <- function(x, y)
{
  if(is.null(x)) y else x
}

#' @noRd
aes_short_to_long <- function(data, prefix, short.aes)
{
  if(is.null(data)) {
    return(data)
  }
  if (any(names(data) == "color")) {
    names(data)[names(data) == "color"] <- "colour"
  }
  short.aes.id <- names(data) %in% short.aes
  names(data)[short.aes.id] <- paste(prefix, names(data)[short.aes.id],
                                     sep = "_")
  data
}

#' @noRd
aes_long_to_short <- function(data, prefix, long.aes)
{
  if(is.null(data)) {
    return(data)
  }
  long.color <- paste(prefix, "color", sep = "_")
  if (any(names(data) == long.color)) {
    names(data)[names(data) == long.color] <- paste(prefix, "colour", sep = "_")
  }
  long.aes.id <- names(data) %in% long.aes
  names(data)[long.aes.id] <- gsub(paste0(prefix, "_"), "",
                                   names(data)[long.aes.id], fixed = TRUE)
  data
}

#' @noRd
remove_short_aes <- function(data, short.aes) {
  data[ , !names(data) %in% short.aes, drop = FALSE]
}

#' @noRd
short_aes <- c("color", "colour", "fill", "size", "linetype", "alpha", "r0",
               "width", "height", "n", "ratio")

#' @noRd
long_aes_upper <- paste("upper", short_aes, sep = "_")

#' @noRd
long_aes_lower <- paste("lower", short_aes, sep = "_")

#' @noRd
utils::globalVariables(
  c(
    "r",
    "x",
    "y",
    "angle",
    "p.value",
    ".row.id",
    ".col.id",
    ".group",
    "spec",
    "env",
    ".start.filter",
    ".end.filter"
  )
)

#' @noRd
xrange <- function(.plot) {
  stopifnot(inherits(.plot, "gg"))
  if(is_dplot(.plot)) {
    class(.plot) <- setdiff(class(.plot), "dplot")
  }
  ggplot_build(.plot)$layout$panel_params[[1]]$x.range
}

#' @noRd
yrange <- function(.plot) {
  stopifnot(inherits(.plot, "gg"))
  if(is_dplot(.plot)) {
    class(.plot) <- setdiff(class(.plot), "dplot")
  }
  ggplot_build(.plot)$layout$panel_params[[1]]$y.range
}

#' @noRd
aes_intersect <- function (aes1, aes2, .except = NULL)
{
  id <- names(aes1) %in% c(names(aes2), .except)
  aes <- unclass(aes1)[id]
  class(aes) <- "uneval"
  aes
}

#' @importFrom utils modifyList
#' @noRd
aes_modify <- function(aes1, aes2) {
  aes <- modifyList(as.list(aes1), as.list(aes2))
  class(aes) <- "uneval"
  aes
}

#' @noRd
is_binary <- function(x) {
  is.character(x) || is.factor(x)
}

#' @noRd
empty <- function (df)
{
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

#' @noRd
`%||%` <- function(x, y)
{
  if(is.null(x)) y else x
}
