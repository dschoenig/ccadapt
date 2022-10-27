rename_baselevel <- function(x, level) {
  y <- x
  levels(y)[1] <- level
  return(y)
}


na2baselevel <- function(x, level) {
  levels.new <- c(level, levels(x))
  y <- as.character(x)
  y[is.na(x)] <- level
  y <- factor(y, levels = levels.new)
  return(y)
}

na_replace <- function(x, replacement) {
  y <- x
  id.repl <- which(is.na(x))
  y[is.na(x)] <- replacement
  return(y)
}
