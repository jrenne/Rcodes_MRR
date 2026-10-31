# Align scalar or country-specific parameters with the matrix order.
align_country_vector <- function(x, country_labels, arg_name) {
  if (length(x) == 1L) {
    return(rep(as.numeric(x), length(country_labels)))
  }

  if (is.null(names(x))) {
    if (length(x) != length(country_labels)) {
      stop(
        arg_name, " must either be scalar, a named vector, or an unnamed vector of length ",
        length(country_labels), "."
      )
    }
    return(as.numeric(x))
  }

  missing_names <- setdiff(country_labels, names(x))
  if (length(missing_names) > 0L) {
    stop(arg_name, " is missing values for: ", paste(missing_names, collapse = ", "))
  }

  as.numeric(x[country_labels])
}

# Rows enumerate binary configurations in the paper's country order.
binary_configurations <- function(n) {
  t(matrix(vapply(0:(2^n - 1), function(k) {
    rev(as.integer(intToBits(k))[seq_len(n)])
  }, integer(n)), nrow = n))
}
