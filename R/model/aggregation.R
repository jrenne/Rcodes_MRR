# Group counterparties for model exposition and figures. The 24-account reporting matrix
# and instrument-level register retain their original classifications.
aggregate_passive_counterparties <- function(reporting_matrix) {
  m <- reporting_matrix
  passive <- c("ROW:S2", "UNALLOC:ISSUER", "UNALLOC:HOLDER")
  stopifnot(
    is.matrix(m), nrow(m) == ncol(m), identical(rownames(m), colnames(m)), !anyDuplicated(rownames(m)),
    all(passive %in% rownames(m)), all(is.finite(m)), all(m >= 0)
  )
  target <- rownames(m)
  target[target %in% passive] <- "OTHER:PASSIVE"
  nodes <- unique(target)
  membership <- matrix(0, nrow(m), length(nodes), dimnames = list(rownames(m), nodes))
  membership[cbind(seq_len(nrow(m)), match(target, nodes))] <- 1
  grouped <- t(membership) %*% m %*% membership
  # Keep gross claims, including any claims within the combined passive block.
  stopifnot(abs(sum(grouped) - sum(m)) < 1e-07)
  list(
    matrix = grouped, node_map = data.frame(reporting_account = rownames(m), model_node = target),
    units = "EUR bn", treatment = paste(
      "Counterparty aggregation only; gross positions are not netted.",
      "Geography, source classifications and instrument-specific settlement", "rules remain in the reporting data and accounting controls."
    )
  )
}
