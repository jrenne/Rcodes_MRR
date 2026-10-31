# End-2024 resident banking office construction. Amounts internally EUR billions.  No
# model solver or parameter calibration is called by this module.
EA_RES <- c(
  "AT", "BE", "CY", "DE", "EE", "ES", "FI", "FR", "GR", "HR", "IE", "IT", "LT",
  "LU", "LV", "MT", "NL", "PT", "SI", "SK"
)
MAIN_RES <- c("DE", "FR", "IT", "ES")
ISO3_RES <- c(
  DE = "DEU", FR = "FRA", IT = "ITA", ES = "ESP", AT = "AUT", BE = "BEL", CY = "CYP",
  EE = "EST", FI = "FIN", GR = "GRC", HR = "HRV", IE = "IRL", LT = "LTU", LU = "LUX", LV = "LVA",
  MT = "MLT", NL = "NLD", PT = "PRT", SI = "SVN", SK = "SVK"
)
read_bsi_residence <- function(directory) {
  files <- c(
    "bsi_monthly_ea.csv", "bsi_quarterly_ea.csv", "bsi_bank_totals_all_ea.csv",
    "bsi_mmf_detail.csv"
  )
  d <- bind_rows(lapply(files, function(f) {
    mutate(read.csv(file.path(directory, f), colClasses = "character"),
      snapshot = f
    )
  }))
  stopifnot(all(d$DATA_TYPE == "1"), all(d$CURRENCY_TRANS == "Z01"), all(d$BS_SUFFIX ==
    "E"))
  if (any(!d$TIME_PERIOD %in% c("2024-12", "2024-Q4"))) {
    stop("Wrong residence input period")
  }
  if (all(c("UNIT", "UNIT_MULT") %in% names(d))) {
    stopifnot(all(d$UNIT == "EUR"), all(d$UNIT_MULT == "6"))
  }
  d$OBS_VALUE <- as.numeric(d$OBS_VALUE) / 1000
  d <- d %>%
    filter(is.finite(OBS_VALUE)) %>%
    distinct(KEY, .keep_all = TRUE)
  function(country, item, area = "U2", sector = "0000", rep = "A", required = TRUE) {
    x <- d %>%
      filter(REF_AREA == country, BS_ITEM == item, COUNT_AREA == area, BS_COUNT_SECTOR ==
        sector, BS_REP_SECTOR == rep)
    # December and Q4 stocks are alternatives, never additive observations.
    x <- x %>%
      arrange(match(FREQ, c("M", "Q")), match(MATURITY_ORIG, c("A", "X")))
    if (!nrow(x)) {
      if (required) {
        stop("Missing BSI stock: ", paste(country, rep, item, area, sector, sep = "/"))
      }
      return(list(value = NA_real_, key = NA_character_))
    }
    list(value = x$OBS_VALUE[1], key = x$KEY[1])
  }
}
# Weighted quadratic projection of a prior onto transport margins and cell caps.
# Relative squared deviations are weighted by the size of each prior position.
fit_residence_transport <- function(prior, rows, cols, upper = NULL, tolerance = 1e-07, soft_columns = FALSE) {
  stopifnot(all(is.finite(prior)), all(prior >= 0), all(rows >= 0), all(cols >= 0))
  if (abs(sum(rows) - sum(cols)) > tolerance) {
    stop("Transport totals differ")
  }
  nr <- nrow(prior)
  nc <- ncol(prior)
  if (is.null(upper)) {
    upper <- matrix(Inf, nr, nc)
  }
  use <- which(upper > 1e-12 & outer(rows, cols) > 0)
  ans <- matrix(0, nr, nc, dimnames = dimnames(prior))
  if (!length(use)) {
    stopifnot(sum(rows) < tolerance)
    return(ans)
  }
  ii <- row(prior)[use]
  jj <- col(prior)[use]
  p <- pmax(prior[use], 1e-05)
  # Remove the redundant last positive column equality.
  ir <- which(rows > 0)
  jc <- which(cols > 0)
  jc <- head(jc, -1)
  eq <- rbind(sapply(ir, function(i) as.numeric(ii == i)) %>%
    t(), sapply(jc, function(j) as.numeric(jj == j)) %>%
    t())
  if (!length(jc)) {
    eq <- t(sapply(ir, function(i) as.numeric(ii == i)))
  }
  rhs <- c(rows[ir], cols[jc])
  nv <- length(use)
  if (soft_columns) {
    eq <- t(sapply(ir, function(i) as.numeric(ii == i)))
    rhs <- rows[ir]
  }
  finite <- which(is.finite(upper[use]))
  bounds <- diag(nv)
  am <- cbind(t(eq), bounds, if (length(finite)) {
    -bounds[, finite, drop = FALSE]
  } else {
    matrix(numeric(), nv, 0)
  })
  bm <- c(rhs, rep(0, nv), if (length(finite)) -upper[use][finite] else numeric())
  w <- 1 / p
  if (soft_columns) {
    design <- sapply(seq_len(nc), function(j) as.numeric(jj == j)) %>%
      t()
    weights <- 1 / pmax(cols, 0.001)
    dm <- diag(w * 0.001, nv) + t(design) %*% (design * weights)
    dv <- w * 0.001 * prior[use] + as.numeric(t(design) %*% (weights * cols))
  } else {
    dm <- diag(w, nv)
    dv <- w * prior[use]
  }
  sol <- quadprog::solve.QP(dm, dv, am, bm, meq = length(rhs))$solution
  ans[use] <- pmax(sol, 0)
  if (max(abs(rowSums(ans) - rows)) > tolerance || (!soft_columns && max(abs(colSums(ans) -
    cols)) > tolerance)) {
    stop("Transport reconciliation failed")
  }
  if (any(ans - upper > tolerance)) {
    stop("Transport cell bound violated")
  }
  ans
}
allocate_residence_total <- function(total, prior, upper = rep(Inf, length(prior))) {
  stopifnot(total >= -1e-08, all(is.finite(prior)), all(prior >= 0), all(upper >= 0))
  if (total < 1e-10) {
    return(rep(0, length(prior)))
  }
  if (sum(upper) < total - 1e-07) {
    stop("Allocation exceeds published envelope")
  }
  ans <- rep(0, length(prior))
  left <- total
  for (k in seq_len(length(prior) + 2)) {
    free <- which(upper - ans > 1e-10)
    if (!length(free)) {
      break
    }
    weight <- pmax(prior[free], 1e-08)
    add <- pmin(left * weight / sum(weight), upper[free] - ans[free])
    ans[free] <- ans[free] + add
    left <- total - sum(ans)
    if (left < 1e-08) {
      break
    }
  }
  if (abs(sum(ans) - total) > 1e-07) {
    stop("Allocation did not exhaust target")
  }
  ans
}
