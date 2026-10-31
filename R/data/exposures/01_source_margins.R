get <- read_bsi_residence("data/raw/residence_2024Q4")
# Net-negative published positions cannot be non-negative gross claims. Keep their
# exact source keys in an exception register; they receive zero weight, while
# aggregate bank controls remain unchanged.
negative_positions <- bind_rows(lapply(c(
  "bsi_monthly_ea.csv", "bsi_quarterly_ea.csv",
  "bsi_bank_totals_all_ea.csv", "bsi_mmf_detail.csv"
), function(f) {
  d <- read.csv(file.path("data/raw/residence_2024Q4", f), colClasses = "character")
  d$eur_bn <- as.numeric(d$OBS_VALUE) / 1000
  d %>%
    filter(is.finite(eur_bn), eur_bn < 0) %>%
    transmute(source_key = KEY, reported_eur_bn = eur_bn, treatment = "Zero allocation weight and upper envelope in non-negative representation; aggregate stock retained, not an observed zero")
})) %>%
  distinct(source_key, .keep_all = TRUE)
write.csv(negative_positions, file.path(out, "negative_source_positions.csv"), row.names = FALSE)

vres <- function(...) get(...)$value
kres <- function(...) get(...)$key
ref <- read.csv(file.path(out, "reference_layers_2024Q4.csv"))
raw_stocks <- read.csv(file.path(out, "eurostat_raw_stock_reference.csv"))
# Use disjoint CPIS sector margins converted at the closing exchange rate.
# Reconcile missing bilateral sectors within the reported country-pair totals.
cpis_disjoint_path <- "data/raw/reference/pip_disjoint_observations_eur_million.csv"
if (!file.exists(cpis_disjoint_path)) {
  stop("Missing the supplied disjoint IMF PIP observations")
}
cpis_prior <- read.csv(cpis_disjoint_path) %>%
  filter(c_from != c_to)
cpis_sectors <- c("S11", "S122", "S12nbfi", "S13", "S14")
cpis_rows <- c(cpis_sectors, "UNALLOC")
cpis_register <- list()
cpis_margin <- list()
cpis_count <- 0L
for (a in unname(ISO3_RES[MAIN_RES])) {
  for (b in setdiff(unname(ISO3_RES[MAIN_RES]), a)) {
    d <- cpis_prior %>%
      filter(c_from == a, c_to == b)
    value <- function(h, s) {
      x <- d$claim[d$s_from == h & d$s_to == s]
      if (length(x)) {
        sum(x) / 1000
      } else {
        NA_real_
      }
    }
    total <- value("S1", "S1")
    if (!is.finite(total) || total <= 0) {
      stop("Missing CPIS country-pair total")
    }
    rows <- sapply(cpis_sectors, function(h) value(h, "S1"))
    cols <- sapply(cpis_sectors, function(s) value("S1", s))
    observed_rows <- rows
    observed_cols <- cols
    rows[!is.finite(rows)] <- 0
    holder_scale <- min(1, total / sum(rows))
    rows <- rows * holder_scale
    rows <- c(rows, UNALLOC = max(0, total - sum(rows)))
    # Missing issuer sectors share the residual using published nonresident securities
    # liabilities. S14 receives zero when its issuer stock is zero.
    known <- is.finite(cols)
    issuer_scale <- min(1, total / sum(cols[known]))
    cols[known] <- cols[known] * issuer_scale
    missing <- which(!known)
    remaining <- max(0, total - sum(cols[known]))
    if (length(missing)) {
      country <- names(ISO3_RES)[ISO3_RES == b]
      w <- sapply(cpis_sectors[missing], function(s) {
        codes <- switch(s,
          S11 = "S11",
          S122 = "S121_S122_S123",
          S12nbfi = c(
            "S123",
            "S124", "S125_S126_S127", "S128", "S129"
          ),
          S13 = "S13",
          S14 = "S14_S15"
        )
        z <- raw_stocks %>%
          filter(
            geo == country, finpos == "LIAB", na_item == "F3", sector2 == "S2",
            sector %in% codes
          )
        sum(z$values, na.rm = TRUE)
      })
      if (sum(w) == 0 && remaining > 1e-08) {
        stop("Unidentified CPIS issuer residual")
      }
      cols[missing] <- allocate_residence_total(remaining, w)
    } else if (remaining > 1e-08) {
      cols <- cols * total / sum(cols)
    }
    prior <- outer(rows, cols) / total
    for (i in seq_along(cpis_sectors)) {
      for (j in seq_along(cpis_sectors)) {
        v <- value(cpis_sectors[i], cpis_sectors[j])
        if (is.finite(v)) {
          prior[i, j] <- max(0, v)
        }
      }
    }
    # Preserve jointly reported sector cells where they are compatible with observed
    # margins; allocate only the remaining row/column budgets.
    fixed <- matrix(FALSE, length(rows), length(cols))
    fixed_values <- prior * 0
    for (i in seq_along(cpis_sectors)) {
      for (j in seq_along(cpis_sectors)) {
        v <- value(cpis_sectors[i], cpis_sectors[j])
        if (is.finite(v)) {
          fixed[i, j] <- TRUE
          fixed_values[i, j] <- v
        }
      }
    }
    remaining_rows <- rows - rowSums(fixed_values)
    remaining_cols <- cols - colSums(fixed_values)
    fit_method <- "Reported joint cells fixed; unpublished sector cells allocated within disjoint margins"
    fitted <- tryCatch(
      {
        if (min(remaining_rows, remaining_cols) < -1e-07) {
          stop("Reported joint cells exceed margins")
        }
        u <- matrix(Inf, length(rows), length(cols))
        u[fixed] <- 0
        fixed_values + fit_residence_transport(prior, pmax(remaining_rows, 0), pmax(
          remaining_cols,
          0
        ), u)
      },
      error = function(e) {
        fit_method <<- "Inconsistent reported joint cells reconciled by weighted projection; raw observations retained"
        fit_residence_transport(prior, rows, cols)
      }
    )
    cpis_count <- cpis_count + 1L
    cpis_margin[[cpis_count]] <- data.frame(
      c_from = a, c_to = b, total_eur_bn = total,
      raw_covered_holder_total = sum(observed_rows, na.rm = TRUE), holder_scale = holder_scale,
      unallocated_holder = rows["UNALLOC"], raw_known_issuer_total = sum(observed_cols,
        na.rm = TRUE
      ), issuer_scale = issuer_scale, imputed_nfc_issuer = if (!known[1]) {
        cols[1]
      } else {
        0
      }, fitted_total = sum(fitted), max_margin_error = max(abs(rowSums(fitted) -
        rows), abs(colSums(fitted) - cols)), method = fit_method, max_reported_cell_adjustment = if (any(fixed)) {
        max(abs(fitted[fixed] - fixed_values[fixed]))
      } else {
        0
      }
    )
    cpis_register[[cpis_count]] <- expand.grid(
      s_from = cpis_rows, s_to = cpis_sectors,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        c_from = a, c_to = b, claim = as.vector(fitted) * 1000, raw_prior_eur_bn = as.vector(prior),
        source = "IMF_CPIS_RESIDENCE_RECONCILED", estimated = TRUE, instrument = "debt_securities"
      )
  }
}
cpis <- bind_rows(cpis_register)
write.csv(cpis, file.path(out, "cpis_sector_reconciled_priors.csv"), row.names = FALSE)
write.csv(bind_rows(cpis_margin), file.path(out, "cpis_margin_reconciliation_eur_bn.csv"),
  row.names = FALSE
)
shss <- read.csv("data/raw/residence_2024Q4/shss_holdings.csv")
shss$OBS_VALUE <- as.numeric(shss$OBS_VALUE) / 1000
shss_get <- function(c, s, valuation = "F") {
  z <- shss %>%
    filter(REF_AREA == c, REF_SECTOR == "S122", COUNTERPART_AREA == "W2", COUNTERPART_SECTOR ==
      s, VALUATION == valuation, is.finite(OBS_VALUE))
  if (!nrow(z)) {
    return(list(value = NA_real_, key = NA_character_))
  }
  stopifnot(nrow(z) == 1)
  list(value = z$OBS_VALUE, key = z$KEY)
}
