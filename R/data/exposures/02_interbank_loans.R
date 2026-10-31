# S122 credit-sector controls. Detailed MFI stocks are proportionally adjusted within
# each published instrument/counterparty class, never assumed MMF-free.
bank_controls <- bind_rows(lapply(EA_RES, function(c) {
  b <- function(item, sector = "0000", area = "U2") {
    vres(c, item, area, sector, "B",
      required = c %in% MAIN_RES
    )
  }
  a <- function(item, sector = "0000", area = "U2") vres(c, item, area, sector, "A")
  data.frame(
    country = c, assets = b("T00", area = "Z5"), capital = b("L60", area = "Z5"),
    loans_ea = b("A20"), loans_mfi = b("A20", "1000"), loans_gov = b("A20", "2100"),
    loans_private = b("A20", "2200"), bonds_ea = b("A30"), bonds_mfi = b("A30", "1000"),
    bonds_gov = b("A30", "2100"), bonds_private = b("A30", "2200"), deposits_ea = b("L20"),
    deposits_mfi = b("L20", "1000"), bonds_issued = b("L40"), outside_assets = b("AXG",
      area = "U4"
    ), outside_liabilities = b("LXG", area = "U4"), other_liabilities = b("L70",
      area = "Z5"
    ), loan_mfi_scale = b("A20", "1000") / a("A20", "1000"), loan_private_scale = b(
      "A20",
      "2200"
    ) / a("A20", "2200"), loan_gov_scale = b("A20", "2100") / a("A20", "2100"),
    deposit_scale = b("L20") / a("L20"), bond_mfi_scale = if (a("A30", "1000") > 0) {
      b("A30", "1000") / a("A30", "1000")
    } else {
      1
    }, bond_nonmfi_scale = (b("A30", "2100") + b("A30", "2200")) / (a("A30", "2100") +
      a("A30", "2200"))
  )
}))
stopifnot(all(is.finite(as.matrix(bank_controls[
  bank_controls$country %in% MAIN_RES,
  -1
]))))
for (nm in grep("_scale$", names(bank_controls), value = TRUE)) {
  bad <- !is.finite(bank_controls[[nm]])
  if (any(bad & bank_controls$country %in% MAIN_RES)) {
    stop("Missing sample-country S122 control")
  }
  bank_controls[[nm]][bad] <- 1 # Auxiliary country only: explicitly recorded MFI proxy.
}
bank_controls$auxiliary_missing_controls <- apply(is.na(bank_controls[, -1]), 1, any)
rownames(bank_controls) <- bank_controls$country
write.csv(bank_controls, file.path(out, "bank_source_controls_eur_bn.csv"), row.names = FALSE)
# Euro-area gross bank loans. Asset rows are authoritative. Mirror deposit margins
# supply column weights; their adjustment is reported explicitly.
auxiliary_margins <- list()
bank_margin <- function(c, item, area) {
  direct <- get(c, item, area, "1210", required = FALSE)
  if (is.finite(direct$value)) {
    return(direct$value)
  }
  if (c %in% MAIN_RES) {
    stop("Missing sample bank margin ", c, item, area)
  }
  # Preserve the distinction between an unpublished auxiliary margin and zero.
  total <- get(c, item, area, "1000", required = FALSE)
  cb <- get(c, item, area, "1100", required = FALSE)
  if (is.finite(total$value) && is.finite(cb$value)) {
    value <- max(0, total$value - cb$value)
    method <- "Auxiliary S122 proxy: MFIs less observed central-bank counterpart, MMF counterparty unresolved"
  } else {
    grossitem <- if (item == "A20G") {
      "A20"
    } else if (item == "L20G") {
      "L20"
    } else {
      item
    }
    fractions <- sapply(setdiff(EA_RES, c), function(j) {
      num <- get(j, item, area, "1210", required = FALSE)$value
      den <- get(j, grossitem, area, if (grepl("G$", item)) {
        "1210"
      } else {
        "1000"
      }, required = FALSE)$value
      if (is.finite(num) && is.finite(den) && den > 0) {
        num / den
      } else {
        NA_real_
      }
    })
    fraction <- median(fractions, na.rm = TRUE)
    if (!is.finite(fraction)) {
      stop("Cannot impute auxiliary margin")
    }
    denominator <- if (grepl("G$", item)) {
      bank_margin(c, grossitem, area)
    } else {
      total$value
    }
    if (!is.finite(denominator)) {
      stop("Missing auxiliary MFI envelope")
    }
    value <- denominator * fraction
    method <- "Auxiliary unpublished margin: median observed EA counterparty or intra-group fraction within published envelope"
  }
  auxiliary_margins[[paste(c, item, area)]] <<- data.frame(
    country = c, item = item,
    area = area, imputed_eur_bn = value, method = method
  )
  value
}
ea_country_count <- length(EA_RES)
empty <- function() {
  matrix(0, ea_country_count, ea_country_count, dimnames = list(
    EA_RES,
    EA_RES
  ))
}
interbank_allocation_prior <- empty()
interbank_caps <- matrix(Inf, ea_country_count, ea_country_count, dimnames = list(
  EA_RES,
  EA_RES
))
loan_asset_margins <- C <- deposit_liability_margins <- GR <- GC <- GD <- setNames(
  numeric(ea_country_count),
  EA_RES
)
for (c in EA_RES) {
  z <- bank_controls[c, ]
  deposit_liability_margins[c] <- bank_margin(c, "A20", "U6") * z$loan_mfi_scale
  loan_asset_margins[c] <- bank_margin(c, "A20", "U5") * z$loan_mfi_scale
  C[c] <- bank_margin(c, "L20", "U5") * z$deposit_scale
  GR[c] <- min(bank_margin(c, "A20G", "U5"), loan_asset_margins[c]) * z$loan_mfi_scale
  GC[c] <- min(bank_margin(c, "L20G", "U5"), C[c]) * z$deposit_scale
  GD[c] <- min(bank_margin(c, "A20G", "U6") * z$loan_mfi_scale, bank_margin(
    c, "L20G",
    "U6"
  ) * z$deposit_scale, deposit_liability_margins[c])
  for (j in setdiff(EA_RES, c)) {
    p <- vres(c, "A20", j, "1000", required = FALSE)
    if (is.finite(p)) {
      interbank_caps[c, j] <- p
    } else {
      p <- NA_real_
    }
    interbank_allocation_prior[c, j] <- p * z$loan_mfi_scale
  }
}
# Unpublished country links use foreign bank deposit margins as borrower-size weights.
missing_prior <- !is.finite(interbank_allocation_prior)
gravity <- outer(loan_asset_margins, C) / sum(C)
interbank_allocation_prior[missing_prior] <- gravity[missing_prior]
Cfit <- C * sum(loan_asset_margins) / sum(C)
allocation_caps <- interbank_caps
diag(allocation_caps) <- 0
gross_mirror_mode <- "hard mirrors after common-total scaling"
N <- tryCatch(fit_residence_transport(
  interbank_allocation_prior, loan_asset_margins,
  Cfit, allocation_caps
), error = function(e) {
  gross_mirror_mode <<- "asset rows and observed bilateral caps hard; deposit mirrors fitted by weighted least squares"
  fit_residence_transport(interbank_allocation_prior, loan_asset_margins, Cfit, allocation_caps,
    soft_columns = TRUE
  )
})
Cfit <- colSums(N)
# All identified asset-side related-office margins are removed. Reconcile funding
# mirrors instead of shrinking identified positions to force an exact match between
# incompatible reporting margins.
Gcols <- GC * sum(GR) / sum(GC)
group_mirror_mode <- "hard funding mirrors after common-total scaling"
group_claims <- tryCatch(fit_residence_transport(N, GR, Gcols, N), error = function(e) {
  group_mirror_mode <<- "all identified asset margins fixed; related funding mirrors fitted within gross bilateral bounds"
  fit_residence_transport(N, GR, Gcols, N, soft_columns = TRUE)
})
Gscale <- 1
diag(N) <- deposit_liability_margins
diag(group_claims) <- GD
inter <- N - group_claims
stopifnot(min(inter) > -1e-07)
write.csv(interbank_allocation_prior, file.path(out, "interbank_allocation_prior_eur_bn.csv"))
write.csv(interbank_caps, file.path(out, "interbank_published_caps_eur_bn.csv"))
write.csv(
  data.frame(
    creditor = rep(EA_RES, ea_country_count), debtor = rep(EA_RES, each = ea_country_count),
    unpublished_bilateral = as.vector(missing_prior)
  ), file.path(out, "interbank_prior_coverage.csv"),
  row.names = FALSE
)
write.csv(N, file.path(out, "gross_ea_interbank_loans_eur_bn.csv"))
write.csv(group_claims, file.path(out, "removed_ea_intragroup_loans_eur_bn.csv"))
write.csv(inter, file.path(out, "net_ea_interbank_loans_eur_bn.csv"))
interbank_margins <- data.frame(
  country = EA_RES, gross_loan_assets = loan_asset_margins,
  reported_deposit_mirror = C, reconciled_deposit_mirror = Cfit, mirror_adjustment = Cfit -
    C, mirror_method = gross_mirror_mode, intragroup_asset_margin = GR, intragroup_funding_margin = GC,
  intragroup_assets_removed = rowSums(group_claims) - diag(group_claims), intragroup_funding_removed = colSums(group_claims) -
    diag(group_claims), intragroup_funding_mirror_adjustment = colSums(group_claims) -
    diag(group_claims) - GC, intragroup_mirror_method = group_mirror_mode, intragroup_feasibility_scale = Gscale
)
write.csv(bind_rows(auxiliary_margins), file.path(out, "auxiliary_ea_margin_imputations.csv"),
  row.names = FALSE
)
write.csv(interbank_margins, file.path(out, "interbank_margin_reconciliation_eur_bn.csv"),
  row.names = FALSE
)
