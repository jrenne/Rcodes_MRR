ledger <- bind_rows(ledger)
# Issuer stocks: S1 domestic and S2 foreign are distinct counterpart totals.  S2 is
# not the total liability of an issuer.
issuer_total <- function(c, s, geo) {
  if (geo == "S1") {
    rr <- raw_stocks %>%
      filter(.data$geo == c, finpos == "ASS", na_item == "F3", sector == "S1")
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
    if (s == "S12nbfi" && !any(rr$sector2 == "S125_S126_S127" & is.finite(rr$values))) {
      codes <- c(setdiff(codes, "S125_S126_S127"), "S125", "S126", "S127")
    }
    vals <- rr$values[rr$sector2 %in% codes]
    if (!length(vals) || all(is.na(vals))) {
      return(NA_real_)
    }
    return(sum(vals, na.rm = TRUE) / 1000)
  }
  rows <- raw_stocks %>%
    filter(.data$geo == c, finpos == "LIAB", na_item == "F3", sector2 == .env$geo)
  codes <- switch(s,
    S11 = "S11",
    S122 = "S121_S122_S123",
    S12nbfi = c(
      "S123", "S124",
      "S125_S126_S127", "S128", "S129"
    ),
    S13 = "S13",
    S14 = "S14_S15"
  )
  if (s == "S12nbfi" && !any(rows$sector == "S125_S126_S127" & is.finite(rows$values))) {
    codes <- c(setdiff(codes, "S125_S126_S127"), "S125", "S126", "S127")
  }
  vals <- rows$values[rows$sector %in% codes]
  if (!length(vals) || all(is.na(vals))) {
    return(NA_real_)
  }
  sum(vals, na.rm = TRUE) / 1000
}
self_government <- ledger %>%
  filter(from == to, grepl(":S13$", from), instrument == "debt_securities")
write.csv(self_government, file.path(out, "government_self_holdings_excluded_eur_bn.csv"),
  row.names = FALSE
)
self_ids <- which(ledger$from == ledger$to & grepl(":S13$", ledger$from) & ledger$instrument ==
  "debt_securities")
ledger$in_matrix[self_ids] <- FALSE
ledger$method[self_ids] <- paste(ledger$method[self_ids], "Excluded: intra-S13 holding cancels under the EDP consolidated government liability anchor")
issuer_reconciliations <- list()
rrn <- 0L
for (c in MAIN_RES) {
  for (s in c("S11", "S122", "S12nbfi", "S13", "S14")) {
    issuer <- node(c, s)
    dom <- issuer_total(c, s, "S1")
    foreign <- issuer_total(c, s, "S2")
    if (!is.finite(dom) || !is.finite(foreign)) {
      stop("Missing issuer domestic/foreign stock ", issuer)
    }
    if (s == "S13") {
      dom <- dom - sum(self_government$raw_eur_bn[self_government$from == issuer])
    }
    market_total <- dom + foreign
    target <- if (s == "S122") {
      bank_controls[c, "bonds_issued"]
    } else if (s == "S13") {
      sovereign_totals[ISO3_RES[[c]], "total_liab"] / 1000
    } else {
      market_total
    }
    if (!is.finite(target)) {
      stop("Missing issuer total ", issuer)
    }
    factor <- if (market_total > 0) {
      target / market_total
    } else {
      1
    }
    ids <- which(ledger$to == issuer & ledger$instrument == "debt_securities" & ledger$in_matrix)
    # Convert national-account/CPIS market stocks to nominal issuer controls.
    for (k in ids) {
      if (!grepl("^ECB_", ledger$source[k]) && abs(factor - 1) > 1e-10) {
        ledger$amount_eur_bn[k] <- ledger$amount_eur_bn[k] * factor
        if (ledger$status[k] == "observed") {
          ledger$status[k] <- "derived"
        }
        ledger$method[k] <- paste(ledger$method[k], sprintf(
          "Issuer-average nominal/market conversion %.8f",
          factor
        ))
      }
    }
    for (home in c(TRUE, FALSE)) {
      # EA central-bank domestic portfolios are identified by the source issuer
      # country.
      idx <- ids[(substr(ledger$from[ids], 1, 3) == ISO3_RES[[c]]) | ledger$from[ids] ==
        "EA:S121"]
      if (!home) {
        idx <- setdiff(ids, idx)
      }
      cap <- if (home) {
        dom * factor
      } else {
        foreign * factor
      }
      before <- sum(ledger$amount_eur_bn[idx])
      after <- before
      if (before > cap + 1e-08) {
        uncertain <- idx[ledger$status[idx] == "imputed"]
        remove <- min(before - cap, sum(ledger$amount_eur_bn[uncertain]))
        if (remove > 0) {
          ledger$amount_eur_bn[uncertain] <- ledger$amount_eur_bn[uncertain] * (1 -
            remove / sum(ledger$amount_eur_bn[uncertain]))
        }
        left <- sum(ledger$amount_eur_bn[idx]) - cap
        if (left > 1e-08) {
          ledger$amount_eur_bn[idx] <- ledger$amount_eur_bn[idx] * cap / sum(ledger$amount_eur_bn[idx])
        }
        changed <- idx[abs(ledger$amount_eur_bn[idx] - ledger$raw_eur_bn[idx]) >
          1e-08]
        ledger$status[changed] <- ifelse(ledger$status[changed] == "imputed", "imputed",
          "reconciled"
        )
        ledger$method[idx] <- paste(ledger$method[idx], "Issuer geographic stock cap: imputed cells adjusted first, then remaining holdings proportionally if necessary")
        after <- sum(ledger$amount_eur_bn[idx])
      }
      # Unknown domestic holders are not relabelled non-residents.
      gap <- max(0, cap - after)
      if (gap > 1e-09) {
        ledger <- bind_rows(ledger, data.frame(
          id = paste0("ISSUER_", issuer, if (home) {
            "_DOM"
          } else {
            "_FOR"
          }), from = if (home) {
            "UNALLOC:HOLDER"
          } else {
            "ROW:S2"
          }, to = issuer, instrument = "debt_securities", raw_eur_bn = gap,
          amount_eur_bn = gap, source = "ISSUER_STOCK_RECONCILIATION", source_keys = "Eurostat F3 liabilities S1/S2; BSI L40 or EDP F3 issuer control",
          status = if (home) {
            "unallocated"
          } else {
            "derived"
          }, method = if (home) {
            "Unallocated domestic holder, not household or ROW"
          } else {
            "Outside-sample foreign holders: non-resident issuer stock less represented sample claims"
          },
          in_matrix = TRUE, lower_eur_bn = NA_real_, upper_eur_bn = NA_real_
        ))
      }
      rrn <- rrn + 1L
      issuer_reconciliations[[rrn]] <- data.frame(
        issuer = issuer, holder_geography = if (home) {
          "domestic"
        } else {
          "foreign"
        }, market_stock = if (home) {
          dom
        } else {
          foreign
        }, nominal_or_book_control = cap, conversion_factor = factor, represented_before_cap = before,
        represented_after_cap = after, holdings_adjustment = after - before, unallocated_or_outside_holder = gap
      )
    }
  }
}
# Add reconciliation residuals needed to match observed bank instrument totals.  Bank
# assets outside loans and debt securities remain non-network resources.
rows_balance <- list()
for (c in MAIN_RES) {
  z <- bank_controls[c, ]
  B <- node(c, "S122")
  gA <- sum(group_claims[c, ])
  gL <- sum(group_claims[, c])
  ea_loans_in <- sum(ledger$amount_eur_bn[ledger$from == B & ledger$instrument %in%
    c("loans", "government_loans", "interbank_loans", "central_bank_deposits")])
  # Gross related-office loans were removed from the interbank part above.
  missing_loan <- z$loans_ea - gA - ea_loans_in
  if (missing_loan < -1e-06) {
    stop("EA bank loan envelope oversubscribed ", c, ": ", missing_loan)
  }
  if (missing_loan > 1e-08) {
    ledger <- bind_rows(ledger, data.frame(
      id = paste0("UNALLOC_LOANS_", c), from = B,
      to = "UNALLOC:ISSUER", instrument = "loans", raw_eur_bn = missing_loan, amount_eur_bn = missing_loan,
      source = "BSI_S122_LOAN_CONTROL", source_keys = kres(
        c, "A20", "U2", "0000",
        "B"
      ), status = "unallocated", method = "Unallocated EA loan counterpart: other MFIs/MMFs and reported classification differences",
      in_matrix = TRUE, lower_eur_bn = NA_real_, upper_eur_bn = NA_real_
    ))
  }
  bonds_in <- sum(ledger$amount_eur_bn[ledger$from == B & ledger$instrument == "debt_securities"])
  missing_bond <- z$bonds_ea - bonds_in
  if (missing_bond < -1e-06) {
    stop("EA securities envelope exceeded ", c)
  }
  if (missing_bond > 1e-08) {
    ledger <- bind_rows(ledger, data.frame(
      id = paste0("UNALLOC_BONDS_", c), from = B,
      to = "UNALLOC:ISSUER", instrument = "debt_securities", raw_eur_bn = missing_bond,
      amount_eur_bn = missing_bond, source = "BSI_S122_SECURITIES_CONTROL", source_keys = kres(
        c,
        "A30", "U2", "0000", "B"
      ), status = "unallocated", method = "Issuer allocation reconciliation within observed S122 EA securities margin",
      in_matrix = TRUE, lower_eur_bn = NA_real_, upper_eur_bn = NA_real_
    ))
  }
  # All funding other than bank debt securities: close against published bank
  # deposit, other liability and external liability controls, after group removal.
  liabilities_target <- z$assets - z$capital - gL
  liabilities_in <- sum(ledger$amount_eur_bn[ledger$to == B & ledger$in_matrix])
  missing_liability <- liabilities_target - liabilities_in
  if (missing_liability < -1e-06) {
    stop("Bank funding oversubscribed ", c, ": ", missing_liability)
  }
  ea_assigned <- sum(ledger$amount_eur_bn[ledger$to == B & ledger$instrument %in% c(
    "deposits",
    "interbank_loans", "interbank_deposits", "central_bank_funding"
  )])
  outside_assigned <- sum(ledger$amount_eur_bn[ledger$to == B & ledger$instrument ==
    "outside_deposits"])
  residual_parts <- c(ea_deposits = z$deposits_ea - gL - ea_assigned, outside_other = z$outside_liabilities -
    outside_assigned, other_liabilities = z$other_liabilities)
  if (any(residual_parts < -1e-06) || abs(sum(residual_parts) - missing_liability) >
    1e-06) {
    stop("Funding component reconciliation failed ", c)
  }
  for (part in names(residual_parts)) {
    if (residual_parts[part] > 1e-08) {
      ledger <- bind_rows(ledger, data.frame(
        id = paste0(
          "UNALLOC_FUNDING_", c, "_",
          part
        ), from = if (part == "outside_other") {
          "ROW:S2"
        } else {
          "UNALLOC:HOLDER"
        }, to = B, instrument = if (part == "ea_deposits") {
          "unallocated_deposits"
        } else {
          part
        }, raw_eur_bn = residual_parts[part], amount_eur_bn = residual_parts[part],
        source = "BSI_S122_LIABILITY_CONTROL", source_keys = if (part == "other_liabilities") {
          kres(c, "L70", "Z5", "0000", "B")
        } else if (part == "outside_other") {
          kres(c, "LXG", "U4", "0000", "B")
        } else {
          kres(c, "L20", "U2", "0000", "B")
        }, status = if (part == "outside_other") {
          "derived"
        } else {
          "unallocated"
        }, method = if (part == "ea_deposits") {
          "Unallocated EA depositors: MMF foreign geography and mirror reconciliation; never household funding"
        } else if (part == "outside_other") {
          "Observed outside-EA external liabilities less separately represented deposit funding; cash-flow class unresolved"
        } else {
          "Published other-liability stock, creditor and cash-flow/rank treatment unresolved; not imputed household deposits"
        },
        in_matrix = TRUE, lower_eur_bn = NA_real_, upper_eur_bn = NA_real_
      ))
    }
  }
  # Non-debt assets are excluded from the exposure matrix rather than being converted
  # to fictitious claims. Government loans are outside the bond shock.
  asset_in <- sum(ledger$amount_eur_bn[ledger$from == B & ledger$in_matrix])
  govt_out <- sum(ledger$amount_eur_bn[ledger$from == B & ledger$instrument == "government_loans"])
  nondebt <- z$assets - gA - asset_in - govt_out
  if (nondebt < -1e-06) {
    stop("Bank assets oversubscribed ", c, ": ", nondebt)
  }
  net_asset <- max(0, gA - gL)
  net_funding <- max(0, gL - gA)
  adjusted_assets <- z$assets - min(gA, gL)
  adjusted_debt <- z$assets - z$capital - min(gA, gL)
  household <- sum(ledger$amount_eur_bn[ledger$to == B & grepl(":S14$", ledger$from)])
  rows_balance[[c]] <- data.frame(
    country = ISO3_RES[[c]], reported_assets = z$assets,
    reported_liabilities = z$assets - z$capital, reported_capital = z$capital, gross_intragroup_assets_removed = gA,
    gross_intragroup_funding_removed = gL, net_group_asset_bridge = net_asset, net_group_funding_bridge = net_funding,
    adjusted_assets = adjusted_assets, adjusted_liabilities = adjusted_debt, matrix_assets = asset_in,
    government_loans_outside_bond_shock = govt_out, non_network_and_remaining_assets = nondebt,
    matrix_liabilities = liabilities_target, household_funding = household, domestic_interbank = sum(ledger$amount_eur_bn[ledger$from ==
      B & ledger$to == B & ledger$in_matrix]), domestic_interbank_loans = inter[
      c,
      c
    ], domestic_sovereign_bonds = sum(ledger$amount_eur_bn[ledger$from ==
      B & ledger$to == node(c, "S13") & ledger$in_matrix]), central_bank_assets = sum(ledger$amount_eur_bn[ledger$from ==
      B & ledger$to == "EA:S121"]), central_bank_funding = sum(ledger$amount_eur_bn[ledger$to ==
      B & ledger$from == "EA:S121"]), monetary_policy_refinancing = sum(ref$claim[ref$layer ==
      "ecb_refinancing" & ref$c_to == ISO3_RES[[c]]], na.rm = TRUE) / 1000, unallocated_other_funding = z$other_liabilities,
    unallocated_ea_deposits = residual_parts["ea_deposits"], outside_other_funding = residual_parts["outside_other"],
    unallocated_funding = sum(ledger$amount_eur_bn[ledger$from == "UNALLOC:HOLDER" &
      ledger$to == B]), balance_check = asset_in + govt_out + nondebt + net_asset -
      liabilities_target - net_funding - z$capital
  )
}
stopifnot(all(is.finite(ledger$amount_eur_bn)), min(ledger$amount_eur_bn) > -1e-08)
model_sectors <- c("S11", "S122", "S12nbfi", "S13", "S14")
account_names <- c(
  unlist(lapply(MAIN_RES, function(c) node(c, model_sectors))), "EA:S121",
  "ROW:S2", "UNALLOC:ISSUER", "UNALLOC:HOLDER"
)
mat <- matrix(0, length(account_names), length(account_names), dimnames = list(
  account_names,
  account_names
))
for (i in which(ledger$in_matrix)) {
  mat[ledger$from[i], ledger$to[i]] <- mat[
    ledger$from[i],
    ledger$to[i]
  ] + ledger$amount_eur_bn[i]
}
bs <- bind_rows(rows_balance)
rownames(bs) <- NULL
stopifnot(max(abs(bs$balance_check)) < 1e-06)
# SHSS bank-security holdings are a benchmark, not a replacement for BSI-net
# holdings. The gap is not identified exclusively as retained own securities.
security_benchmark <- bind_rows(lapply(MAIN_RES, function(c) {
  data.frame(
    country = ISO3_RES[[c]],
    shss_domestic_bank_securities_face = shss_get(c, "S122")$value, bsi_matched_domestic_bank_securities = sum(ledger$amount_eur_bn[ledger$from ==
      node(c, "S122") & ledger$to == node(c, "S122") & ledger$instrument == "debt_securities"]),
    source_definition = "SHSS is gross of own holdings; BSI is net. Valuation and geographic allocation also differ."
  )
}))
write.csv(security_benchmark, file.path(out, "shss_bsi_bank_securities_benchmark_eur_bn.csv"),
  row.names = FALSE
)
write.csv(mat, file.path(out, "exposure_matrix.csv"))
write.csv(ledger, file.path(out, "exposure_register.csv"), row.names = FALSE)
write.csv(
  ledger %>%
    filter(status %in% c("imputed", "reconciled", "unallocated")), file.path(out, "imputed_and_reconciled_exposures.csv"),
  row.names = FALSE
)
write.csv(bs, file.path(out, "bank_balance_sheets.csv"), row.names = FALSE)
write.csv(bind_rows(issuer_reconciliations), file.path(out, "issuer_reconciliation_eur_bn.csv"),
  row.names = FALSE
)
write.csv(ledger %>%
  group_by(in_matrix, instrument, status) %>%
  summarise(positions = n(), eur_bn = sum(amount_eur_bn), .groups = "drop"), file.path(
  out,
  "exposure_status_summary.csv"
), row.names = FALSE)
