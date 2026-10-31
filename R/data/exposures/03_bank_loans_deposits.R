# Exposure register. Separate unmatched issuers from unmatched holders.
ledger <- list()
counter <- 0L
add <- function(
  from, to, amount, instrument, source, keys = NA_character_, status = "observed",
  method = "Direct published stock", raw = amount, in_matrix = TRUE, lower = NA_real_,
  upper = NA_real_
) {
  if (!is.finite(amount) || amount < -1e-07) {
    stop("Invalid exposure: ", from, " -> ", to, " ", instrument, " ", amount)
  }
  if (amount < 1e-10) {
    return(invisible(NULL))
  }
  counter <<- counter + 1L
  ledger[[counter]] <<- data.frame(
    id = sprintf("RES%05d", counter), from = from, to = to,
    instrument = instrument, raw_eur_bn = raw, amount_eur_bn = amount, source = source,
    source_keys = paste(keys, collapse = "; "), status = status, method = method,
    in_matrix = in_matrix, lower_eur_bn = lower, upper_eur_bn = upper, stringsAsFactors = FALSE
  )
}
node <- function(c, s) paste(ISO3_RES[[c]], s, sep = ":")
# Recover unpublished domestic financial/government depositor splits from EA totals
# minus published foreign-EA country positions. Residual uncertainty is bounded by
# the unmatched foreign non-bank deposit budget.
deposit_domestic <- list()
for (c in MAIN_RES) {
  ea_m <- vres(c, "L20", "U2", "2210") + vres(c, "L20", "U2", "2220")
  ea_g <- vres(c, "L20", "U2", "2100")
  known_m <- known_g <- 0
  for (j in setdiff(EA_RES, c)) {
    gg <- vres(c, "L20", j, "2100", required = FALSE)
    if (is.finite(gg)) {
      known_g <- known_g + max(0, gg)
    }
    for (group in list(c("2210", "2260", "2270"), c("2220", "2221", "2222"))) {
      total <- vres(c, "L20", j, group[1], required = FALSE)
      if (is.finite(total)) {
        known_m <- known_m + max(0, total)
      } else {
        vals <- sapply(group[-1], function(code) vres(c, "L20", j, code, required = FALSE))
        known_m <- known_m + sum(pmax(vals[is.finite(vals)], 0))
      }
    }
  }
  foreign_residual <- vres(c, "L20", "U5", "2000") - vres(c, "L20", "U5", "2240") -
    vres(c, "L20", "U5", "2250")
  published_m <- known_m
  published_g <- known_g
  foreign_scale <- 1
  unknown <- max(0, foreign_residual - known_m - known_g)
  if (known_m + known_g > foreign_residual) {
    foreign_scale <- foreign_residual / (known_m + known_g)
    known_m <- known_m * foreign_scale
    known_g <- known_g * foreign_scale
  }
  weights <- c(ea_m, ea_g) / sum(ea_m, ea_g)
  dom <- pmax(c(ea_m - known_m, ea_g - known_g) - unknown * weights, 0)
  dom_control <- vres(c, "L20", "U6", "2000") - vres(c, "L20", "U6", "2240") - vres(
    c,
    "L20", "U6", "2250"
  )
  domestic_scale <- dom_control / sum(dom)
  dom <- dom * domestic_scale
  deposit_domestic[[c]] <- data.frame(
    country = c, nonbank_financial_deposits = dom[1],
    government_deposits = dom[2], unmatched_foreign_sector_budget = unknown, domestic_residual_control = dom_control,
    published_foreign_financial = published_m, published_foreign_government = published_g,
    foreign_control = foreign_residual, foreign_sector_scale = foreign_scale, domestic_sector_scale = domestic_scale,
    status = if (unknown > 1e-08) {
      "imputed"
    } else if (abs(foreign_scale - 1) > 1e-08 || abs(domestic_scale - 1) > 1e-08) {
      "reconciled"
    } else {
      "derived"
    }, financial_lower = max(0, ea_m - known_m - unknown), financial_upper = ea_m -
      known_m
  )
}
write.csv(bind_rows(deposit_domestic), file.path(out, "domestic_depositor_reconciliation_eur_bn.csv"),
  row.names = FALSE
)
# Observed and allocated bank loans/deposits for sample non-bank sectors.
for (c in MAIN_RES) {
  z <- bank_controls[c, ]
  B <- node(c, "S122")
  for (j in MAIN_RES) {
    area <- if (j == c) {
      "U6"
    } else {
      j
    }
    ag <- get(c, "A20", area, "2100", required = FALSE)
    af <- get(c, "A20", area, "2240")
    ah <- get(c, "A20", area, "2250")
    gov_imputed <- !is.finite(ag$value)
    if (gov_imputed) {
      nb <- vres(c, "A20", area, "2000")
      gov_other <- vres(c, "A20", "U2", "2100") - vres(c, "A20", "U6", "2100")
      nb_other <- vres(c, "A20", "U2", "2000") - vres(c, "A20", "U6", "2000")
      ag <- list(
        value = min(max(0, nb - af$value - ah$value), nb * gov_other / nb_other),
        key = "Unpublished bilateral government loans allocated from foreign EA government/nonbank margin"
      )
    }
    ap <- get(c, "A20", area, "2200", required = FALSE)
    if (!is.finite(ap$value)) {
      ap <- list(value = vres(c, "A20", area, "2000") - ag$value, key = kres(
        c,
        "A20", area, "2000"
      ))
    }
    am <- ap$value - af$value - ah$value
    if (am < -1e-06) {
      stop("Negative private loan sector residual")
    }
    for (s in c("S11", "S14", "S12nbfi")) {
      a <- switch(s,
        S11 = af$value,
        S14 = ah$value,
        S12nbfi = am
      )
      sk <- switch(s,
        S11 = af$key,
        S14 = ah$key,
        S12nbfi = paste(ap$key, af$key,
          ah$key,
          sep = "; "
        )
      )
      fac <- z$loan_private_scale
      add(B, node(j, s), a * fac, "loans", "ECB_BSI", sk, if (abs(fac - 1) > 1e-10 ||
        (s == "S12nbfi" && gov_imputed)) {
        "imputed"
      } else if (s == "S12nbfi") {
        "derived"
      } else {
        "observed"
      }, "MFI private loans allocated to S122 using the published S122/MFI private-loan margin; residual private sector is NBFIs",
      a,
      lower = max(0, a - (vres(c, "A20", "U2", "2200") - z$loans_private)),
      upper = a
      )
    }
    add(B, node(j, "S13"), ag$value * z$loan_gov_scale, "government_loans", "ECB_BSI",
      ag$key, if (gov_imputed) {
        "imputed"
      } else {
        "derived"
      }, "Government loans kept outside the bond-default matrix",
      ag$value,
      in_matrix = FALSE
    )
    # Household and firm funding never absorb residual leverage adjustments.
    for (s in c("S11", "S14")) {
      code <- if (s == "S11") {
        "2240"
      } else {
        "2250"
      }
      a <- get(c, "L20", area, code)
      add(
        node(j, s), B, a$value * z$deposit_scale, "deposits", "ECB_BSI", a$key,
        if (abs(z$deposit_scale - 1) < 1e-10) {
          "observed"
        } else {
          "imputed"
        }, "Published depositor-sector stock, adjusted to S122 margin",
        a$value
      )
    }
    finance_sum <- function(total, components) {
      t <- get(c, "L20", area, total, required = FALSE)
      if (is.finite(t$value)) {
        return(t)
      }
      pieces <- lapply(components, function(k) get(c, "L20", area, k, required = FALSE))
      if (any(!is.finite(sapply(pieces, `[[`, "value")))) {
        return(list(value = NA_real_, key = NA_character_))
      }
      list(value = sum(sapply(pieces, `[[`, "value")), key = paste(sapply(
        pieces,
        `[[`, "key"
      ), collapse = "; "))
    }
    f1 <- finance_sum("2210", c("2260", "2270"))
    f2 <- finance_sum("2220", c("2221", "2222"))
    if (j != c && is.finite(f1$value) && is.finite(f2$value)) {
      add(
        node(j, "S12nbfi"), B, (f1$value + f2$value) * z$deposit_scale, "deposits",
        "ECB_BSI", c(f1$key, f2$key), "derived", "Sum of disjoint published non-MFI financial depositor sectors",
        f1$value + f2$value
      )
    }
    gg <- get(c, "L20", area, "2100", required = FALSE)
    if (!is.finite(gg$value) && j == c) {
      gg <- get(c, "L20", area, "2110", required = FALSE)
    }
    if (j != c && is.finite(gg$value)) {
      add(
        node(j, "S13"), B, gg$value * z$deposit_scale, "deposits", "ECB_BSI",
        gg$key, "derived", "Published government depositor sector; central-government only where broader total unavailable",
        gg$value
      )
    }
    if (j == c) {
      dm <- deposit_domestic[[c]]
      for (sector in c("S12nbfi", "S13")) {
        add(
          node(c, sector), B, if (sector ==
            "S12nbfi") {
            dm$nonbank_financial_deposits
          } else {
            dm$government_deposits
          }, "deposits", "ECB_BSI_DOMESTIC_SECTOR_RECONCILIATION",
          c(kres(c, "L20", "U2", "2210"), kres(c, "L20", "U2", "2220"), kres(
            c, "L20",
            "U2", "2100"
          )), dm$status, "EA financial/government depositor total less published foreign-EA sectors; unmatched foreign budget allocated within the observed domestic control"
        )
      }
      # MFI counterpart aggregate includes MMF depositors, which are NBFIs in our
      # sector map and must not be recoded as lending banks.
      mmf_dep <- vres(c, "L20", "U6", "1000") - vres(c, "L20", "U6", "1100") -
        vres(c, "L20", "U6", "1210")
      add(node(c, "S12nbfi"), B, max(0, mmf_dep), "deposits", "ECB_BSI", c(kres(
        c,
        "L20", "U6", "1000"
      ), kres(c, "L20", "U6", "1210"), kres(
        c, "L20", "U6",
        "1100"
      )), "derived", "Domestic MMF depositor residual within the MFI counterpart aggregate")
    }
  }
  # Bank-to-bank lending across all EA countries, other offices aggregated outside
  # sample.
  for (j in EA_RES) {
    target <- if (j %in% MAIN_RES) {
      node(j, "S122")
    } else {
      "ROW:S2"
    }
    add(B, target, inter[c, j], "interbank_loans", "ECB_BSI_RECONCILED", c(kres(c,
      "A20", if (c == j) "U6" else j, if (c == j) "1210" else "1000",
      required = FALSE
    )),
    "imputed", "S122 row margins; deposit-mirror column weights; bounded allocation of intra-group positions removed",
    N[c, j],
    lower = 0, upper = N[c, j]
    )
    if (!j %in% MAIN_RES) {
      add(
        "ROW:S2", B, inter[j, c], "interbank_deposits", "ECB_BSI_RECONCILED",
        NA_character_, "imputed", "Other EA resident banks, fitted gross loans less fitted related-office positions",
        N[j, c]
      )
    }
  }
  made <- bind_rows(ledger)
  gov_sample <- sum(made$amount_eur_bn[made$from == B & made$instrument == "government_loans"])
  add(B, "ROW:S2", z$loans_gov - gov_sample, "government_loans", "ECB_BSI", kres(
    c,
    "A20", "U2", "2100", "B"
  ), "derived", "Other EA government loans outside the bond-default experiment",
  in_matrix = FALSE
  )
  # All EA central-bank counterparties are separated from credit institutions.
  cb <- get(c, "A20", "U2", "1100")
  add(
    B, "EA:S121", cb$value * z$loan_mfi_scale, "central_bank_deposits", "ECB_BSI",
    cb$key, "derived", "S122 creditor allocation of central-bank deposits", cb$value
  )
  cbf <- get(c, "L20", "U2", "1100")
  add(
    "EA:S121", B, cbf$value * z$deposit_scale, "central_bank_funding", "ECB_BSI",
    cbf$key, "derived", "All reported central-bank deposit funding; monetary-policy refinancing is a disclosed subset",
    cbf$value
  )
  # Loans to non-sample EA private borrowers are a derived geographic residual.
  made <- bind_rows(ledger)
  private_sample <- sum(made$amount_eur_bn[made$from == B & made$instrument == "loans"])
  add(
    B, "ROW:S2", z$loans_private - private_sample, "loans", "ECB_BSI", "BSI S122 private-loan margin",
    "derived", "Other EA private borrowers: S122 aggregate less sample borrower sectors"
  )
  # Pure S122 outside-EA loans and bonds: remove the published MMF total by
  # instrument.
  for (item in c("A20", "A30")) {
    raw <- vres(c, item, "U4", "1000") + vres(c, item, "U4", "2000")
    mmf <- get(c, item, "U4", "0000", "F", required = FALSE)
    if (!is.finite(mmf$value)) {
      stop("Missing outside-EA MMF adjustment")
    }
    add(
      B, "ROW:S2", raw - mmf$value, if (item == "A20") {
        "outside_loans"
      } else {
        "outside_debt_securities"
      }, "ECB_BSI", c(kres(c, item, "U4", "1000"), kres(
        c,
        item, "U4", "2000"
      ), mmf$key), "derived", "Outside-EA MFI instrument total less published MMF instrument total",
      raw
    )
  }
  # Other EA non-bank depositors have an observed geographic aggregate.
  ea_private_deposits <- z$deposits_ea - z$deposits_mfi
  domestic_nonbank <- vres(c, "L20", "U6", "2000") * z$deposit_scale
  sample_foreign_nonbank <- sum(sapply(setdiff(MAIN_RES, c), function(j) {
    vres(
      c, "L20",
      j, "2000"
    )
  })) * z$deposit_scale
  add(
    "ROW:S2", B, ea_private_deposits - domestic_nonbank - sample_foreign_nonbank,
    "deposits", "ECB_BSI", "S122 non-MFI deposit margins and sample counterparty totals",
    "derived", "Non-sample EA non-bank depositors only; no unknown domestic holders assigned to ROW"
  )
  outside_raw <- vres(c, "L20", "U4", "1000") + vres(c, "L20", "U4", "2000")
  # MMF outside funding is not silently zero: allocate with the published
  # external-liability controls.
  mmfo <- get(c, "LXG", "U4", "0000", "F", required = FALSE)
  if (!is.finite(mmfo$value)) {
    stop("Missing MMF external liability control")
  }
  outside_dep <- outside_raw * z$outside_liabilities / (z$outside_liabilities + mmfo$value)
  add("ROW:S2", B, outside_dep, "outside_deposits", "ECB_BSI", c(kres(
    c, "L20", "U4",
    "1000"
  ), kres(c, "L20", "U4", "2000"), mmfo$key), "imputed", "Outside-EA deposit funding allocated between S122 and MMFs using external-liability margins",
  outside_raw,
  lower = max(0, outside_raw - mmfo$value), upper = min(
    outside_raw,
    z$outside_liabilities
  )
  )
}
