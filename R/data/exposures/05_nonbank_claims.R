# The Eurostat domestic NBFI source layer omits unpublished S123 holders. Add
# domestic MMF debt securities from SHSS/BSI; foreign NBFI CPIS positions already
# combine financial subsectors and must not receive an MMF top-up.
mmf_coverage <- list()
for (c in MAIN_RES) {
  published <- raw_stocks %>%
    filter(geo == c, sector == "S123", finpos == "ASS", na_item == "F3", is.finite(values))
  if (nrow(published)) {
    stop("MMF domestic source overlap requires explicit reconciliation")
  }
  mmf_sh <- function(s) {
    x <- shss %>%
      filter(REF_AREA == c, REF_SECTOR == "S123", COUNTERPART_AREA == "W2", COUNTERPART_SECTOR ==
        s, VALUATION == "F")
    if (!nrow(x)) {
      return(list(value = NA_real_, key = NA_character_))
    }
    stopifnot(nrow(x) == 1)
    list(value = x$OBS_VALUE, key = x$KEY)
  }
  total <- mmf_sh("S1")
  known_g <- mmf_sh("S13")
  known_b <- mmf_sh("S122")
  known_f <- mmf_sh("S11")
  if (!is.finite(total$value)) {
    stop("Missing domestic MMF aggregate stock")
  }
  targets <- c(government = vres(c, "A30", "U2", "2100", "F"), banks = vres(
    c, "A30",
    "U2", "1000", "F"
  ), private = vres(c, "A30", "U2", "2200", "F"))
  lower <- c(
    if (is.finite(known_g$value)) known_g$value else 0, if (is.finite(known_b$value)) known_b$value else 0,
    if (is.finite(known_f$value)) known_f$value else 0
  )
  cap <- targets
  if (is.finite(known_g$value)) {
    cap[1] <- known_g$value
  }
  if (is.finite(known_b$value)) {
    cap[2] <- known_b$value
  }
  prior <- c(vres(c, "A30", "U6", "2100") / vres(c, "A30", "U2", "2100"), vres(
    c, "A30",
    "U6", "1000"
  ) / vres(c, "A30", "U2", "1000"), (vres(c, "A30", "U6", "2000") - vres(
    c,
    "A30", "U6", "2100"
  )) / (vres(c, "A30", "U2", "2000") - vres(c, "A30", "U2", "2100"))) *
    targets
  dom <- lower + allocate_residence_total(total$value - sum(lower), pmax(
    prior - lower,
    0
  ), pmax(cap - lower, 0))
  M <- node(c, "S12nbfi")
  add(M, node(c, "S13"), dom[1], "debt_securities", "ECB_SHSS_MMF", if (is.finite(known_g$value)) {
    known_g$key
  } else {
    total$key
  }, if (is.finite(known_g$value)) {
    "observed"
  } else {
    "imputed"
  }, "Domestic MMF holdings: SHSS face values with missing sector detail bounded by the SHSS domestic aggregate and BSI MMF portfolio controls")
  add(M, node(c, "S122"), dom[2], "debt_securities", "ECB_SHSS_MMF", if (is.finite(known_b$value)) {
    known_b$key
  } else {
    total$key
  }, if (is.finite(known_b$value)) {
    "observed"
  } else {
    "imputed"
  }, "Domestic MMF holdings added to NBFIs; S123 was unpublished in the cached domestic Eurostat holder slice")
  firm <- if (is.finite(known_f$value)) {
    known_f$value
  } else {
    0
  }
  add(
    M, node(c, "S11"), firm, "debt_securities", "ECB_SHSS_MMF", known_f$key, "observed",
    "Published domestic MMF holding of NFC securities at face value"
  )
  add(
    M, node(c, "S12nbfi"), dom[3] - firm, "debt_securities", "ECB_SHSS_MMF", total$key,
    "imputed", "Remaining domestic MMF private securities after the observed NFC holding; no household debt securities assigned"
  )
  mmf_coverage[[c]] <- data.frame(
    country = c, domestic_face_total = total$value, government = dom[1],
    banks = dom[2], firms = firm, other_financial = dom[3] - firm, foreign_treatment = "No top-up: existing foreign CPIS NBFI portfolio combines financial subsectors"
  )
}
write.csv(bind_rows(mmf_coverage), file.path(out, "mmf_domestic_coverage_eur_bn.csv"),
  row.names = FALSE
)
# Combine resident nonbank holdings with foreign portfolio allocations.
nonbank_holdings <- bind_rows(
  ref %>%
    filter(layer %in% c("eurostat_households", "eurostat_nfc", "eurostat_s12nbfi")),
  cpis
) %>%
  filter(instrument == "debt_securities", s_from %in% c("S11", "S12nbfi", "S14"), s_to %in%
    c("S11", "S122", "S12nbfi", "S13", "S14"))
for (i in seq_len(nrow(nonbank_holdings))) {
  x <- nonbank_holdings[i, ]
  add(
    paste(x$c_from, x$s_from, sep = ":"), paste(x$c_to, x$s_to, sep = ":"), x$claim / 1000,
    "debt_securities", x$source, if (x$source == "IMF_CPIS_RESIDENCE_RECONCILED") {
      paste(x$c_from, x$c_to, sep = " -> ")
    } else {
      NA_character_
    }, if (isTRUE(x$estimated)) {
      "imputed"
    } else {
      "observed"
    }, if (x$source == "IMF_CPIS_RESIDENCE_RECONCILED") {
      "CPIS sector allocation prior projected to S1 country-pair total and reconciled holder/issuer margins; subsequent issuer valuation/stock caps disclosed separately"
    } else {
      "Corrected resident-sector security stock; issuer valuation and geographic margins reconciled below"
    }
  )
}
# CPIS government portfolios are separate resident holders. Missing holder sectors
# remain unknown; their known foreign country is retained in the key.
cpis_extra <- cpis %>%
  filter(s_from %in% c("S13", "UNALLOC"))
for (i in seq_len(nrow(cpis_extra))) {
  x <- cpis_extra[i, ]
  add(
    if (x$s_from == "UNALLOC") {
      "UNALLOC:HOLDER"
    } else {
      paste(x$c_from, x$s_from, sep = ":")
    }, paste(x$c_to, x$s_to, sep = ":"), x$claim / 1000,
    "debt_securities", x$source, paste(x$c_from, x$c_to, sep = " -> "), "imputed",
    "CPIS sector prior projected to country-pair total and reconciled holder/issuer margins; missing holder sector remains explicit"
  )
}
# Allocate published domestic government and central-bank portfolios using
# non-overlapping counterpart sectors.
government_security_rows <- raw_stocks %>%
  filter(finpos == "ASS", na_item == "F3", sector %in% c("S13", "S121"))
for (c in MAIN_RES) {
  for (holder in c("S13", "S121")) {
    r <- government_security_rows %>%
      filter(geo == c, sector == holder)
    for (s in c("S11", "S122", "S12nbfi", "S13", "S14")) {
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
      vals <- r$values[r$sector2 %in% codes]
      value <- if (length(vals) && any(is.finite(vals))) {
        sum(vals, na.rm = TRUE) / 1000
      } else {
        NA_real_
      }
      if (!is.finite(value) && holder == "S121" && s == "S13") {
        bundle <- raw_stocks %>%
          filter(
            geo == c, finpos == "ASS", na_item == "F3", sector == "S121_S122_S123",
            sector2 == "S13"
          )
        bank_market <- shss_get(c, "S13", "M")$value
        mmf_market <- shss %>%
          filter(REF_AREA == c, REF_SECTOR == "S123", COUNTERPART_AREA == "W2", COUNTERPART_SECTOR ==
            "S13", VALUATION == "M")
        if (nrow(bundle) == 1 && is.finite(bank_market) && nrow(mmf_market) == 1) {
          value <- bundle$values / 1000 - bank_market - mmf_market$OBS_VALUE
        }
      }
      if (!is.finite(value)) {
        next
      }
      from <- if (holder == "S121") {
        "EA:S121"
      } else {
        node(c, "S13")
      }
      add(
        from, node(c, s), value, "debt_securities", "EUROSTAT_NASQ_STK_ASS", NA_character_,
        "derived", "Published domestic government/central-bank security holding; sector-proxy and issuer valuation reconciliation"
      )
    }
  }
}
