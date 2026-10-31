# All government securities and bank-to-bank securities use resident S122 portfolio
# controls. Domestic SHSS face-value holdings take priority.
government_holdings <- matrix(0, length(MAIN_RES), length(EA_RES), dimnames = list(
  MAIN_RES,
  EA_RES
))
bankbond <- list()
privatebond <- list()
sovereign_totals <- read.csv(file.path(out, "government_nominal_bonds.csv"))
rownames(sovereign_totals) <- sovereign_totals$c_to
for (c in MAIN_RES) {
  z <- bank_controls[c, ]
  other <- setdiff(EA_RES, c)
  B <- node(c, "S122")
  for (sector in c("S13", "S122")) {
    target <- if (sector == "S13") {
      z$bonds_gov
    } else {
      z$bonds_mfi
    }
    # SHSS can include own-retained bank securities excluded from BSI. Do not splice
    # those gross holdings into the BSI-net bank-to-bank block.
    domsh <- if (sector == "S122") {
      list(value = NA_real_, key = NA_character_)
    } else {
      shss_get(c, sector)
    }
    if (is.finite(domsh$value)) {
      dom <- domsh$value
      st <- "observed"
      sk <- domsh$key
      method <- "SHSS S122 domestic face-value holding"
    } else {
      poolcode <- if (sector == "S13") {
        "2100"
      } else {
        "1000"
      }
      dom <- vres(c, "A30", "U6", poolcode) * target / vres(c, "A30", "U2", poolcode)
      st <- "imputed"
      sk <- kres(c, "A30", "U6", poolcode)
      method <- "Missing domestic S122 holding allocated from MFI geography within the S122 portfolio control; book stock is face-value proxy"
    }
    if (dom > target) {
      stop("Domestic face-value holding exceeds S122 portfolio control")
    }
    add(B, node(c, sector), dom, "debt_securities", "ECB_SHSS_BSI", sk, st, method)
    weights <- sapply(other, function(j) {
      vres(c, "A30", j, if (sector == "S13") {
        "2000"
      } else {
        "1000"
      }, required = FALSE)
    })
    weights[!is.finite(weights)] <- 1e-05
    weights <- pmax(weights, 0)
    if (sector == "S13") {
      # Sector composition prior from the residence CPIS portfolio, not CBS levels.
      for (j in intersect(other, MAIN_RES)) {
        pp <- cpis %>%
          filter(
            c_from == ISO3_RES[[c]], s_from == "S122", c_to == ISO3_RES[[j]],
            s_to == "S13"
          )
        if (nrow(pp) && sum(pp$claim) > 0) {
          weights[j] <- sum(pp$claim) / 1000
        }
      }
    }
    caps <- sapply(other, function(j) {
      vres(c, "A30", j, if (sector == "S13") {
        "2000"
      } else {
        "1000"
      }, required = FALSE)
    })
    caps[!is.finite(caps)] <- Inf
    caps <- pmax(caps, 0)
    alloc <- allocate_residence_total(target - dom, weights, caps)
    if (sector == "S13") {
      government_holdings[c, c] <- dom
      government_holdings[c, other] <- alloc
    }
    for (k in seq_along(other)) {
      j <- other[k]
      target_node <- if (j %in% MAIN_RES) {
        node(j, sector)
      } else {
        "ROW:S2"
      }
      add(
        B, target_node, alloc[k], "debt_securities", "ECB_BSI_CPIS_ALLOCATION",
        kres(c, "A30", j, if (sector == "S13") {
          "2000"
        } else {
          "1000"
        }, required = FALSE), "imputed", if (sector == "S13") {
          "S122 government securities control allocated using residence CPIS priors where available and bilateral non-MFI securities elsewhere; book/face approximation"
        } else {
          "S122 MFI securities control allocated using bilateral MFI securities weights; issuer-bank proxy"
        }
      )
    }
  }
  # Private securities: geography from MFI non-MFI securities excluding government.
  allgeo <- c(c, other)
  pp <- sapply(allgeo, function(j) {
    ar <- if (j == c) {
      "U6"
    } else {
      j
    }
    non <- vres(c, "A30", ar, "2000", required = FALSE)
    gov <- vres(c, "A30", ar, "2100", required = FALSE)
    if (!is.finite(non)) {
      return(1e-05)
    }
    if (is.finite(gov)) {
      max(0, non - gov)
    } else {
      non * vres(c, "A30", "U5", "2200", required = FALSE) / vres(
        c, "A30", "U5",
        "2000"
      )
    }
  })
  # Some public foreign series omit sector 2200; use the published total less
  # government.
  if (any(!is.finite(pp))) {
    share <- (vres(c, "A30", "U2", "2000") - vres(c, "A30", "U2", "2100")) / vres(
      c,
      "A30", "U2", "2000"
    )
    for (k in which(!is.finite(pp))) {
      pp[k] <- vres(c, "A30", allgeo[k], "2000", required = FALSE) *
        share
    }
  }
  pp[!is.finite(pp)] <- 1e-05
  pp <- pmax(pp, 0)
  private_caps <- pmax(sapply(allgeo, function(j) {
    vres(c, "A30", if (j == c) {
      "U6"
    } else {
      j
    }, "2000", required = FALSE)
  }), 0) - government_holdings[c, allgeo]
  private_caps[!is.finite(private_caps)] <- Inf
  if (any(private_caps < -1e-06)) {
    stop("Government allocation exceeds non-MFI security envelope")
  }
  alloc <- allocate_residence_total(
    min(z$bonds_private, sum(pmax(private_caps, 0))),
    pp, pmax(private_caps, 0)
  )
  for (k in seq_along(allgeo)) {
    j <- allgeo[k]
    if (!j %in% MAIN_RES) {
      add(
        B, "ROW:S2", alloc[k], "debt_securities", "ECB_BSI", NA_character_, "imputed",
        "Non-sample EA private securities allocated within the S122 private portfolio control"
      )
      next
    }
    cp <- cpis %>%
      filter(
        c_from == ISO3_RES[[c]], s_from == "S122", c_to == ISO3_RES[[j]],
        s_to %in% c("S11", "S12nbfi")
      )
    w <- sapply(c("S11", "S12nbfi"), function(s) sum(cp$claim[cp$s_to == s]) / 1000)
    domfirm <- if (j == c) {
      shss_get(c, "S11")
    } else {
      list(value = NA_real_, key = NA_character_)
    }
    if (j == c && is.finite(domfirm$value) && domfirm$value <= alloc[k]) {
      split <- c(domfirm$value, alloc[k] - domfirm$value)
    } else {
      if (sum(w) == 0) {
        # Published issuer liabilities provide a residence/instrument matched prior.
        rr <- raw_stocks %>%
          filter(geo == j, finpos == "LIAB", na_item == "F3", sector2 %in% c(
            "S1",
            "S2"
          ))
        w <- c(sum(rr$values[rr$sector == "S11"], na.rm = TRUE), sum(rr$values[rr$sector %in%
          c("S123", "S124", "S125_S126_S127", "S128", "S129")], na.rm = TRUE))
      }
      if (sum(w) == 0) {
        w <- c(1, 1)
      }
      split <- allocate_residence_total(alloc[k], w)
    }
    for (h in 1:2) {
      add(
        B, node(j, c("S11", "S12nbfi")[h]), split[h], "debt_securities",
        "ECB_BSI_SHSS_CPIS", if (j == c) {
          domfirm$key
        } else {
          NA_character_
        }, "imputed", "S122 private-securities margin; domestic SHSS NFC holding when feasible, otherwise residence CPIS/issuer-liability sector prior"
      )
    }
  }
}
