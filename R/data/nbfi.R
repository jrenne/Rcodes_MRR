# Pool S123--S129 financial accounts; equity/fund shares are not ordinary debt.
build_nbfi_data <- function(output_directory) {
  source("R/data/capital.R")
  out <- output_directory
  read_json <- jsonlite::read_json
  raw_dir <- "data/raw/nbfi_buffers_2024Q4"
  geos <- c(DEU = "DE", FRA = "FR", ITA = "IT", ESP = "ES")
  cells <- sectors <- country_rows <- manifests <- list()
  cell_id <- 0L
  for (country in names(geos)) {
    geo <- geos[[country]]
    path <- file.path(raw_dir, paste0("annual_", geo, ".json"))
    x <- read_json(path, simplifyVector = TRUE)
    stopifnot(x$class == "dataset", all(x$size > 0))
    cell <- function(sector, finpos, item, record = TRUE) {
      codes <- c(
        freq = "A", unit = "MIO_EUR", co_nco = "NCO", sector = sector, finpos = finpos,
        na_item = item, geo = geo, time = "2024"
      )
      index <- 0
      for (k in seq_along(x$id)) {
        p <- x$dimension[[x$id[k]]]$category$index[[codes[x$id[k]]]]
        if (is.null(p)) {
          index <- NA_real_
          break
        }
        index <- index * x$size[k] + p
      }
      value <- if (is.finite(index)) {
        x$value[[as.character(index)]]
      } else {
        NULL
      }
      flag <- if (is.finite(index)) {
        x$status[[as.character(index)]]
      } else {
        NULL
      }
      value <- if (is.null(value)) {
        NA_real_
      } else {
        value / 1000
      }
      if (record) {
        cell_id <<- cell_id + 1L
        cells[[cell_id]] <<- data.frame(
          country = country, sector = sector, finpos = finpos,
          item = item, value_eur_bn = value, source_status = if (is.null(flag)) {
            ""
          } else {
            flag
          }, json_index = index, source_path = path, source_key = paste(c(
            "nasa_10_f_bs",
            codes[x$id]
          ), collapse = ".")
        )
      }
      value
    }
    selected <- c("S123", "S124", "S125_S126_S127")
    if (all(is.finite(c(cell("S128", "ASS", "F", FALSE), cell("S129", "ASS", "F", FALSE))))) {
      selected <- c(selected, "S128", "S129")
    } else {
      selected <- c(selected, "S128_S129")
    }
    d <- do.call(rbind, lapply(selected, function(s) {
      A <- cell(s, "ASS", "F")
      L <- cell(s, "LIAB", "F")
      v <- setNames(
        vapply(paste0("F", 1:8), function(n) cell(s, "LIAB", n), numeric(1)),
        paste0("F", 1:8)
      )
      # A blank is not an observed zero. Infer a zero only where the total is exhausted
      # by the published disjoint instrument components, allowing rounding of up to EUR
      # 5m across eight million-euro observations.
      residual <- L - sum(v, na.rm = TRUE)
      stopifnot(is.finite(A), is.finite(L), is.finite(v["F5"]), abs(residual) <= 0.005)
      inferred <- names(v)[is.na(v)]
      v[is.na(v)] <- 0
      D <- sum(v[c("F2", "F3", "F4")])
      E <- A - L + v["F5"]
      O <- L - v["F5"] - D
      stopifnot(E >= 0, D >= 0, O >= -0.005)
      data.frame(
        country = country, sector = s, financial_assets_eur_bn = A, financial_liabilities_eur_bn = L,
        issued_equity_and_fund_shares_eur_bn = v["F5"], financial_net_worth_eur_bn = A -
          L, loss_absorption_buffer_eur_bn = E, debt_pool_eur_bn = D, fixed_other_obligations_eur_bn = O,
        raw_instrument_rounding_gap_eur_bn = residual, zero_inferred_items = paste(inferred,
          collapse = "+"
        )
      )
    }))
    sectors[[country]] <- d
    fields <- c(
      "financial_assets_eur_bn", "financial_liabilities_eur_bn", "issued_equity_and_fund_shares_eur_bn",
      "financial_net_worth_eur_bn", "loss_absorption_buffer_eur_bn", "debt_pool_eur_bn",
      "fixed_other_obligations_eur_bn"
    )
    totals <- as.list(colSums(d[, fields]))
    # Independent sector hierarchy: non-MFI financial corporations plus MMFs.
    cross <- function(f, n) {
      cell("S12", f, n) - cell("S121_S122_S123", f, n) + cell(
        "S123",
        f, n
      )
    }
    gaps <- c(A = totals$financial_assets_eur_bn - cross("ASS", "F"), L = totals$financial_liabilities_eur_bn -
      cross("LIAB", "F"), F5 = totals$issued_equity_and_fund_shares_eur_bn - cross(
      "LIAB",
      "F5"
    ))
    stopifnot(all(is.finite(gaps)), max(abs(gaps)) <= 0.01)
    country_rows[[country]] <- data.frame(country = country, totals, sector_partition = paste(selected,
      collapse = "+"
    ), max_sector_hierarchy_gap_eur_bn = max(abs(gaps)), row.names = NULL)
    manifests[[country]] <- data.frame(
      country = country, path = path, source_url = paste0(
        "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasa_10_f_bs?lang=EN&freq=A&geo=",
        geo, "&time=2024&unit=MIO_EUR&co_nco=NCO"
      ), dataset_updated = if (is.null(x$updated)) {
        NA_character_
      } else {
        x$updated
      }, downloaded_date_utc = format(file.info(path)$mtime, tz = "UTC", usetz = TRUE),
      md5 = unname(tools::md5sum(path))
    )
  }
  buffers <- do.call(rbind, country_rows)
  subsectors <- do.call(rbind, sectors)
  observations <- do.call(rbind, cells)
  manifest <- do.call(rbind, manifests)
  write.csv(buffers, file.path(out, "nbfi_buffers.csv"), row.names = FALSE)
  write.csv(subsectors, file.path(out, "nbfi_subsector_buffer_components.csv"), row.names = FALSE)
  write.csv(observations, file.path(out, "public_balance_sheet_cells.csv"), row.names = FALSE)
  write.csv(manifest, file.path(out, "public_source_manifest.csv"), row.names = FALSE)
}
