# Run each test in a fresh R process after main.R.
files <- list.files("tests", pattern = "^test_.*[.]R$", full.names = TRUE)
dir.create("outputs/tests", recursive = TRUE, showWarnings = FALSE)
results <- lapply(files, function(file) {
  log <- file.path("outputs/tests", paste0(basename(file), ".log"))
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(file), stdout = log, stderr = log)
  cat(if (status == 0) "PASS" else "FAIL", basename(file), "\n")
  data.frame(test = basename(file), passed = status == 0, log = log)
})
results <- do.call(rbind, results)
write.csv(results, "outputs/tests/results.csv", row.names = FALSE)
if (!all(results$passed)) stop("Inspect the failed test logs in outputs/tests/.")
