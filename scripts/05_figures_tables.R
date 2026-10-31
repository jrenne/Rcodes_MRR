source("R/setup.R")
# Each output script reads the saved results and writes its named paper assets.
for (file in c("figures", "exposures", "capital", "dependence", "coverage")) {
  source(file.path("R", "output", paste0(file, ".R")), local = new.env())
}
