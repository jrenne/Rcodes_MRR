# Manuscript sources

`main.tex` is the paper and `online_appendix.tex` is the Online Appendix. Both use `float_style.tex` and the local `Figures/` directory. Sections are in `Sections/` and `OnlineAppendix/`.

Run `Rscript main.R` from the repository root to regenerate the numerical assets. Then compile from the root with `latexmk -cd -pdf manuscript/paper/main.tex` and `latexmk -cd -pdf manuscript/paper/online_appendix.tex`.
