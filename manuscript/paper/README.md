# Paper and Online Appendix

The compiled documents are `main.pdf` and `online_appendix.pdf`. Their LaTeX sources are `main.tex` and `online_appendix.tex`.

To rebuild the PDFs, first run `Rscript main.R` from the repository root, then:

```sh
latexmk -cd -pdf manuscript/paper/main.tex
latexmk -cd -pdf manuscript/paper/online_appendix.tex
```

PDF compilation requires LaTeX and `latexmk`; they are not needed for the numerical replication.
