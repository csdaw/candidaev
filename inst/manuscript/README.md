
<!-- README.md is generated from README.Rmd. Please edit that file -->

# /vignettes

## description

This directory contains the submitted manuscript (see `manuscript.pdf`)
for this paper :

> Authors, (YYYY). *Title of paper*. Name of journal/book
> <https://doi.org/xxx/xxx>

It also contains a more detailed analysis of the LC-MS/MS data, upon
which the manuscript is based (see `analysis.md` or `analysis.html`).
The Rmarkdown (`.Rmd`) files used to generate these documents are
provided.

Both the manuscript and the analysis documents *are* included as
vignettes in this package so they can be easily viewed (or regenerated
completely from the raw data) if the package is installed.
Alternatively, one can download these documents directly from this
GitHub repository.

## how to access the manuscript

## how to access the analysis

## contents

``` 
    .
    ├── ...
    ├── /vignettes
    │    ├── /figures            # Contains figures
    │    ├── /components         # Contains required components for knitting manuscript and analysis
    │    ├── analysis.Rmd        # R markdown file to produce analysis.md or analysis.html
    │    ├── analysis.html       # Full analysis upon which the manuscript is based.
    │    ├── analysis.md         # Full analysis upon which the manuscript is based.
    │    ├── manuscript.Rmd      # R markdown file to produce manuscript.pdf or manuscript.docx
    │    ├── manuscript.pdf      # Final submitted manuscript
    │    ├── README.Rmd          # R markdown file to produce README.md
    │    └── README.md           # Description of /vignettes directory
    └── ...
```