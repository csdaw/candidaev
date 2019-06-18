
<!-- README.md is generated from README.Rmd. Please edit that file -->

# candidaev

![GitHub last
commit](https://img.shields.io/github/last-commit/csdaw/candidaev.svg?style=popout)

This repository contains the research compendium for our paper:

> Dawson, C. S., Garcia-Ceron, D.,. Rajapaksha, H., Faou, P., Anderson,
> M. A., Bleackley, M. R. (2019) Sur7 family proteins are putative
> markers for *Candida albicans* extracellular vesicles. Mol. Cell.
> Proteomics, submitted.

It has been permanently archived at Zenodo with the DOI shown above.

The compendium includes all the data, code, and text associated with the
publication. It is structured as an R package to facilitate
reproducilibity and distribution. The principles and motivations behind
using research compendiums for academic research is detailed in [Marwick
et al. (2018)](https://doi.org/10.1080/00031305.2017.1375986).

The R package [rrtools](https://github.com/benmarwick/rrtools) was used
in the production of this compendium.

### Citation

Please cite this compendium as:

> Dawson, C. S., Garcia-Ceron, D.,. Rajapaksha, H., Faou, P., Anderson,
> M. A., Bleackley, M. R. (2019) Research compendium for: *Sur7 family
> proteins are* *putative markers for* Candida albicans *extracellular
> vesicles*, 2019. Version x.x.x. Zenodo. URL.

### Contents

``` 
    .
    ├── /R                   # Script files for functions and included data
    ├── /data-raw            # Raw data used (but not modified) in analyses
    ├── /data                # Data tables installed with package
    ├── /inst/manuscript     # Final submitted manuscript and supplemental files
    ├── /man                 # Auto-generated documentation for functions and included data
    ├── /vignettes           # Detailed examples of how analyses were performed
    ├── .Rbuildignore        # List of files and directories to ignore during R package build
    ├── .gitattributes       # List of files for github/linguist to ignore
    ├── .gitignore           # List of files to ignore during git commit
    ├── DESCRIPTION          # Research compendium metadata
    ├── LICENSE.md           # MIT License for code
    ├── NAMESPACE            # Auto-generated file for function export
    ├── README.Rmd           # R markdown file to produce README.md
    ├── README.md            # Description of research compendium
    └── candidaev.Rproj      # R project file for compendium
```

### Accessing the compendium

#### Downloading directly from GitHub

You can download the compendium as a zip from from this URL:
<https://github.com/csdaw/candidaev/archive/master.zip>. Note that the
contents of `/data-raw` is not installed when the package is installed
but instructions for downloaded just these files is provided
[here](https://github.com/csdaw/candidaev/tree/master/data-raw).

#### Cloning via `git`

#### Installing the package using R and `devtools`

This research compendium can be installed as an R package `candidaev`
from GitHub using `devtools`:

``` r
# install.packages("devtools")
devtools::install_github("csdaw/candidaev")
```

### Using the `candidaev` package

### R session information

The system on which this document was compiled was running pandoc v2.6.
Here is output of `sessionInfo()`.

    #> R version 3.6.0 (2019-04-26)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows 10 x64 (build 18362)
    #> 
    #> Matrix products: default
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252   
    #> [3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
    #> [5] LC_TIME=English_Australia.1252    
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_3.6.0   magrittr_1.5     tools_3.6.0      htmltools_0.3.6 
    #>  [5] yaml_2.2.0       Rcpp_1.0.1       stringi_1.4.3    rmarkdown_1.13.1
    #>  [9] knitr_1.23       stringr_1.4.0    xfun_0.7         digest_0.6.19   
    #> [13] evaluate_0.14
