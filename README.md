
  - [candidaev](#candidaev)
      - [Citation](#citation)
      - [Contents](#contents)
      - [References](#references)
      - [Accessing the compendium](#accessing-the-compendium)
          - [1. Installing the package using R and
            `devtools`](#installing-the-package-using-r-and-devtools)
          - [2. Cloning via `git`](#cloning-via-git)
          - [3. Downloading directly from
            GitHub](#downloading-directly-from-github)
      - [Using the `candidaev` package after
        installation](#using-the-candidaev-package-after-installation)
          - [Accessing included MS data](#accessing-included-ms-data)
          - [Viewing the vignettes](#viewing-the-vignettes)
          - [Viewing the manuscript](#viewing-the-manuscript)
          - [Building the figures from
            scratch](#building-the-figures-from-scratch)
      - [R session information](#r-session-information)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# candidaev

![GitHub last
commit](https://img.shields.io/github/last-commit/csdaw/candidaev.svg?style=popout)

This repository contains the research compendium for our paper:

> Dawson, C. S., Garcia-Ceron, D., Rajapaksha, H., Faou, P., Anderson,
> M. A., Bleackley, M. R. (2019) Protein markers for *Candida albicans*
> EVs include claudin-like Sur7 family proteins and GTPases. *Journal of
> Extracellular* *Vesicles*, submitted.

It has been permanently archived at Zenodo with the DOI shown above. The
raw MS data has been deposited in the ProteomeXchange Consortium
database via the PRIDE partner repository with the data set identifiers
PXD014367, PXD014388, and PXD014389. The analysis techniques and
workflow used in this compendium were derived from the *Bioconductor*
package **[DEP](https://doi.org/10.18129/B9.bioc.DEP)**<sup>1</sup>.

The compendium includes all the data, code, and text associated with the
publication. It is structured as an R package to facilitate
reproducilibity and distribution. The principles and motivations behind
using research compendiums for academic research is detailed in [Marwick
et al. (2018)](https://doi.org/10.1080/00031305.2017.1375986). The R
package **[rrtools](https://github.com/benmarwick/rrtools)**<sup>2</sup>
was used in the production of this compendium.

### Citation

Please cite this compendium as:

> Dawson, C. S., Garcia-Ceron, D.,. Rajapaksha, H., Faou, P., Anderson,
> M. A., Bleackley, M. R. (2019) Research compendium for “Protein
> markers for *Candida albicans* EVs include claudin-like Sur7 family
> proteins and GTPases”. version x.x.x. Zenodo. URL.

### Contents

``` 
    .
    ├── /R                   # Script files for functions and included data
    ├── /data-raw            # Raw data used (but not modified) in analyses
    ├── /data                # Data tables installed with package
    ├── /inst/manuscript     # Final submitted manuscript, figures, and supplementary data
    ├── /man                 # Auto-generated documentation for functions and included data
    ├── /vignettes           # Detailed examples of how analyses were performed
    ├── .Rbuildignore        # List of files and directories to ignore during R package build
    ├── .gitattributes       # List of files for github/linguist to ignore
    ├── .gitignore           # List of files to ignore during git commit
    ├── DESCRIPTION          # Research compendium metadata
    ├── LICENSE.md           # MIT License for code
    ├── NAMESPACE            # Auto-generated file for function export
    ├── README.Rmd           # Rmarkdown file to produce README.md
    ├── README.md            # Description of research compendium
    └── candidaev.Rproj      # R project file for compendium
```

### References

1.  Zhang, X., Smits, A., van Tilburg, G., Ovaa, H., Huber, W.,
    Vermeulen, M. (2018). Proteome-wide identification of ubiquitin
    interactions using UbIA-MS. *Nature Protocols*, **13**, 530-550.
    [doi:10.1038/nprot.2017.147](https://doi.org/10.1038/nprot.2017.147)

2.  Marwick, B., Boettiger, C., Mullen, L. (2018). Packaging data
    analytical work reproducibly using R (and friends). *The American
    Statistician*, **72**(1), 80-88.
    [doi:10.1080/00031305.2017.1375986](https://doi.org/10.1080/00031305.2017.1375986)

### Accessing the compendium

#### 1\. Installing the package using R and `devtools`

This research compendium can be installed as an R package `candidaev`
from GitHub using `devtools`. A full installation will include all the
functions and data included in the package as well as some HTML
vignettes, the manuscript text, the figures `.Rmd` file which can be
used to reproduce the analysis and figures as submitted.

A partial installation will only install functions and data, which can
be used to analyse other MaxQuant `.txt` files.

``` r
# install the devtools package from CRAN if not already installed
# install.packages("devtools")

# install partial compendium (functions, data)
devtools::install_github("csdaw/candidaev", 
                         INSTALL_opts = "--no-inst")

# install full compendium (functions, data, vignettes, manuscript) 
# and other packages required to build vignettes and manuscript
devtools::install_github("csdaw/candidaev", 
                         build_opts = c("--no-resave-data", "--no-manual"), 
                         dependencies = TRUE, build_vignettes = TRUE)
```

**Note:** the contents of `/data-raw` and
`/inst/manuscript/supplementary_data` are not installed when the package
is installed using `install_github`. Instead, `/data-raw` can be
manually downloaded from
[here](https://github.com/csdaw/candidaev/tree/master/data-raw) and
`/inst/manuscript/supplementary_data` can be manually downloaded from
[here](https://github.com/csdaw/candidaev/tree/master/inst/manuscript).

#### 2\. Cloning via `git`

The research compendium can be downloaded from GitHub as is using `git`.
First install git from [here](https://git-scm.com/), navigate to a
directory of choice then execute this line at a Bash prompt:

    git clone https://github.com/csdaw/candidaev.git

**Note:** the `candidaev` package must be installed to function.

#### 3\. Downloading directly from GitHub

The research compendium can be downloaded from GitHub as a zip using
this URL: <https://github.com/csdaw/candidaev/archive/master.zip>.

**Note:** the `candidaev` package must be installed to function.

### Using the `candidaev` package after installation

#### Accessing included MS data

The MaxQuant `proteinsGroups.txt` files on which the analyses in this
compendium are based is available from the PRIDE data sets mentioned
above. They are also included when the `candidaev` package is installed,
and can be accessed with:

``` r
library(candidaev)

data(atcc)

data(biofilm)

data(yeast)
```

#### Viewing the vignettes

Three HTML vignettes detailing the analysis procedures used in the
manuscript are included in this research compendium. They are
automatically built from scratch when the full research compendium is
installed with `devtools`. These vignettes can be viewed with:

``` r
# if full compendium package was installed with devtools::install_github
browseVignettes("candidaev")

# if the partial compendium package was installed 
# the vignettes are not included. Please install the full compendium.
```

#### Viewing the manuscript

The manuscript `.docx` file, as submitted, can be downloaded
[here](https://github.com/csdaw/candidaev/tree/master/inst/manuscript).

The figures for the manuscript were produced using the
`candidaev_figures.Rmd` file which can be downloaded
[here](https://github.com/csdaw/candidaev/tree/master/inst/manuscript).

#### Building the figures from scratch

To generate the reproducible figures, an installation of LaTeX is
required. TinyTex is a good option for this and can be installed using:

``` r
# install TinyTex R package
install.packages("tinytex")

# install TinyTex LaTex distribution
tinytex::install_tinytex()
```

Then the manuscript figures (and supplement) can be built using:

``` r
# requires LaTeX installation and full compendium installation
rmarkdown::render(system.file("manuscript", "candidaev_figures.Rmd", 
                              package = "candidaev"))
rmarkdown::render(system.file("manuscript", "candidaev_supplement.Rmd", 
                              package = "candidaev"))
```

### R session information

The system on which this document was compiled was running pandoc
v2.7.1. Here is output of `sessionInfo()`.

    #> - Session info ----------------------------------------------------------
    #>  setting  value                       
    #>  version  R version 3.6.0 (2019-04-26)
    #>  os       Windows 10 x64              
    #>  system   x86_64, mingw32             
    #>  ui       RTerm                       
    #>  language (EN)                        
    #>  collate  English_Australia.1252      
    #>  ctype    English_Australia.1252      
    #>  tz       Australia/Sydney            
    #>  date     2019-07-18                  
    #> 
    #> - Packages --------------------------------------------------------------
    #>  package     * version date       lib source                            
    #>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.0)                    
    #>  backports     1.1.4   2019-04-10 [1] CRAN (R 3.6.0)                    
    #>  callr         3.3.0   2019-07-04 [1] CRAN (R 3.6.1)                    
    #>  cli           1.1.0   2019-03-19 [1] CRAN (R 3.6.0)                    
    #>  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.0)                    
    #>  desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.0)                    
    #>  devtools      2.1.0   2019-07-06 [1] CRAN (R 3.6.1)                    
    #>  digest        0.6.20  2019-07-04 [1] CRAN (R 3.6.1)                    
    #>  evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.0)                    
    #>  fs            1.3.1   2019-05-06 [1] CRAN (R 3.6.0)                    
    #>  glue          1.3.1   2019-03-12 [1] CRAN (R 3.6.0)                    
    #>  htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.6.0)                    
    #>  knitr         1.23    2019-05-18 [1] CRAN (R 3.6.0)                    
    #>  magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.0)                    
    #>  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.0)                    
    #>  pkgbuild      1.0.3   2019-03-20 [1] CRAN (R 3.6.0)                    
    #>  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.6.0)                    
    #>  prettyunits   1.0.2   2015-07-13 [1] CRAN (R 3.6.0)                    
    #>  processx      3.4.0   2019-07-03 [1] CRAN (R 3.6.0)                    
    #>  ps            1.3.0   2018-12-21 [1] CRAN (R 3.6.0)                    
    #>  R6            2.4.0   2019-02-14 [1] CRAN (R 3.6.0)                    
    #>  Rcpp          1.0.1   2019-03-17 [1] CRAN (R 3.6.0)                    
    #>  remotes       2.1.0   2019-06-24 [1] CRAN (R 3.6.0)                    
    #>  rlang         0.4.0   2019-06-25 [1] CRAN (R 3.6.0)                    
    #>  rmarkdown     1.13.6  2019-07-03 [1] Github (rstudio/rmarkdown@5335b69)
    #>  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.0)                    
    #>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.0)                    
    #>  stringi       1.4.3   2019-03-12 [1] CRAN (R 3.6.0)                    
    #>  stringr       1.4.0   2019-02-10 [1] CRAN (R 3.6.0)                    
    #>  testthat      2.1.1   2019-04-23 [1] CRAN (R 3.6.0)                    
    #>  usethis       1.5.1   2019-07-04 [1] CRAN (R 3.6.1)                    
    #>  withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.0)                    
    #>  xfun          0.8     2019-06-25 [1] CRAN (R 3.6.0)                    
    #>  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.6.0)                    
    #> 
    #> [1] C:/Software/R-3.6.0/library
