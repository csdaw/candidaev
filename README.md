
  - [candidaev](#candidaev)
      - [Citation](#citation)
      - [Contents](#contents)
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
          - [Building the manuscript from
            scratch](#building-the-manuscript-from-scratch)
      - [R session information](#r-session-information)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# candidaev

![GitHub last
commit](https://img.shields.io/github/last-commit/csdaw/candidaev.svg?style=popout)

This repository contains the research compendium for our paper:

> Dawson, C. S., Garcia-Ceron, D., Rajapaksha, H., Faou, P., Anderson,
> M. A., Bleackley, M. R. (2019) Protein markers for *Candida albicans*
> EVs include claudin-like Sur7 family proteins and Rho GTPases. Mol.
> Cell. Proteomics, submitted.

It has been permanently archived at Zenodo with the DOI shown above. The
raw MS data has been deposited in the ProteomeXchange Consortium
database via the PRIDE partner repository with the data set identifiers
PXD014367, PXD014388, and PXD014389.

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
> M. A., Bleackley, M. R. (2019) Research compendium for “Protein
> markers for *Candida albicans* EVs include claudin-like Sur7 family
> proteins and Rho GTPases”. version x.x.x. Zenodo. URL.

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
    ├── README.Rmd           # Rmarkdown file to produce README.md
    ├── README.md            # Description of research compendium
    └── candidaev.Rproj      # R project file for compendium
```

### Accessing the compendium

#### 1\. Installing the package using R and `devtools`

This research compendium can be installed as an R package `candidaev`
from GitHub using `devtools`. A full installation will include all the
functions and data included in the package as well as some HTML
vignettes, and the manuscript `.Rmd` file which can be used to reproduce
the manuscript as it was submitted.

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
                         dependencies = TRUE)
```

**Note:** the contents of `/data-raw` and
`/inst/manuscript/supplement_files` are not installed when the package
is installed using `install_github`. Instead, `/data-raw` can be
manually downloaded from
[here](https://github.com/csdaw/candidaev/tree/master/data-raw) and
`/inst/manuscript/supplement_files` can be manually downloaded from
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

The manuscript `.pdf` file, as submitted, can be downloaded
[here](https://github.com/csdaw/candidaev/tree/master/inst/manuscript).

#### Building the manuscript from scratch

To generate the reproducible manuscript, an installation of LaTeX is
required. TinyTex is a good option for this and can be installed using:

``` r
# install TinyTex R package
install.packages("tinytex")

# install TinyTex LaTex distribution
tinytex::install_tinytex()
```

Then the manuscript (and supplement) can be built using:

``` r
# requires LaTeX installation and full compendium installation
rmarkdown::render(system.file("manuscript", "manuscript.Rmd", 
                              package = "candidaev"))
rmarkdown::render(system.file("manuscript", "manuscript_supplement.Rmd", 
                              package = "candidaev"))
```

### R session information

The system on which this document was compiled was running pandoc
v2.7.1. Here is output of `sessionInfo()`.

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
