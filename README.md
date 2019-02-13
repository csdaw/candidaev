
<!-- README.md is generated from README.Rmd. Please edit that file -->

# candidaev

This repository contains the research compendium for our paper:

> Authors, (YYYY). *Title of paper*. Name of journal/book
> <https://doi.org/xxx/xxx>

The compendium includes all the data, code, and text associated with the
publication. It is structured as an R package to facilitate
reproducilibity and distribution. The principles and motivations behind
using research compendiums for academic research is detailed in [Marwick
et al. (2018)](https://doi.org/10.1080/00031305.2017.1375986).

The R package [rrtools](https://github.com/benmarwick/rrtools) was used
in the production of this compendium.

### Contents

``` 
    .
    ├── /R                   # Script files containing reused functions
    ├── /analysis            # Full analysis report and scripts
    ├── /data                # Raw, cleaned, external, and results data
    ├── /manuscript          # Final manuscript and associated files
    ├── .Rbuildignore        # List of files and directories to ignore during R package build
    ├── .gitignore           # List of files to ignore during git commit
    ├── DESCRIPTION          # Research compendium metadata
    ├── LICENSE.md           # MIT License for code
    ├── NAMESPACE            # Auto-generated file for function export
    ├── README.Rmd           # R markdown file to produce README.md
    ├── README.md            # Description of research compendium
    └── candidaev.Rproj      # R project file for compendium
```

### How to cite

Please cite this compendium as:

> Authors, (2019). *Compendium of R code and data for ‘Title of paper’*.
> Accessed 12 Feb 2019. Online at <https://doi.org/xxx/xxx>

### How to download or install

You can download the compendium as a zip from from this URL:
<https://github.com/csdaw/candidaev/archive/master.zip>

Or you can install this compendium as an R package `candidaev` from
GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("csdaw/candidaev")
```

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
