
<!-- README.md is generated from README.Rmd. Please edit that file -->

# /data-raw

## description

This directory contains all of the raw data files used in this project.
These files are *not* included when the package is installed using
`devtools::install_github()`. Rather *‘clean’* versions of the `.csv`
and `.tab` files are provided in the [`/data`](../data) folder as `.rda`
files which are accessible once the package has been installed and
loaded or attached.

To access these raw files:

1.  Right click on a desired file and select `Save link as...`.

2.  Copy the url for the desired directory, paste into
    [DownGit](https://minhaskamal.github.io/DownGit/#/home), then
    download as a `.zip`.

3.  Download this entire repository as a `.zip` from this URL:
    <https://github.com/csdaw/candidaev/archive/master.zip>

4.  Clone this repository.

## contents

``` 
    .
    ├── ...
    ├── /data-raw
    │    ├── /external       # Supplementary data tables from other Candida EV proteomics papers
    │    ├── /lcms           # proteinGroups.txt files output by MaxQuant
    │    ├── /nta            # Nanosight ns300 .csv summary tables
    │    ├── /qubit          # Qubit 4 EV protein quantification data 
    │    ├── /reference      # UniProt and Candida Genome Database reference tables
    │    ├── README.Rmd      # R markdown file to produce README.md
    │    └── README.md       # Description of /data-raw directory
    └── ...
```