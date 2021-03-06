# README

This is a repo for an R-based analysis of non-native species occurrences in the Lower 48 U.S. (including the District of Columbia), also referred to as the Bad Neighbor analysis.  The project uses the Biodiversity Information Serving Our Nation (BISON) species occurrence database [https://bison.usgs.gov](https://bison.usgs.gov).

## Installation

The project was developed using [RStudio](https://www.rstudio.com/products/rstudio/download/), and installing the IDE is strongly recommended.

To install the package from GitHub, use the code below.

```
# Make sure the R devtools package is installed
# install.packages("devtools")

# then run
devtools::install_github("usgs-bis/bad-neighbor-invasives")
```

Installing from GitHub will not provide the analysis notebooks, only the functions from the package.  To obtain the full package, download or clone the repo and install locally.

```
# Make sure the R devtools package is installed
# install.packages("devtools")

# navigate to the folder containing the project, or open the bap_bad_neighbor_analysis.Rproj in the downloaded file
# setwd("bad-neighbor-invasives") # assuming you kept the folder name
devtools::install()

```

### Get Started

There are several notebooks outlining various aspects of the project.  All of the analysis and results in the notebooks are self-contained and can be generated running the code chunks in the `.Rmd` versions of the notebooks, in the order they are presented within each notebook.  The `.Rmd` versions of the notebooks can also be opened and viewed as plain text in any text editor (e.g. Windows Notepad, Mac TextEdit, etc.).

The HTML versions of the notebooks are pre-rendered versions of the code notebooks with the `.Rmd` extensions.  The results and graphics should already be rendered in the html.  If that’s not the case, the notebook should be run by running *all* the code chunks (Mac keyboard: `option-command-R`; windows keyboard: `option-control-R`).

The notebooks are all meant to stand alone to describe different elements of development or illustrate the analysis.  Thus each notebook can be run without the others and are organized as follows:

1) **1_bap_bad_neighbor_analysis.Rmd** (.html) documents the overall project and provides a results from the Bad Neighbor analysis for several taxonomic groups.  This notebook should be considered the main notebook;  

2) **2_bap_bad_neighbor_development_notes.Rmd** (.html) documents the development of the functions used in the package and provides example input/output.  This notebook should be used for anyone interested in the background of the development;

3) **3_grouped_species_notes.Rmd** (.html) documents the development of informal taxonomic groups like tree/shrub from formal taxonomic Families.


### Code and Data

This work is R-based.  The **DESCRIPTION** file contains all the information about the R version and library dependencies.  The function documentation (help) files can be found in the **man** directory, and can be opened in any plain-text editor.  An HTML version of the help files of each source script is available in the `docs/reference/index.html` folder.

The project is structured in the following directories:  

**R**: the main R source code;
**docs**: HTML version of documentation for all the functions;  
**man**: Rendered documentation for the functions.  These can be opened in any plain-text editor;  
**data**: source data for the project;  
**result_csv**: result bad neighbor summaries stored as csv;  
**result_json**: result bad neighbor summaries and species lists stored as JSON;  
**graphics**: Various graphics generated as part of the analysis;  
**js**: Some old javascript functions used as reference.


## Use Constraints

This analysis package is designed to provide information on a state-by-state basis, and is limited to that scale.  The analysis results and reported non-native species within a given state are only as accurate as the underlying occurrence data provided to the BISON service, and may not fully reflect the distribution of a species.  Please refer to BISON for a list of [data providers](https://bison.usgs.gov/#providers) and information regarding those collections.  The analysis package does not suggest in any way the absolute accuracy of these data or the results.


## Who do I talk to?

Primary contact: Enrique Montano (emontano@usgs.gov or tresmont@gmail.com)


## USGS Provisional Software

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.
