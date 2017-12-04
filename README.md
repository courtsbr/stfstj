[![Travis-CI Build Status](https://travis-ci.org/jjesusfilho/stfstj.svg?branch=master)](https://travis-ci.org/jjesusfilho/stfstj)

<!-- README.md is generated from README.Rmd. Please edit that file -->
stfstj
======

The goal of stfstj is to download data from the Brazilian Supreme Court (STF) and Superior Court of Justice (STJ) decisions.

Installation
------------

You can install stfstj from github with:

``` r
# install.packages("devtools")
devtools::install_github("jjesusfilho/stfstj")
```

You also have to make sure the packages [tesseract](https://github.com/ropensci/tesseract) and [pdftools](https://github.com/ropensci/pdftools) are installed as well as their dependencies.

You also have to download the `tesseract` trained data for Portuguese. You can find directions for Linux, Mac-OS and Windows, [here](https://github.com/tesseract-ocr/tesseract/wiki)

Usage
-----

### Read metadata

Suppose you want to download the metadata lawsuits precedentes (acordãos) with the expression "excesso de prazo". You can run this function:

``` r
df<-stf_metadata(open_search="excesso de prazo",database="acordaos")
```

Or simply:

``` r
df<-stf_metadata("excesso adj2 prazo")
```

Using "adj2" you are telling the engine that "prazo" is one word apart from "excesso". You don't have to include "acordaos" in the argument `database` because this is the default.

I you want to search for monocratic decisions, you specify it in the `database` argument:

``` r
df<-stf_metadata("excesso adj2 prazo",database="monocraticas")
```

In order to find all the options, use the help function:

``` r
?stf_metadata()
```

Suppose now that you want to download all precedents where "Telefônica" is part in the lawsuit. You can add the suffix ".PART" to the search:

``` r
telefonicaDF<-stf_metadata("telefonica.PART.")
```

If you want to see all the possible suffixes, the function `stf_help_view()` will load on the Rstudio viewer pane the help page:

``` r
stf_help_view()
```

### Download whole decision (inteiro teor):

Once you have imported the metadata, you can you the same data frame to import the whole decision. It is important to be aware that decisions before 2011 and even some of that year are in pdf image not text. Those decisions are converted to `png` and submmited to OCR in order to be read. The limitation is that it might take a long time to read all decisions.

``` r
decisionTelefonica<-stf_acordaos(telefonicaDF[1,]). 
# Downloads just the first decision from the dataset imported above.
```
