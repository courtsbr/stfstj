[![Travis-CI Build Status](https://travis-ci.org/jjesusfilho/stfstj.svg?branch=master)](https://travis-ci.org/jjesusfilho/stfstj)

<!-- README.md is generated from README.Rmd. Please edit that file -->
stfstj
======

The goal of stfstj is to download data from the Brazilian Supreme Court decisions

Installation
------------

You can install stfstj from github with:

``` r
# install.packages("devtools")
devtools::install_github("jjesusfilho/stfstj")
```

You also have to make sure the packages [tesseract](https://github.com/ropensci/tesseract) and [pdftools](https://github.com/ropensci/pdftools) are installed also their dependencies.

Usage
-----

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
