<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{rYouteria Guide}
%\VignettePackage{rYoutheria}
%\usepackage[utf8]{inputenc}
-->

Extracting data from the YouTheria database into R
======

# Introduction
YouTheria is an online data set containing data on the life history, ecology, taxonomy and geography of mammals. This package provides methods to retrieve data from this resource

# Installation
The package can be installed directly from CRAN like this:


```r
install.packages('rYoutheria')
```

Alternatively you can install the most recent development version of the package like this:


```r
# Install devtools
install.packages("devtools")

# Load devtools
library(devtools)

# Install rYoutheria from github
devtools::install_github("BiologicalRecordsCentre/rYoutheria")
```

# Choosing search terms
When searching YouTheria it is likely that you have a measurement type in mind, such as body mass or diet. To look up what measurement types are available use the `getMeasurementTypes()` function:


```r
# Load rYoutheria
library(rYoutheria)

# Get a list of all measurement types
MTs <- getMeasurementTypes()
head(MTs)
```

```
##   Id               Name
## 1 18     Activity Cycle
## 2  9 Age at Eye Opening
## 3 13 Age at First Birth
## 4 14   Average Lifespan
## 5  1          Body Mass
## 6 21               Diet
```

```r
# Look up a specific measurement type
getMeasurementTypes(measurementType='Body Mass')
```

```
##   Id      Name
## 1  1 Body Mass
```

Species names in YouTheria are linked to definitions given in the Mammal Species of the World books, and when it comes to searching you can search under either the 1993 definitions or the 2005 definitions.

rYoutheria also allows searching by country or study site ID. You can get a list of countries by using the getCountries() function. This takes no arguments and simply gives you a list of all countries available


```r
# Get a list of countries
Cs <- getCountries()
head(Cs)
```

```
##      countryName countryId
## 1     Afganistan         2
## 2  Aland Islands         5
## 3        Albania         6
## 4        Algeria        65
## 5 American Samoa        12
## 6        Andorra         7
```

# Retrieving data
Once we have decided what our search terms are going to be we can use the 'getMeasurementData()' function.


```r
# Get measurement data for dispersal age
dispAge <- getMeasurementData(measurementType = 'Dispersal Age',
                              silent = TRUE)
# Preview some of the results
head(dispAge[,c('Genus','Species','Data Value','Measure')])
```

```
##       Genus   Species Data Value     Measure
## 1 Georychus  capensis         50 Unspecified
## 2   Otocyon megalotis        5.5    Midrange
## 3      Lynx      lynx        113      Median
## 4  Acinonyx   jubatus         16    Midrange
## 5    Sousa  chinensis        3.5        Mean
## 6 Mellivora  capensis         14        Mean
```

```r
# Get measurement data for body mass of Daubenton's bats
bodyMassDaub <- getMeasurementData(measurementType = 'Body Mass',
                                   MSW05Binomial = 'Myotis daubentonii',
                                   silent = TRUE)
head(bodyMassDaub[,c('Genus','Species','Data Value','Units Weight')])
```

```
##    Genus     Species Data Value Units Weight
## 1 Myotis daubentonii      0.007    kilograms
## 2 Myotis  daubentoni        7.4        grams
## 3 Myotis  daubentoni        7.4        grams
## 4 Myotis  daubentoni       2.09        grams
## 5 Myotis daubentonii          7        grams
## 6 Myotis daubentonii        8.5        grams
```

```r
# Get measurement data for age of maturity of Swiss Lynx
LynxSwiss <- getMeasurementData(measurementType = 'Sexual Maturity Age',
                                MSW05Binomial = 'Lynx lynx',
                                country = 'Switzerland',
                                silent = TRUE)
head(LynxSwiss[,c('Genus','Species','Data Value','Measure')])                              
```

```
##   Genus Species Data Value Measure
## 1  Lynx    lynx       1.75    Mean
## 2  Lynx    lynx       2.75    Mean
```

For more help, and to see all of the functions in the package use the following command:


```r
help(package = rYoutheria)
```
