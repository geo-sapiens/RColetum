# RColetum
[![Build Status](https://travis-ci.org/geo-sapiens/RColetum.svg)](https://travis-ci.org/geo-sapiens/RColetum)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RColetum)](https://cran.r-project.org/package=RColetum)

An R package to get data from [Coletum](https://coletum.com).

## Prerequisites
To be able to use this package, first of all, you need access your Coletum's
account [https://coletum.com] from your browser to generate your
authenticated token in Web service page.

If you don't have an account, [SIGN UP NOW](https://coletum.com/en_US/register/).

## How to install RColetum

Install this package from CRAN:
```{r}
install.packages("RColetum")
```

Or install the development version from GitHub using the 'devtools' package:
```{r}
install.packages("devtools")
devtools::install_github("geo-sapiens/RColetum")
```

## How to use RColetum
In this version of the package, there are three main functions available, 
Those able you to get your main data from [Coletum](https://coletum.com):

### Get my forms
* `GetForms` this function get the list of forms in your account.

```{r}
myForms <- GetForms("TOKEN_HERE")
```
### Get form structure
* `GetFormStructure` this function gets the structure from a specific form. The
structure contains the specifications of each field, like the name, type, 
hierarchy and others.

```{r}
myFormStructure <- GetFormStructure(token = "TOKEN_HERE", idForm = FORM_ID)
```
### Get answers
* `GetAnswers` this function gets the answers from a specific form. The data 
structure returned depends of the form structure. When the form has no 
field with cardinality greater than 1, the structure is a data frame. When the 
form has one or more fields with cardinality greater then one, the structure 
is a list of data frames.

```{r}
myAnswers <- GetAnswers(token = "TOKEN_HERE", idForm = FORM_ID)
```

If you want to get the answers in a single data frame with redundant data 
(caused by fields with cardinality greater than 1), you should use 
`singleDataFrame` parameter as TRUE.

```{r}
myAnswers <- GetAnswers(token = "TOKEN_HERE", 
                        idForm = FORM_ID, 
                        singleDataFrame = TRUE)
```

## Full example
```{r}
install.packages("devtools")
devtools::install_github("geo-sapiens/RColetum")

####@> Loading RColetum package
library(RColetum)

####@> Creating a variable to store my Coletum API Token
####@> This variable will be used in all bellow methods
myToken <- "cizio7xeohwgc8k4g4koo008kkoocwg"

####@> Getting my forms
myForms <- GetForms(myToken)

####@> Getting form structure using form id
starWarsFormStructure <- GetFormStructure(token = myToken,
                                          idForm = 5713)

####@> Getting form structure using form name
starWarsFormStructure <- GetFormStructure(token = myToken,  
                                          nameForm = "RColetum Test - Star Wars")

####@> Getting answers for a form using form id
starWarsFormAnswer <- GetAnswers(token = myToken, 
                                 idForm = 5713)

####@> Getting answers for a form using form name
####@> In this case we have X + 1 dataframes, where X is number of 
####@> N fields (fields with cardinality > 1)
starWarsFormAnswer <- GetAnswers(token = myToken, 
                                 nameForm = "RColetum Test - Star Wars")

####@> Getting answers for a form getting result as single dataframe
####@> In this case we have redundancy for N fields
starWarsFormAnswerSingleDataframe <- GetAnswers(token = myToken, 
                                                idForm = 5713, 
                                                singleDataFrame = TRUE)


####@> LET'S HAVE SOME FUN AND SHOW A CHART WITH BMI (BODY MASS INDEX) 
####@> OF EACH STAR WARS CHARACTERS
library(ggplot2)

myChart <- ggplot(data = starWarsFormAnswer[[1]], 
                  mapping = 
                    aes(x = name66298, 
                        y = (mass66300) / (height66299/100)^2 )) +
  geom_bar(stat = "identity", 
           fill = "black", 
           colour = "green",
           alpha = 0.8) +
  geom_label(mapping = aes(label = (mass66300) / (height66299/100)^2)) +
  labs(x = "Character", 
       y = "BMI") +
  theme_bw(base_size = 14)

myChart

```

## Versioning
We use [SemVer](http://semver.org/) for versioning. For the versions available,
see the [tags on this repository](https://github.com/geo-sapiens/RColetum/tags).

-----
Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/geo-sapiens/RColetum/blob/master/docs/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
