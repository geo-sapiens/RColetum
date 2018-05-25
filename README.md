# RColetum
[![Build Status](https://travis-ci.org/geo-sapiens/RColetum.svg)](https://travis-ci.org/geo-sapiens/RColetum)

An R package to get information from [COLETUM](https://coletum.com).

## Prerequisites
To be able to use this package, first of all, you need access your Coletum's
account [https://coletum.com] from your browser to generate your
authenticated token in Webservice page.

If you don't have an account, [SIGN UP NOW](https://coletum.com/register/).

For instructions to generate your token, please visit the
[API documentation](https://coletum.docs.apiary.io/).

## How to install RColetum
Install this package directly in R, using the 'devtools' package:

```{r}
install.packages("devtools")
devtools::install_github("geo-sapiens/RColetum")
```

## How to use RColetum
In this first version of the package, three main functions are available.
Those able you to get your main data from [COLETUM](https://coletum.com):

### Get my forms
* `GetForms` allow to get the principal information about each form available
in your account;

```{r}
myForms <- GetForms("YOUR_TOKEN_HERE")
```

* `GetFormStructure` this function get the structure from a specific form. This
structure contains information about the names, types, hierarchy and others of 
each question, that's can be used to get the answers posteriorly;

```{r}
myFormStructure <- GetFormStructure("YOUR_TOKEN_HERE", FORM_ID)
```

* `GetAnswers` return one or more data frames with all the answers of a specific
form.
```{r}
myAnswers <- GetAnswers("YOUR_TOKEN_HERE", ,FORM_NAME)
```
## Versioning
We use [SemVer](http://semver.org/) for versioning. For the versions available,
see the [tags on this repository](https://github.com/geo-sapiens/RColetum/tags).
