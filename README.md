# RColetum

An R package to get information from [COLETUM](https://coletum.com).

## Prerequisites
To be able to use this package, first you need access your Coletum account by
the navegator to generate your authenticated token in Webservice page.

### Install RColetum
Install this package directly in R, using the 'devtools' package: 

```{r}
install.packages("devtools")
devtools::install_github("geo-sapiens/RColetum")
```
## What you can do
In this first version of package, is avaliable three main functions to enable
to get your main data from [COLETUM](https://coletum.com):

* `GetForms` allowed to get the principal information about each form avaliable
in your account;

* `GetFormStructure` this function works to get the structure from a specific 
form. This structure contains informations about the ids, names, types, 
hierarquy and others of each question, thats can be used to get the answers 
posteriorly.

* `GetAnswers` return one or more data frames with all the answers of a specific 
form. 
This function make a call to `GetFormStructure` and use the result to automatic
generate the exact query for any form, takes the result and mount the data frame
in a friendly way.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, 
see the [tags on this repository](https://github.com/geo-sapiens/RColetum/tags). 

## Authors

* **André I. Smaniotto** - [aismaniotto](https://github.com/aismaniotto)
