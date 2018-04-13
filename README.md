# RColetum

An R package to get information from [COLETUM](www.coletum.com).

## Prerequisites
To be able to use this package, first you need access your Coletum account by
the navegator to get your account id and generate your authenticated token in
Webservice page.

### Install RColetum
Install this package directly in R, using the 'devtools' package: 

```{r}
install.packages("devtools")
devtools::install_github("geo-sapins/RColetum")
```
## What you can do
In this first version of package, is avaliable three main functions to enable
to get your main data from [COLETUM](www.coletum.com):

* `GetForms` allowed to get the principal information about each form avaliable
in your account;

* `GetFormsSchema` this function works to get the schema from a specific form. 
This schema contains informations about the ids, names, types, hierarquy and 
others of each question, thats can be used to get the answers posteriorly.

* `GetAnswers` return a data frame with all the answers of a specific form. 
This function make a call to `GetFormsSchema` and use the result to automatic
generate the exact query for any form, takes the result and mount the data frame
in a friendly way.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, 
see the [tags on this repository](https://github.com/geo-sapiens/RColetum/tags). 

## Authors

* **André I. Smaniotto** - [aismaniotto](https://github.com/aismaniotto)
