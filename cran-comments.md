## Test environments
* local OS Linux Mint 18.1 install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* `getAnswers` Add the field UpdatedAt, according the API
* `getAnswers` Remove the functionality to choose the format in the data filters 
* Update the format to dates from createdAt and updatedAt to complete format the
  ISO8601
* `getAnswers` Removed the need to use the field `name` from `getFormStructure`
* `getFormStructure` Removed field `name` from query
* Small adjusts in automated tests
