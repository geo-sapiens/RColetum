# RColetum 0.2.1.9000

* Replace `as.character` by `format`.
* Reorder expected dataframes on tests.
* Fix some lint warnings.
* Replace `travis-ci` by `github-action`.

# RColetum 0.2.1

* Removed references from `countryfield` from tests.

# RColetum 0.2.0

* `GetAnswers` Add the filters "createdDeviceBefore" and "createdDeviceAfter" 
for the field `createAtDevice`.
`GetAnswers` Add field `createAtDevice` in answer's metadata, informing the
moment of creation of the answer, with device's data.
* `GetAnswers` Add the filters "updatedBefore" and "updatedAfter" for the field
`updatedAt`.
* `GetAnswers` Add field `userId` in answer's metadata, indicating the id from 
the user who answered the form.

# RColetum 0.1.2

* `GetAnswers` Add the field UpdatedAt, according the API.
* `GetAnswers` Remove the functionality to choose the format in the data filters 
* Update the format to dates from createdAt and updatedAt to complete format the
  ISO8601.
* `GetAnswers` Removed the need to use the field `name` from `getFormStructure`.
* `GetFormStructure` Removed field `name` from query.
* Small adjusts in automated tests.
