# RColetum 1.0.0

* Migrated all API calls from GraphQL (V1) to REST (`/api/webservice/v2`).
* `GetFormStructure` replaced by `GetForm` (returns form metadata and component
  tree via the new REST endpoint).
* `GetAnswers` no longer accepts `singleDataFrame` parameter. Use the new
  `FlattenAnswers()` function instead.
* Added `FlattenAnswers()`: joins all nested data frames from `GetAnswers` into
  a single flat data frame using `dplyr::full_join`.
* Added pagination support to `GetForms` and `GetAnswers` (`page`, `page_size`,
  `all_pages`).
* Authentication header changed to `Token: <value>` (was `Authorization: Token
  <value>` in V1).
* Metadata columns renamed to snake_case: `created_by_user_name`,
  `created_by_user_id`, `created_at_source`, etc.
* `GetAnswers` now returns a correctly-shaped empty structure (never `NULL`)
  when no submissions match the filters.


# RColetum 0.2.2

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
