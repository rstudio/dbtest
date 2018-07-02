# dbtest 0.1.0.9000

* BREAKING (sort of): `test_database` now returns a list object by default.  If
executing on a single database, can turn this off with `return_list = FALSE`.
`test_databases` and `test_single_database` should continue with old behavior

* BREAKING (sort of): change `plot_tests` implementation now returns a list instead
of a ggplot object

* Deprecated `test_databases` and `test_single_database` in favor of `test_database`

* Added a `NEWS.md` file to track changes to the package.
