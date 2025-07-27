### /mutual_import_1.ncl
{
  x = (import "mutual_import_2.ncl").y,
  y = 1,
}
### /mutual_import_2.ncl
{
  x = (import "mutual_import_1.ncl").y,
  y = 2,
}
### diagnostic = ["file:///mutual_import_2.ncl"]
