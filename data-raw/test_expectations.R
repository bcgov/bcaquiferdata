
# primary (X), secondary (with X), tertiary (Xy, or Xly), Category
lith_expect <- list(
  list(c("sand", "gravel"), "",      "",      "Sand and Gravel (Clean)"),
  list("sand",              "",      "",      "Sand"),
  list("clay",              "",      "",      "Clay"),
  list("gravel",            "",      "",      "Gravel"),
  list("till",              "sand",  "",      "Sand and Gravel Till or Diamicton"),
  list("till",              "",      "sand", "Sand and Gravel Till or Diamicton")
)
