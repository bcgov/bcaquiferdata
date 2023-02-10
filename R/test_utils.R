

lith_expect <- function(){
  # primary (X), secondary (with X), tertiary (Xy, or Xly), Category
  list(
    list(c("sand", "gravel"), "",      "",      "Sand and Gravel (Clean)"),
    list("sand",              "",      "",      "Sand"),
    list("clay",              "",      "",      "Clay"),
    list("gravel",            "",      "",      "Gravel"),
    list("till",              "sand",  "",      "Sand or Gravel Till or Diamicton"),
    list("till",              "",      "sand",  "Sand or Gravel Till or Diamicton"),
    list("bedrock",              "bedrock", "",    "Bedrock")
  )
}
