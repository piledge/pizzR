change_dots <- function(string_data, replacement = '_'){
  return(gsub('\\.', replacement, string_data))
}
