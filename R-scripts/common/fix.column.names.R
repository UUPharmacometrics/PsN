fix_column_names <- function(col_names) {
  for (i in 1:length(col_names)) { # column names can start only with letters and points
    if(!grepl("^[A-z.]",col_names[i])) {
      col_names[i] <- paste0("X",col_names[i])
    }
    if(grepl("^[_]",col_names[i])) {
      col_names[i] <- paste0("X",col_names[i])
    }
  }
  col_names <- gsub("[^A-z0-9_]",".",col_names) # if not letter, digit or underscore, replace with point
  return(col_names)
}