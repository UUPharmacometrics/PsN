data.npfit <- function(raw.nonparametric.file) {

  raw_nonparametric_real <- read.csv(raw.nonparametric.file)
  # delete rows with NA values
  raw_nonparametric <- raw_nonparametric_real[complete.cases(raw_nonparametric_real),]
  rownames(raw_nonparametric)=NULL
  
  return(raw_nonparametric)
}