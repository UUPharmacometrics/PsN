data.npfit <- function(raw.nonparametric.file) {
  make_pdf <- TRUE
  raw_nonparametric_real <- read.csv(raw.nonparametric.file)
  # delete rows with NA values
  raw_nonparametric <- raw_nonparametric_real[complete.cases(raw_nonparametric_real),]
  
  if (nrow(raw_nonparametric) == 0) {
    print("Don't have npofv values, can't create a plot.")
    make_pdf <- FALSE
  }
  return(list(make_pdf=make_pdf,raw_nonparametric=raw_nonparametric))
}