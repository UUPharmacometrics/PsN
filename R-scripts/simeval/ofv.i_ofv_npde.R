# iOFV NPDE
i_ofv_npde <- function(iofv.file) {
  iOFV <- read.csv(iofv.file) #input data
  iOFV_npde <- iOFV$NPDE # look only on NPDE column
  iOFV_npde <- iOFV_npde[!is.na(iOFV_npde)] # delete rows with NA values
  rownames(iOFV_npde) <- NULL
  len <- length(iOFV_npde) # how many values are there
  H <- hist(iOFV_npde,plot=FALSE)
  x <- seq(-3,3,length=100) # we are interested only in the interval where -3 < x < 3
  dx <- min(diff(H$breaks))
  dy <- len*dx*dnorm(x)
  dy1 <- len*dx*H$density
  ylimit <- max(c(max(dy1),max(dy)))
  xlimit_min <- min(x,min(iOFV_npde))
  xlimit_max <- max(x,max(iOFV_npde))
  
  #output
  out <- list(iOFV=iOFV,
              iOFV_npde=iOFV_npde,
              ylimit=ylimit,
              xlimit_min=xlimit_min,
              xlimit_max=xlimit_max,
              x=x,
              dy=dy)
  return(out)   
}