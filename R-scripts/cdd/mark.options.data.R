mark.options.data <- function(p1,markeropt) {
  # Choose one option for the markers (create talbe for text and table for points)
  if( markeropt == 1 ) { 
    cdd.txt  <- subset(p1, FALSE)
    cdd.pt   <- subset(p1, TRUE)
  } else if ( markeropt == 2 ) {
    cdd.txt  <- subset(p1, TRUE)
    cdd.pt   <- subset(p1, FALSE)
  } else if ( markeropt == 3 ) { 
    cdd.txt  <- subset(p1, outside.n.sd == 1)
    cdd.pt   <- subset(p1, outside.n.sd == 0)
    rownames(cdd.txt) <- NULL
    rownames(cdd.pt) <- NULL
  }
return(list(cdd.pt = cdd.pt,cdd.txt = cdd.txt))
}