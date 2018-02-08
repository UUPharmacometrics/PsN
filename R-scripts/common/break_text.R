#' Break command text in reasonable places so it can fit in a pdf report.
#' 
#' @param text A sting
#' @param max_symb A number of elements per row. By default max_symb=100.
#' @return A string
break_text <- function(text,max_symb=100) {
  new_text <- c()
  nr_symb <- nchar(text)
  start_ch <- 1
  end_ch <- max_symb
  left <- nr_symb
  while(left > max_symb) {
    one_row <- substr(text,start_ch,end_ch)
    #find last " -" and add \n\n before that
    if(substr(text,end_ch,end_ch)!=" ") {
      one_row <- sapply(strsplit(one_row,"\\ -"), function(x) paste0(head(x,-1),collapse=" -") )
      end_ch <- start_ch + nchar(one_row)
    }
    new_text <- paste0(new_text,one_row,"\n\n")
    start_ch <- end_ch + 1
    end_ch <- start_ch + max_symb - 1
    left <- nchar(substr(text,start_ch,nr_symb))
  }  
  one_row <- substr(text,start_ch,nr_symb)
  new_text <- paste0(new_text,one_row)
  return(new_text)
}



