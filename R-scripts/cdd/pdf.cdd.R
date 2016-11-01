pdf.cdd <- function(raw.results.file,skipped.id.file,pdf.filename,markeropt,
                    min.failed,cov.failed,cov.warnings,boundary,legend,cutoff_cook,cutoff_cov,
                    outlier_ID) {
  #default for legend is FALSE
  if (missing(legend)) {
    legend <- FALSE
  }  
  ## read files
  cdd.data <- read.csv(raw.results.file)
  cdd.inds <- read.csv(skipped.id.file, header=F)
  # 1. Create table cdd.data
  cdd.data <- create.data(cdd.data,cdd.inds)
  
  # 2. Create tables cdd.warn and p1
  p1 <- warnings.data(cdd.data,min.failed,cov.failed,cov.warnings,boundary) 
  if (class(p1) == "list") {
    cdd.warn <- p1$cdd.warn
    p1 <- p1$p1
  }
  # 3. Create tables cdd.pt and cdd.txt
  out <- mark.options.data(p1,markeropt)
  cdd.pt <- out$cdd.pt
  cdd.txt <- out$cdd.txt
  
  # 4. Count not successful values
  fail <- failed.values(cdd.data)

  # create pdf file
  pdf(file=pdf.filename,width=11.69, height=8.27)
  
  # 5. Create a plot
  if (exists("cdd.warn")) {
    plot.cdd(cdd.data,cdd.pt,cdd.txt,legend,fail,cdd.warn)
  } else {
    plot.cdd(cdd.data,cdd.pt,cdd.txt,legend,fail)
  }
  
  # 6. Create 2 tables where cov ratio and cook score are bigger than cutoffs
  if (!missing(cutoff_cook) || !missing(cutoff_cov)) {
    cutoff.cov.cook(raw.results.file,skipped.id.file,cutoff_cook,cutoff_cov)
  }
  
  # 7. read in input data
  out_cdd.data.all <- create.data.full(raw.results.file,skipped.id.file)
  cdd.data.all <- out_cdd.data.all$cdd.data.all
  
  # 8. influential indivifuals
  if (missing(outlier_ID)) {
    # create needed data form plotting
    list.delta.ofv <- delta.ofv.data(cdd.data.all)
    cook.score <- list.delta.ofv$cook.score
    delta.ofv <- list.delta.ofv$delta.ofv
    row_infl <- list.delta.ofv$row_infl
    ID <- list.delta.ofv$ID
    fail_ID <- list.delta.ofv$fail_ID
    # create a plot
    plot.delta.ofv(delta.ofv,cook.score,ID,row_infl,fail_ID)
  } else {
    # create needed data for plotting
    list.delta.ofv <- delta.ofv.data(cdd.data.all,outlier_ID)
    cook.score <- list.delta.ofv$cook.score
    delta.ofv <- list.delta.ofv$delta.ofv
    row_outl_infl <- list.delta.ofv$row_outl_infl
    row_outl <- list.delta.ofv$row_outl
    row_infl <- list.delta.ofv$row_infl
    ID <- list.delta.ofv$ID
    fail_ID <- list.delta.ofv$fail_ID
    # create a plot
    plot.delta.ofv(delta.ofv,cook.score,ID,row_infl,fail_ID,row_outl,row_outl_infl)
  }
  
  #close pdf
  dev.off()
}
