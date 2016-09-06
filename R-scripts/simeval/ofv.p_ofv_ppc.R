# pOFV PPC
p_ofv_ppc <- function(raw.results.file) {
  # input data 
  rawres_input <- read.csv(raw.results.file)
  rawres <- rawres_input[!is.na(rawres_input$ofv),] #take away NA values
  rownames(rawres) <- NULL
  len <- length(rawres$ofv) # numbers of rows
  pOFV_obs <- rawres$ofv[1] # obsorved OFV vaule
  pOFV_sim <- rawres$ofv[2:len] # simulated OFV values
  sort_pOFV_sim <- sort(pOFV_sim[1:len-1]) # Sort all simulated values
  newxlim <- c(sort_pOFV_sim[1],sort_pOFV_sim[len-1]) # limit for the x axis in the pOFV PPC histogram
  if(pOFV_obs > sort_pOFV_sim[len-1]){
    newxlim <- c(sort_pOFV_sim[1],pOFV_obs)}
  if(pOFV_obs < sort_pOFV_sim[1]){
    newxlim <- c(pOFV_obs,sort_pOFV_sim[len-1])}
    
  # output
  out <- list(rawres_input=rawres_input,
              rawres=rawres,
              pOFV_sim=pOFV_sim,
              pOFV_obs=pOFV_obs,
              sort_pOFV_sim=sort_pOFV_sim, # need for testing
              newxlim=newxlim) # need for testing
  return(out)  
}