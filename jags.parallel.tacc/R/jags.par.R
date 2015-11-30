#' Parallel n chain jags
#'
#' This function parallels n.chain and runs jags.
#' @param cl    an object of class "cluster"
#' @param ...   other arguments. See in jags in R2jags
#' @keywords jags.par
#' @export
#' @examples
#' jags.par()

jags.par <- function(cl, data, inits, parameters.to.save
                     ,model.file, n.iter,
                     n.chains, n.thin, n.adapt=0, n.burnin,
                     DIC = TRUE){
  requireNamespace("dclone")
  n.update=n.burnin
  parListModules(cl)
  tmp <- clusterEvalQ(cl, library(dclone))
  parLoadModule(cl, "glm")
  
  if (DIC) {
    parameters.to.save <- c(parameters.to.save, "deviance")
    parLoadModule(cl,"dic")
  }
  samples <- jags.parfit(cl,data = data,
                         inits = inits,
                         params=parameters.to.save,
                         model= model.file, 
                         n.iter= (n.iter-n.burnin),
                         n.chains=n.chains.global,thin=n.thin,
                         n.adapt= 0,n.update = n.burnin)
  stopCluster(cl)
  
  print(proc.time()-pt)
  print("parallel completed")
  save(samples,file=paste("samples",".Rdat",sep=""))
  
  library(R2WinBUGS)
  fit <- mcmc2bugs(samples, model.file = model.file, program = "jags", 
                   DIC = DIC, DICOutput = NULL, n.iter = n.iter
                   , n.burnin = n.burnin, 
                   n.thin = n.thin)
  
  out <- list(BUGSoutput = fit, parameters.to.save = parameters.to.save, 
                             model.file = model.file, 
                             n.iter = n.iter, DIC = DIC)
  class(out) <- "rjags"
  
  rm(data,inits);gc()
  
  return(out)  
  
}  
  