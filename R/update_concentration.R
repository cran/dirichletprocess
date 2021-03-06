
update_concentration <- function(oldParam, n, nParams, priorParameters){

  x <- rbeta(1, oldParam + 1, n)

  pi1 <- priorParameters[1] + nParams - 1
  pi2 <- n * (priorParameters[2] - log(x))
  pi1 <- pi1/(pi1 + pi2)

  postParams1 <- priorParameters[1] + nParams
  postParams2 <- priorParameters[2] - log(x)

  if (runif(1) > pi1) {
    #g1 <- rgamma(1, priorParameters[1] + nParams, priorParameters[2] - log(x))
    #new_alpha <- g1
    postParams1 <- postParams1 - 1
  } #else {
    #g2 <- rgamma(1, priorParameters[1] + nParams - 1, priorParameters[2] - log(x))
    #new_alpha <- g2
  #}

  new_alpha <- rgamma(1, postParams1, postParams2)
  return(new_alpha)
}

UpdateGamma <- function(dpobjlist){

  globalLabels <- lapply(seq_along(dpobjlist$indDP), function(x) match(dpobjlist$indDP[[x]]$clusterParameters[[1]],
                                                                 dpobjlist$globalParameters[[1]]))
  for (i in seq_along(globalLabels)){
    globalLabels[[i]] <- true_cluster_labels(globalLabels[[i]], dpobjlist)
  }
  globalParamTable <- data.frame(table(GlobalParam=unlist(globalLabels)))
  globalParamTable$GlobalParam <- as.numeric(levels(globalParamTable$GlobalParam))

  numParams <- nrow(globalParamTable)
  numTables <- sum(globalParamTable$Freq)

  newGamma <- update_concentration(dpobjlist$gamma, numTables, numParams, dpobjlist$gammaPriors)

  for(i in seq_along(dpobjlist)){
    dpobjlist$gamma <- newGamma
  }
  return(dpobjlist)
}
