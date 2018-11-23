#' Create a multivariate normal mixing distribution with semi conjugate prior
#'
#' @param priorParameters The prior parameters for the Multivariate Normal.
#' @export
Mvnormal2Create <- function(priorParameters) {


  if (class(priorParameters$mu0) != "matrix"){
    priorParameters$mu0 <- matrix(priorParameters$mu0, nrow=1)
  }

  mdobj <- MixingDistribution("mvnormal2", priorParameters, "nonconjugate")

  return(mdobj)
}

Likelihood.mvnormal2 <- function(mdobj, x, theta) {

  y <- sapply(seq_len(dim(theta[[1]])[3]),
              function(i) mvtnorm::dmvnorm(x, theta[[1]][,, i], theta[[2]][, , i]))

  return(y)
}

PriorDraw.mvnormal2 <- function(mdobj, n = 1) {

  priorParameters <- mdobj$priorParameters

  sig <- simplify2array(lapply(seq_len(n),
                               function(x) solve(rWishart(1,
                                                          priorParameters$nu0,
                                                          solve(priorParameters$phi0))[,,1])))

  mu <- simplify2array(lapply(seq_len(n),
                              function(x) mvtnorm::rmvnorm(1,
                                                           priorParameters$mu0,
                                                           priorParameters$sigma0)))

  theta <- list(mu = mu, sig = sig)
  return(theta)
}


# PosteriorDraw.mvnormal2 <- function(mdobj, x, n = 1) {
#
#   post_parameters <- PosteriorParameters(mdobj, x)
#
#   sig <- rWishart(n, post_parameters$nu_n, post_parameters$t_n)
#   mu <- simplify2array(lapply(seq_len(n), function(x) mvtnorm::rmvnorm(1, post_parameters$mu_n,
#                                                                        solve(post_parameters$kappa_n * sig[, , x]))))
#
#   return(list(mu = mu, sig = sig/post_parameters$kappa_n^2))
# }

PosteriorDraw.mvnormal2 <- function(mdobj, x, mhDraws = 1, start_pos) {

  if (!is.matrix(x)) {
    x <- matrix(x, ncol = length(x))
  }

  phi0 <- mdobj$priorParameters$phi0

  mu0 <- mdobj$priorParameters$mu0
  sigma0 <- mdobj$priorParameters$sigma0

  muSamples <- array(dim = c(dim(mu0), mhDraws))
  sigSamples <- array(dim = c(dim(phi0), mhDraws))

  muSamp <- matrix(rep_len(0, ncol(mu0)), ncol=ncol(mu0))

  for (i in seq_len(mhDraws)){

    nuN <- nrow(x) +  mdobj$priorParameters$nu0
    phiN <- phi0 + Reduce("+", lapply(seq_len(nrow(x)),
                                    function(j) (x[j,] - c(muSamp)) %*% t(x[j,] - c(muSamp))))

    sigSamp <- solve(rWishart(1, nuN, solve(phiN))[,,1])

    sigN <- solve(solve(sigma0) + nrow(x) * solve(sigSamp))
    muN <- sigN %*% (nrow(x)*solve(sigSamp) %*% colMeans(x) + solve(sigma0) %*% c(mu0))

    muSamp <- mvtnorm::rmvnorm(1, muN, sigN)

    muSamples[,,i] <- muSamp
    sigSamples[,,i] <- sigSamp

  }

  return(list(mu=muSamples, sig=sigSamples))
}



