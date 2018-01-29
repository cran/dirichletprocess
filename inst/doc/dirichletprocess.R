### R code from vignette source 'dirichletprocess.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: preliminaries
###################################################
#options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library(dirichletprocess)


###################################################
### code chunk number 2: student-t
###################################################
y <- rt(200, 3) + 2 #generate sample data
dp <- DirichletProcessGaussian(y)
dp <- Fit(dp, 1000, progressBar = FALSE)


###################################################
### code chunk number 3: oldfaithfull (eval = FALSE)
###################################################
## its <- 500
## faithfulTransformed <- faithful$waiting - mean(faithful$waiting)
## faithfulTransformed <- faithfulTransformed/sd(faithful$waiting)
## dp <- DirichletProcessGaussian(faithfulTransformed)
## dp <- Fit(dp, its)
## plot(dp)


###################################################
### code chunk number 4: customsampling (eval = FALSE)
###################################################
## dp <- DirichletProcessGaussian(y)
## 
## samples <- list()
## for(s in seq_len(1000)){
##   dp <- ClusterComponentUpdate(dp)
##   dp <- ClusterParameterUpdate(dp)
## 
##   if(s %% 10 == 0) {
##   	dp <- UpdateAlpha(dp)
##   }
##   samples[[s]] <- list()
##   samples[[s]]$phi <- dp$clusterParameters
##   samples[[s]]$weights <- dp$weights
## }


###################################################
### code chunk number 5: toy-beta (eval = FALSE)
###################################################
## y <- c(rbeta(150, 1, 3), rbeta(150, 7, 3)) #generate sample data
## dp <- DirichletProcessBeta(y, 1)
## dp <- Fit(dp, 1000)


###################################################
### code chunk number 6: clustering (eval = FALSE)
###################################################
## faithfulTrans <- as.matrix(apply(faithful, 2, function(x) (x-mean(x))/sd(x)))


###################################################
### code chunk number 7: clustering-fit (eval = FALSE)
###################################################
## dp <-  DirichletProcessMvnormal(faithfulTrans)
## dp <- Fit(dp, 1000)


###################################################
### code chunk number 8: clustering-print (eval = FALSE)
###################################################
## faithRes <- cbind(faithful, Labels = mvDPFaith$clusterLabels)
## ggplot(faithRes, aes(x=eruptions, y=waiting, colour=as.factor(Labels))) + geom_point()


###################################################
### code chunk number 9: rats (eval = FALSE)
###################################################
## numSamples = 200
## thetaDirichlet <- matrix(nrow=numSamples, ncol=nrow(rats))
## 
## dpobj <- DirichletProcessBeta(rats$y/rats$N,
## maxY=1, g0Priors = c(2, 150),
## mhStep=c(0.25, 0.25), hyperPriorParameters = c(1, 1/150))
## dpobj <- Fit(dpobj, 10)
## 
## clusters <- dpobj$clusterParameters
## 
## a <- clusters[[1]] * clusters[[2]]
## b <- (1 - clusters[[1]]) * clusters[[2]]
## 
## for(i in seq_len(numSamples)){
## 
##   posteriorA <- a[dpobj$clusterLabels] + rats$y
##   posteriorB <- b[dpobj$clusterLabels] + rats$N - rats$y
##   thetaDirichlet[i, ] <- rbeta(nrow(rats), posteriorA, posteriorB)
## 
##   dpobj <- ChangeObservations(dpobj, theta_dir[i, ])
##   dpobj <- Fit(dpobj, 5)
##   clusters <- dpobj$clusterParameters
## 
##   a <- clusters[[1]] * clusters[[2]]
##   b <- (1 - clusters[[1]]) * clusters[[2]]
## }


###################################################
### code chunk number 10: hierarachical-gen (eval = FALSE)
###################################################
## mu <- c(0.25, 0.75, 0.4)
## tau <- c(5, 6, 10)
## a <- mu * tau
## b <- (1 - mu) * tau
## y1 <- c(rbeta(100, a[1], b[1]), rbeta(100, a[2], b[2]))
## y2 <- c(rbeta(100, a[1], b[1]), rbeta(100, a[3], b[3]))


###################################################
### code chunk number 11: hierarchical (eval = FALSE)
###################################################
## dpobjlist <- DirichletProcessHierarchicalBeta(list(y1, y2), maxY=1,
## hyperPriorParameters = c(1, 0.01), mhStepSize = c(0.1, 0.1),
## gammaPriors = c(2, 4), alphaPriors = c(2, 4))
## dpobjlist <- Fit(dpobjlist, 5000, TRUE)


###################################################
### code chunk number 12: stickbreaking-gen (eval = FALSE)
###################################################
## y <- cumsum(runif(1000))
## pdf <- function(x) sin(x/50)^2
## accept_prob <- pdf(y)
## pts <- sample(y, 500, prob=accept_prob)


###################################################
### code chunk number 13: stickbreaking (eval = FALSE)
###################################################
## dp <- DirichletProcessBeta(sample(pts, 100), maxY = max(pts)*1.01,
## alphaPrior = c(2, 0.01))
## dp <- Fit(dp, 100, TRUE)
## 
## for(i in seq_len(2000)){
##   lambdaHat <- PosteriorFunction(dp)
##   newPts <- sample(pts, 150, prob=lambdaHat(pts))
##   newPts[is.infinite(newPts)] <- 1
##   newPts[is.na(newPts)] <- 0
##   dp <- ChangeObservations(dp, newPts)
##   dp <- Fit(dp, 2, TRUE)
## }


###################################################
### code chunk number 14: poisson (eval = FALSE)
###################################################
## y <- c(rpois(150, 3), rpois(150, 10)) #generate sample data
## dp <- DirichletProcessCreate(y, poisMd)
## dp <- Initialise(dp)
## dp <- Fit(dp, 1000)


###################################################
### code chunk number 15: gamma (eval = FALSE)
###################################################
## y <- c(rgamma(100, 2, 4), rgamma(100, 6, 3)) #generate sample data
## dp <- DirichletProcessCreate(y, gammaMd)
## dp <- Initialise(dp)
## dp <- Fit(dp, 1000)


###################################################
### code chunk number 16: cluster-prediciton (eval = FALSE)
###################################################
## faithfulTrans <- as.matrix(apply(faithful, 2, function(x) (x-mean(x))/sd(x)))
## trainIndex <- 1:(nrow(faithfulTrans)-5)
## 
## dp <-  DirichletProcessMvnormal(faithfulTrans[trainIndex, ])
## dp <- Fit(dp, 1000)
## 
## labelPred <- ClusterLabelPredict(mvDPFaith, faithfulTrans[-trainIndex, ])


###################################################
### code chunk number 17: censored (eval = FALSE)
###################################################
## dpA <- DirichletProcessCreate(data_a, mdobjA, c(2, 0.9))
## dpA <- Initialise(apA)
## 
## dpB <- DirichletProcessCreate(data_b, mdobjB, c(2, 0.9))
## dpB <- Initialise(dpB)
## 
## dpA <- Fit(dpA, 500, TRUE)
## dpB <- Fit(dpB, 500, TRUE)


