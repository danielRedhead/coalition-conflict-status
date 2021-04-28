rm(list = ls())

#Load packages
library(RSiena)

# Load functions

siena07RunToConvergence <- function(alg, dat, eff, ans0, modelName, ...){

  numr <- 0

  ans <- siena07(alg, data = dat, effects = eff, prevAns = ans0, returnDeps = TRUE, ...)

  repeat{
    numr <- numr + 1
    tconv.max <- ans$tconv.max
    tratio.max <- max( abs( ans$tstat[eff$type[eff$include] != "rate"] ) )

    if (tconv.max > 100) {
      print(ans)
      cat("WARNING: Extreme divergence. Terminating run.\n")
      return("WARNING: Extreme divergence. Terminating run")
    }

    else if (tconv.max < 0.20 & tratio.max < 0.10) {
      print(ans)
      cat(paste0("Maximum Absolute Value Amongst Convergence t-Ratios: ", tratio.max, "\n"))
      cat(paste0("Model Has Converged After ", numr, " iterations. \n"))

      return(ans)
    }
    else {
      print(ans)
      cat("WARNING: Convergence Inadequate.\n")
      cat(paste0("Overall maximum convergence ratio: ", tconv.max, "\n"))
      cat(paste0("Iteration Number: ", numr), "\n")

      ans <- siena07(alg, data = dat, effects = eff, prevAns = ans, returnDeps = TRUE, ...)

    }
  }
}

normalize <- function(y) {
    x<-y[!is.na(y)]
    x<-(x - min(x)) / (max(x) - min(x))
    y[!is.na(y)]<-x
    return(y)
}

#######################################
#         LOAD & SPECIFY DATA         #
#######################################

att <- read.csv("./2-analyses/v2-attributes.csv", stringsAsFactors = FALSE, header = TRUE)
a1 <- as.matrix(read.table("./2-analyses/networks/v2-coalition.csv", sep = ","))
a2 <- as.matrix(read.table("./2-analyses/networks/v2-coalition.csv", sep = ","))
c <- as.matrix(read.table("./2-analyses/networks/v2-conflict.csv", sep = ","))
coop <- as.matrix(read.table("./2-analyses/networks/v2-cooperation.csv", sep = ","))
k <- as.matrix(read.table("./2-analyses/networks/v2-kinship.csv", sep = ","))

# We need to randomly change a tie in the 'second' (duplicate) network to fit the analysis framework
random_tie <- sample( c(1:nrow(a2[-1,])), 2, replace = FALSE)

a2[random_tie[1], random_tie[2]] <- 1

att$status <- normalize(att$status)

data <- sienaDataCreate(
                coalition =  sienaNet(array(c(
								    a1[-1, -1],
							      a2[-1, -1]),
							    	  dim = c(89, 89, 2)), allowOnly = FALSE),
                conflict = coDyadCovar(
                    c[2:nrow(c), 2:ncol(c)]),
                cooperation = coDyadCovar(
                    coop[2:nrow(coop), 2:ncol(coop)]),
                kinship = coDyadCovar(
                    k[2:nrow(k), 2:ncol(k)]),
							  status = coCovar(att$status),
							  strength = coCovar(att$strength)
							   )

alg1 <- sienaAlgorithmCreate(projname = './2-analyses/outputs/v2_coalition_conflict',
                             n3 = 1000,
                             cond = FALSE,
                             diagonalize = 1,
                             firstg = 0.15,
                             seed = 12345)

#Increase n3 to 10000 iterations for final model
alg2 <- sienaAlgorithmCreate(projname = '../2-analyses/outputs/v2_coalition_conflict',
                             n3 = 10000,
                             cond = FALSE,
                             diagonalize = 1,
                             firstg = 0.15,
                             seed = 12345)

eff1 <- getEffects(data)

# Set rate to a high number to reach a stationary distribution.
eff1 <- setEffect(eff1, Rate, initialValue=50, parameter = 50, type="rate", fix = TRUE)

# Add equivalent parameters to village 1 analyses
eff1 <- includeEffects(eff1, gwespFF,
						sharedPop, inPopSqrt, outActSqrt, name = 'coalition')
eff1 <- includeEffects(eff1, WWX, name = "coalition", interaction1 = "conflict")
eff1 <- includeEffects(eff1, X, name = "coalition", interaction1 = "kinship")
eff1 <- includeEffects(eff1, X, name = "coalition", interaction1 = "cooperation")
eff1 <- includeEffects(eff1, egoX, altX, simX, interaction1 = "status")
eff1 <- includeEffects(eff1, egoX, altX, simX, interaction1 = "strength")

# Run the model until adequate convergence
model <- siena07RunToConvergence(alg = alg1, dat = data, eff = eff1, ans0 = NULL, modelName = "model1.ans", batch = TRUE, verbose = FALSE, silent = FALSE, nbrNodes = 20, useCluster = TRUE)

# Re-run the model with a higher n3
model2 <- siena07(alg2,
                  data = data,
                  effects = eff1,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  prevAns = model,
                  returnDeps = TRUE
)

model2

save.image("./analyses/outputs/v2-analyses.RData")

print("Workspace image has been saved. End of village 2 analysis script")

######## Script stops here ##############
