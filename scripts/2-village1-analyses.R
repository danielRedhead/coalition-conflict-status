

# Set working directory


#Load packages
library(RSiena)
library(dplyr)

# Load functions

normalize <- function(y) {
    x<-y[!is.na(y)]
    x<-(x - min(x)) / (max(x) - min(x))
    y[!is.na(y)]<-x
    return(y)
}


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

    else if (tconv.max < 0.19 & tratio.max < 0.10) {
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

#######################################
#         LOAD & SPECIFY DATA         #
#######################################

att <- read.csv("./2-analyses/v1-attributes.csv", stringsAsFactors = FALSE, header = TRUE)
a1 <- as.matrix(read.table("./2-analyses/networks/v1-coalition-1.csv", sep = ","))
a2 <- as.matrix(read.table("./2-analyses/networks/v1-coalition-2.csv", sep = ","))
a3 <- as.matrix(read.table("./2-analyses/networks/v1-coalition-3.csv", sep = ","))
c1 <- as.matrix(read.table("./2-analyses/networks/v1-conflict-1.csv", sep = ","))
c2 <- as.matrix(read.table("./2-analyses/networks/v1-conflict-2.csv", sep = ","))
coop1 <- as.matrix(read.table("./2-analyses/networks/v1-cooperation-1.csv", sep = ","))
coop2 <- as.matrix(read.table("./2-analyses/networks/v1-cooperation-2.csv", sep = ","))
k1 <- as.matrix(read.table("./2-analyses/networks/v1-kinship-1.csv", sep = ","))
k2 <- as.matrix(read.table("./2-analyses/networks/v1-kinship-2.csv", sep = ","))

att$status_09 <- normalize(att$status_09)
att$status_14 <- normalize(att$status_14)


data <- sienaDataCreate(
                coalition =  sienaDependent(array(c(
								    a1[-1, -1],
							      a2[-1, -1],
                    a3[-1, -1]),
							    	  dim = c(113, 113, 3))),
                conflict = varDyadCovar(array(c(
                    c1[2:nrow(c1), 2:ncol(c1)],
                    c2[2:nrow(c2), 2:ncol(c2)]),
                      dim = c(113, 113, 2)), centered = FALSE),
                kinship = varDyadCovar(array(c(
                    k1[2:nrow(k1), 2:ncol(k1)],
                    k2[2:nrow(k2), 2:ncol(k2)]),
                      dim = c(113, 113, 2)), centered = FALSE),
                cooperation = varDyadCovar(array(c(
                    coop1[2:nrow(coop1), 2:ncol(coop1)],
                    coop2[2:nrow(coop2), 2:ncol(coop2)]),
                      dim = c(113, 113, 2)), centered = FALSE),
							status = varCovar(as.matrix(select(att, status_09, status_14))),
							strength = varCovar(as.matrix(select(att, strength_09, strength_14))),
							leader = varCovar(as.matrix(select(att, corregidor_09, corregidor_14))),
                composition = sienaCompositionChangeFromFile("./2-analyses/v1-composition.txt")
							   )


alg1 <- sienaAlgorithmCreate(projname = './2-analyses/outputs/coalition_conflict',
                             n3 = 1000,
                             firstg = 0.15,
                             seed = 12345)

#Increase n3 to 10000 iterations for final model

alg2 <- sienaAlgorithmCreate(projname = './2-analyses/outputs/coalition_conflict',
                             n3 = 10000,
                             firstg = 0.15,
                             seed = 12345)

eff1 <- getEffects(data)
eff1 <- includeEffects(eff1, gwespFF,
						sharedPop, inPopSqrt, outActSqrt, name = 'coalition')
eff1 <- setEffect(eff1, Rate, initialValue=40, parameter = 40, type="rate", period=1, fix = TRUE)

#Run base model

model1 <- siena07RunToConvergence(alg1,
                        dat = data,
                        eff = eff1,
                        initC = TRUE,
                        useCluster = TRUE,
                        nbrNodes = 20,
                        batch = TRUE,
						silent = FALSE,
                        ans0 = NULL,
                 		modelName = "model.ans"
						)

model1


# Add exogenous effects network

eff2 <- includeEffects(eff1, WWX, name = "coalition", interaction1 = "conflict")
eff2 <- includeEffects(eff2, X, name = "coalition", interaction1 = "kinship")
eff2 <- includeEffects(eff2, X, name = "coalition", interaction1 = "cooperation")
eff2 <- includeEffects(eff2, egoX, altX, simX, interaction1 = "status", name = "coalition")
eff2 <- includeEffects(eff2, egoX, altX, simX, interaction1 = "strength", name = "coalition")
eff2 <- includeEffects(eff2, egoX, altX, interaction1 = "leader", name = "coalition")

model2 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff2,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model1,
                  modelName = "model.ans"
)

model2


tt_1 <- sienaTimeTest(model2)
summary(tt_1)

eff3 <- includeTimeDummy(eff2, X, interaction1 = "cooperation")

model3 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff3,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model2,
                  modelName = "model.ans"
)

model3


tt_2 <- sienaTimeTest(model3)
summary(tt_2)

eff4 <- includeTimeDummy(eff3, WWX, interaction1 = "conflict")

model4 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff4,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model3,
                  modelName = "model.ans"
)

model4

tt_3 <- sienaTimeTest(model4)
summary(tt_3)

eff5 <- includeTimeDummy(eff4, egoX, interaction1 = "status")

model5 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff5,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model4,
                  modelName = "model.ans"
)


model5

tt_4 <- sienaTimeTest(model5)
summary(tt_4)


eff6 <- includeTimeDummy(eff5, altX, interaction1 = "leader")

model6 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff6,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model5,
                  modelName = "model.ans"
)


model6

tt_5 <- sienaTimeTest(model6)
summary(tt_5)

eff7 <- includeTimeDummy(eff6, altX, interaction1 = "strength")

model7 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff7,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model6,
                  modelName = "model.ans"
)

model7

model8 <- siena07(alg2,
  dat = data,
  eff = eff7,
  initC = TRUE,
  useCluster = TRUE,
  nbrNodes = 20,
  batch = TRUE,
  verbose = FALSE,
  silent = FALSE,
  prevAns = model7)


save.image("./2-analyses/v1-analyses.RData")


print("Workspace image has been saved. End of village 1 analysis script")


######## Script stops here ##############
