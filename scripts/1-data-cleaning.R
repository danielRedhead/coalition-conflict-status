
# Let's load in the data, libraries and functions

rm(list = ls())

# Set the working directory

# Load in functions

source("./scripts/1-functions.R")

# Install & load relevant packages

library(igraph)
library(RColorBrewer)
library(sbgcop)
library(tidyverse)


#######################################
#         LOAD & SPECIFY DATA         #
#######################################

attributes <- read.csv("./1-inputs/village-1-attributes.csv",
stringsAsFactors = FALSE)


a_09 <- as.matrix(read.csv("./1-inputs/networks/village-1-allies-2009.csv",
                           header = FALSE, stringsAsFactors = FALSE))
a_14 <- as.matrix(read.csv("./1-inputs/networks/village-1-allies-2014.csv",
                           header = FALSE, stringsAsFactors = FALSE))
a_17 <- as.matrix(read.csv("./1-inputs/networks/village-1-allies-2017.csv",
                           header = FALSE, stringsAsFactors = FALSE))

# Load in the conflict networks
c_09 <- as.matrix(read.csv("./1-inputs/networks/village-1-conflict-2009.csv",
                           header = FALSE, stringsAsFactors = FALSE))
c_14 <- as.matrix(read.csv("./1-inputs/networks/village-1-conflict-2014.csv",
                           header = FALSE, stringsAsFactors = FALSE))
c_17 <- as.matrix(read.csv("./1-inputs/networks/village-1-conflict-2017.csv",
                           header = FALSE, stringsAsFactors = FALSE))


# Load food sharing
coop_09 <- as.matrix(read.csv("./1-inputs/networks/village-1-cooperation-2009.csv",
                              header = FALSE, stringsAsFactors = FALSE))
coop_14 <- as.matrix(read.csv("./1-inputs/networks/village-1-cooperation-2014.csv",
                              header = FALSE, stringsAsFactors = FALSE))
coop_17 <- as.matrix(read.csv("./1-inputs/networks/village-1-cooperation-2017.csv",
                              header = FALSE, stringsAsFactors = FALSE))

# Load in the kinship networks
k_09 <- as.matrix(read.csv("./1-inputs/networks/village-1-kinship-2009.csv",
                           header = FALSE, stringsAsFactors = FALSE))
k_14 <- as.matrix(read.csv("./1-inputs/networks/village-1-kinship-2014.csv",
                           header = FALSE, stringsAsFactors = FALSE))
k_17 <- as.matrix(read.csv("./1-inputs/networks/village-1-kinship-2017.csv",
                           header = FALSE, stringsAsFactors = FALSE))

# Village 2

v2_attributes <- read.csv("./1-inputs/village-2-attributes.csv",
                       stringsAsFactors = FALSE)
v2_a<- as.matrix(read.csv("./1-inputs/networks/village-2-allies-2008.csv",
                           header = FALSE, stringsAsFactors = FALSE))
v2_c<- as.matrix(read.csv("./1-inputs/networks/village-2-conflict-2008.csv",
                          header = FALSE, stringsAsFactors = FALSE))
v2_coop<- as.matrix(read.csv("./1-inputs/networks/village-2-cooperation-2008.csv",
                          header = FALSE, stringsAsFactors = FALSE))
v2_k <- as.matrix(read.csv("./1-inputs/networks/village-2-kinship-2008.csv",
                          header = FALSE, stringsAsFactors = FALSE))


##########################
# Village 1
##########################

##########################
# COMPOSITION
##########################

#Calculate when individuals are missing using the network tables
only_09 <- data.frame(id = attributes$id[!attributes$id %in% a_14[,1] & !attributes$id %in% a_17[,1] & attributes$id %in% a_09[,1]], comp = "1 1.8")
only_09_14 <- data.frame(id = attributes$id[attributes$id %in% a_14[,1] & !attributes$id %in% a_17[,1] & attributes$id %in% a_09[,1]], comp = "1 2.8")
only_09_17 <- data.frame(id = attributes$id[!attributes$id %in% a_14[,1] & attributes$id %in% a_17[,1] & attributes$id %in% a_09[,1]], comp = "1 1.8 2.2 3")
only_14 <- data.frame(id = attributes$id[attributes$id %in% a_14[,1] & !attributes$id %in% a_17[,1] & !attributes$id %in% a_09[,1]], comp = "2 2.8")
only_14_17 <- data.frame(id = attributes$id[attributes$id %in% a_14[,1] & attributes$id %in% a_17[,1] & !attributes$id %in% a_09[,1]], comp = "2 3")
only_17 <- data.frame(id = attributes$id[!attributes$id %in% a_14[,1] & attributes$id %in% a_17[,1] & !attributes$id %in% a_09[,1]], comp = "2.2 3")
in_all <- data.frame(id = attributes$id[attributes$id %in% a_14[,1] & attributes$id %in% a_17[,1] & attributes$id %in% a_09[,1]], comp = "1 3")

any(only_09 %in% a_14)
any(only_09_17 %in% a_14)

any(attributes$id[!is.na(attributes$respect_09)] %in% only_14$id | attributes$id[!is.na(attributes$respect_09)] %in% only_17$id
		| attributes$id[!is.na(attributes$respect_09)] %in% only_14_17$id)

any(attributes$id[!is.na(attributes$respect_14)] %in% only_09$id | attributes$id[!is.na(attributes$respect_14)] %in% only_17$id
		| attributes$id[!is.na(attributes$respect_14)] %in% only_09_17$id)

# All seems to match up

# rbind all of these tables to create a composition list
composition <- do.call("rbind", list(only_09, only_09_14, only_09_17, only_14, only_14_17, only_17, in_all))
composition <- composition[order(composition$id), ]

custom_missing_ids <- c('b7f3','ffwq','gosd','n9oc','o6k5','op7o') # Add custom ids here
all(custom_missing_ids %in% composition$id[composition$comp != "1 3"])
# Looks good

##########################
# ATTRIBUTES
##########################

# Impute age from estimated age at other observations
 for(i in 1:length(attributes$age_09)) {
   if(is.na(attributes$age_09[i])) {
         attributes$age_09[i] <- attributes$age_14[i] - 5
     }
 }

for(i in 1:length(attributes$age_09)) {
  if(is.na(attributes$age_09[i])) {
         attributes$age_09[i] <- attributes$age_17[i] - 8
      }
 }

for(i in 1:length(attributes$age_14)) {
 if(is.na(attributes$age_14[i])) {
        attributes$age_14[i] <- attributes$age_17[i] - 3
     }
}

for(i in 1:length(attributes$age_14)) {
 if(is.na(attributes$age_14[i])) {
         attributes$age_14[i] <- attributes$age_09[i] + 5
      }
 }

 for(i in 1:length(attributes$age_17)) {
   if(is.na(attributes$age_17[i])) {
    attributes$age_17[i] <- attributes$age_09[i] + 8
  }
 }

for(i in 1:length(attributes$age_17)) {
  if(is.na(attributes$age_17[i])) {
    attributes$age_17[i] <- attributes$age_14[i] + 3
  }
}

# Create a binary variable for corregidors at each observation point
attributes$corregidor_09 <- ifelse(attributes$id %in% c("ligb", "fuwh"),1,0)
attributes$corregidor_14 <- ifelse(attributes$id == "lx3h" ,1,0)
attributes$corregidor_17 <- ifelse(attributes$id == "n2wa" ,1,0)

# Check n of imputed individuals
impute <- attributes
impute$comp <- composition$comp[match(impute$id, composition$id)]

att09 <- impute[impute$comp == "1 1.8" |impute$comp == "1 2.8" | impute$comp ==  "1 1.8 2.2 3"| impute$comp ==  "1 3" ,]
att09 <- select(att09, id, comp, age_09, respect_09, influence_09, height_09, weight_09, UBS_09)
att09[is.na(att09$respect_09)|is.na(att09$influence_09),]


att14 <- impute[impute$comp == "2 2.8" |impute$comp == "2 2.2" | impute$comp ==  "2 3"| impute$comp ==  "1 3" ,]
att14 <- select(att14, id, comp, age_14, respect_14, influence_14, height_14, weight_14, UBS_14)

# compute % missing for status
(nrow(att14[is.na(att14$respect_14)|is.na(att14$influence_14),]) +
nrow(att09[is.na(att09$respect_09)|is.na(att09$influence_09),]))/((nrow(attributes)*2)*2)
# n = 9 / total n of possible status entries ((n of individuals = 113 * n of items = 2) * n of waves = 2) = 452
# This is fine as is there is missing respect, then there has to be missing influence

# Compute % missing for physical strength and size
# These have to be done separately
(nrow(att09[is.na(att09$height_09),]) + nrow(att09[is.na(att09$weight_09),]) +
  nrow(att09[is.na(att09$UBS_09),]) +
  nrow(att14[is.na(att14$height_14),]) + nrow(att14[is.na(att14$weight_14),]) +
  nrow(att14[is.na(att14$UBS_14),])) / ((nrow(attributes)*3)*2)
# n = 78 / total n of possible formidability entries ((n of individuals = 113 * n of items = 3) * n of waves = 2) = 678
# Now let's impute the missing data

attributes$internal_id <- 1:nrow(attributes)

to_imp <- select(attributes, -c(id, respect_17, influence_17,income_09,income_14))
att <-sbgcop.mcmc(to_imp, nsamp = 2000, impute = any(is.na(to_imp)), seed = 1)

# Check the imputation summaries
summary(att)
plot(att)

# Create the imputed dataset
imp_attributes <-data.frame(att$Y.pmean)
imp_attributes$dataset <- "Imputed"
imp_attributes$internal_id <- 1:nrow(imp_attributes)
imp_attributes$id <- attributes$id[match(imp_attributes$internal_id, attributes$internal_id)]
to_imp$dataset <- "Original"
to_imp$id <- attributes$id[match(to_imp$internal_id, attributes$internal_id)]

# Bind the imputed and original datasets together
to_imp <- rbind(to_imp,imp_attributes)

# Visually compare the datasets to assess the differences

plots <-  c()
iter <-  1
for (column in names(to_imp)[1:10])  {
  plots[[iter]] <- ggplot(to_imp, aes_string(x=column, color="dataset", fill="dataset")) +
    geom_density(alpha=.3)
  plots[[iter]] + scale_fill_manual( values = c("#016c59", "#67a9cf")) + scale_color_manual( values = c("#016c59", "#67a9cf"))
  iter <- iter+1
}
rm(column, iter)

# Now lets respecify attributes
imp_attributes <- select(imp_attributes, id, internal_id, everything(), -dataset)
imp_attributes <- imp_attributes[order(imp_attributes$id) , ]

# Composite status
imp_attributes$status_09 <- (imp_attributes$respect_09 +imp_attributes$influence_09)/2
imp_attributes$status_14 <- (imp_attributes$respect_14 +imp_attributes$influence_14)/2

# Create composite physical strength & size
imp_attributes$strength_09 <- (imp_attributes$height_09-mean(imp_attributes$height_09)) + (imp_attributes$weight_09-mean(imp_attributes$weight_09)) + (imp_attributes$UBS_09-mean(imp_attributes$UBS_09))/3
imp_attributes$strength_14 <- (imp_attributes$height_14-mean(imp_attributes$height_14)) + (imp_attributes$weight_14-mean(imp_attributes$weight_14)) + (imp_attributes$UBS_14-mean(imp_attributes$UBS_14))/3


##########################
# NETWORKS
##########################


v1 <-  list(a_09, a_14, a_17, c_09, c_14, c_17, coop_09, coop_14,coop_17, k_09, k_14, k_17)
village1 <- list()

for (i in seq_along(v1))  {
  village1[[i]] <- expand_network(network = v1[[i]], new_ids = missing_network_ids(v1[[i]], composition), needs_ordering = TRUE)
}


# Run a couple of sanity checks
a1 <- village1[[1]]
a2 <- village1[[2]]

all(a1[, 2:ncol(a1)][a1[1 , ] %in% only_14, ] == 0)
all(a2[, 2:ncol(a2)][a2[1 , ] %in% only_09, ] == 0)
all(a1[, 2:ncol(a1)][a1[1 , ] %in% only_17, ] == 0)
all(a2[, 2:ncol(a2)][a2[1 , ] %in% only_17, ] == 0)


# All is good

# Change id to numeric internal id
for (i in seq_along(village1)) {
 village1[[i]][1,] <- imp_attributes$internal_id[match(village1[[i]][1,], imp_attributes$id)]
  village1[[i]][,1] <- imp_attributes$internal_id[match(village1[[i]][,1], imp_attributes$id)]
}


# Change class to numeric
for (i in seq_along(village1)) {
  class(village1[[i]]) <- "numeric"
}

#Check for self-nominations & binary-network
sapply(village1,self_nominations)
sapply(village1,binary_network)


##########################
# VILLAGE 2
##########################


##########################
# ATTRIBUTES
##########################

v2_attributes$internal_id <- 1:nrow(v2_attributes)

v2_to_imp <- select(v2_attributes, -id:-community)
v2_att <-sbgcop.mcmc(v2_to_imp, nsamp = 2000, impute = any(is.na(v2_to_imp)), seed = 1)

# Check the imputation summaries
summary(v2_att)
plot(v2_att)

v2_imp_attributes <-data.frame(v2_att$Y.pmean)
v2_imp_attributes$dataset <- "Imputed"
v2_imp_attributes$internal_id <- 1:nrow(v2_imp_attributes)
v2_imp_attributes$id <- v2_attributes$id[match(v2_imp_attributes$internal_id, v2_attributes$internal_id)]
v2_to_imp$dataset <- "Original"
v2_to_imp$id <- v2_attributes$id[match(v2_to_imp$internal_id, v2_attributes$internal_id)]

# Bind the imputed and original datasets together
v2_to_imp <- rbind(v2_to_imp,v2_imp_attributes)

# Visually compare the datasets to assess the differences

plots <-  c()
iter <-  1
for (column in names(v2_to_imp)[1:10])  {
  plots[[iter]] <- ggplot(v2_to_imp, aes_string(x=column, color="dataset", fill="dataset")) +
    geom_density(alpha=.3)
  plots[[iter]] + scale_fill_manual( values = c("#016c59", "#67a9cf")) + scale_color_manual( values = c("#016c59", "#67a9cf"))
  iter <- iter+1
}
rm(column, iter)

# Now lets respecify attributes
v2_imp_attributes <- select(v2_imp_attributes, id, internal_id, everything(), -dataset)
v2_imp_attributes <- v2_imp_attributes[order(v2_imp_attributes$id) , ]

# Composite status
v2_imp_attributes$status <- (v2_imp_attributes$respect + v2_imp_attributes$influence)/2

# Create composite physical strength & size
v2_imp_attributes$strength <- (v2_imp_attributes$height-mean(v2_imp_attributes$height)) + (v2_imp_attributes$weight-mean(v2_imp_attributes$weight)) + (v2_imp_attributes$ubs-mean(v2_imp_attributes$ubs))/3

##########################
# NETWORKS
##########################

village2 <-  list(v2_a, v2_c, v2_coop, v2_k)

sapply(village2,self_nominations)
sapply(village2,binary_network)

diag(village2[[1]]) <- 0
diag(village2[[3]]) <- 0

# Change id to numeric internal id
for (i in seq_along(village2)) {
  village2[[i]][1,] <- v2_imp_attributes$internal_id[match(village2[[i]][1,], v2_imp_attributes$id)]
  village2[[i]][,1] <- v2_imp_attributes$internal_id[match(village2[[i]][,1], v2_imp_attributes$id)]
}

# Change class to numeric
for (i in seq_along(village2)) {
  class(village2[[i]]) <- "numeric"
}

##########################
# SAVE NEW DATA
##########################

v1_att_clean <- select(imp_attributes, id:internal_id, corregidor_09:strength_14)
v2_att_clean <- select(v2_imp_attributes, id:internal_id, status:strength)

# Write out the data for analyses
write.table(village1[[1]], "./2-analyses/networks/v1-coalition-1.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[2]], "./2-analyses/networks/v1-coalition-2.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[3]], "./2-analyses/networks/v1-coalition-3.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[4]], "./2-analyses/networks/v1-conflict-1.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[5]], "./2-analyses/networks/v1-conflict-2.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[6]], "./2-analyses/networks/v1-conflict-3.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[7]], "./2-analyses/networks/v1-cooperation-1.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[8]], "./2-analyses/networks/v1-cooperation-2.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[9]], "./2-analyses/networks/v1-cooperation-3.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[10]], "./2-analyses/networks/v1-kinship-1.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[11]], "./2-analyses/networks/v1-kinship-2.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village1[[12]], "./2-analyses/networks/v1-kinship-3.csv", row.names = FALSE, col.names = FALSE, sep = ',')

write.csv(v1_att_clean, "./2-analyses/v1-attributes.csv", row.names = FALSE)

write.table(composition[,-1], "./2-analyses/v1-composition.txt", sep=";", col.names=FALSE, quote=FALSE, row.names=FALSE)

write.table(village2[[1]], "./2-analyses/networks/v2-coalition.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village2[[2]], "./2-analyses/networks/v2-conflict.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village2[[3]], "./2-analyses/networks/v2-cooperation.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.table(village2[[4]], "./2-analyses/networks/v2-kinship.csv", row.names = FALSE, col.names = FALSE, sep = ',')
write.csv(v2_att_clean, "./2-analyses/v2-attributes.csv", row.names = FALSE)

print("Data cleaning has finished & new files have been written into the analyses folder")

######## Script stops here ##############


