
rm(list = ls())

# Set working directory

#Load relevant packages
library(igraph)
library(RColorBrewer)
library(tidyverse)
library(ggpubr)
library(viridis)
library(readxl)
library(dplyr)
library(grid)
library(corrplot)
#Load relevant functions

normalize <- function(y) {
    x<-y[!is.na(y)]
    x<-(x - min(x)) / (max(x) - min(x))
    y[!is.na(y)]<-x
    return(y)
}

Jaccard <- function(net1,net2){
  # a and b should be 0-1 arrays
  tab <- table(net1[2:nrow(net1), 2:ncol(net1)],net2[2:nrow(net2), 2:ncol(net2)])
  pa <- tab["1","1"] + tab["1","0"]
  pb <- tab["1","1"] + tab["0","1"]
  pab_ind <- pa*pb/sum(tab)
  J_ind <- pab_ind / (pa + pb - pab_ind)
  tab["1","1"]/(tab["1","1"] + tab["0","1"] + tab["1","0"])
}

####################################################################################################################################################################################################################################
# BELOW IS COMMENTED OUT CODE TO PRODUCE FIGURE 1. AS THE DATA IS VERY SENSITIVE, WE CANNOT RELEASE IT OPENLY. PLEASE ALSO NOT THAT VILLAGE NAMES HAVE BEEN REPLACED TO ENSURE PRIVACY.
####################################################################################################################################################################################################################################
#figure_1a <- function(main = "Total Conflicts", cutoff = 0.001, printlabels = T, newlabels = c(), alpha = 0.5) {
#  conflicts <- as.data.frame(read_excel("./1-inputs/ChrisConflictData.xlsx"))
#  conflicts <- as.data.frame(conflicts)
#  names(conflicts)[names(conflicts) == 'Village'] <- 'community'
#  names(conflicts)[names(conflicts) == 'MidPID'] <- 'ego_id'
#  names(conflicts)[names(conflicts) == 'ConflictPartner MidPID'] <- 'alter_id'
#  names(conflicts)[names(conflicts) == 'Conflict Type'] <- 'type'
#  conflicts <- conflicts %>% select(community, ego_id, alter_id, type)
#  included_communities <- c('village_2', 'village_1 2009','village_1 2014','village_1 2017')
#  conflicts <- conflicts %>% filter(community %in% included_communities) %>% filter(!is.na(type))
#  input <- conflicts$type
#  data <- data.frame(table(input))
#  colnames(data) <- c("category", "count")
#  if(length(newlabels) > 0) {
#    if(length(data$category) != length(newlabels))  {
#      stop("Mismatched length of labels present and labels provided. Exiting function")
#    }
#    data$category <- newlabels
#   }
#  if(printlabels == T)  {
#    cat("Manually check the labels of the column for discrepacies:")
#    print(data['category'])
#  }
#  data$fraction <- data$count / sum(data$count)
#
#  if(nrow(data[which(data$fraction <= cutoff), ]) != 0) {
#    others <- data[which(data$fraction <= cutoff), ]
#    add <- data.frame(category = "others", count = sum(others$count), fraction = sum(others$fraction))
#    data <- data[which(data$fraction > cutoff), ]
#    data <- rbind(data, add)
#  }
#
#  data <- data[order(data$count, decreasing = T),] # Ordering the data
#  row.names(data) <- 1:nrow(data)  # Changing row names so the plot color follows the viridis palette
#  data$ymax <- cumsum(data$fraction)
#  data$ymin <- c(0, head(data$ymax, n=-1))
#  data$labelPosition <- (data$ymax + data$ymin) / 2
#  # data$label <- paste0(data$category, "\n n: ", data$count, " (", round(data$fraction * 100, 0),"%)")
#  data$label <- paste0(round(data$fraction * 100, 1)," % ",data$category)
#
#  data$new_category <- factor(as.character(data$category), levels = as.character(data$category)) # To change factor levels
#  p <- ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = new_category)) +
#    geom_rect() +
#    # geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 3, label.size = NA ) +
#    geom_text(x = 2, aes(y = c(0), label = c(paste0(main,"\n", sum(data$count)))), size = 4) +
#    # Using custom colors with custom alpha to match the ones in the stacked bar plot
#    scale_fill_manual(name = 'Conflict Type', values=viridis(length(unique(conflicts$type)), alpha = 0.5), label = data$label) +
#    coord_polar(theta = "y") +
#    xlim(c(2, 4)) +
#
#    # theme_void() +
#    # theme(legend.position = "none")
#
#    theme_minimal() +
#    theme(
#      legend.position = c(1.8,0.5),
#      axis.text = element_blank(),
#      axis.title = element_blank(),
#      panel.grid = element_blank(),
#      plot.margin = unit(rep(-1,4), "cm")
#    )
#  return(p)
#}
#a <- figure_1a()
#
#figure_1b <- function(){
#
#  conflicts <- as.data.frame(read_excel("./data/ChrisConflictData.xlsx"))
#  conflicts <- as.data.frame(conflicts)
#  names(conflicts)[names(conflicts) == 'Village'] <- 'community'
#  names(conflicts)[names(conflicts) == 'MidPID'] <- 'ego_id'
#  names(conflicts)[names(conflicts) == 'ConflictPartner MidPID'] <- 'alter_id'
#  names(conflicts)[names(conflicts) == 'Conflict Type'] <- 'type'
#  conflicts <- conflicts %>% select(community, ego_id, alter_id, type)
#  included_communities <- c('village_2', 'village_1 2009','village_1 2014','village_1 2017')
#  conflicts <- conflicts %>% filter(community %in% included_communities) %>% filter(!is.na(type))
#  conflicts[conflicts == 'village_1 2009' | conflicts == 'village_1 2014' | conflicts == 'village_1 2017'] <- 'Village 1'
# conflicts[conflicts == 'village_2'] <- 'Village 2'
#
#  conflicts <- conflicts %>% select(-c(alter_id))
#
#  con_type <- unique(conflicts$type)
#  con_id <- unique(conflicts$ego_id[!is.na(conflicts$ego_id)])
#
#  new_con <- data.frame(matrix(ncol=length(con_type),nrow=0, dimnames=list(NULL, con_type)), stringsAsFactors = F)
#
#  new_con <- rbind(new_con, data.frame(id = con_id))
#  new_con[,con_type] <- 0
#  rownames(new_con) <- con_id
#  new_con <- new_con %>% select(-c(id))
#
#  for (i in con_id) {
#    for (j in con_type) {
#      new_con[i,j] <- sum((conflicts %>% filter(ego_id == i))$type == j)
#    }
#  }
#
#
#  data <- cbind(data.frame(individual = con_id), new_con)
#  data <- cbind(data.frame(group = conflicts[match(data$individual, conflicts$ego_id),"community"]), data)
#  rownames(data) <- seq(1:nrow(data))
#  data <- data[,c(2,1,3:ncol(data))]
#
#
#  # To order by total number of conflicts
#  data <- data %>% mutate(sum = rowSums(.[3:ncol(data)]))
#  data <- data %>% arrange(group, sum)
#
#  order_v1 <- data$individual[data$group == "Village 1"]
#  order_v2 <- data$individual[data$group == "Village 2"]
#
#  data <- data %>% select(-c(sum))
#
#  # group column needs to be factors for nlevels() function to work below
#  data$group <- as.factor(data$group)
#
#  data_temp <- data
#  # Transform data in a tidy format (long format)
#  data <- data %>% gather(key = "observation", value="value", -c(1,2))
#
#  # Set a number of 'empty bar' to add at the end of each group
#  empty_bar <- 2
#  nObsType <- nlevels(as.factor(data$observation))
#  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
#  colnames(to_add) <- colnames(data)
#  to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
#  data <- rbind(data, to_add)
#  data <- data %>% arrange(group, individual)
#
#  # To order the individuals using factor levels so it is plotted from low to higher number of conflicts
#  data1 <- data %>% filter(group == "Village 1")
#  data1 <- data1[order(factor(data1$individual, levels = order_v1)),]
#  data2 <- data %>% filter(group == "Village 2")
#  data2 <- data2[order(factor(data2$individual, levels = order_v2)),]
#  data <- rbind(data2, data1)
#
#  row.names(data) <- 1:nrow(data)
#
#  data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
#
#  # Get the name and the y position of each label
#  label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
#
#  # Now, sorting label_data according to total
#  #label
#  number_of_bar <- nrow(label_data)
#  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
#  label_data$hjust <- ifelse( angle < -90, 1, 0)
#  label_data$angle <- ifelse(angle < -90, angle+180, angle)
#
#  # prepare a data frame for base lines
#  base_data <- data %>%
#    group_by(group) %>%
#    summarize(start=min(id), end=max(id) - empty_bar) %>%
#    rowwise() %>%
#    mutate(title=mean(c(start, end)))
#
#  # prepare a data frame for grid (scales)
#  grid_data <- base_data
#  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
#  grid_data$start <- grid_data$start - 1
#  grid_data <- grid_data[-1,]    # Include this line only for more than 1 group in data
#
#  # Make the plot
#  data <-  data %>% arrange(group, value)
#  data <- data %>% filter(is.na(value) | value != 0)
#
#  # Creating labels
#  labels <- group_by(data, observation) %>% summarize(n = sum(value)) %>% filter(!is.na(observation))
#  labels$percentage <- round(labels$n/sum(labels$n, na.rm = T)*100, 1)
#  labels$label <- paste0(labels$percentage,' % ',labels$observation)
#  labels <- labels[order(labels$n, decreasing = T),] # Order according to decreasing percentages
#
#  data$observation <- factor(data$observation, levels = as.character(labels$observation))
#  p <- ggplot(data) +
#
#    # Add the stacked bar
#    geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
#    scale_fill_manual(name = 'Observation', values=viridis(length(unique(conflicts$type)), alpha = 0.5), label = labels$label) +
#
#    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
#    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#    geom_segment(data=grid_data, aes(x = end, y = 2, xend = start, yend = 2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#    geom_segment(data=grid_data, aes(x = end, y = 4, xend = start, yend = 4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#    geom_segment(data=grid_data, aes(x = end, y = 6, xend = start, yend = 6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#    geom_segment(data=grid_data, aes(x = end, y = 8, xend = start, yend = 8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#    geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + # Added another line for 10. Changed values
#
#    # Add text showing the value of each categories
#    ggplot2::annotate("text", x = rep(max(data$id),6), y = c(0, 2, 4, 6, 8, 10), label = c("0", "2", "4", "6", "8", "10") , color="grey", size=4 , angle=0, fontface="bold", hjust=1) + # Changed values and labels
#
#    ylim(-20,max(label_data$tot, na.rm=T)+6) + #Changed ylim values
#    theme_minimal() +
#    theme(
#      legend.position = "none",
#      axis.text = element_blank(),
#      axis.title = element_blank(),
#      panel.grid = element_blank(),
#      plot.margin = unit(rep(-1,4), "cm")
#    ) +
#    coord_polar() +
#
#    # Add base line information
#    geom_segment(dat=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE ) +
#
#    # Modify 'hjust' values to the same length as groups in data
#    geom_text(data=base_data, aes(x = title, y = 7, label=group), hjust=c(0.1,0.7), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) # Changed y value
#
#  return(p)
#}
#b <- figure_1b()

####################################################################################################################################################################################################################################


table_1 <- function(networks, names, directed) {
  output <- setNames(data.frame(matrix(ncol = length(parameters), nrow = 0)), names)
  for(network in networks) {
    nets <- graph_from_adjacency_matrix(as.matrix(network[2:nrow(network),2:ncol(network)]), mode= directed )
    size <- sum(network[2:nrow(network),2:ncol(network)], na.rm = T)
    den <- round(edge_density(nets), 3)                 # get the network density
    rec <- round(reciprocity(nets), 3)                  # general tendency towards reciprocity
    tra <- round(transitivity(nets), 3)                 # general tendency towards transitivity
    cent <- round((centralization.degree(nets)$centralization), 3)
    ind <- round(mean(degree(nets, mode = "in")),3)              # indegree for plotting
    oud <- round(mean(degree(nets, mode = "out")),3)             # outdegree for plotting
    rin <- range(degree(nets, mode="in"))               # range of indegree
    rim <- paste(rin[1], rin[2], sep = " - ")
    rou <- range(degree(nets, mode="out"))              # range of outdegree
    rov <- paste(rou[1], rou[2], sep = " - ")
    output <- rbind(output, data.frame(n = size,
                                       density = den,
                                       reciprocity = rec,
                                       transitivity = tra,
                                       centralization = cent,
                                       mean_degree = ind,
                                       indegree = rim,
                                       outdegree = rov,  stringsAsFactors=FALSE))
  }
  return(output)
}

#######################################
#         LOAD & SPECIFY DATA         #
#######################################

att <- read.csv("./2-analyses/v1-attributes.csv")
coal1 <- as.matrix(read.csv("./2-analyses/networks/v1-coalition-1.csv", header = FALSE))
coal2 <- as.matrix(read.csv("./2-analyses/networks/v1-coalition-2.csv", header = FALSE))
coal3 <- as.matrix(read.csv("./2-analyses/networks/v1-coalition-3.csv", header = FALSE))
c1 <- as.matrix(read.csv("./2-analyses/networks/v1-conflict-1.csv", header = FALSE))
c2 <- as.matrix(read.csv("./2-analyses/networks/v1-conflict-2.csv", header = FALSE))
c3 <- as.matrix(read.csv("./2-analyses/networks/v1-conflict-3.csv", header = FALSE))
coop1 <- as.matrix(read.csv("./2-analyses/networks/v1-cooperation-1.csv", header = FALSE))
coop2 <- as.matrix(read.csv("./2-analyses/networks/v1-cooperation-2.csv", header = FALSE))
kin1 <- as.matrix(read.csv("./2-analyses/networks/v1-kinship-1.csv", header = FALSE))
kin2 <- as.matrix(read.csv("./2-analyses/networks/v1-kinship-2.csv", header = FALSE))
comp <- read.csv("./2-analyses/v1-composition.txt", stringsAsFactors = FALSE)

v2_att <- read.csv("./2-analyses/v2-attributes.csv")
v2_coal <- as.matrix(read.csv("./2-analyses/networks/v2-coalition.csv", header = FALSE))
v2_con <- as.matrix(read.csv("./2-analyses/networks/v2-conflict.csv", header = FALSE))
v2_coop <- as.matrix(read.csv("./2-analyses/networks/v2-cooperation.csv", header = FALSE))
v2_kin <- as.matrix(read.csv("./2-analyses/networks/v2-kinship.csv", header = FALSE))

#############################################################################
#                           CONFLICT PLOTS                                  #
#############################################################################

a <- figure_1a()
b <- figure_1b()

ggsave("./3-manuscript/figures/figure_1a.png", a,
       height = 22, width = 22, units = "cm", device = "png", dpi = 1200)
ggsave("./3-manuscript/figures/figure_1b.png", b,
       height = 22, width = 22, units = "cm", device = "png", dpi = 1200)

# Create an inset plot
png("./3-manuscript/figures/figure-1.png", width = 2500, height = 1500, res = 200)
print(b)
vp <- viewport(width = 0.36, height = 0.36, x = 0.5, y = 0.5)
print(a, vp = vp)
dev.off()

#############################################################################
#               NETWORK DESCRIPTIVES                          #
#############################################################################

parameters <- c(
"n",
"density",
"reciprocity",
"transitivity",
"centralization",
"mean_degree",
"indegree",
"outdegree"
)

n_list <- list(coal1, coal2, coal3, c1, c2, c3, coop1, coop2, coop3)
n_list2 <- list(kin1, kin2, kin3)
out <- table_1(n_list, names = parameters, directed = "directed")
out2 <- table_1(n_list2, names = parameters, directed = "undirected")

out <- rbind(out, out2)

out$variable <- c(
              "Coalition 2009",
              "Coalition 2014",
              "Coalition 2017",
              "Conflict 2009",
              "Conflict 2014",
              "Conflict 2017",
              "Sharing 2009",
              "Sharing 2014",
              "Sharing 2017",
              "Kinship 2009",
              "Kinship 2014",
              "Kinship 2017"
             )

out <- out[,c(ncol(out),2:ncol(out)-1)]

write.csv(out,"./3-manuscript/tables/table-1.csv", row.names = F)

n_list <- list(v2_coal, v2_con, v2_coop, v2_kin)

out <- table_1(n_list, names = parameters)
out$variable <- c(
              "coalition",
              "conflict",
              "Sharing",
              "kinship"
             )

out <- out[,c(ncol(out),2:ncol(out)-1)]

out$n[out$variable %in% c("Kinship")] <- (as.numeric(out$n[out$variable %in% c("Kinship")])/2)

write.csv(out,"./3-manuscript/tables/SM-table-2.csv", row.names = F)

############################################################################
#                 Plot network graphs for all networks         #
############################################################################

colrs <- colorRampPalette(c( "#440154DD", "#404788FF", "#238A8DFF", "#55C667FF", "#FDE725FF"),
                         alpha = TRUE, bias = 7)

coalition1 <- graph_from_adjacency_matrix(coal1[2:nrow(coal1),2:ncol(coal1)], mode = "directed", diag = F)
coalition2 <- graph_from_adjacency_matrix(coal2[2:nrow(coal2),2:ncol(coal2)], mode = "directed", diag = F)
coalition3 <- graph_from_adjacency_matrix(coal3[2:nrow(coal3),2:ncol(coal3)], mode = "directed", diag = F)
conflict1 <- graph_from_adjacency_matrix(c1[2:nrow(c1),2:ncol(c1)], mode = "directed", diag = F)
conflict2 <- graph_from_adjacency_matrix(c2[2:nrow(c2),2:ncol(c2)], mode = "directed", diag = F)
conflict3 <- graph_from_adjacency_matrix(c3[2:nrow(c3),2:ncol(c3)], mode = "directed", diag = F)
v2_coalition <- graph_from_adjacency_matrix(v2_coal[2:nrow(v2_coal),2:ncol(v2_coal)], mode = "directed", diag = F)
v2_conflict <- graph_from_adjacency_matrix(v2_con[2:nrow(v2_con),2:ncol(v2_con)], mode = "directed", diag = F)

g1 <- coalition1 %>% add_edges(t(get.edgelist(conflict1)))
inf1 <- infomap.community(coalition1, nb.trials = 1000, modularity = FALSE)
V(g1)$membership <- inf1$membership[match(V(g1)$name, inf1$names)]
# Assign node colours based on community membership
len_g1 <- length(unique(V(g1)$membership))
V(g1)$color <- colrs(len_g1)[factor(V(g1)$membership, labels = 1:len_g1)]
g1 <- delete.vertices(g1 , which(degree(g1)==0))

g2 <- coalition2 %>%  add_edges(t(get.edgelist(conflict2)))
inf2 <- infomap.community(coalition2, nb.trials = 1000, modularity = FALSE)
V(g2)$membership <- inf2$membership[match(V(g2)$name, inf2$names)]
V(g2)$color <- colrs(length(unique(V(g2)$membership)))[factor(V(g2)$membership, labels = 1:length(unique(V(g2)$membership)))]
g2 <- delete.vertices(g2 , which(degree(g2)==0))

g3 <- coalition3 %>%  add_edges(t(get.edgelist(conflict3)))
inf3 <- infomap.community(coalition3, nb.trials = 1000, modularity = FALSE)
V(g3)$membership <- inf3$membership[match(V(g3)$name, inf3$names)]
V(g3)$color <- colrs(length(unique(V(g3)$membership)))[factor(V(g3)$membership, labels = 1:length(unique(V(g3)$membership)))]
g3 <- delete.vertices(g3 , which(degree(g3)==0))

g4 <- v2_coalition %>% add_edges(t(get.edgelist(v2_conflict)))
inf4 <- infomap.community(v2_coalition, nb.trials = 1000, modularity = FALSE)
V(g4)$membership <- inf4$membership[match(V(g4)$name, inf4$names)]
len_g4 <- length(unique(V(g4)$membership))
V(g4)$color <- colrs(len_g4)[factor(V(g4)$membership, labels = 1:len_g4)]
g4 <- delete.vertices(g4 , which(degree(g4)==0))

# Check n of communities
table(V(g1)$membership)
table(V(g2)$membership)
table(V(g3)$membership)
table(V(g4)$membership)

png("./3-manuscript/figures/figure-2a.png", width = 1500, height = 1500, res = 200)
plot(g1,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.1,
     edge.color = c(rep("firebrick4",ecount(conflict1)),rep("gray",ecount(coalition1))),
     edge.curved = c(rep(0.25,ecount(conflict1)),rep(0,ecount(coalition1))),
     layout = layout_nicely
)
dev.off()

png("./3-manuscript/figures/figure-2b.png", width = 1500, height = 1500, res = 200)
plot(g2,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.1,
     edge.color = c(rep("firebrick4",ecount(conflict2)),rep("gray",ecount(coalition2))),
     edge.curved = c(rep(0.25,ecount(conflict2)),rep(0,ecount(coalition2))),
     layout = layout_nicely
)
dev.off()

png("./3-manuscript/figures/figure-2c.png", width = 1500, height = 1500, res = 200)
plot(g3,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.1,
     edge.color = c(rep("firebrick4",ecount(conflict3)),rep("gray",ecount(coalition3))),
     edge.curved = c(rep(0.25,ecount(conflict3)),rep(0,ecount(coalition3))),
     layout = layout_nicely
)
dev.off()

png("./3-manuscript/figures/SM-figure-3.png", width = 1500, height = 1500, res = 200)
plot(g4,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.1,
     edge.color = c(rep("firebrick4",ecount(v2_conflict)),rep("gray",ecount(v2_coalition))),
     edge.curved = c(rep(0.25,ecount(v2_conflict)),rep(0,ecount(v2_coalition))),
     layout = layout_nicely
)
dev.off()

############################################################################
#                            Network Overlap                               #
############################################################################

# Create matrix
x<- c("coalition1",
"coalition2",
"coalition3",
"conflict1",
"conflict2",
"conflict3",
"sharing1",
"sharing2",
"kinship1",
"kinship2")

y <- c("coalition1",
       "coalition2",
       "coalition3",
       "conflict1",
       "conflict2",
       "conflict3",
       "sharing1",
       "sharing2",
       "kinship1",
       "kinship2")

data <- expand.grid(x,y, stringsAsFactors = FALSE)

# ughhhhhh, manually compute for combinations
data$value <- 1
data$value[c(2, 11)] <- Jaccard(coal2, coal1)
data$value[c(3,21)] <- Jaccard(coal3, coal1)
data$value[c(4,31)] <- Jaccard(c1, coal1)
data$value[c(5,41)] <- Jaccard(c2, coal1)
data$value[c(6,51)] <- Jaccard(c3, coal1)
data$value[c(7,61)] <- Jaccard(coop1, coal1)
data$value[c(8,71)] <- Jaccard(coop2, coal1)
data$value[c(9,81)] <- Jaccard(kin1, coal1)
data$value[c(10,91)] <- Jaccard(kin2, coal1)
data$value[c(13,22)] <- Jaccard(coal3, coal2)
data$value[c(14,32)] <- Jaccard(c1, coal2)
data$value[c(15,42)] <- Jaccard(c2, coal2)
data$value[c(16,52)] <- Jaccard(c3, coal2)
data$value[c(17,62)] <- Jaccard(coop1, coal2)
data$value[c(18,72)] <- Jaccard(coop2, coal2)
data$value[c(19,82)] <- Jaccard(kin1, coal2)
data$value[c(20,92)] <- Jaccard(kin2, coal2)
data$value[c(24,63)] <- Jaccard(coop1, coal3)
data$value[c(24,33)] <- Jaccard(c1, coal3)
data$value[c(25,43)] <- Jaccard(c2, coal3)
data$value[c(26,53)] <- Jaccard(c3, coal3)
data$value[c(27,63)] <- Jaccard(coop1, coal3)
data$value[c(28,73)] <- Jaccard(coop2, coal3)
data$value[c(29,83)] <- Jaccard(kin1, coal3)
data$value[c(30,93)] <- Jaccard(kin2, coal3)
data$value[c(35,44)] <- Jaccard(c2, c1)
data$value[c(36,54)] <- Jaccard(c3, c1)
data$value[c(36,54)] <- Jaccard(c3, c1)
data$value[c(37,64)] <- Jaccard(coop1, c1)
data$value[c(38,74)] <- Jaccard(coop2, c1)
data$value[c(39,84)] <- Jaccard(kin1, c1)
data$value[c(40,94)] <- Jaccard(kin2, c1)
data$value[c(46,55)] <- Jaccard(c3, c2)
data$value[c(47,65)] <- Jaccard(coop1, c2)
data$value[c(48,75)] <- Jaccard(coop2, c2)
data$value[c(49,85)] <- Jaccard(kin1, c2)
data$value[c(50,95)] <- Jaccard(kin2, c2)
data$value[c(57,66)] <- Jaccard(coop1, c3)
data$value[c(58,76)] <- Jaccard(coop2, c3)
data$value[c(59,86)] <- Jaccard(kin1, c3)
data$value[c(60,96)] <- Jaccard(kin2, c3)
data$value[c(68,77)] <- Jaccard(coop2, coop1)
data$value[c(69,87)] <- Jaccard(kin1, coop1)
data$value[c(70,97)] <- Jaccard(kin2, coop1)
data$value[c(79,88)] <- Jaccard(kin1, coop2)
data$value[c(80,98)] <- Jaccard(kin2, coop2)
data$value[c(80,98)] <- Jaccard(kin2, coop2)
data$value[c(90,99)] <- Jaccard(kin2, kin1)

# Make the plot
figure3 <- ggplot(data, aes(Var1, Var2, fill= value)) +
  geom_tile() +
  scale_fill_viridis(na.value = "white", alpha = 0.8) +
  theme_classic() +
  xlab(label = "") +
  ylab(label = "")

ggsave(plot = figure3, filename = "./3-manuscript/figures/SM-figure-1.png", device = "png", dpi = 800)

############################################################################
#                         PARAMETER ESTIMATES TABLE                        #
############################################################################

analyses <-load("./2-analyses/outputs/v1-analyses.RData")
options(scipen=999)
results <- model8
parameter <- results$effects$effectName
estimate <- results$theta
st_error <- sqrt(diag(results$covtheta))
norm_var <- estimate/st_error
pval <- 2*pnorm(abs(norm_var), lower.tail = FALSE)

results_out <- data.frame(parameter,
                          estimate = round(estimate, 2),
                          SE = round(st_error, 2),
                          p = round(pval, 3),
                          OR = round(exp(estimate), 2),
                          CI = paste(round(exp(estimate -
                                                 qnorm(0.975)*st_error), 2),
                                     round(exp(estimate + qnorm(0.975)*st_error), 2), sep = "-"),
                          CI.low = round(estimate -
                                           qnorm(0.975)*st_error, 2),
                          CI.high = round(estimate + qnorm(0.975)*st_error, 2),
                          stringsAsFactors = FALSE
)

results_out[1, 3:8] <- "-"
results_out[2, 5:8] <- "-"
results_out$p[results_out$p == 0] <- ">.001"
results_out

results_out[1 , 1] <- "Rate: 2009-2014(Fixed)"
results_out[2 , 1] <- "Rate: 2014-2017"
results_out[3 , 1] <- "Outdegree"
results_out[4 , 1] <- "Reciprocity"
results_out[5 , 1] <- "Transitive group formation (GWESP)"
results_out[6 , 1] <- "Shared Popularity"
results_out[7 , 1] <- "Indegree popularity (sqrt)"
results_out[8 , 1] <- "Outdegree activity (sqrt)"
results_out[9 , 1] <- "Mixed closure with conflict (2009-2014)"
results_out[10 , 1] <- "Main effect of kinship"
results_out[11 , 1] <- "Main effect of sharing (2009-2014)"
results_out[12 , 1] <- "Status indegree"
results_out[13 , 1] <- "Status outdegree (2009-2014)"
results_out[14 , 1] <- "Status similarity"
results_out[15 , 1] <- "Physical formidability indegree (2009-2014)"
results_out[16 , 1] <- "Physical formidability outdegree"
results_out[17 , 1] <- "Physical formidability similarity"
results_out[18 , 1] <- "Corregidor indegree (2009-2014)"
results_out[19 , 1] <- "Corregidor outdegree"
results_out[21 , 1] <- "Mixed closure with conflict (2014-2017)"
results_out[22 , 1] <- "Main effect of sharing (2014-2017)"
results_out[23 , 1] <- "Status outdegree (2014-2017)"
results_out[24 , 1] <- "Physical formidability indegree (2014-2017)"
results_out[25 , 1] <- "Corregidor indegree (2014-2017)"

results_out <- results_out[-20, ]
table2 <- results_out[,-c(7,8)]

table2 <- table2[c(1:9,20,10,11,21,12,13,22,14,15,23,16:18,24,19),]

write.csv(table2, "./3-manuscript/tables/table-2.csv", row.names = FALSE)

v1 <- results_out[-c(1:3),]

v2_analyses <- load("./2-analyses/outputs/v2-analyses.RData")

options(scipen=999)
results <- model2
parameter <- results$effects$effectName
estimate <- results$theta
st_error <- sqrt(diag(results$covtheta))
norm_var <- estimate/st_error
pval <- 2*pnorm(abs(norm_var), lower.tail = FALSE)

results_out <- data.frame(parameter,
                          estimate = round(estimate, 2),
                          SE = round(st_error, 2),
                          p = round(pval, 3),
                          OR = round(exp(estimate), 2),
                          CI = paste(round(exp(estimate -
                                                 qnorm(0.975)*st_error), 2),
                                     round(exp(estimate + qnorm(0.975)*st_error), 2), sep = "-"),
                          CI.low = round(estimate -
                                           qnorm(0.975)*st_error, 2),
                          CI.high = round(estimate + qnorm(0.975)*st_error, 2),
                          stringsAsFactors = FALSE
)


results_out[1 , 1] <- "Rate: Period 1 (Fixed)"
results_out[2 , 1] <- "Outdegree"
results_out[3 , 1] <- "Reciprocity"
results_out[4 , 1] <- "Transitive group formation (GWESP)"
results_out[5 , 1] <- "Shared Popularity"
results_out[6 , 1] <- "Indegree popularity (sqrt)"
results_out[7 , 1] <- "Outdegree activity (sqrt)"
results_out[8 , 1] <- "Mixed closure with conflict"
results_out[9 , 1] <- "Main effect of kinship"
results_out[10 , 1] <- "Main effect of sharing"
results_out[11 , 1] <- "Status indegree"
results_out[12 , 1] <- "Status outdegree"
results_out[13 , 1] <- "Status similarity"
results_out[14 , 1] <- "Physical formidability indegree"
results_out[15 , 1] <- "Physical formidability outdegree"
results_out[16 , 1] <- "Physical formidability similarity"


results_out[1, 3:8] <- "-"
results_out$p[results_out$p == 0] <- ">.001"

SOM_table1 <- results_out[,-c(7,8)]

write.csv(SOM_table1, "./3-manuscript/tables/SM-table-3.csv", row.names = FALSE)

############################################################################
#                         PARAMETER ESTIMATES PLOT                         #
############################################################################

v2 <- results_out[-c(1,2),]

out <- v1
out$type <- "2009-2014-2017"
out$type[c(6,8,10,12,15)] <- "2009-2014 only"
out$type[c(17:21)] <- "2014-2017 only"
out$parameter[c(6,17)] <- "Mixed closure with conflict"
out$parameter[c(8,18)] <- "Main effect of sharing"
out$parameter[c(10,19)] <- "Status outdegree"
out$parameter[c(12,20)] <- "Physical formidability indegree"
out$parameter[c(15,21)] <- "Corregidor indegree"

figure4_data <- data.frame(
  parameter = out$parameter,
  est = as.numeric(out$est),
  lower = as.numeric(out$CI.low),
  upper = as.numeric(out$CI.high),
  type = out$type
)

figure4_data$parameter <- factor(figure4_data$parameter, levels = rev(unique(figure4_data$parameter)))


figure4 <- ggplot(data=figure4_data, aes(x=parameter, y=est, ymin=lower, ymax=upper, color = type, )) +
    geom_pointrange(position=position_dodge(width = 0.5)) +
    geom_hline(yintercept= 0, lty=2, aes(fill=Group)) +
    coord_flip() +
    scale_color_manual(values = c("#481567FF", "#287D8EFF", "#00cca5")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
   theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.text=element_text(size=15))
figure4

ggsave(figure4, filename = "./3-manuscript/figures/figure-3.png", device = "png", dpi = 800)


############################################################################
#                               CORRELATIONS                               #
############################################################################

v1_correlation_data <- data.frame(
  id = att$id,
  status_09 = normalize(att$status_09),
  status_14 = normalize(att$status_14),
  indeg_09 = degree(conflict1, mode = "in"),
  indeg_14 = degree(conflict2, mode = "in"),
  indeg_17 = degree(conflict3, mode = "in"),
  outdeg_09 = degree(conflict1, mode = "out"),
  outdeg_14 = degree(conflict2, mode = "out"),
  outdeg_17 = degree(conflict3, mode = "out")
)

v1_cor <- cor(v1_correlation_data[, -1])

colrs2 <- colorRampPalette(c("#440154FF", "#404788FF", "#33638DFF", "#238A8DFF", "#3CBB75FF", "#95D840FF"),
                          alpha = TRUE, bias = 10)

png("./3-manuscript/figures/SM-figure-2.png", width = 1500, height = 1500, res = 200)
corrplot.mixed(v1_cor, upper = "square", lower = "number", lower.col = "black", tl.col = "black", tl.cex = 0.8, upper.col = colrs2(100))
dev.off()

v2_correlation_data <- data.frame(
id = v2_att$id,
status_09 = normalize(v2_att$status),
v2_indeg = degree(v2_conflict, mode = "in"),
v2_outdeg = degree(v2_conflict, mode = "out")
)

v2_cor <- cor(v2_correlation_data[, -1], method = "pearson")
corrplot.mixed(v2_cor, upper = "square", lower = "number", lower.col = "black", tl.col = "black", tl.cex = 0.8, upper.col = colrs2(100))

write.csv(v1_cor, file = "./3-manuscript/tables/village1_correlations.csv")
write.csv(v2_cor, file = "./3-manuscript/tables/village2_correlations.csv")

############################################################################
#                        COVARIATE DESCIPTIVES                             #
############################################################################

stat_1 <- data.frame(variable = "Village 1 Status 2009", n = nrow(att), mean = round(mean(normalize(att$status_09)),2),
                     sd = round(sd(normalize(att$status_09)),2), median = round(median(normalize(att$status_09)),2),
                     range = paste(round(min(normalize(att$status_09)),2),"-",round(max(normalize(att$status_09)),2), sep = ""))
stat_2 <- data.frame(variable = "Village 1 Status 2014", n = nrow(att), mean = round(mean(normalize(att$status_14)),2),
                     sd = round(sd(normalize(att$status_14)),2), median = round(median(normalize(att$status_14)),2),
                     range = paste(round(min(normalize(att$status_14)),2),"-",round(max(normalize(att$status_14)),2), sep = ""))
stat_3 <- data.frame(variable = "Village 2 Status", n = nrow(v2_att), mean = round(mean(normalize(v2_att$status)),2),
                     sd = round(sd(normalize(v2_att$status)),2), median = round(median(normalize(v2_att$status)),2),
                     range = paste(round(min(normalize(v2_att$status)),2),"-",round(max(normalize(v2_att$status)),2), sep = ""))
form_1 <- data.frame(variable = "Village 1 Formidability 2009", n = nrow(att), mean = round(mean(att$strength_09),2),
                     sd = round(sd(att$strength_09),2), median = round(median(att$strength_09),2),
                     range = paste(round(min(att$strength_09),2),"-",round(max(att$strength_09),2), sep = ""))
form_2 <- data.frame(variable = "Village 1 Formidability 2014", n = nrow(att), mean = round(mean(att$strength_14),2),
                     sd = round(sd(att$strength_14),2), median = round(median(att$strength_14),2),
                     range = paste(round(min(att$strength_14),2),"-",round(max(att$strength_14),2), sep = ""))
form_3 <- data.frame(variable = "Village 2 Formidability", n = nrow(v2_att), mean = round(mean(v2_att$strength),2),
                     sd = round(sd(v2_att$strength),2), median = round(median(v2_att$strength),2),
                     range = paste(round(min(v2_att$strength),2),"-",round(max(v2_att$strength),2), sep = ""))

cov_des <- rbind(stat_1, stat_2, stat_3, form_1, form_2, form_3)
write.csv(cov_des,"./3-manuscript/tables/SM-table-1.csv", row.names = F)
rm(stat_1, stat_2, stat_3, form_1, form_2, form_3)

print("Code for descriptives and plots has finished running. Files have been written out")

######## Script stops here ##############
