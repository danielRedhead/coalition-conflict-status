# Functions 

expand_network <- function(network, new_ids, needs_ordering = FALSE) {
  
  y <- matrix(nrow = length(network[ , 1]) + length(new_ids[ , 1]), ncol = length(network[ , 1]) + length(new_ids[ , 1] ))
  #Bring in the observed matrix
  y[1:nrow(network), 1:nrow(network)] <- network
  #Add the ids of the missing participants
  y[(nrow(network) + 1):nrow(y), 1] <- new_ids[ , 1 ]
  y[1 , (nrow(network) + 1):nrow(y)] <- new_ids[ , 1 ]
  #Add 0s to the new columns
  y[2:nrow(y) , (nrow(network) + 1):nrow(y)] <- 0
  #Add NA to new rows of missing data
  y[(nrow(network) + 1):nrow(y), 2:nrow(y) ] <- 0 # Changed from NA
  if (needs_ordering == TRUE) {
    #Order matrix columns by id
    y <- y[order(y[ , 1]), ]
    #Order matrix rows by id
    y <- y[ , order(y[1 , ])]
  }
  return(y)
  
}

missing_network_ids <- function (network, composition) {
#Get id's that appear in 2009
id <- data.frame(network[1 , ])
#Remove empty rows
id <- data.frame(id = id[!id == ""], stringsAsFactors = FALSE)
#Get the names that appear in the population but not in the 2009 networks
id <- as.matrix(data.frame(id = composition$id[!(composition$id %in% id$id)], stringsAsFactors = FALSE))
return(id)

}

self_nominations <- function(network) {
  all(diag(network[2:nrow(network), 2:ncol(network)]) <1, na.rm = TRUE )
}

binary_network <- function(network) {
  all(network[2:nrow(network), 2:ncol(network)] <2, na.rm = TRUE )
}

numeric_network_id <- function(network, id) {
  # Check for repeated ids
  if(nrow(network)-1 != length(unique(network[2:nrow(network), 1])))  {stop("Repeated ids in rows")}
  if(ncol(network)-1 != length(unique(network[1, 2:ncol(network)])))  {stop("Repeated ids in columns")}
  
  # Check for ids not in the multi-attributes file
  if(sum(is.na(network[,1])) != sum(is.na(network[1,])))  {stop("NA values appear as elements assymmetrically")}
  nacount <- sum(is.na(network[1,]))
  if(length(na.omit(match(network[,1],id))) != length(unique(network[2:nrow(network), 1])) - nacount)  {stop("New ids appear in the column")}
  if(length(na.omit(match(network[1,],id))) != length(unique(network[1, 2:ncol(network)])) - nacount)  {stop("New ids appear in the row")}
  
  # Sort the dataframe by row and column
  network <- network[order(network[,1]),]
  network <- network[,order(network[1,])]
  
  #Rename the ids with numbers
  network[2:nrow(network),1] <- 1:length(unique(network[2:nrow(network), 1]))
  network[1, 2:ncol(network)] <- 1:length(unique(network[1, 2:ncol(network)]))
  return(network)
}