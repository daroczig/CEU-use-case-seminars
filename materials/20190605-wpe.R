library(data.table)
library(reshape2)
library(ecodist)

#Read the data
data <- fread("http://bit.ly/CEU-R-usecase-2019-wpe-data")

#Aggregate visit numbers
visit_numbers <- data[, .(visit_count = .N), by=.(Entity, Planet)]

#Pivot
visit_matrix <- data.table(dcast(visit_numbers, Entity ~ Planet, value.var = "visit_count"))
visit_matrix[is.na(visit_matrix)] <- 0

#Remove robots (R2D2)
visit_matrix <- visit_matrix[!(visit_matrix$Entity=="R2D2"),]

#Calculate relative frequencies
cols <- names(visit_matrix[,-1])
visit_matrix[, row_sum:= rowSums(visit_matrix[,-1], na.rm = FALSE)]
visit_matrix[, (cols) := lapply(.SD, function(x) x/row_sum), .SDcols = cols]
visit_matrix[, row_sum:=NULL]

#Calculate planet distances
planet_distances <- bcdist(t(visit_matrix[,-1]), rmzero = FALSE)
planet_distances_long <- melt(as.matrix(planet_distances), varnames = c("row", "col"))
planet_distances_wide <- dcast(planet_distances_long, row ~ col)

#Convert distances to similarities
planet_similarities <- cbind(Planet=planet_distances_wide$row, 
                             1 - planet_distances_wide[,c(-1)])

#Calculate affinities
affinities <- data.table(cbind(Entity=visit_matrix$Entity ,
                               as.matrix(visit_matrix[,-1]) %*% 
                                 as.matrix(planet_similarities[,-1])))

#Calculate anomaly scores
anomaly_scores = copy(affinities)
anomaly_scores[, (cols) := lapply(.SD, function(x) 100*(1-as.numeric(x))), .SDcols = cols]