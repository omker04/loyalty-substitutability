library(data.table)
library(plyr)
library(dplyr)
library(bit64)

setwd("/Users/omahala/Desktop/GM Insights/Substitutability/INFANT_689")
manual_CDT <- data.frame(fread("manual_CDT.csv"))
manual_CDT$rollup_id <- unlist(strsplit(manual_CDT$Leaf.Desc, split="__"))[seq(1,2*nrow(manual_CDT),2)]
subs_689 <- data.frame(fread("subs_689.csv"))
colnames(subs_689) <- c("rollup_id", "rollup_desc", "wm_yr_wk", "avg_price", "units")
subs_689$rollup_id <- as.numeric(subs_689$rollup_id)
select_data_689 <- data.frame(fread("select_item_689.csv"))
subs_689 <- inner_join(subs_689, data.frame(a=as.numeric(select_data_689[,2])), by = c("rollup_id"="a"))
subs_689 <- inner_join(subs_689, data.frame(a=as.numeric(manual_CDT$rollup_id)), by = c("rollup_id"="a"))
all_items <- uniqueN(paste(subs_598$rollup_id, subs_598$rollup_desc, sep = "--"))
all_items <- unique(manual_CDT$rollup_id)
subs_689 <- split(subs_689, subs_689$rollup_id)

delta <- lapply(subs_689, function(x) {c(mean(diff(x$avg_price, 1))/mean(x$avg_price), mean(diff(x$units, 1))/mean(x$units))})

subs_mtx <- matrix(0, length(delta), length(delta))
colnames(subs_mtx) <- all_items
rownames(subs_mtx) <- all_items

for(i in 1:nrow(subs_mtx)){
  for(j in 1:nrow(subs_mtx))
    subs_mtx[i,j] <- delta[[j]][2]/delta[[i]][1]
}
diag(subs_mtx) <- 0


delta_perc <- lapply(subs_689, function(x) {c(mean(diff(x$avg_price, 1)/x$avg_price[-length(x$avg_price)]),
                                              mean(diff(x$units, 1)/x$avg_price[-length(x$units)]))})

subs_mtx_perc <- matrix(0, length(delta), length(delta_perc))
colnames(subs_mtx_perc) <- all_items
rownames(subs_mtx_perc) <- all_items

for(i in 1:nrow(subs_mtx_perc)){
  for(j in 1:nrow(subs_mtx_perc))
    subs_mtx_perc[i,j] <- delta_perc[[j]][2]/delta_perc[[i]][1]
}
diag(subs_mtx_perc) <- 0

text_CDT <- data.frame(fread("text_item_cluster_assignment_689.csv"))
text_CDT$item <- unlist(lapply(strsplit(text_CDT[,1], split="--"), function(x) {paste(x[1], x[2], sep="--")}))
attribute_CDT <- data.frame(fread("attributes_item_cluster_assignment_689.csv"))
graph_CDT <- data.frame(fread("select_item_cluster_assignment_689.csv"))
graph_CDT$item <- unlist(lapply(strsplit(graph_CDT[,1], split="--"), function(x) {paste(x[1], x[2], sep="--")}))
text_CDT <- split(text_CDT, text_CDT$cluster)

all_items <- unique(graph_CDT$item)
all_items <- data.frame(t(combn(all_items, 2)))
colnames(all_items) <- c("item1", "item2")
for(i in 1:nrow(all_items)){
  j1 = which(colnames(subs_mtx) == all_items[i,1])
  j2 = which(rownames(subs_mtx) == all_items[i,2])
  all_items$subs[i] <- (subs_mtx[j1,j2] + subs_mtx[j2,j1])/2
  all_items$subs_perc[i] <- (subs_mtx_perc[j1,j2] + subs_mtx_perc[j2,j1])/2
}
all_items$item1 <- as.character(all_items$item1)
all_items$item2 <- as.character(all_items$item2)
all_items <- inner_join(all_items, graph_CDT, by = c("item1"="item"))
all_items <- inner_join(all_items, graph_CDT, by = c("item2"="item"))
colnames(all_items)[6:7] <- c("cluster1", "cluster2")

subs_mean <- data.frame(all_items %>% group_by(cluster.x, cluster.y) %>% summarise(avg_subs=mean(subs)))
                        