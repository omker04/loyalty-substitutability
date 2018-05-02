require(data.table) || install.packages("data.table")
require(dplyr) || install.packages("dplyr")
require(bit64) || install.packages("bit64")
require(sqldf) || install.packages("sqldf")
require(foreach) || install.packages("foreach")
require(doSNOW) || install.packages("doSNOW")
require(parallel) || install.packages("parallel")



setwd("/Users/omahala/Desktop/GM Insights/Loyalty")
hh_data_627 <- data.frame(fread("hh_loyalty_627.csv"))
colnames(hh_data_627) <- c("household_id","rollup_id","rollup_desc","brand_name","fineline_nbr","visit_date")

top20_brands <- arrange(data.frame(hh_data_627  %>% group_by(brand_name) %>% summarise(nbr=n())),desc(nbr))[1:20,]
hh_data_627 <- inner_join(hh_data_627, data.frame(a = top20_brands[,1]), by = c("brand_name" = "a"))

hh_freq <- data.frame(hh_data_627 %>% group_by(household_id) %>% summarise(nbr=n()))
hh_only1 <- hh_freq[which(hh_freq$nbr==1),]
hh_data_627 <- anti_join(hh_data_627, hh_only1, by = c("household_id" = "household_id"))
hh_fineline_freq <- sqldf("select household_id, count(distinct fineline_nbr) as count_fineline from hh_data_627 group by household_id")
hh_fineline_freq <- inner_join(hh_freq, hh_fineline_freq, by = c("household_id" = "household_id"))

random_hh <- hh_fineline_freq[which(hh_fineline_freq$nbr==hh_fineline_freq$count_fineline),]
hh_data_627 <- anti_join(hh_data_627, random_hh, by = c("household_id" = "household_id"))


loyalty <- function(hh_data_627, household_id, brand_name){
  loyalty_data <- hh_data_627[which(hh_data_627$household_id==household_id),]
  transaction <- ifelse(loyalty_data$brand_name==brand_name,1,0)
  loyalty_list <- vector("list",2)
  if(sum(transaction)==0)
    loyalty_list[[1]] <- "No purchase"
  else{
    if(sum(transaction)==1 & transaction[length(transaction)]==1)
      loyalty_list[[1]] <- "No scope to repeat"
    else{
      m <- transaction[1]
      loyalty_df <- data.frame("transaction"=array(0, length(transaction)-1))
      for(i in 2:length(transaction)){
        m <- paste0(m,transaction[i])
        loyalty_df$transaction[i-1] <- m
        loyalty_df$nbr[i-1] <- sum(as.numeric(unlist(strsplit(m,""))))
        loyalty_df$last[i-1] <- as.numeric(unlist(strsplit(m,"")))[i]
        loyalty_df$denominator[i-1] <- loyalty_df$nbr[i-1] - loyalty_df$last[i-1]
        run_length <- rle(as.numeric(unlist(strsplit(m,""))))
        run_length <- run_length$lengths[run_length$values==1]
        k <- run_length[1]
        if(length(run_length) > 1){
        for(j in 2:length(run_length))
          k <- paste(k, run_length[j],sep=" ")
        }
        loyalty_df$run[i-1] <- k
        loyalty_df$numerator[i-1] <- ifelse(length(run_length)==0,NA,sum(run_length-1))
        fraction <- loyalty_df$numerator[i-1] / loyalty_df$denominator[i-1]
        loyalty_df$loyalty[i-1] <- ifelse(is.nan(fraction),NA,fraction)
      }
      loyalty_index = sum(0.5 ^ (dim(loyalty_df)[1]:1) * loyalty_df$loyalty, na.rm = TRUE) / 
      sum(0.5 ^ (dim(loyalty_df)[1]:1) * ifelse(is.na(loyalty_df$loyalty),0,1))
      loyalty_list[[1]] <- loyalty_index
      loyalty_list[[2]] <- loyalty_df
    }
  }
  return(loyalty_list)
}

household_id = 652691859
brand_name = "FISHER PRICE"
p <- loyalty(household_id, brand_name)
plot(p[[1]]$loyalty, ylim = c(0,1), type = "b", xaxt = 'n', yaxt = 'n', pch = 19, lwd = 2,
     xlab = "Transaction nbr", ylab = "Loyalty index", sub = paste("Loyalty Score", p[[2]], sep = " = "), 
     main = paste("Time based Loyalty for hh",household_id,"and brand",brand_name,sep=" "))
axis(1, at = 1:dim(p[[1]])[1])
axis(2, at = seq(0, 1, 0.2))


hh_item_loyalty <- data.frame(matrix(nrow = uniqueN(hh_data_627$household_id), ncol = uniqueN(hh_data_627$brand_name) + 1))
colnames(hh_item_loyalty) <- c("household_id", unique(hh_data_627$brand_name))
hh_item_loyalty$household_id <- unique(hh_data_627$household_id)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
getDoParWorkers()


hh_item_loyalty <- foreach(i = 1:10, .combine = rbind, .inorder = FALSE) %dopar% f(i)



f <- function(x){
  c <- array(hh_item_loyalty[x,1],21)
  for(j in 2:21)
    c[j] <- loyalty(hh_data_627, hh_item_loyalty[x,1], colnames(hh_item_loyalty[x,])[j])[[1]]
  return(c)
}

# system.time(t(apply(head(hh_item_loyalty,50), 1, function(x) f(x))))

## the commented part needs to be done as an apply()

# sample <- hh_item_loyalty[1:100,]
# 
# 
loyalty_fineline <- function(household_id, fineline_nbr){
  loyalty_data <- hh_data_627[which(hh_data_627$household_id==household_id),]
  if(dim(loyalty_data[which(loyalty_data$fineline_nbr==fineline_nbr),])[1]==0)
    return("No purchase")
  else{
    loyalty_data <- loyalty_data[which(loyalty_data$fineline_nbr==fineline_nbr),]
    brands <- unique(loyalty_data$brand_name)
    loyalty_list <- vector("list",length(brands))
    for(i in 1:length(brands))
      loyalty_list[[i]] <- loyalty(loyalty_data, household_id, brands[i])
    return(loyalty_list)
  }
}




sample <- hh_item_loyalty[1:10,1:2]
GRACO <- apply(hh_item_loyalty[,1:2], 1, function(x) loyalty(hh_data_627,as.numeric(x[1]),"GRACO")[[1]])


