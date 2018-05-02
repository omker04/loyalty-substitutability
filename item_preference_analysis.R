library(data.table)
library(bit64)
library(car)

setwd("/Users/omahala/Desktop/GM Insights/GeC Data/item preference")
attribute_data <- data.frame(fread("qarthExtract_infantFurniture.csv"))[,c(1,3:8)]
attribute_data$wupc <- as.numeric(attribute_data$wupc)
price_data <- data.frame(fread("item_price_627.csv"))
colnames(price_data) <- c("rollup_id","rollup_desc","brand","fineline_nbr","avg_price","total_sales")
price_data$rollup_id <- as.numeric(price_data$rollup_id)
price_data$units_sold <- price_data$total_sales/price_data$avg_price
full_data <- inner_join(price_data,attribute_data,by=c("rollup_id"="wupc"))
full_data <- full_data[-which(full_data$color=="?"),]
full_data <- full_data[,-c(8,9,12,13)]
full_data <- full_data[-which(full_data$brand.x=="VOLPI"),]
full_data$brand.x[which(full_data$brand.x=="DISNEY DTR")] <- "DISNEY - DISNEY"

all_item_data <- data.frame(fread("all_item_627.csv"))
colnames(all_item_data) <- c("item_nbr","upc_nbr","brand_name","fineline_nbr","fineline_desc")

color <- full_data[,c("rollup_id","color")]
for(i in 1:867){
  if("Multicolor" %in% strsplit(color[i,2], split = " ; ")[[1]])
    color$colour[i] <- "multicolor"
  else{
    if(length(strsplit(color[i,2], split = " ; ")[[1]])==2)
      color$colour[i] <- "bicolor"
    if(length(strsplit(color[i,2], split = " ; ")[[1]])==1)
      color$colour[i] <- "single-color"
    if(length(strsplit(color[i,2], split = " ; ")[[1]]) > 2)
      color$colour[i] <- "multicolor"
  }
}

full_data$color <- color$colour

anova <- function(fineline_nbr){
  all_item_data_fn <- all_item_data[which(all_item_data$fineline_nbr==fineline_nbr),]
  all_item_data_fn <- data.frame(all_item_data_fn %>% group_by(brand_name) %>% summarise(nbr=n()))
  full_data_fn <- full_data[which(full_data$fineline_nbr==fineline_nbr),]
  brand_popularity <- data.frame(full_data_fn %>% group_by(brand.x) %>% summarise(nbr=n()))
  brand_popularity <- inner_join(brand_popularity, all_item_data_fn, by = c("brand.x"="brand_name"))
  brand_popularity <- inner_join(brand_popularity, data.frame(full_data_fn %>% group_by(brand.x) %>% summarise(units=sum(units_sold))),
                                 by=c("brand.x"="brand.x"))
  brand_popularity$pop_score <- brand_popularity$nbr.x*brand_popularity$units/brand_popularity$nbr.y
  quantile <- as.vector(quantile(brand_popularity$pop_score,c(0.33,0.67,1)))
  brand_popularity$popular[which(brand_popularity$pop_score<=quantile[3])] <- "highly popular"
  brand_popularity$popular[which(brand_popularity$pop_score<=quantile[2])] <- "medium popular"
  brand_popularity$popular[which(brand_popularity$pop_score<=quantile[1])] <- "not popular"
  brand_popularity$popular[which(is.na(brand_popularity$popular)==TRUE)] <- "medium popular"
  full_data_fn$brand_pop <- inner_join(full_data_fn, brand_popularity[,-(2:5)], by=c("brand.x"="brand.x"))$popular
  quantile_price <- as.vector(quantile(full_data_fn$avg_price,c(0.33,0.67,1)))
  full_data_fn$price[which(full_data_fn$avg_price<=quantile_price[3])] <- "expensive"
  full_data_fn$price[which(full_data_fn$avg_price<=quantile_price[2])] <- "budget"
  full_data_fn$price[which(full_data_fn$avg_price<=quantile_price[1])] <- "cheap"
  anova_data <- full_data_fn[,-c(1:5,8)]
#   all_combination <- data.frame("color"=rep(c("single-color","bicolor","multicolor"),each=15),
#                                 "brand_pop"=rep(c("highly popular","medium popular","not popular"),each=3),
#                                 "price"=c("expensive","budget","cheap"))
#   present_combination <- unique(anova_data[,3:5])
#   not_present <- anti_join(all_combination,present_combination,by=c("color"="color","brand_pop"="brand_pop","price"="price"))
#   anova_data <- rbind(anova_data, not_present[,c(3:5,1:2)])
#   anova_data$percent_units <- (anova_data$units_sold*100)/sum(anova_data$units_sold)
  anova <- aov(units_sold ~ brand_pop*color*price, data = full_data_fn,
               contrasts = list(brand_pop=contr.Helmert, color=contr.Helmert, price=contr.Helmert))
  summary_list <- vector("list",3)
  summary_list[[1]] <- Anova(anova)
  summary_list[[2]] <- full_data_fn
#   summary_list[[3]] <- anova_data
  return(summary_list)
}

a <- anova(502)[[2]]
with(a, {
  interaction.plot(color, brand_pop, units_sold, type="b", legend="T", main="fineline 502")
  interaction.plot(price, brand_pop, units_sold, type="b", legend="T", main="fineline 502")
  interaction.plot(color, price, units_sold, type="b", legend="T", main="fineline 502")
}
)

with(a, tapply(units_sold, list(color), mean))
with(a, tapply(units_sold, list(price), mean))
with(a, tapply(units_sold, list(brand_pop), mean))
with(a, tapply(units_sold, list(brand_pop,color), mean))
with(a, tapply(units_sold, list(brand_pop,price), mean))
with(a, tapply(units_sold, list(color,price), mean))
with(a, tapply(units_sold, list(brand_pop,color,price), mean))


