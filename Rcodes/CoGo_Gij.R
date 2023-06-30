library(dplyr)
library(spnaf)
library(sf)

#read the individual CoGo trip data aggregated by census block groups
cogo_2019<-read.csv("Data/cogodata_2019.csv")

#Calculate the trip volumes
cogo_volume<-cogo_2019%>% group_by(From_ID,To_ID)%>%summarise(f_n=sum(count))

#Change the column names
names(cogo_volume)<-c("oid", "did","n")
#Change the data type of oid and did
cogo_volume$oid <- as.character(cogo_volume$oid)
cogo_volume$did <- as.character(cogo_volume$did)

#Shapefile: Ohio Census Block Groups(CBGs) boundaries
shape <-st_read('Data/Franklin_BG_1.shp')

#Extract the CBGs that generates bike sharing trips
shape <- shape %>% filter(id %in% cogo_volume$oid | id %in% cogo_volume$did)

#Calculate the Gij* and store the results as a list class
result <- Gij.polygon(df = cogo_volume ,shape = shape , queen = TRUE, snap = 1, method = 't')

#Transform the list form of the result to the data.frame form (including WKT)
result<-as.data.frame(result[[2]])
#Select the level of the p-value(0.05)
cogo_pval<-subset(result, result$pval<0.05)
head(cogo_pval)
