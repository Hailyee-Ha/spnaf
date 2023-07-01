#{dplyr} for data manipulation
library(dplyr)
#{spnaf} for spatial analysis function
library(spnaf)
#{sf} for handling spatial data
library(sf)

#Read the CoGo (a bike sharing program) trip data for the year 2019.
#The data, aggregated by Census Block Groups (CBGs), is imported from a CSV file.
cogo_2019<-read.csv("Data/cogodata_2019.csv")

#Calculate the trip volumes by grouping the data based on 'From_ID' and 'To_ID'
#(which presumably represent the origin and destination of each trip),
#and then summing up the 'count' (probably the number of trips).
cogo_volume<-cogo_2019%>% group_by(From_ID,To_ID)%>%summarise(f_n=sum(count))

#Rename the columns of the created data frame 'cogo_volume' to 'oid', 'did', and 'n'.
#The meaning of these new column names is not specified.
names(cogo_volume)<-c("oid", "did","n")
#Change the data type of the 'oid' and 'did' columns to character
cogo_volume$oid <- as.character(cogo_volume$oid)
cogo_volume$did <- as.character(cogo_volume$did)

#Import a shapefile containing the boundaries of Ohio's CBGs
shape <-st_read('Data/Franklin_BG_1.shp')

#Filter the imported shapefile to only retain the CBGs that have been involved in the bike sharing trips
#(either as a trip origin or a destination).
shape <- shape %>% filter(id %in% cogo_volume$oid | id %in% cogo_volume$did)

#Calculate the Gij* using the 'Gij.polygon' function from the 'spnaf' library.

result <- Gij.polygon(df = cogo_volume ,shape = shape , queen = TRUE, snap = 1, method = 't')

#Convert the list output of the 'Gij.polygon' function into a data frame.
result<-as.data.frame(result[[2]])
#Extract a subset of the result where the p-value is less than 0.05,
#which usually indicates that the results are statistically significant.
cogo_pval<-subset(result, result$pval<0.05)
head(cogo_pval)
