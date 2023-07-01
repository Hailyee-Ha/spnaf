#'mapdeck' for creating interactive maps
library(mapdeck)
#'RColorBrewer' for creating color palettes
library(RColorBrewer)
#'dplyr' for data wrangling
library(dplyr)

key<-'pk.abc' ## put your own token here
set_token(key)

#open the file including geometry information of each CBG
id_geometry<-read.csv("Data/id_geometry_f.csv")

######visualize the total flow(>20) on the interactive map######
#Merge the previously created 'cogo_volume' dataset with the 'id_geometry' dataset by 'oid' and 'id',
#and then again by 'did' and 'id.
#This process links trip volume data with corresponding geometric information.
total_geo<-merge(cogo_volume,id_geometry,by.x="oid",by="id")
total_geo<-merge(total_geo,id_geometry,by.x="did",by="id")

#Filter out rows where 'oid' is equal to 'did' to remove trips that start and end in the same place.
total_geo<-subset(total_geo,total_geo$oid!=total_geo$did)
total_map<-as.data.frame(cbind(total_geo$x.x,total_geo$y.x,total_geo$x.y,total_geo$y.y,total_geo$n))

#Rename the dataset columns to denote the start and end coordinates of each trip.
names(total_map)<-c("start_lon", "start_lat","end_lon", "end_lat","n")

#Generate an 'id' column to assign unique identifiers to each row
#and a 'stroke' column to determine the width of the lines to be drawn on the map.
total_map$id<-seq_len(nrow(total_map))
total_map$stroke <- round((percent_rank(total_map$n)*5),digits = 3)
#Filter the data to only include trips that have a total volume of more than 20.
total_map<-subset(total_map,total_map$n>20)

#define the range for the 'n' variable
n_range <- range(total_map$n)
#create the break points for quintile scales of the total volumes
break_points <- quantile(total_map$n, probs = seq(0, 1, 0.2), na.rm = TRUE)

#cut 'n' into quintiles
total_map$n_cat <- cut(total_map$n, breaks = break_points, labels = FALSE, include.lowest = TRUE)

#for color palette
color_func <- colorRampPalette(c("#C6DBEF", "#9ECAE1", "#4292C6","#2171B5", "#084594"))
# Create a palette with 5 colors
palette <- color_func(5)

# Assign a color to each category using a color palette.
total_map$n_color <- palette[as.integer(total_map$n_cat)]

#Create an interactive map with arcs representing the bike trips.
#The color of each arc are determined by the number of trips (volume).
mapdeck(token = key, style = mapdeck_style("streets"), pitch = 45) %>%
  add_arc(
    data = total_map,
    layer_id = "arcs",
    origin = c("start_lon", "start_lat"),
    destination = c("end_lon", "end_lat"),
    stroke_from = "n_color",
    stroke_to = "n_color",
    stroke_from_opacity = 0.6,
    stroke_to_opacity = 0.6,
    stroke_width = "stroke",
    auto_highlight = TRUE
  )


######visualize flow hot-spots with a p-value of 0.05######
#Follow similar steps as in the total flow visualization,
#but this time merging 'cogo_pval' with 'id_geometry'.

gij_geo<-merge(cogo_pval,id_geometry,by.x="oid",by="id")
gij_geo<-merge(gij_geo,id_geometry,by.x="did",by="id")
gij_map<-as.data.frame(cbind(gij_geo$x.x,gij_geo$y.x,gij_geo$x.y,gij_geo$y.y,gij_geo$Gij,gij_geo$pval))
names(gij_map)<-c("start_lon", "start_lat","end_lon", "end_lat","Gij","pval")

gij_map$id<-seq_len(nrow(gij_map))
gij_map$stroke <- round((percent_rank(gij_map$Gij)*5),digits = 3)


# Define the range for the 'Gij' variable (Gij*)
n_range <- range(gij_map$Gij)

#Categorize 'Gij' into quintiles.
break_points <- quantile(gij_map$Gij, probs = seq(0, 1, 0.2), na.rm = TRUE)
gij_map$n_cat <- cut(gij_map$Gij, breaks = break_points, labels = FALSE, include.lowest = TRUE)

# Define color function with 10 colors
# For Hot-spot color palette
color_func <- colorRampPalette(c("#FFF5F0", "#FCBBA1", "#FC9272", "#CB181D", "#99000D"))


# Create a palette with 10 colors
palette <- color_func(5)

#Assign a color to each category using a color palette.
gij_map$n_color <- palette[as.integer(gij_map$n_cat)]

#Create an interactive map with arcs representing the bike trips.
#The color of each arc are determined by the Gij* values.
mapdeck(token = key, style = mapdeck_style("streets"), pitch = 45) %>%
  add_arc(
    data = gij_map,
    layer_id = "arcs",
    origin = c("start_lon", "start_lat"),
    destination = c("end_lon", "end_lat"),
    stroke_from = "n_color",
    stroke_to = "n_color",
    stroke_from_opacity = 0.6,
    stroke_to_opacity = 0.6,
    stroke_width = gij_map$Gij* 3, # assuming 'n' also determines width of arcs
    auto_highlight = TRUE
  )


