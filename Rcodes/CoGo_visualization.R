
library(mapdeck)
library(RColorBrewer)
# for data wrangling
library(dplyr)
roads_hamburg <- read_sf("hamburg.shp")

key<-'pk.eyJ1IjoiaGFpbHllZSIsImEiOiJjbDhrZDJ2N2kwZm9yM3lsNXN0Yjh3azAwIn0.J1kLy6DOpr0NlKIKJePxtQ' ## put your own token here
set_token(key)

#open the file including geometry information of each CBG
id_geometry<-read.csv("Data/id_geometry_f.csv")

######visualize the total flow(>20) on the interactive map######
#create the data set having longitude and latitute information for each flow
total_geo<-merge(cogo_volume,id_geometry,by.x="oid",by="id")
total_geo<-merge(total_geo,id_geometry,by.x="did",by="id")
total_geo<-subset(total_geo,total_geo$oid!=total_geo$did)
total_map<-as.data.frame(cbind(total_geo$x.x,total_geo$y.x,total_geo$x.y,total_geo$y.y,total_geo$n))
names(total_map)<-c("start_lon", "start_lat","end_lon", "end_lat","n")

total_map$id<-seq_len(nrow(total_map))
total_map$stroke <- round((percent_rank(total_map$n)*5),digits = 3)
#subset the total_map data for the CBGs having more than 20 bike sharing trips
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

# Assign colors to 'n' values
total_map$n_color <- palette[as.integer(total_map$n_cat)]

#create the interactive map of the CoGo bike sharing trips
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

