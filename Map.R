library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
world <- ne_countries(scale = "medium", returnclass = "sf")

map<-ggplot(data = world) +
    geom_sf()+
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_orienteering(line_width =0.5 ))  +
    annotate(geom = "text", x = 0, y = 70, label = paste("Norwegian", "Sea",sep="\n"), 
             fontface = "italic", color = "grey22", size = 12)+
    annotate(geom = "text", x = 40, y = 75, label = paste("Barents", "Sea",sep="\n"), 
             fontface = "italic", color = "grey22", size = 12)+
    coord_sf(xlim = c(-25, 60), ylim = c(55, 82), expand = FALSE) +
    xlab("Longitude") +
    ylab("Latitude")

map
