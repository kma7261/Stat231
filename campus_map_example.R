library(tidyverse)
library(ggpubr)
library(jpeg)

#campus map image from:
# https://www.amherst.edu/system/files/styles/original/private/media/0971/campus%2520map%2520of%2520libraries.jpg

# update path to the location of the file on your computer
path <- "C:/Users/kcorreia/Dropbox (Amherst College)/Teaching/Spring 2021/STAT231/Calendar Assignment"
# update campus_map to the name of the file
img <- readJPEG(paste0(path,"/campus_map.jpg"))

# blank map
ggplot() +
  background_image(img)

# add coordinates
ggplot() +
  background_image(img) +
  xlim(0, 100) +
  ylim(0, 100)

# add (fake) data
fake_data <- data.frame(location = c("Frost Library", "Johnson Chapel", "Keefe Campus Center")
                        , tot_hrs = c(10,1,5)
                        # will need to change this based on the map you use 
                        , x_coord = c(26, 53, 40)
                        , y_coord = c(50, 40, 95))

ggplot(data=fake_data, aes(x = x_coord, y = y_coord, color = tot_hrs)) +
  background_image(img) +
  geom_point(size = 10) + 
  xlim(0, 100) +
  ylim(0, 100) 

# clean up plot
ggplot(data=fake_data, aes(x = x_coord, y = y_coord, color = tot_hrs)) +
  background_image(img) +
  geom_point(size = 10) + 
  geom_point(shape = 1, size = 10,colour = "black") +
  xlim(0, 100) +
  ylim(0, 100) +
  labs(color = "Total hours in location") + 
  # change color scheme to sequential palette in brewer (for continuous scale)
  scale_color_distiller(type = "seq", palette = "BuPu", direction = 1) +
  # remove x and y axis ticks and labels
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

# could map size and color as visual cues for total hours
# but some (smaller and lighter) points may hard to see then
ggplot(data=fake_data, aes(x = x_coord, y = y_coord, color = tot_hrs
                           , size = tot_hrs)) +
  background_image(img) +
  geom_point() + 
  geom_point(shape = 1, color = "black") +
  xlim(0, 100) +
  ylim(0, 100) +
  labs(color = "Total hours in location"
       , size = "Total hours in location") + 
  # set minimum and maximum point sizes 
  # (otherwise smallest point may be too hard to see)
  scale_size_continuous(range = c(5, 10)) + 
  # change color scheme to sequential palette in brewer (for continuous scale)
  scale_color_distiller(type = "seq", palette = "BuPu", direction = 1) +
  # remove x and y axis ticks and labels
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="bottom")

# could imagine numerous variations of this if wanted to incorporate other information
# e.g. two points at each location with size relative to total hours spent there
# and color indicating weekday or weekend (one point for weekday and one point for weekend)
# etc. . . .
