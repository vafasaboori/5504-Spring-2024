# ggplot2 is one of the core members of the tidyverse
install.packages("tidyverse")
library(tidyverse)

# mpg data frame found in ggplot2
library(ggplot2)
ggplot2::mpg
str(mpg)
?mpg # or help(mpg)

# To plot mpg, run this code to put displ on the x-axis and hwy on the y-axis:
ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point()

# Alternate method
mpg %>% ggplot(aes(x = displ, y = hwy)) + #shortcut for pipe shift + command + m 
  geom_point()

# map the colors of points to the class variable to reveal the class of each car
ggplot(mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point()

# alpha aesthetic controls the transparency of the points
ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) + 
  geom_point()

# shape aesthetic controls the shape of the points
ggplot(mpg, aes(x = displ, y = hwy, shape = class)) + 
  geom_point()

# set the aesthetic properties of your geom manually. all points blue
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")

# split your plot into facets (subplots) each display one subset of the data
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~ class, nrow = 2)

# facet plot on the combination of two variables add facet_grid()
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl)
    
# smooth geom, a smooth line fitted to the data
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_smooth()

# different line type for each unique value of the mapped variable
ggplot(mpg, aes(x = displ, y = hwy, linetype = drv)) + 
  geom_smooth()

# or color
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_smooth()

# display multiple geoms in the same plot
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# add color manually
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color ="blue") + 
  geom_smooth(color = "red")

# different color and a separate trend-line for each class
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point() + 
  geom_smooth()

# same graph without standard error 
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# different color for each class with a single trend-line for entire data set
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth(se = FALSE)

# Scales, let's start with a simple graph
ggplot(mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()

# let's reverse scales
ggplot(mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse()

# Color Sclase
ggplot(mpg, mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point() +
  scale_color_brewer()

# Color Palettes
ggplot(mpg, mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point() +
  scale_color_brewer(palette = 5)
  
# create a bar graph based on the class variable (categorical)
ggplot(mpg, aes(x = class)) + 
  geom_bar()
# count is a new value obtained through statistical transformation

# we can do this manually
(class_count <- dplyr::count(mpg, class))

# create a bar graph based on the hwy variable (continuous)
ggplot(mpg, aes(x = hwy)) + 
  geom_histogram()

# create a stacked bar graph based on the drv variable (categorical)
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar()

# Cartesian Coordinate System
# Limit X Axis
ggplot(mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  coord_cartesian(xlim=c(0,5))

# Bar graph with flipped bars
ggplot(mpg, aes(x=class)) +
  geom_bar() +
  coord_flip()
  
# Facet Grid creates subplots based on two categorical variable
ggplot(mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  facet_grid(year ~ cyl)

# Labels and Annotations
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  labs(title = "Fuel Efficiency by Engine Volume",
       subtitle = "Fuel Economy Data from 1998 to 2008 for 38 Popular Cae Models",
       x = "Engine Volume (Liters)",
       y = "Fuel Efficiency (Miles per Gallon",
       color = "Car Type")
# ctrl + shift + 6 (Maximize Plot Window)
