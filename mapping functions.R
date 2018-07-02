########
### 7.02.18 Elizabeth Tokarz
### Map functions for 50 states and SCA regions
######### Chicago office
#####
####

# load necessary packages
library(ggplot2)
library("fiftystater")
library("maps")
library("mapproj")
library("usmap")

###########################################################################
# normal 50 states with reverse heat colors
SCA_50states <- function(dataset, state_category, title, label){
ggplot(dataset, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = state_category), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  guides(fill = guide_colorbar(direction = "horizontal", title = label, barwidth = 15,
  label.theme = element_text(angle = 0)))+
  coord_map() + fifty_states_inset_boxes() + ggtitle(title)
}


###########################################################################
# 50 states with rainbows

SCA_50states_rb <- function(dataset, state_category, title, label){
ggplot(dataset, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = state_category), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=(rainbow(10, start = .13, end = 1)),na.value="grey90")+
  guides(fill = guide_colorbar(direction = "horizontal", title = label, barwidth = 15,
  label.theme = element_text(angle = 0)))+
  coord_map() + fifty_states_inset_boxes() + ggtitle(title)
}

###########################################################################
# 50 states with color options

SCA_50states_color <- function(dataset, state_category, title, label, start, end){
ggplot(dataset, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = state_category), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
scale_fill_gradient(low = start, high = end, na.value = "grey90") +
  guides(fill = guide_colorbar(direction = "horizontal", title = label, barwidth = 15,
  label.theme = element_text(angle = 0)))+
  coord_map() + fifty_states_inset_boxes() + ggtitle(title)
}

###########################################################################
#REGIONAL MAPS # unsure how to do in ggplot at the moment

Northeast <- function(dataset_ne, category_ne, border_col = "green", start_col = "white", end_col = "green", title_ne, label_ne){
plot_usmap(
    data = dataset_ne, values = category_ne, 
	include = c("ME", "VT", "NH", "MA", "RI", "CT", "NY", "NJ", "PA"), lines = border_col
  ) + 
  scale_fill_continuous(
    low = start_col, high = end_col, name = label_ne, label = scales::comma
  ) + 
  labs(title = title_ne) +
  theme(legend.position = "right")
}
########################
Southeast <- function(dataset_ne, category_ne,  border_col = "darkgreen", start_col = "white", end_col = "darkgreen", title_ne, label_ne){
plot_usmap(
    data = dataset_ne, values = category_ne, 
	include = c("MD", "DE", "VA", "WV", "KY", "TN", "NC", "SC", "GA"), lines = border_col
  ) + 
  scale_fill_continuous(
    low = start_col, high = end_col, name = label_ne, label = scales::comma
  ) + 
  labs(title = title_ne) +
  theme(legend.position = "right")
}
########################
Midwest <- function(dataset_ne, category_ne,  border_col = "salmon", start_col = "white", end_col = "salmon", title_ne, label_ne){
plot_usmap(
    data = dataset_ne, values = category_ne, 
	include = c("OH", "MI", "IN", "IL", "WI", "MN", "IA", "ND", 
	"SD", "NE", "KS", "OK", "AR", "MO"), lines = border_col
 ) + 
  scale_fill_continuous(
    low = start_col, high = end_col, name = label_ne, label = scales::comma
  ) + 
  labs(title = title_ne) +
  theme(legend.position = "right")
}
########################
South <- function(dataset_ne, category_ne,  border_col = "firebrick", start_col = "white", end_col = "firebrick", title_ne, label_ne){
plot_usmap(
 data = dataset_ne, values = category_ne, 
	include = c("FL", "MS", "AL", "LA", "TX"), lines = border_col
 ) + 
  scale_fill_continuous(
    low = start_col, high = end_col, name = label_ne, label = scales::comma
  ) + 
  labs(title = title_ne) +
  theme(legend.position = "right")
}
########################
Mountains <- function(dataset_ne, category_ne,  border_col = "powderblue", start_col = "white", end_col = "powderblue", title_ne, label_ne){
plot_usmap(
 data = dataset_ne, values = category_ne, 
	include = c("ID", "MT", "WY", "UT", "CO", "NM"), lines = border_col
 ) + 
  scale_fill_continuous(
    low = start_col, high = end_col, name = label_ne, label = scales::comma
  ) + 
  labs(title = title_ne) +
  theme(legend.position = "right")
}
########################
Southwest <- function(dataset_ne, category_ne,  border_col = "dodgerblue3", start_col = "white", end_col = "dodgerblue3", title_ne, label_ne){
plot_usmap(
 data = dataset_ne, values = category_ne, 
	include = c("CA", "NV", "AZ"), lines = border_col
 ) + 
  scale_fill_continuous(
    low = start_col, high = end_col, name = label_ne, label = scales::comma
  ) + 
  labs(title = title_ne) +
  theme(legend.position = "right")
}
########################
Northwest <- function(dataset_ne, category_ne,  border_col = "slateblue4", start_col = "white", end_col = "slateblue4", title_ne, label_ne){
plot_usmap(
 data = dataset_ne, values = category_ne, 
	include = c("WA", "OR"), lines = border_col
 ) + 
  scale_fill_continuous(
    low = start_col, high = end_col, name = label_ne, label = scales::comma
  ) + 
  labs(title = title_ne) +
  theme(legend.position = "right")
}
########################
Pacific <- function(dataset_ne, category_ne,  border_col = "turquoise1", start_col = "white", end_col = "turquoise1", title_ne, label_ne){
plot_usmap(
 data = dataset_ne, values = category_ne, 
	include = c("AK", "HI"), lines = border_col
 ) + 
  scale_fill_continuous(
    low = start_col, high = end_col, name = label_ne, label = scales::comma
  ) + 
  labs(title = title_ne) +
  theme(legend.position = "right")
}
##############################################################################
