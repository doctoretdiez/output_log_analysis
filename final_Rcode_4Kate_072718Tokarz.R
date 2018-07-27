########
### 7.27.18 Elizabeth Tokarz
### R graphics with SCA 2017 data
######### Chicago office
#####
####

# All annotations will be written following pound signs.
# The pound sign tells R not to read that line and not to process it.

# First run the following lines of code to load 
# the necessary packages and functions

# To run the code, place the cursor at the front of the line and 
# type Ctrl + R or Ctrl + Enter (different computers may be set up differently)
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

# Packages

library(ggplot2)
library("fiftystater")
library("maps")
library("mapproj")
library("usmap")

###########################################################################
# normal 50 states with reverse heat colors function
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
# 50 states with rainbows function

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
# 50 states with color options function

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

####################################################################
# Load the REGIONAL MAP FUNCTIONS too

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
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
######### Now that the packages and map functions have been loaded, 
# we can load the data

# First let's load the Positions document to make maps for openings in 2017

# This below function sets the working directory, which differs by computer
# Pick a place in your computer to store the positions document
setwd( "C:/Users/elizabethtokarz/Desktop")
# read in SCA data file
# if you change the name of the position file, or use a different one,
# be sure to change the name below when reading in the csv
SCA <- read.csv("report1529602920161_R_use.csv")


### Now we will manipulate the position data to tally the number 
# of openings and weeks spent by SCA members per state
# make a new column with number of weeks completed by all members
SCA$Member.Weeks <- SCA$Actual.Number.of.Openings*SCA$Duration.In.Weeks
table(SCA$Position.State, SCA$Actual.Number.of.Openings)

# make some new vectors to organize our data
states_vec <- unique(SCA$Position.State)

# make this into a new dataframe
states.df <- data.frame(states_vec)

# fill it in with a loop
for (i in 1:length(states_vec)){
states.df$count.openings.per.state[i] <- sum(SCA$Actual.Number.of.Openings[which(SCA$Position.State == states_vec[i])])
}

# fill this one in with a loop too
for (i in 1:length(states_vec)){
states.df$count.weeks.per.state[i] <- (sum(SCA$Member.Weeks[which(SCA$Position.State == states_vec[i])]))
}


### Do the same for the different program types
# make another set of new columns for opening types.
library(dplyr)
internSCA <- filter(SCA, Conservation.Request.Type == "Internships")
crewSCA <- filter(SCA, Conservation.Request.Type == "Crew")
corpsSCA <- filter(SCA, Conservation.Request.Type == "Corps")
communitySCA <- filter(SCA, Conservation.Request.Type == "Community")

for (i in 1:length(states_vec)){
states.df$intern[i] <- sum(internSCA$Actual.Number.of.Openings[which(internSCA$Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.df$crew[i] <- sum(crewSCA$Actual.Number.of.Openings[which(crewSCA$Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.df$corps[i] <- sum(corpsSCA$Actual.Number.of.Openings[which(corpsSCA$Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.df$community[i] <- sum(communitySCA$Actual.Number.of.Openings[which(communitySCA$Position.State == states_vec[i])])
}

# Prepare the data to be entered into the heat maps
states.df$id <- c("new mexico", "alaska", "pennsylvania", "south dakota", 
	"indiana", "north dakota", "district of colombia", "louisiana", "montana",
	"texas", "wyoming", "florida", "california", "alabama", "arkansas", 
	"minnesota", "colorado", "new york", "georgia", "maryland", "idaho",
	"virginia", "washington", "maine", "oregon", "kansas", "ohio", "illinois",
	"kentucky", "new jersey", "utah", "missouri", "massachusetts", "mississippi",
	"west virginia", "oklahoma", "north carolina", "nebraska", "rhode island",
	"new hampshire", "arizona", "virgin islands", "tennessee", "guam", "michigan",
	"hawaii", "south carolina", "iowa", "wisconsin", "vermont", "delaware",
	"american samoa", "puerto rico", "connecticut", "blank")

states.df$state <- states.df$id
map.df <- merge(fifty_states,states.df, by="id", all.x=T)
map.df <- map.df[order(map.df$order),]


############ Openings per state
SCA_50states(map.df, map.df$count.openings.per.state, title = "SCA Openings 2017", label = "Openings")

Northeast(states.df, "count.openings.per.state", title_ne = "Openings in 2017", label_ne = "Openings")

############# Weeks worked in each state

SCA_50states(map.df, map.df$count.weeks.per.state, "SCA Weeks 2017", "weeks")

Pacific(states.df, "count.weeks.per.state", title_ne = "SCA weeks in 2017", label_ne = "Weeks")

#####################################################################







########## Discrete maps

# Discrete version Openings Map ######################## Run all the below
spr <- select(states.df, state, count.openings.per.state)
spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(count.openings.per.state, quantile(count.openings.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))

gusa_spr <- left_join(states.df, spr, "state")

ggplot(gusa_spr, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = pcls), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +
	fifty_states_inset_boxes() + ggtitle("SCA Openings 2017") +
	scale_fill_brewer(palette = "YlOrRd", name = "Openings", 
	labels = c("Lower 20%", "Lower-Middle 20%", "Middle 20%",
			"Upper-Middle 20%", "Upper 20%", "None"))

#########################################################################





