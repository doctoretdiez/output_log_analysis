########
### 6.29.18 Elizabeth Tokarz
### R maps with SCA 2017 output data
######### Chicago office
#####
####

setwd( "C:/Users/elizabethtokarz/Desktop")
# read in SCA data file
SCAoutput <- read.csv("output-log-2017.csv")
names(SCAoutput)
summary(SCAoutput)

#Position..Position.State
#Category.of.Work.You.Did #Improving trail, Education & outreach to people
#Sub.Category.of.work
#How.Much.You.Did
#Position..Conservation.Request.Type (community, corps, etc.)
#Position..Organization.Managing.This.Position..Sub.Type (federal, state)
#Position..Organization.Managing.This.Position..Agency (NPS, etc.)

# can't really count conservation impact or categories because some positions filled in many output lines.

# make some new vectors to organize our data
states_vec <- unique(SCAoutput$Position..Position.State)

# make this into a new dataframe where we will look at different outputs by state:
states.output <- data.frame(states_vec)

# Now we will add summations of different output categories
library(dplyr)

############ Trails
trail_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Improving trail (Unit: # feet)")
summary(trail_rows)
new_trail_rows <- filter(trail_rows, Sub.Category.of.work == "Building new trail")
names(new_trail_rows)
maintain_trail_rows <- filter(trail_rows, Sub.Category.of.work == "Blowdown, brushing, or widening")
summary(maintain_trail_rows)

# fill it in with a loop
for (i in 1:length(states_vec)){
states.output$new.trail.per.state[i] <- (sum(new_trail_rows$How.Much.You.Did[which(new_trail_rows$Position..Position.State == states_vec[i])]))/5280
}

for (i in 1:length(states_vec)){
states.output$maintain.trail.per.state[i] <- (sum(maintain_trail_rows$How.Much.You.Did[which(new_trail_rows$Position..Position.State == states_vec[i])]))/5280
}

############ Improving Land
land_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Improving land (Unit: # acres)")
summary(land_rows)
burn_rows <- filter(land_rows, Sub.Category.of.work == "Prescribed burns")
trashout_rows <- filter(land_rows, Sub.Category.of.work == "Trash clearing and removing structures")
invasive_rows <- filter(land_rows, Sub.Category.of.work == "Removing invasive species")

for (i in 1:length(states_vec)){
states.output$burns.per.state[i] <- sum(burn_rows$How.Much.You.Did[which(burn_rows$Position..Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.output$trashout.per.state[i] <- sum(trashout_rows$How.Much.You.Did[which(trashout_rows$Position..Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.output$invasive.per.state[i] <- sum(invasive_rows$How.Much.You.Did[which(invasive_rows$Position..Position.State == states_vec[i])])
}

############ Education
edu_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Education & outreach to people (Unit: # people)")
summary(edu_rows)
tabling_rows <- filter(edu_rows, Sub.Category.of.work == "Community events or tabling")
visitor_rows <- filter(edu_rows, Sub.Category.of.work == "Visitor center contacts or roving")
volunteer_rows <- filter(edu_rows, Sub.Category.of.work == "Leading volunteers in service")
tour_rows <- filter(edu_rows, Sub.Category.of.work == "Giving interpretive programs or tours")
curriculum_rows <- filter(edu_rows, Sub.Category.of.work == "Delivering environmental education curriculum")

for (i in 1:length(states_vec)){
states.output$tabling.per.state[i] <- sum(tabling_rows$How.Much.You.Did[which(tabling_rows$Position..Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.output$visitor.per.state[i] <- sum(visitor_rows$How.Much.You.Did[which(visitor_rows$Position..Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.output$volunteer.per.state[i] <- sum(volunteer_rows$How.Much.You.Did[which(volunteer_rows$Position..Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.output$tour.per.state[i] <- sum(tour_rows$How.Much.You.Did[which(tour_rows$Position..Position.State == states_vec[i])])
}

for (i in 1:length(states_vec)){
states.output$curriculum.per.state[i] <- sum(curriculum_rows$How.Much.You.Did[which(curriculum_rows$Position..Position.State == states_vec[i])])
}

summary(states.output)

############################ Shores
water_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Improving shore/waterway (Unit: # feet)")
summary(water_rows)
water_trashout_rows <- filter(water_rows, Sub.Category.of.work == "Trash clearing and removing structures")
water_invasive_rows <- filter(water_rows, Sub.Category.of.work == "Removing invasive species")
water_debris_rows <- filter(water_rows, Sub.Category.of.work == "Removing natural debris")

for (i in 1:length(states_vec)){
states.output$water.trashout.per.state[i] <- (sum(water_trashout_rows$How.Much.You.Did[which(water_trashout_rows$Position..Position.State == states_vec[i])]))/5280
}

for (i in 1:length(states_vec)){
states.output$water.invasive.per.state[i] <- (sum(water_invasive_rows$How.Much.You.Did[which(water_invasive_rows$Position..Position.State == states_vec[i])]))/5280
}

for (i in 1:length(states_vec)){
states.output$water.debris.per.state[i] <- (sum(water_debris_rows$How.Much.You.Did[which(water_debris_rows$Position..Position.State == states_vec[i])]))/5280
}

###########################################
###########################################
###########################################
###########################################
########################################### Now time to apply these to maps
###########################################
########### set up 50-state map#################
library("fiftystater")
library("mapproj")
library(ggplot2)

states.output$id <- c("new mexico", "alaska", "pennsylvania", "south dakota", 
	"indiana", "north dakota", "district of colombia", "louisiana", "montana",
	"texas", "wyoming", "florida", "california", "alabama", "arkansas", 
	"minnesota", "colorado", "new york", "georgia", "maryland", "idaho",
	"virginia", "washington", "maine", "oregon", "kansas", "ohio", "illinois",
	"kentucky", "new jersey", "utah", "missouri", "massachusetts", "mississippi",
	"west virginia", "oklahoma", "north carolina", "nebraska", "rhode island",
	"new hampshire", "arizona", "virgin islands", "tennessee", "guam", "michigan",
	"hawaii", "south carolina", "iowa", "wisconsin", "vermont", "delaware",
	"american samoa", "puerto rico", "connecticut", "blank")

map.output <- merge(fifty_states,states.output, by="id", all.x=T)
map.output <- map.output[order(map.output$order),]

# Make map into a function so we can breeze through different categories:
# Make them prettier later after we decide which data we like.

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


# When using this function, include the dataset and category you want to map,
# plus the title and label you will give it.

# if using the special color function, then also include the low and high colors.

##################################################################
# set up regional maps for quick use also
# unsure how to do in ggplot at the moment
library(usmap)

# very important to do this one first
states.output$state <- states.output$id

#make a function

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
##################################################################################
##################################################################################
##################################################################################
##################################################################################
#
# Now that the functions are loaded in, you can run the below and make some maps!
#
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
# new trail per state
SCA_50states_color(map.output, map.output$new.trail.per.state, 
"New Trail Built in 2017", "Miles", "white", "sienna")

Southwest(states.output, "new.trail.per.state", title_ne = "New Trail built in 2017", label_ne = "Miles")
Midwest(states.output, "new.trail.per.state", title_ne = "New Trail built in 2017", label_ne = "Miles")
Northeast(states.output, "new.trail.per.state", title_ne = "New Trail built in 2017", label_ne = "Miles")
Northwest(states.output, "new.trail.per.state", title_ne = "New Trail built in 2017", label_ne = "Miles")
Pacific(states.output, "new.trail.per.state", title_ne = "New Trail built in 2017", label_ne = "Miles")
South(states.output, "new.trail.per.state", title_ne = "New Trail built in 2017", label_ne = "Miles")
Mountains(states.output, "new.trail.per.state", title_ne = "New Trail built in 2017", label_ne = "Miles")
Southeast(states.output, "new.trail.per.state", title_ne = "New Trail built in 2017", label_ne = "Miles")


#####################################################################
# maintained trail per state
SCA_50states_color(map.output, map.output$maintain.trail.per.state, 
"Miles of Trail Maintained in 2017", "Miles", "white", "sienna")

Southwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Northwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Pacific(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Mountains(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
South(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Southeast(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Midwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Northeast(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")

#####################################################################
# acres burned per state
SCA_50states(map.output, map.output$burns.per.state, "Acres burned in 2017", "Acres")

Pacific(states.output, "burns.per.state", title_ne = "Acres burned in 2017", label_ne = "Acres")
Southwest(states.output, "burns.per.state", title_ne = "Acres burned in 2017", label_ne = "Acres")
Northwest(states.output, "burns.per.state", title_ne = "Acres burned in 2017", label_ne = "Acres")
Mountains(states.output, "burns.per.state", title_ne = "Acres burned in 2017", label_ne = "Acres")
Midwest(states.output, "burns.per.state", title_ne = "Acres burned in 2017", label_ne = "Acres")
South(states.output, "burns.per.state", title_ne = "Acres burned in 2017", label_ne = "Acres")
Southeast(states.output, "burns.per.state", title_ne = "Acres burned in 2017", label_ne = "Acres")
Northeast(states.output, "burns.per.state", title_ne = "Acres burned in 2017", label_ne = "Acres")

#####################################################################
# acres trash cleared per state
SCA_50states_color(map.output, map.output$trashout.per.state, 
"Acres cleared of trash in 2017", "Acres", "thistle4", "white")

SCA_50states(map.output, map.output$trashout.per.state, 
"Acres cleared of trash in 2017", "Acres")

Mountains(states.output, "trashout.per.state", title_ne = "Acres cleared on trash in 2017", label_ne = "Acres")
#####################################################################
# acres invasive cleared per state
SCA_50states(map.output, map.output$invasive.per.state, "Acres cleared of invasives in 2017", "Acres")

South(states.output, "invasive.per.state", title_ne = "Acres cleared of invasives in 2017", label_ne = "Acres")
Southeast(states.output, "invasive.per.state", title_ne = "Acres cleared of invasives in 2017", label_ne = "Acres")
Midwest(states.output, "invasive.per.state", title_ne = "Acres cleared of invasives in 2017", label_ne = "Acres")
South(states.output, "invasive.per.state", title_ne = "Acres cleared of invasives in 2017", label_ne = "Acres")
Pacific(states.output, "invasive.per.state", title_ne = "Acres cleared of invasives in 2017", label_ne = "Acres")
Southwest(states.output, "invasive.per.state", title_ne = "Acres cleared of invasives in 2017", label_ne = "Acres")

#####################################################################
# try rainbow palette for people?
# people encountered during tabling per state
SCA_50states_color(map.output, map.output$tabling.per.state, 
"People encountered while tabling in 2017", "People", "yellow", "salmon")

SCA_50states(map.output, map.output$tabling.per.state, 
"People encountered while tabling in 2017", "People")

SCA_50states_rb(map.output, map.output$tabling.per.state, 
"People encountered while tabling in 2017", "People")

Northeast(states.output, "tabling.per.state", title_ne = "People encountered while tabling 2017", label_ne = "People")
Southeast(states.output, "tabling.per.state", title_ne = "People encountered while tabling 2017", label_ne = "People")
South(states.output, "tabling.per.state", title_ne = "People encountered while tabling 2017", label_ne = "People")
Midwest(states.output, "tabling.per.state", title_ne = "People encountered while tabling 2017", label_ne = "People")
Mountains(states.output, "tabling.per.state", title_ne = "People encountered while tabling 2017", label_ne = "People")
Northwest(states.output, "tabling.per.state", title_ne = "People encountered while tabling 2017", label_ne = "People")
Southwest(states.output, "tabling.per.state", title_ne = "People encountered while tabling 2017", label_ne = "People")
Pacific(states.output, "tabling.per.state", title_ne = "People encountered while tabling 2017", label_ne = "People")

#####################################################################
# people encountered at visitor center per state
SCA_50states_rb(map.output, map.output$visitor.per.state, "Visitors encountered in 2017", "People")

Northeast(states.output, "visitor.per.state", title_ne = "Visitors encountered in 2017", label_ne = "People")
Southeast(states.output, "visitor.per.state", title_ne = "Visitors encountered in 2017", label_ne = "People")
South(states.output, "visitor.per.state", title_ne = "Visitors encountered in 2017", label_ne = "People")
Midwest(states.output, "visitor.per.state", title_ne = "Visitors encountered in 2017", label_ne = "People")
Mountains(states.output, "visitor.per.state", title_ne = "Visitors encountered in 2017", label_ne = "People")
Northwest(states.output, "visitor.per.state", title_ne = "Visitors encountered in 2017", label_ne = "People")
Southwest(states.output, "visitor.per.state", title_ne = "Visitors encountered in 2017", label_ne = "People")
Pacific(states.output, "visitor.per.state", title_ne = "Visitors encountered in 2017", label_ne = "People")

#####################################################################
# volunteers recruited per state
SCA_50states_rb(map.output, map.output$volunteer.per.state, "Volunteers recruited in 2017", "People")

#####################################################################
# people attending tours per state
SCA_50states_rb(map.output, map.output$tour.per.state, "People attending SCA tours in 2017", "People")

#####################################################################
# people educated in classroom per state
SCA_50states_rb(map.output, map.output$curriculum.per.state, "People educated in classrooms in 2017", "People")

South(states.output, "curriculum.per.state", title_ne = "People educated in classrooms", label_ne = "People")
Southeast(states.output, "curriculum.per.state", title_ne = "People educated in classrooms", label_ne = "People")
Midwest(states.output, "curriculum.per.state", title_ne = "People educated in classrooms", label_ne = "People")
Northeast(states.output, "curriculum.per.state", title_ne = "People educated in classrooms", label_ne = "People")

#####################################################################
# miles of shoreline cleared of natural debris per state  
SCA_50states_color(map.output, map.output$water.debris.per.state, 
"Miles of shoreline cleared of natural debris in 2017", "Miles", "white", "blue")

#####################################################################
# miles of shoreline cleared of trash per state
SCA_50states_color(map.output, map.output$water.trashout.per.state, 
"Miles of shoreline cleared of trash in 2017", "Miles", "white", "blue")
* note that nevada looks grey, but that is just the default NA color.

Southeast(states.output, "water.trashout.per.state", title_ne = "Miles of shoreline cleared of trash 2017", label_ne = "Miles")
#####################################################################
# miles of shoreline cleared of invasives per state
SCA_50states_color(map.output, map.output$water.invasive.per.state, 
"Miles of shoreline cleared of invasives in 2017", "Miles", "white", "blue")

