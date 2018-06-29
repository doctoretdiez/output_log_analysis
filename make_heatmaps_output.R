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


########################################### Now time to apply these to maps

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
#####################################################################
# new trail per state
SCA_50states_color(map.output, map.output$new.trail.per.state, 
"New Trail Built in 2017", "Miles", "white", "sienna")

#####################################################################
# maintained trail per state
SCA_50states_color(map.output, map.output$maintain.trail.per.state, 
"Miles of Trail Maintained in 2017", "Miles", "white", "sienna")

#####################################################################
# acres burned per state
SCA_50states(map.output, map.output$burns.per.state, "Acres burned in 2017", "Acres")

#####################################################################
# acres trash cleared per state
SCA_50states_color(map.output, map.output$trashout.per.state, 
"Acres cleared of trash in 2017", "Acres", "thistle4", "white")

SCA_50states(map.output, map.output$trashout.per.state, 
"Acres cleared of trash in 2017", "Acres")

#####################################################################
# acres invasive cleared per state
SCA_50states(map.output, map.output$invasive.per.state, "Acres cleared of invasives in 2017", "Acres")

#####################################################################
# try rainbow palette for people?
# people encountered during tabling per state
SCA_50states_color(map.output, map.output$tabling.per.state, 
"People encountered while tabling in 2017", "People", "yellow", "salmon")

SCA_50states(map.output, map.output$tabling.per.state, 
"People encountered while tabling in 2017", "People")

SCA_50states_rb(map.output, map.output$tabling.per.state, 
"People encountered while tabling in 2017", "People")

#####################################################################
# people encountered at visitor center per state
SCA_50states_rb(map.output, map.output$visitor.per.state, "Visitors encountered in 2017", "People")

#####################################################################
# volunteers recruited per state
SCA_50states_rb(map.output, map.output$volunteer.per.state, "Volunteers recruited in 2017", "People")

#####################################################################
# people attending tours per state
SCA_50states_rb(map.output, map.output$tour.per.state, "People attending SCA tours in 2017", "People")

#####################################################################
# people educated in classroom per state
SCA_50states_rb(map.output, map.output$curriculum.per.state, "People educated in classrooms in 2017", "People")

#####################################################################
# miles of shoreline cleared of natural debris per state  
SCA_50states_color(map.output, map.output$water.debris.per.state, 
"Miles of shoreline cleared of natural debris in 2017", "Miles", "white", "blue")

#####################################################################
# miles of shoreline cleared of trash per state
SCA_50states_color(map.output, map.output$water.trashout.per.state, 
"Miles of shoreline cleared of trash in 2017", "Miles", "white", "blue")

#####################################################################
# miles of shoreline cleared of invasives per state
SCA_50states_color(map.output, map.output$water.invasive.per.state, 
"Miles of shoreline cleared of invasives in 2017", "Miles", "white", "blue")

