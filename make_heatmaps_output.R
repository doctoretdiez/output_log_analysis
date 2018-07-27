########
### 6.29.18 Elizabeth Tokarz
### R maps with SCA 2017 output data
######### Chicago office
#####
####

setwd( "C:/Users/elizabethtokarz/Documents")
# read in SCA data file
SCAoutput <- read.csv("Cleaned_Melissa_outputs2017.csv")
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
summary(new_trail_rows)
non_new_trail_rows <- filter(trail_rows, Sub.Category.of.work != "Building new trail")
summary(non_new_trail_rows)
maintain_trail_rows <- filter(trail_rows, Sub.Category.of.work == "Blowdown, brushing, or widening")
summary(maintain_trail_rows)

# fill it in with a loop
for (i in 1:length(states_vec)){
states.output$new.trail.per.state[i] <- (sum(new_trail_rows$How.Much.You.Did[which(new_trail_rows$Position..Position.State == states_vec[i])]))/5280
}

for (i in 1:length(states_vec)){
states.output$maintain.trail.per.state[i] <- (sum(maintain_trail_rows$How.Much.You.Did[which(maintain_trail_rows$Position..Position.State == states_vec[i])]))/5280
}

for (i in 1:length(states_vec)){
states.output$non.new.trail.per.state[i] <- (sum(non_new_trail_rows$How.Much.You.Did[which(non_new_trail_rows$Position..Position.State == states_vec[i])]))/5280
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


# Make some new variables to show how trail maps would look in the future
states.output$new.trail.future <- states.output$new.trail.per.state
states.output$new.trail.future[states.output$new.trail.future > 1.5] <- states.output$new.trail.future[states.output$new.trail.future > 1.5]*2
states.output$new.trail.future[states.output$new.trail.future < 1.5] <- states.output$new.trail.future[states.output$new.trail.future < 1.5]*3.2
states.output$new.trail.future[states.output$new.trail.future < 0.000001] <- states.output$new.trail.future[states.output$new.trail.future < 0.000001] + 0.8
states.output$new.trail.future[c(13, 14, 24, 29, 38, 42, 44, 51, 52, 53, 55)] <- 0
states.output$new.trail.future[45] <- 2
states.output$new.trail.future[30] <- 3.3
states.output$new.trail.future[49] <- 6.8
states.output$new.trail.future[9] <- 1.5
states.output$new.trail.future[39] <- 4.1
states.output$new.trail.future[40] <- 2
states.output$new.trail.future[54] <- 1.6
states.output$new.trail.future[46] <- 3
states.output$new.trail.future[48] <- 0.6
states.output$new.trail.future[51] <- 3.7
states.output$new.trail.future[52] <- 2.5
states.output$new.trail.future[4] <- 24

states.output$non.new.trail.future <- states.output$non.new.trail.per.state
states.output$non.new.trail.future[states.output$non.new.trail.future < 150] <- states.output$non.new.trail.future[states.output$non.new.trail.future < 150] + 100
states.output$non.new.trail.future[c(50, 3, 17, 38, 39, 32)] <- states.output$non.new.trail.future[c(50, 3, 17, 38, 39, 32)] + 150
states.output$non.new.trail.future[19] <- states.output$non.new.trail.future[19] + 450
 

sum(states.output$new.trail.per.state) # 50.4
sum(states.output$new.trail.future) # 158.1
sum(states.output$non.new.trail.per.state) # 3761.9
sum(states.output$non.new.trail.future) # 10111.9
###########################################
###########################################
###########################################
###########################################
########################################### Now time to apply these to maps
###########################################
########### set up 50-state map#################
# make sure the functions have been loaded in.
library(fiftystater)

# very important to do this one first # and whenever the input changes, so does the order
states.output$id <- c("new jersey", "pennsylvania", "illinois", "new york", 
	"virginia", "district of colombia", "maryland", "california", "texas",
	"washington", "massachusetts", "indiana", "blank", "nebraska", "kentucky", 
	"idaho", "alaska", "new hampshire", "west virginia", "south dakota", "wyoming",
	"missouri", "alabama", "mississippi", "georgia", "connecticut", "ohio", 
	"tennessee", "oregon", "louisiana", "maine", "arizona", "colorado", "vermont",
	"montana", "north carolina", "north dakota", "oklahoma", "michigan", "utah",
	"south carolina", "new mexico", "minnesota", "hawaii", "wisconsin", "florida",
	"iowa", "arkansas", "guam", "nevada", "kansas", "virgin islands", "puerto rico",
	"delaware", "rhode island")

states.output$state <- states.output$id
map.output <- merge(fifty_states,states.output, by="id", all.x=T)
map.output <- map.output[order(map.output$order),]

##################################################################################
# new trail per state

barplot(map.output$new.trail.per.state)

SCA_50states_color(map.output, map.output$new.trail.per.state, 
"New Trail Built in 2017", "Miles", "white", "sienna")

SCA_50states_color(map.output, map.output$new.trail.future, 
"Projected New SCA Trail 2017-2022", "Miles", "white", "sienna")

# in five years
map.output$new.trail.per.state

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

barplot(map.output$maintain.trail.per.state)

SCA_50states_color(map.output, map.output$maintain.trail.per.state, 
"Miles of Trail Maintained in 2017", "Miles", "white", "sienna")

SCA_50states_color(map.output, map.output$maintain.trail.future, 
"Projected Maintained SCA Trail 2017-2022", "Miles", "white", "sienna")

##binned long way
spr <- select(states.output, state, maintain.trail.per.state)
spr <- slice(spr, 1:54)
ncls <- 5
spr <- mutate(spr,
              pcls = cut(maintain.trail.per.state, quantile(maintain.trail.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))

gusa_spr <- left_join(states.output, spr, "state")

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
	fifty_states_inset_boxes() + ggtitle("2017 SCA Trail Maintained") +
	scale_fill_brewer(palette = "YlOrRd", name = "Miles", 
	labels = c("Lower 25%", "Lower-Middle 25%", 
			"Upper-Middle 25%", "Upper 25%", "NA"))

Southwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Northwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Pacific(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Mountains(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
South(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Southeast(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Midwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Northeast(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")

Midwest(states.output, "non.new.trail.future", title_ne = "Projected Miles of Trail Maintained 2017-2022", label_ne = "Miles")

#####################################################################
# non-new trails per state (everything else)
SCA_50states_color(map.output, map.output$non.new.trail.per.state, 
"Trail Maintenance in 2017", "Miles", "white", "sienna")

SCA_50states_color(map.output, map.output$non.new.trail.future, 
"Projected Trail Maintenance 2017-2022", "Miles", "white", "sienna")

SCA_50states(map.output, map.output$non.new.trail.per.state, 
"Trail Maintenance in 2017", "Miles")

SCA_50states(map.output, map.output$non.new.trail.future, 
"Projected Trail Maintenance 2017-2022", "Miles")

spr <- select(states.output, state, non.new.trail.per.state)
spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(non.new.trail.per.state, quantile(non.new.trail.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))

gusa_spr <- left_join(states.output, spr, "state")

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
	fifty_states_inset_boxes() + ggtitle("2017 SCA Trail Maintained") +
	scale_fill_brewer(palette = "YlOrRd", name = "Miles", 
	labels = c("Lower 20%", "Lower-Middle 20%", "Middle 20%", 
			"Upper-Middle 20%", "Upper 20%", "NA"))
######## projection now
spr <- select(states.output, state, non.new.trail.future)
spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(non.new.trail.future, quantile(non.new.trail.future, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))

gusa_spr <- left_join(states.output, spr, "state")

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
	fifty_states_inset_boxes() + ggtitle("Projected SCA Trail Maintained, 2017-2022") +
	scale_fill_brewer(palette = "YlOrRd", name = "Miles", 
	labels = c("Lower 20%", "Lower-Middle 20%", "Middle 20%", 
			"Upper-Middle 20%", "Upper 20%", "NA"))

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
# note that nevada looks grey, but that is just the default NA color.

Southeast(states.output, "water.trashout.per.state", title_ne = "Miles of shoreline cleared of trash 2017", label_ne = "Miles")
#####################################################################
# miles of shoreline cleared of invasives per state
SCA_50states_color(map.output, map.output$water.invasive.per.state, 
"Miles of shoreline cleared of invasives in 2017", "Miles", "white", "blue")

#####################################################################
#Conservation impact
order(table(SCAoutput$Conservation.Impact)

# alphabetical order categories
par(mar = (c(10, 4, 2, 2) + 0.1))
barplot(table(SCAoutput$Conservation.Impact), las = 2, cex.names = .7,
horiz = F, main = "2017 Conservation Impact Categories")

# flip it!
par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(table(SCAoutput$Conservation.Impact), las = 1, cex.names = .7,
horiz = T, main = "2017 Conservation Impact Categories")


# pie chart # avoid using!
pie(table(SCAoutput$Conservation.Impact))

# numeric order categories

ppx <- table(SCAoutput$Conservation.Impact)
ppx <- ppx[rev(order(ppx))]
names(ppx)[4] <- "blank"

par(mar = (c(10, 4, 2, 2) + 0.1))
barplot(ppx, las = 2, cex.names = .7,
horiz = F, main = "2017 Conservation Impact Categories", ylab = "entries")

par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(ppx, las = 1, cex.names = .7,
horiz = T, main = "2017 Conservation Impact Categories", xlab = "entries")

# colored by new categories
par(mar = (c(10, 4, 2, 2) + 0.1))
barplot(ppx, las = 2, cex.names = .7,
horiz = F, main = "2017 Conservation Impact Categories", 
col = c("forestgreen", "blue", "orange", "white", "orange", "forestgreen", 
	"blue", "forestgreen", "forestgreen", "blue", "orange", "orange", 
	"blue", "blue", "blue"), ylab = "entries")
legend("topright", legend = c("Restoration", "Recreation", "Resilience"),
fill = c("orange", "forestgreen", "blue"))

par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(ppx, las = 1, cex.names = .7,
horiz = T, main = "2017 Conservation Impact Categories", 
col = c("forestgreen", "blue", "orange", "white", "orange", "forestgreen", 
	"blue", "forestgreen", "forestgreen", "blue", "orange", "orange", 
	"blue", "blue", "blue"), xlab = "entries")
legend("topright", legend = c("Restoration", "Recreation", "Resilience"),
fill = c("orange", "forestgreen", "blue"))


# categories combined
condenseCI <- rep(NA, times = 3)
names(condenseCI) <- c("Restoration", "Recreation", "Resilience")

condenseCI[1] <- ppx[3] + ppx[5] + ppx[11] + ppx[12]
condenseCI[2] <- ppx[1] + ppx[6] + ppx[8] + ppx[9]
condenseCI[3] <- ppx[2] + ppx[7] + ppx[10] + ppx[13] + ppx[14] + ppx[15]

par(mar = c(5, 4, 4, 2) + 0.1)
barplot(condenseCI, main = "2017 Conservation Impact Categories",
col = c("orange", "forestgreen", "blue"), ylab = "entries")

pie(condenseCI, main = "2017 SCA Conservation Impact", 
col = c("orange", "forestgreen", "blue"))

# bad example of output categories
names(SCAoutput)
levels(SCAoutput$Category.of.Work.You.Did)
SCAoutput$Category.of.Work.You.Did
SCAoutput$How.Much.You.Did

table(SCAoutput$How.Much.You.Did, SCAoutput$Category.of.Work.You.Did)
summarise(SCAoutput$Category.of.Work.You.Did, count = sum(How.Much.You.Did))

trail_rows
land_rows
edu_rows
water_rows

build_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Building and maintaining structures (Unit: # structures)")
summary(build_rows)

cert_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Certifications (Unit: # certifications)")
summary(cert_rows)

data_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Collecting data (Unit: # points or samples)")
summary(data_rows)

item_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Creating reports & products (Unit: # items)")
summary(item_rows)

species_rows <- filter(SCAoutput, Category.of.Work.You.Did == "Supporting native species propagation (Unit: # plants or animals)")
summary(species_rows)


condenseOutput <- rep(NA, times = 9)
names(condenseOutput) <- c("structures built", "certifications", "data points",
	"reports", "people educated", "acres land improved", "feet shore improved",
	"feet trail improved", "plants or animals" )

condenseOutput[1] <- sum(build_rows$How.Much.You.Did)
condenseOutput[2] <- sum(cert_rows$How.Much.You.Did)
condenseOutput[3] <- sum(data_rows$How.Much.You.Did)
condenseOutput[4] <- sum(item_rows$How.Much.You.Did)
condenseOutput[5] <- sum(edu_rows$How.Much.You.Did)
condenseOutput[6] <- sum(land_rows$How.Much.You.Did)
condenseOutput[7] <- sum(water_rows$How.Much.You.Did)
condenseOutput[8] <- sum(trail_rows$How.Much.You.Did)
condenseOutput[9] <- sum(species_rows$How.Much.You.Did)

par(mar = (c(10, 4, 2, 2) + 0.1))
barplot(condenseOutput, las = 3, main = "2017 SCA output")
condenseOutput[1] <- sum(build_rows$How.Much.You.Did)

#flip
par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(condenseOutput, las = 1, main = "2017 SCA output", horiz = T)
condenseOutput[1] <- sum(build_rows$How.Much.You.Did)

# try with miles of trail instead of feet
condenseOutput[8] <- condenseOutput[8]/5280
par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(condenseOutput, las = 1, main = "2017 SCA output", horiz = T)
condenseOutput[1] <- sum(build_rows$How.Much.You.Did)



# trail zoom-in
summary(trail_rows)
summary(new_trail_rows)
summary(maintain_trail_rows)
trail_struc_rows <- filter(trail_rows, Sub.Category.of.work == "Installing structures (describe below)")
step_rows <- filter(trail_rows, Sub.Category.of.work == "Installing steps or staircases")
sign_rows <- filter(trail_rows, Sub.Category.of.work == "Signage or blazing")

trail_zoom <- rep(NA, times = 5)
names(trail_zoom) <- c("new trail", "trail manintenance", "structures", "steps", "signs")

trail_zoom[1] <- sum(new_trail_rows$How.Much.You.Did)
trail_zoom[2] <- sum(maintain_trail_rows$How.Much.You.Did)
trail_zoom[3] <- sum(trail_struc_rows$How.Much.You.Did)
trail_zoom[4] <- sum(step_rows$How.Much.You.Did)
trail_zoom[5] <- sum(sign_rows$How.Much.You.Did)

barplot(trail_zoom, las = 3, main = "2017 SCA Trail Work", ylab = "Feet of Trail")

par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(trail_zoom, las = 1, horiz = T, main = "2017 SCA Trail Work", xlab = "Feet of Trail")


# species zoom-in
summary(species_rows)
tree_rows <- filter(species_rows, Sub.Category.of.work == "Planting trees")
water_tree_rows <- filter(species_rows, Sub.Category.of.work == "Watering, mulching, or maintaining plants")
non_tree_rows <- filter(species_rows, Sub.Category.of.work == "Planting vegetation (not trees)")
animal_rows <- filter(species_rows, Sub.Category.of.work == "Feed/care of animals")
seed_rows <- filter(species_rows, Sub.Category.of.work == "Collecting seed/cuttings")

species_zoom <- rep(NA, times = 5)
names(species_zoom) <- c("trees planted", "trees maintained", "non-trees planted",
	"animals cared for", "seeds collected")

species_zoom[1] <- sum(tree_rows$How.Much.You.Did)
species_zoom[2] <- sum(water_tree_rows$How.Much.You.Did)
species_zoom[3] <- sum(non_tree_rows$How.Much.You.Did)
species_zoom[4] <- sum(animal_rows$How.Much.You.Did)
species_zoom[5] <- sum(seed_rows$How.Much.You.Did)

par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(species_zoom, las = 1, horiz = T, main = "2017 SCA Species Work", xlab = "Number of animals or plants")

# item zoom-in
summary(item_rows)
other_rows <- filter(item_rows, Sub.Category.of.work == "Other (describe below)")
report_rows <- filter(item_rows, Sub.Category.of.work == "Report")
media_rows <- filter(item_rows, Sub.Category.of.work == "Media content")
outreach_rows <- filter(item_rows, Sub.Category.of.work == "Outreach material")
lesson_rows <- filter(item_rows, Sub.Category.of.work == "Lesson/Activity")
map_rows <- filter(item_rows, Sub.Category.of.work == "Map")
other_other_rows <- filter(item_rows, Sub.Category.of.work == "(Other)")

item_zoom <- rep(NA, times = 7)
names(item_zoom) <- c("other 1", "report", "media", "outreach", "lesson",
	"map", "other 2")

item_zoom[1] <- sum(other_rows$How.Much.You.Did)
item_zoom[2] <- sum(report_rows$How.Much.You.Did)
item_zoom[3] <- sum(media_rows$How.Much.You.Did)
item_zoom[4] <- sum(outreach_rows$How.Much.You.Did)
item_zoom[5] <- sum(lesson_rows$How.Much.You.Did)
item_zoom[6] <- sum(map_rows$How.Much.You.Did)
item_zoom[7] <- sum(other_other_rows$How.Much.You.Did)

barplot(item_zoom, las = 2, main = "2017 SCA products made")

par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(item_zoom, las = 1, horiz = T, main = "2017 SCA products made")

which(map_rows$How.Much.You.Did == 47520) # 44
map_rows[44,]

# people zoom-in
summary(edu_rows)
other_edu_rows <- filter(edu_rows, Sub.Category.of.work == "Other (describe below)")
other_other_edu_rows <- filter(edu_rows, Sub.Category.of.work == "(Other)")

edu_zoom <- rep(NA, times = 7)
names(edu_zoom) <-  c("while tabling", "visitors met", "volunteers coordinated",
	"on tours", "in classroom", "other 1", "other 2")

edu_zoom[1] <- sum(tabling_rows$How.Much.You.Did)
edu_zoom[2] <- sum(visitor_rows$How.Much.You.Did)
edu_zoom[3] <- sum(volunteer_rows$How.Much.You.Did)
edu_zoom[4] <- sum(tour_rows$How.Much.You.Did)
edu_zoom[5] <- sum(curriculum_rows$How.Much.You.Did)
edu_zoom[6] <- sum(other_edu_rows$How.Much.You.Did)
edu_zoom[7] <- sum(other_other_edu_rows$How.Much.You.Did)

barplot(edu_zoom, las = 3, main = "2017 People Engaged by SCA", ylab = "people")

par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(edu_zoom, las = 1, horiz = T, main = "2017 People Engaged by SCA", xlab = "people")

####### make a barplot of trails in past years
annual_trail <- c(3761.9, 1893.9, 2291.7, 1439.4, 1832, 4000)
names(annual_trail) <- c("2017", "2016", "2015", "2014", "2013", "2009")

barplot(annual_trail, ylab  = "miles", main = "Trails Maintained Annually by SCA", 
sub = "source: SCA annual reports")

cum_trail <- c(3761.9+1893.9+2291.7+1439.4+1832+4000,
			1893.9+2291.7+1439.4+1832+4000,
			2291.7+1439.4+1832+4000,
			1439.4+1832+4000,
			1832+4000,
			4000)
names(cum_trail) <- c("2017", "2016", "2015", "2014", "2013", "2009")
barplot(cum_trail, ylab  = "miles", main = "Cumulative SCA Trail Maintenance", 
sub = "source: SCA annual reports")


################## Three Rs Heatmaps, start 
SCAoutput$Conservation.Impact

for (i in 1:length(states_vec)){
states.output$restoration.per.state[i] <- (sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Coastal & marine restoration") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Endangered & threatened species") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Habitat restoration") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Water conservation"))
}

for (i in 1:length(states_vec)){
states.output$recreation.per.state[i] <- (sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Cultural preservation") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Equal access to nature") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Historic preservation") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Recreation & visitor access"))
}

for (i in 1:length(states_vec)){
states.output$resilience.per.state[i] <- (sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Business sustainability") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Building conservation awareness") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Climate Change & resiliency") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Energy") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Food systems") +
							sum(SCAoutput$Conservation.Impact[which(SCAoutput$Position..Position.State == states_vec[i])]== "Wildfire mitigation"))
}



SCA_50states(states.output, states.output$restoration.per.state, "2017 SCA Restoration Projects", "entries")

SCA_50states(states.output, states.output$recreation.per.state, "2017 SCA Recreation Projects", "entries")

SCA_50states(states.output, states.output$resilience.per.state, "2017 SCA Resilience Projects", "entries")


# binning
# binning long way

spr <- select(states.output, state, restoration.per.state)
#spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(restoration.per.state, quantile(restoration.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))

gusa_spr <- left_join(states.output, spr, "state")

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
	fifty_states_inset_boxes() + ggtitle("2017 SCA Restoration Projects") +
	scale_fill_brewer(palette = "YlOrRd", name = "Entries", 
	labels = c("Lower 20%", "Lower-Middle 20%", "Middle 20%",
			"Upper-Middle 20%", "Upper 20%", "None"))

###################################
spr <- select(states.output, state, recreation.per.state)
#spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(recreation.per.state, quantile(recreation.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))

gusa_spr <- left_join(states.output, spr, "state")

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
	fifty_states_inset_boxes() + ggtitle("2017 SCA Recreation Projects") +
	scale_fill_brewer(palette = "YlOrRd", name = "Entries", 
	labels = c("Lower 20%", "Lower-Middle 20%", "Middle 20%",
			"Upper-Middle 20%", "Upper 20%", "None"))

###################################
spr <- select(states.output, state, resilience.per.state)
#spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(resilience.per.state, quantile(resilience.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))

gusa_spr <- left_join(states.output, spr, "state")

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
	fifty_states_inset_boxes() + ggtitle("2017 SCA Resilience Projects") +
	scale_fill_brewer(palette = "YlOrRd", name = "Entries", 
	labels = c("Lower 20%", "Lower-Middle 20%", "Middle 20%",
			"Upper-Middle 20%", "Upper 20%", "None"))
