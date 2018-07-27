########
### 7.27.18 Elizabeth Tokarz
### R graphics with SCA 2017 data
######### Chicago office
#####
####

## There are four main sections.
# I. Mapping openings from SCA position file.
# II. Mapping trail output from SCA cleaned output file.
# III. Creating graphs of Conservation Impact.
# IV. Making "Three R" Heat Maps.
#
# But before these sections begin, some mapping code needs to be run.
# Here it is directly below the several lines of pound signs.

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
library(dplyr)
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


####### This function will help create the discrete 20 percentile maps
SCA_50states_20percent_quantile_map <- function(dataset, state_pcls, title, label, pals = "YlOrRd"){

gusa_quant <- left_join(dataset, state_pcls, "state")

ggplot(gusa_quant, aes(map_id = id)) + 
  geom_map(aes(fill = pcls), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +
	fifty_states_inset_boxes() + ggtitle(title) +
	scale_fill_brewer(palette = pals, name = label, 
	labels = c("Lower 20%", "Lower-Middle 20%", "Middle 20%",
			"Upper-Middle 20%", "Upper 20%", "None"))

}

##############################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
######### Now that the packages and map functions have been loaded, 
# we can load the data

# I. First let's load the Positions document to make maps for openings in 2017

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

################ Now make heat maps
############ Openings per state
SCA_50states(map.df, map.df$count.openings.per.state, title = "SCA Openings 2017", label = "Openings")

Northeast(states.df, "count.openings.per.state", title_ne = "Openings in 2017", label_ne = "Openings")

# Discrete version Openings Map ########### Run all the below
spr <- select(states.df, state, count.openings.per.state)
spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(count.openings.per.state, quantile(count.openings.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))
SCA_50states_20percent_quantile_map(states.output, spr, "SCA Openings 2017", "Openings")


############# Weeks worked in each state

SCA_50states(map.df, map.df$count.weeks.per.state, "SCA Weeks 2017", "weeks")

Pacific(states.df, "count.weeks.per.state", title_ne = "SCA weeks in 2017", label_ne = "Weeks")

#########
####### II. Second let's load the Output document to make maps for 2017
#########
# read in SCA data file
SCAoutput <- read.csv("Cleaned_Melissa_outputs2017.csv")
names(SCAoutput)
summary(SCAoutput)

# make some new vectors to organize our data
states_vec <- unique(SCAoutput$Position..Position.State)

# make this into a new dataframe where we will look at different outputs by state:
states.output <- data.frame(states_vec)

####### NOTE that the opening file is states.df and the output file is states.output
# now fill in the states output data frame


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


#### Preparing the output data for use in maps is slightly different because
# the order of states differs
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

###### New trail
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

SCA_50states_color(map.output, map.output$non.new.trail.per.state, 
"Trail Maintenance in 2017", "Miles", "white", "sienna")

SCA_50states(map.output, map.output$non.new.trail.per.state, 
"Trail Maintenance in 2017", "Miles")

Southwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Northwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Pacific(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Mountains(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
South(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Southeast(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Midwest(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")
Northeast(states.output, "maintain.trail.per.state", title_ne = "Miles of Trail Maintained in 2017", label_ne = "Miles")

### Discrete Maintained Trail Map########################################
spr <- select(states.output, state, non.new.trail.per.state)
spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(non.new.trail.per.state, quantile(non.new.trail.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))
SCA_50states_20percent_quantile_map(states.output, spr, "2017 SCA Trail Maintained", "Miles")

########## III. Third, Look at the Conservation Impact
# also in the SCAoutput data frame
#####################################################################

ppx <- table(SCAoutput$Conservation.Impact)
ppx <- ppx[rev(order(ppx))]
names(ppx)[4] <- "blank"

# colorless bar graph
par(mar = (c(4, 10, 2, 2) + 0.1))
barplot(ppx, las = 1, cex.names = .7,
horiz = T, main = "2017 Conservation Impact Categories", xlab = "entries")

# colored bar graph
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

# condensed Three R bar graph
par(mar = c(5, 4, 4, 2) + 0.1)
barplot(condenseCI, main = "2017 Conservation Impact Categories",
col = c("orange", "forestgreen", "blue"), ylab = "entries")

# Three R pie chart
pie(condenseCI, main = "2017 SCA Conservation Impact", 
col = c("orange", "forestgreen", "blue"))

### IV. Now prepare this three R data for use in maps
# 
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

########### Heat maps 
### Restoration
#continuous
SCA_50states(states.output, states.output$restoration.per.state, "2017 SCA Restoration Projects", "entries")

# Discrete
# The map might be cut off by the size of the window in R, 
# but the window can be widened manually to reveal the full map and legend.
spr1 <- select(states.output, state, restoration.per.state)
ncls <- 6
spr1 <- mutate(spr1,
              pcls = cut(restoration.per.state, quantile(restoration.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))
SCA_50states_20percent_quantile_map(states.output, spr1, "SCA Restoration Projects 2017", "entries")

### Recreation
#continuous
SCA_50states(states.output, states.output$recreation.per.state, "2017 SCA Recreation Projects", "entries")

# Discrete
spr2 <- select(states.output, state, recreation.per.state)
ncls <- 6
spr2 <- mutate(spr2,
              pcls = cut(recreation.per.state, quantile(recreation.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))
SCA_50states_20percent_quantile_map(states.output, spr2, "SCA Recreation Projects 2017", "entries")

### Resilience
#continuous
SCA_50states(states.output, states.output$resilience.per.state, "2017 SCA Resilience Projects", "entries")

# Discrete
spr3 <- select(states.output, state, resilience.per.state)
ncls <- 6
spr3 <- mutate(spr3,
              pcls = cut(resilience.per.state, quantile(resilience.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))
SCA_50states_20percent_quantile_map(states.output, spr3, "SCA Resilience Projects 2017", "entries")