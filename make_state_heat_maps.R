########
### 6.21.18 Elizabeth Tokarz
### R maps with SCA 2017 data
######### Chicago office
#####
####

setwd( "C:/Users/elizabethtokarz/Desktop")
# read in SCA data file
SCA <- read.csv("report1529602920161_R_use.csv")
names(SCA)


summary(SCA$Position.State)
summary(SCA$Actual.Number.of.Openings)
summary(SCA$Duration.In.Weeks)
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

# new loop
for (i in 1:length(states_vec)){
states.df$count.weeks.per.member[i] <- (states.df$count.weeks.per.state[i]/states.df$count.openings.per.state[i])
}

# number of programs per state?
# new loop
for (i in 1:length(states_vec)){
states.df$count.positions[i] <- length(which(SCA$Position.State == states_vec[i]))
}

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

# now add in full names of states
#states.df$region <- c("new mexico", "alaska", "pennsylvania", "south dakota", 
#	"indiana", "north dakota", "district of colombia", "louisiana", "montana",
#	"texas", "wyoming", "florida", "california", "alabama", "arkansas", 
#	"minnesota", "colorado", "new york", "georgia", "maryland", "idaho",
#	"virginia", "washington", "maine", "oregon", "kansas", "ohio", "illinois",
#	"kentucky", "new jersey", "utah", "missouri", "massachusetts", "mississippi",
#	"west virginia", "oklahoma", "north carolina", "nebraska", "rhode island",
#	"new hampshire", "arizona", "virgin islands", "tennessee", "guam", "michigan",
#	"hawaii", "south carolina", "iowa", "wisconsin", "vermont", "delaware",
#	"american samoa", "puerto rico", "connecticut", "blank")

library("ggplot2")
library("maps")
#map('state')


#####################################################################
# setting up 50-state map
#####################################################################

# we have a preliminary heat map...

# for all fifty states
library("fiftystater")
library("mapproj")
library("scales")

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

map.df <- merge(fifty_states,states.df, by="id", all.x=T)
map.df <- map.df[order(map.df$order),]

#####################################################################
# openings per state
#####################################################################

p <- ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = count.openings.per.state), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map() 

p

p + fifty_states_inset_boxes() + ggtitle("Openings in 2017") 

#####################################################################
# weeks working in each state
#####################################################################

p1 <- ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = count.weeks.per.state), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

p1 + fifty_states_inset_boxes() + ggtitle("SCA weeks in 2017")


# trying different colors here

ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = count.weeks.per.state), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradientn(colours=(topo.colors(3)),na.value="grey90")+
  coord_map()

ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = count.weeks.per.state), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
scale_fill_gradientn(colours=(rainbow(10, start = .13, end = 1)),na.value="grey40") +
guides(fill = guide_colorbar(direction = "horizontal", title = "miles", barwidth = 15,
label.theme = element_text(angle = 0)))+
 coord_map()


rainbow(4, start = 0, end = 0.25)[z])
# slightly alter rainbow palette
rainbow <- rainbow(n, s = 1, v = 1, start = 1/8, end = max(1, n - 1)/n, alpha = 1)

# Color options so far
scale_fill_gradient(low = "yellow", high = "green", na.value = "grey90") --a two color scale...
scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90") --heat colors in reverse...
 scale_fill_gradientn(colours=palette(),na.value="grey90")



library(colorspace)
library(RColorBrewer)
scale_colour_gradient(low = "white", high = "black")

#####################################################################
# weeks per member per state
#####################################################################

p2 <- ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = count.weeks.per.member), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

p2 + fifty_states_inset_boxes() + ggtitle("SCA weeks per member in 2017")
# colors seem most even here because the range is narrower

head(SCA)

#####################################################################
# positions per state
#####################################################################

p3 <- ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = count.positions), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

p3 + fifty_states_inset_boxes() + ggtitle("SCA position distribution 2017")

#####################################################################
# internships per state
#####################################################################

p4 <- ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = intern), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

p4 + fifty_states_inset_boxes() + ggtitle("SCA internship distribution 2017")

#####################################################################
# crew openings per state
#####################################################################

p5 <- ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = crew), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

p5 + fifty_states_inset_boxes() + ggtitle("SCA crew distribution 2017")

##########################################################################################################################################
# corps openings per state
#####################################################################

p6 <- ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = corps), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

p6 + fifty_states_inset_boxes() + ggtitle("SCA corps distribution 2017")

##########################################################################################################################################
# community openings per state  ##this one looks off
#####################################################################

p7 <- ggplot(map.df, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = corps), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

p7 + fifty_states_inset_boxes() + ggtitle("SCA community distribution 2017")

#####################################################################
# regional focus
#####################################################################

# try looking at regions individually:
# only does lower 48...
states <- map_data("state")

map48.df <- merge(states,states.df, by="region", all.x=T)
map48.df <- map.df[order(map48.df$order),]
ggplot(map48.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=count.openings.per.state))+
  geom_path() + 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()


# usmap package soln--not in ggplot...
library(usmap)
# example with population in west
plot_usmap(
    data = statepop, values = "pop_2015", include = c("CA", "ID", "NV", "OR", "WA"), lines = "red"
  ) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + 
  labs(title = "Western US States", subtitle = "These are the states in the Pacific Timezone.") +
  theme(legend.position = "right")

states.df$state <- states.df$id

#Northeast
plot_usmap(
    data = states.df, values = "count.openings.per.state", 
	include = c("ME", "VT", "NH", "MA", "RI", "CT", "NY", "NJ", "PA"), lines = "green"
  ) + 
  scale_fill_continuous(
    low = "white", high = "green", name = "SCA Openings (2017)", label = scales::comma
  ) + 
  labs(title = "Northeastern US States") +
  theme(legend.position = "right")


# Southeast
plot_usmap(
    data = states.df, values = "count.openings.per.state", 
	include = c("MD", "DE", "VA", "WV", "KY", "TN", "NC", "SC", "GA"), lines = "darkgreen"
  ) + 
  scale_fill_continuous(
    low = "white", high = "darkgreen", name = "SCA Openings (2017)", label = scales::comma
  ) + 
  labs(title = "Southeastern US States") +
  theme(legend.position = "right")


# Midwest
plot_usmap(
    data = states.df, values = "count.openings.per.state", 
	include = c("OH", "MI", "IN", "IL", "WI", "MN", "IA", "ND", 
	"SD", "NE", "KS", "OK", "AR", "MO"), lines = "salmon"
  ) + 
  scale_fill_continuous(
    low = "white", high = "salmon", name = "SCA Openings (2017)", label = scales::comma
  ) + 
  labs(title = "Midwestern US States") +
  theme(legend.position = "right")



# ggplot version### not working...
library(ggplot2)
library(maps)
states_MW <- subset(map.df, id %in% c( "illinois", "indiana", "iowa", "kansas", "arkansas",
	 "michigan", "minnesota","missouri", "north dakota", "ohio", "south dakota", "wisconsin", 
	"nebraska", "oklahoma") )


# this works to show the states basically, but not color them in.
states <- subset(all_states, region %in% c( "illinois", "indiana", "iowa", "kansas", "arkansas",
	 "michigan", "minnesota","missouri", "north dakota", "ohio", "south dakota", "wisconsin", 
	"nebraska", "oklahoma") )
p <- ggplot()
p <- p + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white" )
# data specific here for points...can show project sites?
p <- p + geom_point( data=mydata, aes(x=long, y=lat, size = enrollment), color="coral1") + scale_size(name="Total enrollment")
p <- p + geom_text( data=mydata, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=label), colour="gold2", size=4 )
p


#################################################### APPENDIX
06.21.18
### raster soln to looking at regions one at a time
#library(raster)
#states2    <- c('California', 'Nevada', 'Utah', 'Colorado', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington')
#provinces <- c("British Columbia", "Alberta")

#us <- getData("GADM",country="USA",level=1)
#canada <- getData("GADM",country="CAN",level=1)

#us.states <- us[us$NAME_1 %in% states2,]
#ca.provinces <- canada[canada$NAME_1 %in% provinces,]

#us.bbox <- bbox(us.states)
#ca.bbox <- bbox(ca.provinces)
#xlim <- c(min(us.bbox[1,1],ca.bbox[1,1]),max(us.bbox[1,2],ca.bbox[1,2]))
#ylim <- c(min(us.bbox[2,1],ca.bbox[2,1]),max(us.bbox[2,2],ca.bbox[2,2]))
#plot(us.states, xlim=xlim, ylim=ylim)
#plot(ca.provinces, xlim=xlim, ylim=ylim, add=T)

#library(ggplot2)
#ggplot(us.states,aes(x=long,y=lat,group=group))+
#  geom_path()+
#  geom_path(data=ca.provinces)+
#  coord_map()
