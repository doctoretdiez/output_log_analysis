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


#####################################################################
# make sure the map functions have been loaded
#####################################################################

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

#####################################################################
# openings per state
#####################################################################

SCA_50states(map.df, map.df$count.openings.per.state, title = "SCA Openings 2017", label = "Openings")

##SCA_50states_20percent_quantile(states.df, "count.openings.per.state", title = "SCA Openings 2017", label = "Openings")

Northeast(states.df, "count.openings.per.state", title_ne = "Openings in 2017", label_ne = "Openings")

############### in quintiles instead of a continuous scale
# binning long way

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



# alternative representation
# centroids with proportional circles
state_centroids <- summarize(group_by(gusa, region),
                             x = mean(range(long)), y = mean(range(lat)))
names(state_centroids)[1] <- "state"
head(state_centroids)
head(states.df)

map("state")
#with(state_centroids, points(x, y))

states.df <- left_join(states.df, state_centroids, "state")

with(states.df,
     symbols(x, y,
             circles = sqrt(count.openings.per.state), add = TRUE,
             inches = 0.1, bg = "black"))


# ggplot version of centroids with proportional circles
library("ggplot2")
gusa <- map_data("state")

ggplot(gusa) +
    geom_polygon(aes(long, lat, group = group),
                 fill = NA, color = "grey") +
    geom_point(aes(x, y, size = count.openings.per.state), data = states.df) +
    scale_size_area() +
    coord_map("bonne", parameters=45) 

##########################################
# quintiles with non ggplot***
map("state")$names
# default # par(mar = c(5, 4, 4, 2) + 0.1)
usa_pcls <- spr$pcls[match(map("state")$names, spr$state)]
pal <- RColorBrewer::brewer.pal(nlevels(usa_pcls), "Reds")
map("state", fill = TRUE, col = pal[usa_pcls], border = "grey", mar = c(5, 4.1, 7, 0.1))
# make legend on its own!
map("state", fill = T, col = "white", border = "white")
legend("center", inset=c(0,-0.2), legend = unique(spr$pcls[order(spr$pcls)]), fill = pal, ncol = 3)
###### can change the color scheme based on how many ncls!!!

# how to see only one state while in ggplot?
ggplot(filter(region == "illinois")) + coord_map()
# https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/maps.html

################# statebins!
library("statebins")
states.df_edit <- slice(states.df, c(1:6, 8:41, 43, 45:51, 54))
states.df_edit$state <- as.character(states.df_edit$states_vec)
statebins_continuous(states.df_edit, value_col = "count.openings.per.state")

# now in quartiles...
statebin.df <- select(states.df, states_vec, count.openings.per.state)
statebin.df <- slice(statebin.df, 1:54)
ncls <- 4
statebin.df <- mutate(statebin.df,
              pcls = cut(count.openings.per.state, quantile(count.openings.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))
statebin.df$rank <- as.numeric(statebin.df$pcls)
statebins(statebin.df, value_col = statebin.df$rank)
###########unsure why this is not working...


#####################################################################
# weeks working in each state
#####################################################################

SCA_50states(map.df, map.df$count.weeks.per.state, "SCA Weeks 2017", "weeks")

Pacific(states.df, "count.weeks.per.state", title_ne = "SCA weeks in 2017", label_ne = "Weeks")

#####################################################################
# weeks per member per state
#####################################################################
SCA_50states(map.df, map.df$count.weeks.per.member, "SCA Weeks per member in 2017", "weeks per member")

#####################################################################
# positions per state
#####################################################################
SCA_50states(map.df, map.df$count.positions, "SCA position distribution 2017", "Positions") 

#####################################################################
# internships per state
#####################################################################
SCA_50states(map.df, map.df$intern, "SCA internship distribution 2017", "Positions")

spr <- select(states.df, state, intern)
spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(intern, quantile(intern, seq(0, 1, len = ncls)),
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
	fifty_states_inset_boxes() + ggtitle("2017 SCA Internship Distribution") +
	scale_fill_brewer(palette = "YlOrRd", name = "Openings"), 
	labels = c("Lower 20%", "Lower-Middle 20%", "Middle 20%",
			"Upper-Middle 20%", "Upper 20%", "None"))


#####################################################################
# crew openings per state
#####################################################################
SCA_50states(map.df, map.df$crew, "SCA crew distribution 2017", "Positions")

##########################################################################################################################################
# corps openings per state
#####################################################################
SCA_50states(map.df, map.df$corps, "SCA corps distribution 2017", "Positions")

##########################################################################################################################################
# community openings per state  
#####################################################################
SCA_50states(map.df, map.df$community, "SCA community distribution 2017", "Positions")

#####################################################################
# 
#####################################################################

# try looking at regions individually:
# only does lower 48...
states <- map_data("state")

states.df$region <- c("new mexico", "alaska", "pennsylvania", "south dakota", 
	"indiana", "north dakota", "district of colombia", "louisiana", "montana",
	"texas", "wyoming", "florida", "california", "alabama", "arkansas", 
	"minnesota", "colorado", "new york", "georgia", "maryland", "idaho",
	"virginia", "washington", "maine", "oregon", "kansas", "ohio", "illinois",
	"kentucky", "new jersey", "utah", "missouri", "massachusetts", "mississippi",
	"west virginia", "oklahoma", "north carolina", "nebraska", "rhode island",
	"new hampshire", "arizona", "virgin islands", "tennessee", "guam", "michigan",
	"hawaii", "south carolina", "iowa", "wisconsin", "vermont", "delaware",
	"american samoa", "puerto rico", "connecticut", "blank")

map48.df <- merge(states,states.df, by="region", all.x=T)
map48.df <- map.df[order(map48.df$order),]
ggplot(map48.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=count.openings.per.state))+
  geom_path() + 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()



## using quintile bins instead of a continuous scale
spr <- select(states.df, region, count.openings.per.state)
spr <- slice(spr, 1:54)
ncls <- 6
spr <- mutate(spr,
              pcls = cut(count.openings.per.state, quantile(count.openings.per.state, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))

gusa_spr <- left_join(states.df, spr, "region")

# with non ggplot
map("state")$names
usa_pcls <- spr$pcls[match(map("state")$names, spr$region)]
pal <- RColorBrewer::brewer.pal(nlevels(usa_pcls), "Reds")
map("state", fill = TRUE, col = pal[usa_pcls], border = "grey")

#using statebins! 7.2.18
data(USArrests)
USArrests$state <- rownames(USArrests)
statebins_continuous(USArrests, value_col="Murder", text_color="black", font_size=3,
                     legend_title = "Murder", legend_position="bottom")

usmap package soln--not in ggplot...
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


#library(ggplot2)
#ggplot(us.states,aes(x=long,y=lat,group=group))+
#  geom_path()+
#  geom_path(data=ca.provinces)+
#  coord_map()
