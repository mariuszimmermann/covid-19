#Import the data
data = read.csv("") # Data file

#Get a glimpse at the data
head(data)

# Install the relevant libraries - do this one time
install.packages("leaflet")
install.packages("viridis")
install.packages("plotly")
install.packages("rworldmap")
install.packages("maps")
install.packages("dplyr")
install.packages("ggmap")
install.packages("mapproj")

# Load the relevant libraries - do this every time
library(leaflet)
library(viridis)
library(dplyr)
library(plotly)
library(maps)
library(rworldmap)
library(ggmap)
library(mapproj)

data$Province_State = lapply(data$Province_State, as.character)
str(data$Province_State)
View(data)
data$Country_Region = lapply(data$Country_Region, as.character)

#Separate relevant information into variables
country = data$Country_Region
states = data$Province_State

#Static map with ggplot
world  <- map_data("world")

g<-data %>%
  arrange(Confirmed) %>%
  mutate( name=factor(data$Country_Region, unique(data$Country_Region))) %>%
  ggplot() +
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_point( aes(x=data$Long_, y=data$Lat, size=Confirmed, color=Confirmed), alpha=0.4) +
    scale_size_continuous(range=c(1,10)) +scale_color_viridis (trans="log") +
    ggtitle("Coronavirus outbreak until 26 June 2020")  + theme(plot.title =element_text(size = 10, face = "bold"), legend.title = element_text(size = 15), legend.text = element_text(size = 10))+ coord_map()
#Show g
g


#Create a new text field for interactive map labels
data$New_R <- ifelse(data[,3]== "", data[,4], data[,3])

dataN <- data %>%
  mutate(mytext=paste("Country: ", New_R, "\n", "Confirmed: ", Confirmed, sep=""))


#Create new plot with new colors
i <- dataN %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(x=data$Long_, y=data$Lat, size=Confirmed, color=Confirmed, text=mytext, alpha=0.5) ) +
  scale_size_continuous(range=c(1,15)) +
  scale_color_gradient2(low = "white", mid = "yellow", high = "red", trans="sqrt", midpoint = 100 ) +
  scale_alpha_continuous(trans="log") +
  theme_void() +
  coord_map() +
  theme(legend.position = "none")

#Create the interactive map
i <- ggplotly(i, tooltip="text")

#Display the map
i
