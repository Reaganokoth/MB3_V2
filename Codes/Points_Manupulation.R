library(sf)
library(sp)
library(rgdal)
library(ggplot2)
library(stringr)
library(dplyr)

points <- readOGR("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/field Mesurements/Vector data/Survey_points_first_route.shp")
points$notes
# rename the plot ids to have all capital F and capital P
xx <- str_replace(string = plotNames,pattern = "F1p1",replacement ="F1P1" )

x <- grep(x = plotNames2, pattern = "(([F])([1-2]{1}))", value = T)
points$notes <- xx

# Remove unecessary points
points_clean <- points[-c(1,28), ]
points_clean$notes

comments <- read.csv("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/field_Mesurements/Comments.csv")


# change the names of tyhe plot id to be similar in both datasets
# important to make a spatial joint
names(points_clean)[3] <- names(comments)[1]





Percentage <- list()

  
# compute dominant species percentage for each plot  
for(i in 1:nrow(comments)){
  Percentage[[i]] <- ((max(comments[i, ][-c(1, ncol(comments))]))/(sum(comments[i, ][-c(1, ncol(comments))])))*100
}  


  # find the dominant species for each plot
Dominant_species <- colnames(comments)[apply(comments,1, which.max)]

# group the data into forest type based on the dominant tree spss
forest_type <- list()
for(i in 1:length(Dominant_species)){
  if(Dominant_species[i] %in% c("Oak","Beech","Esche","Eiche","Marple")){
    forest_type[[i]] <- "Broad_leaf"
  } else {
    if(Dominant_species[i] %in% c("Dead","Logged")){
      forest_type[[i]] <- "Dead"
    } else{
      forest_type[[i]] <- "Needle_leaf"
    }
  }
}



# Add the data to the bigger comments dataframe.

comments_complete <- comments %>% mutate(Dominant_Species=Dominant_species, 
                    Dominant_Species_Percentage=unlist(Percentage),
                    Forest_type=unlist(forest_type),.before="Notes")

merged <- merge(points_clean, comments_complete, by = "plot_ID")


writeOGR(merged,dsn ="/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/field Mesurements",layer = "Merged3",driver = "ESRI Shapefile")

##################################
# re-project the merged points and create square polygons around


merged_projected <- spTransform(merged, CRS("+init=epsg:25832"))



### MAKE A 11 M RECTANGUILAR BUFFER AROUND EACH POINT ##########################


# create a 30m square polygon around each point
#define radius
radius <- 11
geom(merged_projected)
# get the coordinates information for the points and add it as a column 
#in the spatialPointDataframe

cordinates <- geom(merged_projected) %>% 
  as.data.frame() %>% 
  rename(Lon=x, Lat=y)

merged_projected_withCord <- cbind(cordinates[2:3],merged_projected)
cordinates[2:3]
merged_projected_withCord$Lat

x <- merged_projected
xx <- cbind(merged_projected,cordinates)

xx$fid
merged$photo
# define the plot edges based upon the plot radius. 

yMax <- merged_projected_withCord$Lat + radius
xMax <- merged_projected_withCord$Lon + radius
yMin <- merged_projected_withCord$Lat - radius
xMin <- merged_projected_withCord$Lon - radius

# calculate polygon coordinates for each plot centroid. 
square= as.data.frame(cbind(xMin,yMax,  # NW corner
                            xMax, yMax,  # NE corner
                            xMax,yMin,  # SE corner
                            xMin,yMin, # SW corner
                            xMin,yMax)) # NW corner again - close ploygon)

# make the square object a double class from class list 
square_Double <- coordinates(square)
typeof(square_Double)


##########################################
# create a polygon from square vertex
# Convert the polygons to spatial polygon dataframe
# and write it in the loca;l directory as a shapefile
######################################

polys <- SpatialPolygons(mapply(function(poly, id){
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square_Double, row(square_Double)), merged_projected_withCord$fid),proj4string=CRS(as.character(crs(merged_projected))))

polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=merged_projected_withCord$fid, row.names=merged_projected_withCord$fid))

# Join the original data fields to the created spatialPolygonDataframe
# i.e rejoin to have the tree metadata
polys.df_withData <- cbind(polys.df,as.data.frame (merged_projected[-c(2:4)]))
# save the data in a local directory
rgdal::writeOGR(polys.df_withData, dsn = "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/field_Mesurements/MB3/Data/Data", layer = 'Points_as_Polygon', driver = 'ESRI Shapefile',)
