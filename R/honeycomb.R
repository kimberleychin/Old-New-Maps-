library(sp)
library(maptools)
library(rgdal)
library(reshape2)
library(maptools)
library(leaflet)
library(deldir)
#install.packages("colorspace")
library(colorspace)
#install.packages("viridis")
library(viridis)
library(dismo)
#install.packages("deldir", dependencies=TRUE)
#install.packages(c("maps", "mapproj"), dependencies=TRUE)
library(readxl)
#iconv(change$Name.x, "UTF-8","latin1")
library(grDevices)
library(RColorBrewer)

idQuest<-16
nHex<-3000
oldans <- readRDS("OldNewMaps/input/oldans")
#selecting only points in german speaking europe
#germanCount2 <- readShapePoly("OldNewMaps/input/geom/germanCountries.shp")
# proj4string(germanCount)<-CRS("+init=epsg:4326")
germanCount <- readShapePoly("OldNewMaps/input/geom/merged_italygermany.shp")
proj4string(germanCount)<-CRS("+init=epsg:4326")

ansKey <- read_excel("OldNewMaps/input/oldNewAnswers.xls")
#saveRDS(oldans,"OldNewMaps/input/oldans" )
voro<- readRDS("OldNewMaps/input/vorotess")

#setwd("C:/Users/cderungs/Dropbox/OldNewMaps/")

load(paste("allQuest/questSPDF",idQuest,".Rdata",sep=""))
q <- all.answ.eu


cl.grid<-spsample(germanCount,nHex,"hexagonal")
cl.hex<-HexPoints2SpatialPolygons(cl.grid)
cl.over<-over(q,cl.hex)
cl.over.df<-data.frame(hexID=cl.over, ausID=q$AntwortID)
cl.over.wide <- dcast(cl.over.df, hexID ~ ausID, fun.aggregate=length)

cl.count<-cl.over.wide[,-1]
cl.rel<-cl.count/rowSums(cl.count)

maxLevel<-names(cl.count)[apply(cl.count,1,function(x) which.max(x))]

names(cl.rel)<-paste("rel_",names(cl.rel),sep="")
cl.rel$relMax<-apply(cl.rel,1,max,na.rm=TRUE)
cl.rel$absMax<-apply(cl.count,1,max,na.rm=TRUE)
cl.rel$maxLevel<-maxLevel
cl.rel$total <- rowSums(cl.count)

cl.over.wide<-cbind(cl.over.wide,cl.rel)
cl.over.wide$id<-paste("ID",cl.over.wide$hexID,sep="")

cl.complete<-data.frame(id=sapply(cl.hex@polygons,function(x) x@ID))

cl.complete<-merge(cl.complete,cl.over.wide,by="id",all.x=T)
row.names(cl.complete)<-cl.complete$id

cl.hex.spdf<-SpatialPolygonsDataFrame(cl.hex,cl.complete,match.ID=T)

#writeOGR(cl.hex.spdf, "OldNewMaps/honeycomb/4", paste("honeyComb_",2), driver="ESRI Shapefile")
#hc<- readOGR("OldNewMaps/honeycomb/3")

frage <- paste0("Frage_", idQuest)
change <- paste0("q",idQuest,"_change")
#change <-"agg_change"
q_ansKey <- ansKey[ansKey$frageId ==idQuest,]
# q_ansKey$oldnew <- "New"
# q_ansKey[q_ansKey$id> 1000, "oldnew"] <- "Old"
q_ansKey$ID <- seq(q_ansKey$id)
#map<-oldans
map <- oldans[oldans@data[,frage] != 0 & oldans@data[,frage]!=9999 , ]
#map <- oldans[oldans@data[,frage] != 0 & oldans@data[,frage] <1000 , ]
map <- merge(map,q_ansKey ,by.x= frage, by.y= "id")
names(map@data)[ncol(map@data)]<-"ID"
map@data[is.na(map@data)] <- 1
# map_o <- oldans[oldans@data[,frage]>1000 & oldans@data[,frage]!=9999,]
# map_o <- merge(map_o,q_ansKey ,by.x= frage, by.y= "id")
df <-as.data.frame(q_ansKey[,c("id","antwort")])
#join with ans key
cl.hex.spdf@data <- data.frame(cl.hex.spdf@data, df[match(as.numeric(cl.hex.spdf@data[,"maxLevel"]),df[,"id"]),])

#oldans@data$Frage_6[oldans@data$Frage_6 ==1057] <-48 

#remove NA
cl_noNA <- cl.hex.spdf[complete.cases(cl.hex.spdf@data[ , "maxLevel"]),]
#omit those less than 50 people
cl_noNA <- cl_noNA[cl_noNA@data$total>20,]
cl_noNA <- merge(cl_noNA, q_ansKey, by ="antwort" )
#map <- merge(map, q_ansKey[,c("ID", "id")], by ="antwort" )


changedata <- over(voro, map[,change])
voro$change <-changedata[,change]
voro$change[is.na(voro$change)] <- 0
voro$change <- voro$change*100

pal <- colorFactor(
  plasma(length(q_ansKey$antwort)),
  as.factor(q_ansKey$ID))
#[q_ansKey$id <1000]
#, na.color = "white"
# pal <- colorFactor(
#   inferno(length(ansKey$id[ansKey$frageId== idQuest][ansKey$id[ansKey$frageId== idQuest] <1000])),
#   ansKey$id[ansKey$frageId== idQuest][ansKey$id[ansKey$frageId== idQuest] <1000], na.color = "white")
# pal2 <- colorQuantile("Greens",
#                  voro$change, n = 5)
pal2 <- colorBin("Greens",
                 voro$change, n = 5)


contenthoneycomb <- paste("<b>","Dominant Variable: ",cl_noNA$antwort ,"</b>","<br/>",
                 "Number of people: ",cl_noNA@data$total,"<br/>",
                 "Degree of dominance: ", round(cl_noNA@data$relMax,2)*100,"%")

# content_agg <- paste("<b>","Aggregate change: ","</b>",round(map$agg_change,4)*100 ,"%")
                          
#ansKey[ansKey$id %in% as.integer(cl.hex.spdf$maxLevel), "antwort"]
#cl.hex.spdf$maxLevel

# content_oldpts1000 <- paste("<b>","Dominant Variable: ", map_o$antwort,"</b>","<br/>",
#                           "Degree of change: ", round(map_o@data[,change],2)*100,"%","<br/>")


content_oldpts <- paste("<b>", map@data$Name,"<br/>","Dominant Variable: ", map$antwort, "</b>","<br/>",
                            "Degree of change: ", round(map@data[,change],2)*100,"%","<br/>"
                        )




m2 <- leaflet() %>% 
  # Base groups
  addProviderTiles("Stamen.TonerLite", group = "Basemap") %>%
  addPolygons(data=voro, stroke = FALSE, fillColor = ~pal2(voro$change), fillOpacity = 0.75,
             group = "Change")%>%
  # popup=paste(content_agg)
  addPolygons(data= cl.hex.spdf, fillOpacity = 0, weight =0.3, group = "New Answers")%>%
  addPolygons(data = cl_noNA, fillColor = ~pal(cl_noNA$ID),
              fillOpacity= (cl_noNA$relMax*0.9),
              stroke = FALSE, color="black", weight =1,
              popup=paste(contenthoneycomb), group = "New Answers")%>%
  addCircleMarkers(data = map, stroke= TRUE, weight =1.5,color = "black", fillColor= ~pal(map@data$ID),
                   fillOpacity = 0.75, radius = (4+(map@data[,change]*8)),
                   popup= paste(content_oldpts), group = "Old Answers") %>%
  hideGroup("Change") %>%
  addLegend("bottomleft", pal = pal, values = q_ansKey$ID,
            labFormat = labelFormat(suffix = paste(": ",q_ansKey$antwort)) , #[ansKey$id==sort(unique(as.numeric(cl.pts$maxLevel)))]
            opacity=0.85, title = "Variants") %>%
  addLegend("bottomright", pal = pal2, values = voro$change,
            opacity=0.75, title = "% Change",
            labFormat = labelFormat(suffix= "%")
            )%>%
  addLayersControl(
    
    overlayGroups = c("Basemap","Old Answers", "New Answers", "Change"),
    options = layersControlOptions(collapsed = FALSE))

# Layers control
#addLayersControl(
# baseGroups = c("Terrain","Satellite"),
#overlayGroups = c("hex","Ungulates"),
#options = layersControlOptions(collapsed = TRUE)
#)
m2

