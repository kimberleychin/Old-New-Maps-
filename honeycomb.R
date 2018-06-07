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
library(htmlwidgets)

##list of question numbers 
quest.list<- c(1,2,4,5,6,7,8,9,11,14,16,18,21,23)

##list of age groups for analysis
age.grouplist<- c("all","a_20","a_2140","a_2150","a_4160","a_m50","a_m60")

## threshold for number of participants of particular agegroup when collecting new answers from the nearest 100 neighbours from new answers to be valid

threshold.old <- 20

## threshold for number of participants of particular agegroup when collecting nearest from the nearest 100 neighbours to be valid 
threshold.hc <-20
for (q.no in quest.list){
	
for (ag in age.grouplist){
  
    idQuest<-q.no
    nHex<-3000
    agegroup<-ag
    qQuest <-paste0("q",idQuest)
    aggchange <-paste0("aggchange_",ag)
    oldans <- readRDS("OldNewMaps/input/all.location.answers")
    chnage_byage<-readRDS("all_q_change_byage")
    chnage_byage<-chnage_byage[chnage_byage$q==qQuest,]
    
    oldans<- merge(oldans, chnage_byage, by = "ID")
    change_age<-paste0("change",agegroup)
    freq_age<-paste0("freq",agegroup)
    chnage_byage<-chnage_byage[,c("ID", change_age, freq_age)]
    #selecting only points in german speaking europe

    germanCount <- readShapePoly("OldNewMaps/input/geom/totalmerged.shp")
    proj4string(germanCount)<-CRS("+init=epsg:4326")
    ## codes for all old and new answers   
    ansKey <- read_excel("OldNewMaps/input/oldNewAnswers.xls")
  ## voronoi tesselation of german speaking areas
    voro<- readRDS("OldNewMaps/input/vorotess")
    
 
 # load new answers
    load(paste("allQuest/questSPDF",idQuest,".Rdata",sep=""))
    
    # select age group 
    if (agegroup=="all"){
      q<-all.answ.eu
    }
    if (agegroup=="a_20"){
      q<-all.answ.eu[which(all.answ.eu$Wiealt<21  & all.answ.eu$Wiealt>=0),]
    }
    if (agegroup=="a_2140"){
      q<-all.answ.eu[which(all.answ.eu$Wiealt<41  & all.answ.eu$Wiealt>=21),]
    }
    if (agegroup=="a_2150"){
      q<-all.answ.eu[which(all.answ.eu$Wiealt<51  & all.answ.eu$Wiealt>=21),]
    }
    if (agegroup=="a_4160"){
      q<-all.answ.eu[which(all.answ.eu$Wiealt<60  & all.answ.eu$Wiealt>=41),]
    }
    if (agegroup=="a_m50"){
      q<-all.answ.eu[which(all.answ.eu$Wiealt<9999  & all.answ.eu$Wiealt>=50),]
    }
    if (agegroup=="a_m60"){
      q<-all.answ.eu[which(all.answ.eu$Wiealt<9999  & all.answ.eu$Wiealt>=60),]
    }
    
    #honeycomb and extract answer distributions in each
    cl.grid<-spsample(germanCount,nHex,"hexagonal")
    cl.hex<-HexPoints2SpatialPolygons(cl.grid)
    cl.over<-over(q,cl.hex)
    cl.over.df<-data.frame(hexID=cl.over, ausID=q$AntwortID)
    cl.over.wide <- dcast(cl.over.df, hexID ~ ausID, fun.aggregate=length)
    
    cl.count<-cl.over.wide[,-1]
    cl.rel<-cl.count/rowSums(cl.count)
    # dominant variant of particular question
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
    
    
    frage <- paste0("Frage_", idQuest)
    change <- paste0("q",idQuest,"_change")
    #for aggreagate change calculation
    #change <-"agg_change"
    
    q_ansKey <- ansKey[ansKey$frageId ==idQuest,]
    q_ansKey$ID <- seq(q_ansKey$id)

#selecting points that have answers (not NAs && have sufficient new answers in the same location)
    map <- oldans[oldans@data[,frage] != 0 & oldans@data[,frage]!=9999 , ]
    map<-map[map@data[,freq_age]>threshold.old,]

#merging answer ids wih actual answers    
    map <- merge(map,q_ansKey ,by.x= frage, by.y= "id")
    names(map@data)[ncol(map@data)]<-"answer_ID"
    df <-as.data.frame(q_ansKey[,c("id","antwort")])

#extracting new dominant variant
    cl.hex.spdf@data <- data.frame(cl.hex.spdf@data, df[match(as.numeric(cl.hex.spdf@data[,"maxLevel"]),df[,"id"]),])
    cl.hex.spdf <- cl.hex.spdf[,!(names(cl.hex.spdf) %in% "id.1")] 

    #remove NA
    cl_noNA <- cl.hex.spdf[complete.cases(cl.hex.spdf@data[ , "maxLevel"]),]
    #omit those less than threshold number of participants 
    cl_noNA <- cl_noNA[cl_noNA@data$total>threshold.hc,]
    cl_noNA <- merge(cl_noNA, q_ansKey, by ="antwort" )

#filling voronoi tesselation with data    
    changedata <- over(voro, map)
    voro$change <-changedata[,change_age]
    #voro$change[is.na(voro$change)] <- 0
    voro$change <- voro$change*100
    
    ## creating colour palette
    # for answers
    pal <- colorFactor(
      plasma(length(q_ansKey$antwort)),
      as.factor(q_ansKey$ID))
	#for voronoi change
    pal2 <- colorBin("Greens",
                     voro$change, bins = 5)

    #pop up contents of honeycomb
    contenthoneycomb <- paste("<b>","Dominant Variable: ",cl_noNA$antwort ,"</b>","<br/>",
                              "Number of people: ",cl_noNA@data$total,"<br/>",
                              "Degree of dominance: ", round(cl_noNA@data$relMax,2)*100,"%")
    
	#pop up contents of voronoi (for agg_change)
    # content_agg <- paste("<b>","Aggregate change: ","</b>",round(map$agg_change,4)*100 ,"%")
    

    # content_oldpts1000 <- paste("<b>","Dominant Variable: ", map_o$antwort,"</b>","<br/>",
    #                           "Degree of change: ", round(map_o@data[,change],2)*100,"%","<br/>")
    
	#pop up contents old answers
    content_oldpts <- paste("<b>", map@data$Name.x,"<br/>","Dominant Variable: ", map$antwort, "</b>","<br/>",
                            "Degree of change: ", round(map@data[,change_age],2)*100,"%","<br/>",
                            "Number of people (within voronoi polygon): ", round(map@data[,freq_age],2),"<br/>"
    )
    
    
    # generating leaflet map
    
    m2 <- leaflet() %>% 
      # Base groups
      addProviderTiles("Stamen.TonerLite", group = "Basemap") %>%
      addPolygons(data=voro, stroke = FALSE, fillColor = ~pal2(voro$change), fillOpacity = 0.75,
                  group = "Change") %>%
      # popup=paste(content_agg)
      addPolygons(data= cl.hex.spdf, fillOpacity = 0, weight =0.3, group = "New Answers")%>%
      addPolygons(data = cl_noNA, fillColor = ~pal(cl_noNA$ID),
                  fillOpacity= (cl_noNA$relMax*0.9),
                  stroke = FALSE, color="black", weight =1,
                  popup=paste(contenthoneycomb), group = "New Answers")%>%
      addCircleMarkers(data = map, stroke= TRUE, weight =1.5,color = "black", fillColor= ~pal(map@data$answer_ID),
                       fillOpacity = 0.75, radius = (4+(map@data[,change_age]*8)),
                       popup= paste(content_oldpts), group = "Old Answers") %>%
      hideGroup("Basemap","Old Answers", "New Answers") %>% #"change"
      addLegend("bottomleft", pal = pal, values = q_ansKey$ID,
                labFormat = labelFormat(suffix = paste(": ",q_ansKey$antwort)) , #[ansKey$id==sort(unique(as.numeric(cl.pts$maxLevel)))]
                opacity=0.85, title = "Variants") %>%
      addLegend("bottomright", pal = pal2, values = voro$change,
                opacity=0.75, title = "% Change",
                labFormat = labelFormat(suffix= "%")
      )%>%
      addLayersControl(
        
        overlayGroups = c("Basemap","Old Answers", "New Answers", "Change"),
        options = layersControlOptions(collapsed = TRUE))
    
    # Layers control
    #addLayersControl(
    # baseGroups = c("Terrain","Satellite"),
    #overlayGroups = c("hex","Ungulates"),
    #options = layersControlOptions(collapsed = TRUE)
    #)
    m2
    saveWidget(m2, file=paste0("q", q.no, "_",ag,".html"), selfcontained = T)
  }
}



## generating agg_change for each location for each question
# all_q_change_byage<-readRDS("all_q_change_byage")
# all_q_change_byage$ID <-as.numeric(all_q_change_byage$ID)


# aggchangedf<-data.frame()
# for (i in 1:404){
  # x<-all_q_change_byage[all_q_change_byage$ID==i,]
  # aggchangedf[i,"ID"]<- i
    # aggchangedf[i,"aggchange_all"]<- sum(x$changeall[x$freqall>threshold])/length(x$changeall[x$freqall>threshold])
    # aggchangedf[i,"aggchange_a_20"]<- sum(x$changea_20[x$freqa_20>threshold])/length(x$changea_20[x$freqa_20>threshold])
    # aggchangedf[i,"aggchange_a_2140"]<-sum(x$changea_2140[x$freqa_2140>threshold])/length(x$changea_2140[x$freqa_2140>threshold])
    # aggchangedf[i,"aggchange_a_2150"]<- sum(x$changea_2150[x$freqa_2150>threshold])/length(x$changea_2150[x$freqa_2150>threshold])
    # aggchangedf[i,"aggchange_a_4160"]<- sum(x$changea_4160[x$freqa_4160>threshold])/length(x$changea_4160[x$freqa_4160>threshold])
    # aggchangedf[i,"aggchange_a_m50"]<- sum(x$changea_m50[x$freqa_m50>threshold])/length(x$changea_m50[x$freqa_m50>threshold])
    # aggchangedf[i,"aggchange_a_m60"]<-sum(x$changea_m60[x$freqa_m60>threshold])/length(x$changea_m60[x$freqa_m60>threshold])
    
# }

saveRDS(aggchangedf,file="aggchangedf")
aggchangedf<-merge(oldans,aggchangedf, by="ID")

## creating aggregate change maps for each age group

for (ag in age.grouplist){
  map<-all.location.answers
  aggchange <-paste0("aggchange_",ag)
  #agg maps
  changedata <- over(voro, oldans[,40:46])
  voro$change <-changedata[,aggchange]
  voro$change <- voro$change*100
  
  content_oldpts_aggchange <- paste("<b>", voro@data$Name.x,"</b>","<br/>",
                        "Aggregate change: ", round(voro@data[,"change"],2),"%","<br/>"
                        )
magg2 <- leaflet() %>% 
  # Base groups
  addProviderTiles("Stamen.TonerLite", group = "Basemap") %>%
  addPolygons(data=voro, stroke = FALSE, fillColor = ~pal2(voro$change), fillOpacity = 0.75,
              group = "Change") %>%
  # popup=paste(content_agg)
  addCircleMarkers(data = map, stroke= TRUE, weight =1.5,color = "black", 
                   fillOpacity = 0.65, radius=4,
                   popup= paste(content_oldpts_aggchange), group = "Old Answers") %>%

  addLegend("bottomright", pal = pal2, values = voro$change,
            opacity=0.75, title = "% Change",
            labFormat = labelFormat(suffix= "%")
  )%>%
  addLayersControl(
    
    overlayGroups = c("Basemap","Old Answers", "Change"),
    options = layersControlOptions(collapsed = TRUE))
# Layers control
#addLayersControl(
# baseGroups = c("Terrain","Satellite"),
#overlayGroups = c("hex","Ungulates"),
#options = layersControlOptions(collapsed = TRUE)
#)
magg2
saveWidget(magg2, file=paste0(ag,"_aggchange",".html"), selfcontained = T)
}
#write.table(all_q_change_byage,file="all_q_change_byage.csv",sep=",", row.names = FALSE)
#write.table(aggchangedf,file="aggchangedf.csv",sep=",", row.names = FALSE)
