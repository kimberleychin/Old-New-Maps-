library(rgdal)
library(sp)
library(lme4)
library(maptools)
library(raster)
library(leaflet)
library(ggplot2)
library(plyr)
library(rgeos)
library(FNN)
library(spdep)
library(GISTools)
library(readxl)
library(maps)
library(spatstat)
library(rgeos)
library(sp)
library(rgdal)



all.location.answers <- read_excel("OldNewMaps/input/allLocationAnswers.xls")
ansKey <- read_excel("OldNewMaps/input/oldNewAnswers.xls")

coordinates(all.location.answers) <- ~X+Y
proj4string(all.location.answers) <- CRS("+init=epsg:4326")
#Changing all NAs to 0
all.location.answers@data[is.na(all.location.answers@data)] <- 0
plot(all.location.answers)
locations<- locations[,c("X", "Y","ID", "Name", "Code")]
locations[is.na(locations)] <- 0
saveRDS(all.location.answers,file="OldNewMaps/input/all.location.answers")


################################# data processing for change calculation########################
### repeat for all questions
load("allQuest/questSPDF1.Rdata")

##creating datasets for each age group
q1<-all.answ.eu
q1_a_20<-all.answ.eu[which(all.answ.eu$Wiealt<21  & all.answ.eu$Wiealt>=0),]
q1_a_2140<-all.answ.eu[which(all.answ.eu$Wiealt<41  & all.answ.eu$Wiealt>=21),]
q1_a_2150<-all.answ.eu[which(all.answ.eu$Wiealt<51  & all.answ.eu$Wiealt>=21),]
q1_a_4160<-all.answ.eu[which(all.answ.eu$Wiealt<60  & all.answ.eu$Wiealt>=41),]
q1_a_m50<-all.answ.eu[which(all.answ.eu$Wiealt<9999  & all.answ.eu$Wiealt>=50),]
q1_a_m60<-all.answ.eu[which(all.answ.eu$Wiealt<9999  & all.answ.eu$Wiealt>=60),]

q1coords<- q1@coords
q1_a_20coords<- q1_a_20@coords
q1_a_2140coords<- q1_a_2140@coords
q1_a_2150coords<- q1_a_2150@coords
q1_a_4160coords<- q1_a_4160@coords
q1_a_m50coords<- q1_a_m50@coords
q1_a_m60coords<- q1_a_m60@coords
q1_change_byage<-matrix(ncol = 0, nrow= 404)
q1_change_byage <- as.data.frame(q1_change_byage)

agegrouplist<-c("all","a_20","a_2140","a_2150","a_4160","a_m50","a_m60")
agegroupdatalist<-list(q1, q1_a_20,q1_a_2140,q1_a_2150,q1_a_4160,q1_a_m50,q1_a_m60)
agegroupcoordlist<- list(q1@coords,q1_a_20coords,q1_a_2140coords,q1_a_2150@coords,q1_a_4160@coords,q1_a_m50@coords,q1_a_m60@coords)
##extracting nearest neighbour IDs
for (i in 1:length(agegrouplist)){
  age<-agegrouplist[[i]]
  data<-agegroupdatalist[[i]]
  datacoords<-agegroupcoordlist[[i]]
  nn1 <- get.knnx(datacoords, all.location.answers.coords,100)
  #nn1 <- nn1[[1]]
  
  q1_100nb <- data.frame()             #initialise empty dataframe
  index = 1                                #initialise index counter
  for(j in 1:NROW(nn1$nn.index)){                  #for each survey point
    for(k in 1:length(nn1$nn.index[j,])){           #for each neighbourhood-point of a survey point
      q1_100nb[index,1] <- all.location.answers$ID[j]  # location ID
      q1_100nb[index,2] <-data$AntwortID[nn1$nn.index[j,k]] # new answer location id
      q1_100nb[index,3] <-data$Wiealt[nn1$nn.index[j,k]] # age
      q1_100nb[index,4] <-data@coords[nn1$nn.index[j,k],1] # x coordinate (to ensure that it falls in voronoi polygon)
      q1_100nb[index,5] <-data@coords[nn1$nn.index[j,k],2] #y coordinate (to ensure that it falls in voronoi polygon)
      
      index = index+1                        #index-counter +1
    }
  } 
  
  names(q1_100nb) <- c("location", "new_ans", "age","x","y")
  
  coordinates(q1_100nb)<-~x+y
  proj4string(q1_100nb)<-CRS("+init=epsg:4326")
  
  a<-q1_100nb
## calculating change from old answer and new answers from neighbours
  q1_change <- data.frame()
  for (l in 1:length(unique(q1_100nb$location))){ #length(unique(q1_100nb$location))

      old_ans<-all.location.answers@data$Frage_1[[l]]
      # if data exists for that location
      if (old_ans!=0){
        
        l1 <- subset(q1_100nb, location== l)
        freq <- as.data.frame(table(l1$new_ans))
        # if new answers contain the original answer
        if (old_ans %in% freq$Var1){
          perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
          q1_change[l,1] <- l
          q1_change[l,2] <- perchange
          q1_change[l,3] <- age
          q1_change[l,4]<-sum(complete.cases(over(q1_100nb[q1_100nb$location==l,],tess.spdf[tess.spdf$ID==l,])))
        } else {  
          q1_change[l,1] <- l
          q1_change[l,2] <- 1
          q1_change[l,3] <- age
          q1_change[l,4]<-sum(complete.cases(over(q1_100nb[q1_100nb$location==l,],tess.spdf[tess.spdf$ID==l,])))
        }
      }else {
        q1_change[l,1]<-l
        q1_change[l,2]<-NA
        q1_change[l,3] <- age
        q1_change[l,4]<-sum(complete.cases(over(q1_100nb[q1_100nb$location==l,],tess.spdf[tess.spdf$ID==l,])))
      }
    
  }
  names(q1_change)<-c("ID", paste0("change",age), "age", paste0("freq",age))
  q1_change_byage <- cbind(q1_change_byage,q1_change)
  q1_change_byage$q<-"q1"
  
}
saveRDS(q1_change_byage,file="q1_change_byage")
names(q1_change_byage)<-c("ID", "changeall", "age", "freqall", "q", "ID", "changea_20", 
                          "age", "freqa_20", "ID", "changea_2140", "age", "freqa_2140", 
                          "ID", "changea_2150", "age", "freqa_2150", "ID", "changea_4160", 
                          "age", "freqa_4160", "ID", "changea_m50", "age", "freqa_m50", 
                          "ID", "changea_m60", "age", "freqa_m60")

###combining all changes into single dataframe
all_q_change_byage<-rbind(q1_change_byage,q2_change_byage,q4_change_byage,q5_change_byage
                          ,q6_change_byage,q7_change_byage,q8_change_byage,q9_change_byage
                          ,q11_change_byage,q14_change_byage,q16_change_byage,q18_change_byage
                          ,q21_change_byage,q23_change_byage)
#keeping only relevant columns
#all_q_change_byage<-all_q_change_byage[,c(1,2,4,5,6,8,9,11,12,14,15,17,18,20,21,23)]
saveRDS(all_q_change_byage,file="all_q_change_byage")

##VORONOI

germanCount <- readShapePoly("OldNewMaps/input/geom/totalmerged.shp")
W <- as(germanCount, "owin")
X <- ppp(x=oldans@coords[,1],
           y=oldans@coords[,2], window = W)

y <- dirichlet(X) # Dirichlet tesselation
plot(y) # Plot tesselation
plot(oldans, add = TRUE) # Add points
tile.areas(y) #Areas
tess.spdf <-as(y, "SpatialPolygons")    
proj4string(tess.spdf) <- CRS("+init=epsg:4326")
tess.spdf <- SpatialPolygonsDataFrame(tess.spdf, as.data.frame(oldans[!is.na(over(oldans,tess.spdf)),]), match.ID = F) 
saveRDS(tess.spdf, "OldNewMaps/input/vorotess")


