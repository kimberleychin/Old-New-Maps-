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



all.location.answers <- read_excel("Misc/allLocationAnswers.xls")
ansKey <- read_excel("Misc/oldNewAnswers.xls")




coordinates(all.location.answers) <- ~X+Y
proj4string(all.location.answers) <- CRS("+init=epsg:4326")
#Changing all NAs to 0
all.location.answers@data[is.na(all.location.answers@data)] <- 0
plot(all.location.answers)




locations <- read_excel("OldNewMaps/Misc/allLocationAnswers.xls")
locations<- locations[,c("X", "Y","ID", "Name", "Code")]
locations[is.na(locations)] <- 0
# 
# is.na(all.location.answers@data)


# load("allQuest/questSPDF23.Rdata")
# all.answ.eu[all.answ.eu@data$AntwortID==260] <-259
# all.answ.eu@data$Antwort[all.answ.eu@data$AntwortID == 259] <- "Chäppeli / ds Chäppi"
# save(all.answ.eu, file = "allQuest/questSPDF23.Rdata")
# a<-all.answ.eu@data
#newanswer1 <- all.answ.eu@data

#unique(q9@data[,c('AntwortID','Antwort')])
##############################################   Q1##################################################
load("allQuest/questSPDF1.Rdata")
q1<-all.answ.eu
q1coords<- q1@coords
##extracting nearest neighbour IDs
nn1 <- get.knnx(q1coords, all.location.answers.coords,100)
nn1 <- nn1[[1]]

q1_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn1)){                  #for each survey point
  for(i in 1:length(nn1[j,])){           #for each neighbourhood-point of a survey point
    q1_100nb[index,1] <- all.location.answers$ID[j]
    q1_100nb[index,2] <-q1$AntwortID[nn1[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q1_100nb) <- c("location", "new_ans")

q1_change <- data.frame()
for (j in 1:j){ #length(unique(q1_100nb$location))
  
  old_ans<-all.location.answers@data$Frage_1[[j]]
  if (old_ans!=0){
    l1 <- subset(q1_100nb, location== j)
    freq <- as.data.frame(table(l1$new_ans))
  
  if (old_ans %in% freq$Var1){
  perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
  q1_change[j,1] <- j
  q1_change[j,2] <- perchange
  } else {
    q1_change[j,1] <- j
    q1_change[j,2] <- 1
  }
  }else {
    q1_change[j,1]<-j
    q1_change[j,2]<-NA
  }
}

##############################################   Q2##################################################
load("allQuest/questSPDF2.Rdata")
q2<-all.answ.eu
q2coords<- q2@coords

##extracting nearest neighbour IDs
nn2 <- get.knnx(q2coords, all.location.answers.coords,100)
nn2 <- nn2[[1]]

q2_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn2)){                  #for each survey point
  for(i in 1:length(nn2[j,])){           #for each neighbourhood-point of a survey point
    q2_100nb[index,1] <- all.location.answers$ID[j]
    q2_100nb[index,2] <-q2$AntwortID[nn2[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q2_100nb) <- c("location", "new_ans")

q2_change <- data.frame()
for (j in 1:length(unique(q2_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_2[[j]]
  if (old_ans!=0){
    l <- subset(q2_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
  
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q2_change[j,1] <- j
    q2_change[j,2] <- perchange
  } else {
    q2_change[j,1] <- j
    q2_change[j,2] <- 1
  }
  }else {
    q2_change[j,1]<-j
    q2_change[j,2]<-NA
  }
  
}

##############################################   Q4##################################################
load("allQuest/questSPDF4.Rdata")
q4<-all.answ.eu
q4coords<- q4@coords

##extracting nearest neighbour IDs
nn4 <- get.knnx(q4coords, all.location.answers.coords,100)
nn4 <- nn4[[1]]

q4_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn4)){                  #for each survey point
  for(i in 1:length(nn4[j,])){           #for each neighbourhood-point of a survey point
    q4_100nb[index,1] <- all.location.answers$ID[j]
    q4_100nb[index,2] <-q4$AntwortID[nn4[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q4_100nb) <- c("location", "new_ans")

q4_change <- data.frame()
for (j in 1:length(unique(q4_100nb$location))){ #length(unique(q1_100nb$location))
  l <- subset(q4_100nb, location== j)
  freq <- as.data.frame(table(l$new_ans))
  old_ans<-all.location.answers@data$Frage_4[[j]]
  if (old_ans!=0){
    l <- subset(q4_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
  
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q4_change[j,1] <- j
    q4_change[j,2] <- perchange
  } else {
    q4_change[j,1] <- j
    q4_change[j,2] <- 1
  }
  }
  else {
    q4_change[j,1]<-j
    q4_change[j,2]<-NA
  }
}

##############################################   Q5##################################################
load("allQuest/questSPDF5.Rdata")
q5<-all.answ.eu

q5coords<- q5@coords

##extracting nearest neighbour IDs
nn5 <- get.knnx(q5coords, all.location.answers.coords,100)
nn5 <- nn5[[1]]

q5_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn5)){                  #for each survey point
  for(i in 1:length(nn5[j,])){           #for each neighbourhood-point of a survey point
    q5_100nb[index,1] <- all.location.answers$ID[j]
    q5_100nb[index,2] <-q5$AntwortID[nn5[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q5_100nb) <- c("location", "new_ans")

q5_change <- data.frame()
for (j in 1:length(unique(q5_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_5[[j]]
  if (old_ans!=0){
  l <- subset(q5_100nb, location== j)
  freq <- as.data.frame(table(l$new_ans))
  
  
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q5_change[j,1] <- j
    q5_change[j,2] <- perchange
  } else {
    q5_change[j,1] <- j
    q5_change[j,2] <- 1
  }
  }else {
    q5_change[j,1]<-j
    q5_change[j,2]<-NA
  }
}
##############################################   Q6##################################################
load("allQuest/questSPDF6.Rdata")
q6<-all.answ.eu

q6coords<- q6@coords

##extracting nearest neighbour IDs
nn6 <- get.knnx(q6coords, all.location.answers.coords,100)
nn6 <- nn6[[1]]

q6_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn6)){                  #for each survey point
  for(i in 1:length(nn6[j,])){           #for each neighbourhood-point of a survey point
    q6_100nb[index,1] <- all.location.answers$ID[j]
    q6_100nb[index,2] <-q6$AntwortID[nn6[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q6_100nb) <- c("location", "new_ans")

q6_change <- data.frame()
for (j in 1:length(unique(q6_100nb$location))){ #length(unique(q1_100nb$location))
  
  old_ans<-all.location.answers@data$Frage_6[[j]]
  if (old_ans!=0){
    l <- subset(q6_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q6_change[j,1] <- j
    q6_change[j,2] <- perchange
  } else {
    q6_change[j,1] <- j
    q6_change[j,2] <- 1
  }
  }else {
    q6_change[j,1]<-j
    q6_change[j,2]<-NA
  }
  
}
##############################################   Q7##################################################
load("allQuest/questSPDF7.Rdata")
q7<-all.answ.eu

q7coords<- q7@coords

##extracting nearest neighbour IDs
nn7 <- get.knnx(q7coords, all.location.answers.coords,100)
nn7 <- nn7[[1]]

q7_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn7)){                  #for each survey point
  for(i in 1:length(nn7[j,])){           #for each neighbourhood-point of a survey point
    q7_100nb[index,1] <- all.location.answers$ID[j]
    q7_100nb[index,2] <-q7$AntwortID[nn7[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q7_100nb) <- c("location", "new_ans")

q7_change <- data.frame()
for (j in 1:length(unique(q7_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_7[[j]]
  if (old_ans!=0){
    l <- subset(q7_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q7_change[j,1] <- j
    q7_change[j,2] <- perchange
  } else {
    q7_change[j,1] <- j
    q7_change[j,2] <- 1
  }
  }else {
    q7_change[j,1]<-j
    q7_change[j,2]<-NA
  }
}
##############################################   Q8##################################################
load("allQuest/questSPDF8.Rdata")
q8<-all.answ.eu

q8coords<- q8@coords

##extracting nearest neighbour IDs
nn8 <- get.knnx(q8coords, all.location.answers.coords,100)
nn8 <- nn8[[1]]

q8_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn8)){                  #for each survey point
  for(i in 1:length(nn8[j,])){           #for each neighbourhood-point of a survey point
    q8_100nb[index,1] <- all.location.answers$ID[j]
    q8_100nb[index,2] <-q8$AntwortID[nn8[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q8_100nb) <- c("location", "new_ans")

q8_change <- data.frame()
for (j in 1:length(unique(q8_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_8[[j]]
  if (old_ans!=0){ 
    l <- subset(q8_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q8_change[j,1] <- j
    q8_change[j,2] <- perchange
  } else {
    q8_change[j,1] <- j
    q8_change[j,2] <- 1
  }
  }else {
    q8_change[j,1]<-j
    q8_change[j,2]<-NA
  }
  }

##############################################   Q9##################################################
load("allQuest/questSPDF9.Rdata")
q9<-all.answ.eu

q9coords<- q9@coords

##extracting nearest neighbour IDs
nn9 <- get.knnx(q9coords, all.location.answers.coords,100)
nn9 <- nn9[[1]]

q9_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn9)){                  #for each survey point
  for(i in 1:length(nn9[j,])){           #for each neighbourhood-point of a survey point
    q9_100nb[index,1] <- all.location.answers$ID[j]
    q9_100nb[index,2] <-q9$AntwortID[nn9[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q9_100nb) <- c("location", "new_ans")

q9_change <- data.frame()
for (j in 1:length(unique(q9_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_9[[j]]
  if (old_ans!=0){ 
    l <- subset(q9_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q9_change[j,1] <- j
    q9_change[j,2] <- perchange
  } else {
    q9_change[j,1] <- j
    q9_change[j,2] <- 1
  }
  }else {
    q9_change[j,1]<-j
    q9_change[j,2]<-NA
  }
  }

##############################################   Q11##################################################
load("allQuest/questSPDF11.Rdata")
q11<-all.answ.eu

q11coords<- q11@coords

##extracting nearest neighbour IDs
nn11 <- get.knnx(q11coords, all.location.answers.coords,100)
nn11 <- nn11[[1]]

q11_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn11)){                  #for each survey point
  for(i in 1:length(nn11[j,])){           #for each neighbourhood-point of a survey point
    q11_100nb[index,1] <- all.location.answers$ID[j]
    q11_100nb[index,2] <-q11$AntwortID[nn11[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q11_100nb) <- c("location", "new_ans")

q11_change <- data.frame()
for (j in 1:length(unique(q11_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_11[[j]]
  if (old_ans!=0){ 
    l <- subset(q11_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q11_change[j,1] <- j
    q11_change[j,2] <- perchange
  } else {
    q11_change[j,1] <- j
    q11_change[j,2] <- 1
  }
  }else {
    q11_change[j,1]<-j
    q11_change[j,2]<-NA
  }
}

##############################################   Q14##################################################
load("allQuest/questSPDF14.Rdata")
q14<-all.answ.eu

q14coords<- q14@coords

##extracting nearest neighbour IDs
nn14 <- get.knnx(q14coords, all.location.answers.coords,100)
nn14 <- nn14[[1]]

q14_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn14)){                  #for each survey point
  for(i in 1:length(nn14[j,])){           #for each neighbourhood-point of a survey point
    q14_100nb[index,1] <- all.location.answers$ID[j]
    q14_100nb[index,2] <-q14$AntwortID[nn14[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q14_100nb) <- c("location", "new_ans")

q14_change <- data.frame()
for (j in 1:length(unique(q14_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_14[[j]]
  if (old_ans!=0){ 
    l <- subset(q14_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q14_change[j,1] <- j
    q14_change[j,2] <- perchange
  } else {
    q14_change[j,1] <- j
    q14_change[j,2] <- 1
  }
  }
  else {
    q14_change[j,1]<-j
    q14_change[j,2]<-NA
  }
}

##############################################   Q16##################################################
load("allQuest/questSPDF16.Rdata")
q16<-all.answ.eu

q16coords<- q16@coords

##extracting nearest neighbour IDs
nn16 <- get.knnx(q16coords, all.location.answers.coords,100)
nn16 <- nn16[[1]]

q16_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn16)){                  #for each survey point
  for(i in 1:length(nn16[j,])){           #for each neighbourhood-point of a survey point
    q16_100nb[index,1] <- all.location.answers$ID[j]
    q16_100nb[index,2] <-q16$AntwortID[nn16[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q16_100nb) <- c("location", "new_ans")

q16_change_info<-list()
q16_change <- data.frame()
for (j in 1:length(unique(q16_100nb$location))){ #length(unique(q1_100nb$location))
  
  old_ans<-all.location.answers@data$Frage_16[[j]]
  if (old_ans!=0){ 
    l <- subset(q16_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
  q16_change_info <- append(q16_change_info, list(as.data.frame(freq)))
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q16_change[j,1] <- j
    q16_change[j,2] <- perchange
  } else {
    q16_change[j,1] <- j
    q16_change[j,2] <- 1
  }
  }
  else {
    q16_change[j,1]<-j
    q16_change[j,2]<-NA
  }
}


##############################################   Q18##################################################
load("allQuest/questSPDF18.Rdata")
q18<-all.answ.eu

q18coords<- q18@coords

##extracting nearest neighbour IDs
nn18 <- get.knnx(q18coords, all.location.answers.coords,100)
nn18 <- nn18[[1]]

q18_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn18)){                  #for each survey point
  for(i in 1:length(nn18[j,])){           #for each neighbourhood-point of a survey point
    q18_100nb[index,1] <- all.location.answers$ID[j]
    q18_100nb[index,2] <-q18$AntwortID[nn18[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q18_100nb) <- c("location", "new_ans")

q18_change <- data.frame()
for (j in 1:length(unique(q18_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_18[[j]]
  if (old_ans!=0){ 
    l <- subset(q18_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q18_change[j,1] <- j
    q18_change[j,2] <- perchange
  } else {
    q18_change[j,1] <- j
    q18_change[j,2] <- 1
  }
  }
  else {
    q18_change[j,1]<-j
    q18_change[j,2]<-NA
  }
}
##############################################   Q21##################################################
load("allQuest/questSPDF21.Rdata")
q21<-all.answ.eu
q21coords<- q21@coords

##extracting nearest neighbour IDs
nn21 <- get.knnx(q21coords, all.location.answers.coords,100)
nn21 <- nn21[[1]]

q21_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn21)){                  #for each survey point
  for(i in 1:length(nn21[j,])){           #for each neighbourhood-point of a survey point
    q21_100nb[index,1] <- all.location.answers$ID[j]
    q21_100nb[index,2] <-q21$AntwortID[nn21[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q21_100nb) <- c("location", "new_ans")

q21_change <- data.frame()
for (j in 1:length(unique(q21_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_21[[j]]
  if (old_ans!=0){ 
    l <- subset(q21_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q21_change[j,1] <- j
    q21_change[j,2] <- perchange
  } else {
    q21_change[j,1] <- j
    q21_change[j,2] <- 1
  }
  }else {
    q21_change[j,1]<-j
    q21_change[j,2]<-NA
  }
}
##############################################   Q23##################################################
load("allQuest/questSPDF23.Rdata")
q23<-all.answ.eu

q23coords<- q23@coords

##extracting nearest neighbour IDs
nn23 <- get.knnx(q23coords, all.location.answers.coords,100)
nn23 <- nn23[[1]]

q23_100nb <- data.frame()             #initialise empty dataframe
index = 1                                #initialise index counter
for(j in 1:NROW(nn23)){                  #for each survey point
  for(i in 1:length(nn23[j,])){           #for each neighbourhood-point of a survey point
    q23_100nb[index,1] <- all.location.answers$ID[j]
    q23_100nb[index,2] <-q23$AntwortID[nn23[j,i]]
    
    index = index+1                        #index-counter +1
  }
} 

names(q23_100nb) <- c("location", "new_ans")

q23_change <- data.frame()
for (j in 1:length(unique(q23_100nb$location))){ #length(unique(q1_100nb$location))
  old_ans<-all.location.answers@data$Frage_23[[j]]
  if (old_ans!=0){ 
    l <- subset(q23_100nb, location== j)
    freq <- as.data.frame(table(l$new_ans))
    
  if (old_ans %in% freq$Var1){
    perchange <- 1-(freq$Freq[freq$Var1==old_ans]/(sum(freq$Freq)))
    q23_change[j,1] <- j
    q23_change[j,2] <- perchange
  } else {
    q23_change[j,1] <- j
    q23_change[j,2] <- 1
  }
  }
  else {
    q23_change[j,1]<-j
    q23_change[j,2]<-NA
  }
}
##meging results
unique(q1_100nb$location)
# rm(all_questions_change)
all_questions_change1 <-all_questions_change 
all_questions_change <- merge(q1_change,q2_change, by= "V1")
names(all_questions_change)<- c("location", "q1_change", "q2_change")
all_questions_change <- merge(all_questions_change,q4_change, by.x= "location",by.y= "V1")
names(all_questions_change)[4]<-"q4_change"
all_questions_change <- merge(all_questions_change,q5_change, by.x= "location",by.y= "V1")
names(all_questions_change)[5]<-"q5_change"
all_questions_change <- merge(all_questions_change,q6_change, by.x= "location",by.y= "V1")
names(all_questions_change)[6]<-"q6_change"
all_questions_change <- merge(all_questions_change,q7_change, by.x= "location",by.y= "V1")
names(all_questions_change)[7]<-"q7_change"
all_questions_change <- merge(all_questions_change,q8_change, by.x= "location",by.y= "V1")
names(all_questions_change)[8]<-"q8_change"
all_questions_change <- merge(all_questions_change,q9_change, by.x= "location",by.y= "V1")
names(all_questions_change)[9]<-"q9_change"
all_questions_change <- merge(all_questions_change,q11_change, by.x= "location",by.y= "V1")
names(all_questions_change)[10]<-"q11_change"
all_questions_change <- merge(all_questions_change,q14_change, by.x= "location",by.y= "V1")
names(all_questions_change)[11]<-"q14_change"
all_questions_change <- merge(all_questions_change,q16_change, by.x= "location",by.y= "V1")
names(all_questions_change)[12]<-"q16_change"
all_questions_change <- merge(all_questions_change,q18_change, by.x= "location",by.y= "V1")
names(all_questions_change)[13]<-"q18_change"
all_questions_change <- merge(all_questions_change,q21_change, by.x= "location",by.y= "V1")
names(all_questions_change)[14]<-"q21_change"
all_questions_change <- merge(all_questions_change,q23_change, by.x= "location",by.y= "V1")
names(all_questions_change)[15]<-"q23_change"

all_questions_change <- all_questions_change[,c("ID","X","Y","Name", "Code", "q1_change","q2_change"
                                                ,"q4_change", "V2","q6_change","q7_change","q8_change","q9_change",
                                                "q11_change","q14_change","q16_change","q18_change","q21_change","q23_change")]

all_questions_change <- merge(locations, all_questions_change, by.x="ID", by.y="location")

all_questions_change$agg_change <-rowMeans(all_questions_change[,6:19], na.rm=TRUE)


write.csv(all_questions_change,"OldNewMaps/Misc/allLocationChanges.csv")

###NAMES WITH CORRECT UMLAUTS
change<-read.csv("Misc/allLocationChanges.csv")


oldans<-all.location.answers
oldans<- merge(oldans, change, by = "ID")
remove <- c("Name.y", " Code.y", "X.1")
oldans <- oldans[ , !(names(oldans) %in% remove)]
names(oldans)[2] <- "Name"
names(oldans)[3] <- "Code"
saveRDS(oldans, "input/oldans")

writeOGR(oldans, "OldData_WDU", "oldpoints", driver = "ESRI Shapefile")

# Geldbörse<- read_excel("OldData_WDU/16_Geldbörse.xlsx")
# coordinates(Geldbörse)<-~X+Y
# proj4string(Geldbörse) <- CRS("+init=epsg:4326")
# writeOGR(Geldbörse, "OldData_WDU", "16_Geldbörse", driver = "ESRI Shapefile")


# plot(voronoi(oldans),add=T)
# plot(cl.hex.spdf)
# x <-  -(voro-cl.hex.spdf )
# 

##VORONOI
W <- as(germanCount, "owin")

  
X <- ppp(x=locations$X,
           y=locations$Y, window = W)



y <- dirichlet(X) # Dirichlet tesselation
plot(y) # Plot tesselation
plot(X, add = TRUE) # Add points
tile.areas(y) #Areas
tess.spdf <-as(y, "SpatialPolygons")    
proj4string(tess.spdf) <- CRS("+init=epsg:4326")
saveRDS(tess.spdf, "OldNewMaps/input/vorotess")

