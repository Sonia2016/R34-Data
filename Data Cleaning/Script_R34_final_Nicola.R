library(MASS)   
library(dplyr)

demogData <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/Demographic_recovered_Mar_27_2018.csv")
cbmPrime <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/ParticipantExportDAO_recovered_Mar_27_2018.csv")
taskLog <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/M.TaskLog.3.27.2018.Fixed.csv")

########delete Scammer (20:420) and delete admin accounts##########
cmnIds <- intersect(cbmPrime$id, demogData$participantRSA) 
crctId <- setdiff(cmnIds, 20:420) 
adminIds <- cbmPrime[which(cbmPrime$admin == "True"),]$id 
crctId <- setdiff(crctId, adminIds) 
length(crctId)


#####only keep intent-to-treat sample######
pTaskList <- taskLog[taskLog$participantdao_id %in% crctId,] #Abgleich ids
pTaskList <- pTaskList %>% mutate(date_final = as.POSIXct(pTaskList$corrected_datetime,format="%m/%d/%y %H:%M",tz=Sys.timezone())) #ändert format von date
pTaskList <- pTaskList %>% group_by(participantdao_id) %>% slice(which.max(date_final)) #sucht für jeden participant max date und packt das mit id in neues objekt
View(pTaskList)

tmpLst <- pTaskList %>% filter(session_name != 'PRE') #nimmt alles was nicht pre ist in objekt

crctId <- tmpLst$participantdao_id
length(crctId)
crctId



#######cleaning credibility########
Credibility_recovered_Mar_27_2018 <- read.csv("~/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/Credibility_recovered_Mar_27_2018.csv")
Credibility_recovered_Mar_27_2018$id <- NULL
Credibility_recovered_Mar_27_2018$important <- NULL

##CODING ERROR: recode credibility items 5 to 4######
Credibility_recovered_Mar_27_2018$logical[Credibility_recovered_Mar_27_2018$logical==5] <- 4
Credibility_recovered_Mar_27_2018$recommendable[Credibility_recovered_Mar_27_2018$recommendable==5] <- 4

##calculate mean credibility
Credibility_recovered_Mar_27_2018$credibility_mean <- (Credibility_recovered_Mar_27_2018$logical+Credibility_recovered_Mar_27_2018$recommendable)/2
Credibility2 <- Credibility_recovered_Mar_27_2018

######identify duplicates#######
x <- Credibility2[which(duplicated(Credibility2[,c('participantRSA')])==T),]
length(x$participantRSA)
intersect(x$participantRSA,crctId)

#####check ID 1220 and 1338, keep earlier pre score#####
Credibility2 <- Credibility2[-c(424,1074), ]

###only keep participants from crctId#####
Credibility2$date <- NULL
Iddiff <- setdiff(crctId,Credibility2)
write.csv(Iddiff,"Iddiff.csv")
Iddiff <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/Iddiff.csv",header=T)

Credibilitydiff <- Credibility2[(Credibility2$participantRSA %in% Iddiff$x),]
Credibilitydiff$participantRSA <- as.numeric(as.character(Credibilitydiff$participantRSA))
Credibility_order <- Credibilitydiff[order(Credibilitydiff$participantRSA), ]
View(Credibility_order)

write.csv(Credibility_order,"Credibility_order2.csv")

########Cleaning OASIS################
OASIS <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/OA_recovered_Mar_27_2018_Fixed.csv")
cbmPrime <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/ParticipantExportDAO_recovered_Mar_27_2018.csv")

######recode 'prefer not to answer' (555) to NA#####
OASIS$anxious_freq[OASIS$anxious_freq==555] <- NA
OASIS$anxious_sev[OASIS$anxious_sev==555] <- NA
OASIS$avoid[OASIS$avoid==555] <- NA
OASIS$interfere[OASIS$interfere==555] <- NA
OASIS$interfere_social[OASIS$interfere_social==555] <- NA

######identify duplicates#######
x <- OASIS[which(duplicated(OASIS[,c('participantRSA','corrected_session')])==T),]
length(x$participantRSA)
intersect(x$participantRSA,crctId)

#####check ID 745 --> CODING ERROR: recode first SESSION2 to SESSION1 in corrected_session#####
OASIS[5145,12] <- "SESSION1"

#####check ID 644 --> CODING ERROR: recode first SESSION3 to SESSION2 in corrected_session#####
OASIS[5146,12] <- "SESSION2"

#####check ID 1338 and remove later pre and session1######
OASIS <- OASIS[-c(6569,6701), ]

#####create new dataframe for OASIS mean score*5 (include all possible items)#####
OASISNew <- data.frame(matrix(NA, nrow = dim(OASIS)[1], ncol = 3))
colnames(OASISNew) <- c('Id','session','OASISScore')
cnt <- 1

for(id in crctId){
  sessions <- OASIS$corrected_session[OASIS$participantRSA == id]
  for(session in sessions){
    OASISNew$Id[cnt] <- id
    OASISNew$session[cnt] <- session
    OA_1 <- OASIS$anxious_freq[OASIS$participantRSA == id & OASIS$corrected_session == session]
    OA_2 <- OASIS$anxious_sev[OASIS$participantRSA == id & OASIS$corrected_session == session]
    OA_3 <- OASIS$avoid[OASIS$participantRSA == id & OASIS$corrected_session == session]
    OA_4 <- OASIS$interfere[OASIS$participantRSA == id & OASIS$corrected_session == session]
    OA_5 <- OASIS$interfere_social[OASIS$participantRSA == id & OASIS$corrected_session == session]
    OASISNew$OASISScore[cnt] <- (rowMeans(cbind(OA_1,OA_2,OA_3,OA_4,OA_5),na.rm=T))*5
    cnt <- cnt + 1
  }
}

#OASISNew <- OASISNew[complete.cases(OASISNew),]
#length(unique(OASISNew$Id))

#####add columns for cbm condition and prime condition#####
OASISNew$cbmCondition <- NA
OASISNew$primeCondition <- NA

neutralPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(OASISNew$Id))
length(neutralPrimeBBSIQ)

anxietyPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "ANXIETY" & cbmPrime$currentSession != "PRE",)]),unique(OASISNew$Id))
length(anxietyPrimeBBSIQ)


for(id in neutralPrimeBBSIQ){
  OASISNew$primeCondition[which(OASISNew$Id == id,)] <- "NEUTRAL"
}

for(id in anxietyPrimeBBSIQ){
  OASISNew$primeCondition[which(OASISNew$Id == id,)] <- "ANXIETY"
}


neutralBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(OASISNew$Id))
length(neutralBBSIQ)

positiveBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "POSITIVE" & cbmPrime$currentSession != "PRE",)]),unique(OASISNew$Id))
length(positiveBBSIQ)

fiftyBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "FIFTY_FIFTY" & cbmPrime$currentSession != "PRE",)]),unique(OASISNew$Id))
length(fiftyBBSIQ)


for(id in neutralBBSIQ){
  OASISNew$cbmCondition[which(OASISNew$Id == id,)] <- "No Scenario"
}

for(id in positiveBBSIQ){
  OASISNew$cbmCondition[which(OASISNew$Id == id,)] <- "POSITIVE"
}

for(id in fiftyBBSIQ){
  OASISNew$cbmCondition[which(OASISNew$Id == id,)] <- "FIFTY_FIFTY"
}

####add column for combined conditon#####
OASISNew$condition <- NA
OASISNew[which(OASISNew$cbmCondition == "FIFTY_FIFTY" & OASISNew$primeCondition == "NEUTRAL"),]$condition <- "FIFTY_FIFTY&NEUTRAL"
OASISNew[which(OASISNew$cbmCondition == "FIFTY_FIFTY" & OASISNew$primeCondition == "ANXIETY"),]$condition <- "FIFTY_FIFTY&ANXIETY"
OASISNew[which(OASISNew$cbmCondition == "POSITIVE" & OASISNew$primeCondition == "NEUTRAL"),]$condition <- "POSITIVE&NEUTRAL"
OASISNew[which(OASISNew$cbmCondition == "POSITIVE" & OASISNew$primeCondition == "ANXIETY"),]$condition <- "POSITIVE&ANXIETY"
OASISNew[which(OASISNew$cbmCondition == "No Scenario" & OASISNew$primeCondition == "NEUTRAL"),]$condition <- "NEUTRAL&NEUTRAL"
OASISNew[which(OASISNew$cbmCondition == "No Scenario" & OASISNew$primeCondition == "ANXIETY"),]$condition <- "NEUTRAL&ANXIETY"

####calculate anova for baseline differences in OASIS Score######
baselineOASISData <- OASISNew[which(OASISNew$session == "PRE"),]
length(unique(baselineOASISData$Id))
summary(aov(OASISScore ~ condition, data = baselineOASISData))


######Restructure from long to wide format##############################
library(reshape2)
library(plyr)
OASIS_wide <- data.frame(matrix(NA, nrow = length(unique(OASISNew$Id)), ncol = 13))
colnames(OASIS_wide) <- c('Id','primeCondition','cbmCondition', 'pre_OASIS', 'session1_OASIS','session2_OASIS','session3_OASIS', 'session4_OASIS','session5_OASIS','session6_OASIS','session7_OASIS','session8_OASIS','post_OASIS')
cnt <- 1

for(id in unique(OASISNew$Id)){
  OASIS_wide$Id[cnt] <- id
  if(length(OASISNew[which(OASISNew$session == "PRE" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$pre_OASIS[cnt] <- OASISNew[which(OASISNew$session == "PRE" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "SESSION1" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$session1_OASIS[cnt] <- OASISNew[which(OASISNew$session == "SESSION1" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "SESSION2" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$session2_OASIS[cnt] <- OASISNew[which(OASISNew$session == "SESSION2" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "SESSION3" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$session3_OASIS[cnt] <- OASISNew[which(OASISNew$session == "SESSION3" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "SESSION4" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$session4_OASIS[cnt] <- OASISNew[which(OASISNew$session == "SESSION4" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "SESSION5" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$session5_OASIS[cnt] <- OASISNew[which(OASISNew$session == "SESSION5" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "SESSION6" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$session6_OASIS[cnt] <- OASISNew[which(OASISNew$session == "SESSION6" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "SESSION7" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$session7_OASIS[cnt] <- OASISNew[which(OASISNew$session == "SESSION7" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "SESSION8" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$session8_OASIS[cnt] <- OASISNew[which(OASISNew$session == "SESSION8" & OASISNew$Id == id),]$OASISScore
  }
  if(length(OASISNew[which(OASISNew$session == "POST" & OASISNew$Id == id),]$OASISScore) != 0){
    OASIS_wide$post_OASIS[cnt] <- OASISNew[which(OASISNew$session == "POST" & OASISNew$Id == id),]$OASISScore
  }
  cnt <- cnt + 1
}

####add columns for cbm condition and prime condition#####
cbmPrime <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/ParticipantExportDAO_recovered_Mar_27_2018.csv")
neutralPrimeOASIS <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(OASIS_wide$Id))
length(neutralPrimeOASIS)

anxietyPrimeOASIS <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "ANXIETY" & cbmPrime$currentSession != "PRE",)]),unique(OASIS_wide$Id))
length(anxietyPrimeOASIS)


for(id in neutralPrimeOASIS){
  OASIS_wide$primeCondition[which(OASIS_wide$Id == id,)] <- "NEUTRAL"
}

for(id in anxietyPrimeOASIS){
  OASIS_wide$primeCondition[which(OASIS_wide$Id == id,)] <- "ANXIETY"
}


neutralOASIS <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(OASIS_wide$Id))
length(neutralOASIS)

positiveOASIS <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "POSITIVE" & cbmPrime$currentSession != "PRE",)]),unique(OASIS_wide$Id))
length(positiveOASIS)

fiftyOASIS <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "FIFTY_FIFTY" & cbmPrime$currentSession != "PRE",)]),unique(OASIS_wide$Id))
length(fiftyOASIS)


for(id in neutralOASIS){
  OASIS_wide$cbmCondition[which(OASIS_wide$Id == id,)] <- "No Scenario"
}

for(id in positiveOASIS){
  OASIS_wide$cbmCondition[which(OASIS_wide$Id == id,)] <- "POSITIVE"
}

for(id in fiftyOASIS){
  OASIS_wide$cbmCondition[which(OASIS_wide$Id == id,)] <- "FIFTY_FIFTY"
}


######add new columns for experimental conditons in 0-1 format#####
OASIS_wide$CBM_FIFTY_FIFTY <- NA
OASIS_wide$CBM_POSITIVE <- NA
OASIS_wide$CBM_NONE <- NA
OASIS_wide$I_ANXIOUS <- NA
OASIS_wide$I_NEUTRAL <- NA

OASIS_wide$I_ANXIOUS <- 0 
for (i in 1:nrow(OASIS_wide)) { if
  (OASIS_wide$primeCondition[i]=='ANXIETY') {
    (OASIS_wide$I_ANXIOUS[i] <- 1)
  }}
OASIS_wide$I_NEUTRAL <- 0 
for (i in 1:nrow(OASIS_wide)) { if
  (OASIS_wide$primeCondition[i]=='NEUTRAL') {
    (OASIS_wide$I_NEUTRAL[i] <- 1)
  }}
OASIS_wide$CBM_FIFTY_FIFTY <- 0 
for (i in 1:nrow(OASIS_wide)) { if
  (OASIS_wide$cbmCondition[i]=='FIFTY_FIFTY') {
    (OASIS_wide$CBM_FIFTY_FIFTY[i] <- 1)
  }}
OASIS_wide$CBM_POSITIVE <- 0 
for (i in 1:nrow(OASIS_wide)) { if
  (OASIS_wide$cbmCondition[i]=='POSITIVE') {
    (OASIS_wide$CBM_POSITIVE[i] <- 1)
  }}
OASIS_wide$CBM_NONE <- 0 
for (i in 1:nrow(OASIS_wide)) { if
  (OASIS_wide$cbmCondition[i]=='No Scenario') {
    (OASIS_wide$CBM_NONE[i] <- 1)
  }}

####merge with mean credibility#####
Credibility_order2 <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/cleaned/Credibility_order2.csv",header=T)
Credibility_order2$X <- NULL
Credibility_order2$logical <- NULL
Credibility_order2$recommendable <- NULL
Credibility_order2$session <-NULL
Credibility_order2$X.1 <- NULL
names(Credibility_order2)[names(Credibility_order2) == 'participantRSA'] <- 'Id'
OASIS_Cred <- merge(OASIS_wide,Credibility_order2, by = 'Id')

######remove all rows with all OASiS Scores = NA#######
OASIS_NA <- subset(OASIS_Cred,is.na(OASIS_Cred$pre_OASIS) & is.na(OASIS_Cred$session1_OASIS) & is.na(OASIS_Cred$session2_OASIS) & is.na(OASIS_Cred$session3_OASIS) & is.na(OASIS_Cred$session4_OASIS) & is.na(OASIS_Cred$session5_OASIS) & is.na(OASIS_Cred$session6_OASIS) & is.na(OASIS_Cred$session7_OASIS) & is.na(OASIS_Cred$session8_OASIS) & is.na(OASIS_Cred$post_OASIS))
length(OASIS_NA$Id)
OASIS_NA$Id
OASIS_ohneNA <- OASIS_Cred[!(OASIS_Cred$Id %in% OASIS_NA$Id),]
length(OASIS_ohneNA$Id)
OASIS_ohneNA$Id

#####check number of Scores for each session without NA#####
sum(!is.na(OASIS_ohneNA$pre_OASIS))
sum(!is.na(OASIS_ohneNA$session1_OASIS))
sum(!is.na(OASIS_ohneNA$session2_OASIS))
sum(!is.na(OASIS_ohneNA$session3_OASIS))
sum(!is.na(OASIS_ohneNA$session4_OASIS))
sum(!is.na(OASIS_ohneNA$session5_OASIS))
sum(!is.na(OASIS_ohneNA$session6_OASIS))


write.csv(OASIS_ohneNA,"OASIS_final.csv")



##########Cleaning Recognition Ratings###################
RR <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/RR_recovered_Mar_27_2018.csv",header=T)
cbmPrime <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/ParticipantExportDAO_recovered_Mar_27_2018.csv",header=T)

#########recode 'prefer not to answer' (-1) with NA############
RR[RR$blood_test_NF==-1,"blood_test_NF"]<-NA
RR[RR$blood_test_NS==-1,"blood_test_NS"]<-NA
RR[RR$blood_test_PF==-1,"blood_test_PF"]<-NA
RR[RR$blood_test_PS==-1,"blood_test_PS"]<-NA
RR[RR$elevator_NF==-1,"elevator_NF"]<-NA
RR[RR$elevator_NS==-1,"elevator_NS"]<-NA
RR[RR$elevator_PF==-1,"elevator_PF"]<-NA
RR[RR$elevator_PS==-1,"elevator_PS"]<-NA
RR[RR$job_NF==-1,"job_NF"]<-NA
RR[RR$job_NS==-1,"job_NS"]<-NA
RR[RR$job_PF==-1,"job_PF"]<-NA
RR[RR$job_PS==-1,"job_PS"]<-NA
RR[RR$lunch_NF==-1,"lunch_NF"]<-NA
RR[RR$lunch_NS==-1,"lunch_NS"]<-NA
RR[RR$lunch_PF==-1,"lunch_PF"]<-NA
RR[RR$lunch_PS==-1,"lunch_PS"]<-NA
RR[RR$meeting_friend_NF==-1,"meeting_friend_NF"]<-NA
RR[RR$meeting_friend_NS==-1,"meeting_friend_NS"]<-NA
RR[RR$meeting_friend_PF==-1,"meeting_friend_PF"]<-NA
RR[RR$meeting_friend_PS==-1,"meeting_friend_PS"]<-NA
RR[RR$noise_NF==-1,"noise_NF"]<-NA
RR[RR$noise_NS==-1,"noise_NS"]<-NA
RR[RR$noise_PF==-1,"noise_PF"]<-NA
RR[RR$noise_PS==-1,"noise_PS"]<-NA
RR[RR$scrape_NF==-1,"scrape_NF"]<-NA
RR[RR$scrape_NS==-1,"scrape_NS"]<-NA
RR[RR$scrape_PF==-1,"scrape_PF"]<-NA
RR[RR$scrape_PS==-1,"scrape_PS"]<-NA
RR[RR$shopping_NF==-1,"shopping_NF"]<-NA
RR[RR$shopping_NS==-1,"shopping_NS"]<-NA
RR[RR$shopping_PF==-1,"shopping_PF"]<-NA
RR[RR$shopping_PS==-1,"shopping_PS"]<-NA
RR[RR$wedding_NF==-1,"wedding_NF"]<-NA
RR[RR$wedding_NS==-1,"wedding_NS"]<-NA
RR[RR$wedding_PF==-1,"wedding_PF"]<-NA
RR[RR$wedding_PS==-1,"wedding_PS"]<-NA

#############identifiy duplicates################
x <-RR[which(duplicated(RR[,c('participantRSA','session')])==T),] 
length(x$participantRSA)
intersect(x$participantRSA,crctId)

############check ID 1338 and delete later pre session############
RR <- RR[-c(2442),]

###########create new dataset from RR#######################
###Negative foil####
colnamesNF <- grepl(pattern = "NF$", colnames(RR))
RRatingNF <- cbind(RR[colnamesNF == TRUE],RR$session, RR$participantRSA)
length(colnames(RRatingNF))
colnames(RRatingNF)[11] <- "ID"
colnames(RRatingNF)[10] <- "session"

###Positive foil###
colnamesPF <- grepl(pattern = "PF$", colnames(RR))
RRatingPF <- cbind(RR[colnamesPF == TRUE],RR$session, RR$participantRSA)
length(colnames(RRatingPF))
colnames(RRatingPF)[11] <- "ID"
colnames(RRatingPF)[10] <- "session"

####negative####
colNamesNS <- grepl(pattern = "NS$", colnames(RR))
RRatingNS <- cbind(RR[colNamesNS == TRUE],RR$session, RR$participantRSA)
length(colnames(RRatingNS))
colnames(RRatingNS)[11] <- "ID"
colnames(RRatingNS)[10] <- "session"

###positive###
colNamesPS <- grepl(pattern = "PS$", colnames(RR))
RRatingPS <- cbind(RR[colNamesPS == TRUE],RR$session, RR$participantRSA)
colnames(RRatingPS)[11] <- "ID"
colnames(RRatingPS)[10] <- "session"

#####keep only participants from crctId##########
idRR <- unique(RR$participantRSA)
length(idRR)
length(crctId)
length(intersect(idRR, crctId))

diffIds <- setdiff(crctId,idRR)
View(cbmPrime[which(cbmPrime$id %in% diffIds),])

RR <- RR[which(RR$session != "POST"),]
RR <- RR[which(RR$session != "SESSION8"),]

######calculate means for PF, NF, NS and PS####
####(if items are scored with NA, the mean should still be calculated)#######
RR1 <- data.frame(matrix(NA, nrow = dim(RR)[1], ncol = 6))
colnames(RR1) <- c("Id", "session", "RRNegativeFoil", "RRPositiveFoil", "RRNegative", "RRPositive")
cnt <- 1
for(id in crctId){
  sessions <- RR$session[RR$participantRSA == id]
  for(session in sessions){
    RR1$Id[cnt] <- id
    RR1$session[cnt] <- session 
    RRNS1 <- RRatingNS$blood_test_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RRNS2 <- RRatingNS$elevator_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RRNS3 <- RRatingNS$job_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RRNS4 <- RRatingNS$lunch_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RRNS5 <- RRatingNS$meeting_friend_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RRNS6 <- RRatingNS$noise_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RRNS7 <- RRatingNS$scrape_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RRNS8 <- RRatingNS$shopping_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RRNS9 <- RRatingNS$wedding_NS[RRatingNS$ID == id & RRatingNS$session == session]
    RR1$RRNegative[cnt] <-rowMeans(cbind(RRNS1,RRNS2,RRNS3,RRNS4,RRNS5,RRNS6,RRNS7,RRNS8,RRNS9),na.rm=T)
    RRPS1 <- RRatingPS$blood_test_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RRPS2 <- RRatingPS$elevator_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RRPS3 <- RRatingPS$job_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RRPS4 <- RRatingPS$lunch_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RRPS5 <- RRatingPS$meeting_friend_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RRPS6 <- RRatingPS$noise_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RRPS7 <- RRatingPS$scrape_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RRPS8 <- RRatingPS$shopping_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RRPS9 <- RRatingPS$wedding_PS[RRatingPS$ID == id & RRatingPS$session == session]
    RR1$RRPositive[cnt] <- rowMeans(cbind(RRPS1,RRPS2,RRPS3,RRPS4,RRPS5,RRPS6,RRPS7,RRPS8,RRPS9),na.rm=T)
   
    RRNF1 <- RRatingNF$blood_test_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RRNF2 <- RRatingNF$elevator_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RRNF3 <- RRatingNF$job_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RRNF4 <- RRatingNF$lunch_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RRNF5 <- RRatingNF$meeting_friend_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RRNF6 <- RRatingNF$noise_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RRNF7 <- RRatingNF$scrape_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RRNF8 <- RRatingNF$shopping_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RRNF9 <- RRatingNF$wedding_NF[RRatingNF$ID == id & RRatingNF$session == session]
    RR1$RRNegativeFoil[cnt] <- rowMeans(cbind(RRNF1,RRNF2,RRNF3,RRNF4,RRNF5,RRNF6,RRNF7,RRNF8,RRNF9),na.rm=T)  
     
    RRPF1 <- RRatingPF$blood_test_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RRPF2 <- RRatingPF$elevator_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RRPF3 <- RRatingPF$job_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RRPF4 <- RRatingPF$lunch_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RRPF5 <- RRatingPF$meeting_friend_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RRPF6 <- RRatingPF$noise_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RRPF7 <- RRatingPF$scrape_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RRPF8 <- RRatingPF$shopping_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RRPF9 <- RRatingPF$wedding_PF[RRatingPF$ID == id & RRatingPF$session == session]
    RR1$RRPositiveFoil[cnt] <- rowMeans(cbind(RRPF1,RRPF2,RRPF3,RRPF4,RRPF5,RRPF6,RRPF7,RRPF8,RRPF9),na.rm=T)
    
      cnt <- cnt + 1
  }
}


View(RR1)

length(unique(RR1$Id))

#####add cbm condition and prime condition######
RR1$cbmCondition <- NA
RR1$primeCondition <- NA

neutralPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(RR1$Id))
length(neutralPrimeBBSIQ)

anxietyPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "ANXIETY" & cbmPrime$currentSession != "PRE",)]),unique(RR1$Id))
length(anxietyPrimeBBSIQ)


for(id in neutralPrimeBBSIQ){
  RR1$primeCondition[which(RR1$Id == id,)] <- "NEUTRAL"
}

for(id in anxietyPrimeBBSIQ){
  RR1$primeCondition[which(RR1$Id == id,)] <- "ANXIETY"
}


neutralBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(RR1$Id))
length(neutralBBSIQ)

positiveBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "POSITIVE" & cbmPrime$currentSession != "PRE",)]),unique(RR1$Id))
length(positiveBBSIQ)

fiftyBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "FIFTY_FIFTY" & cbmPrime$currentSession != "PRE",)]),unique(RR1$Id))
length(fiftyBBSIQ)


for(id in neutralBBSIQ){
  RR1$cbmCondition[which(RR1$Id == id,)] <- "No Scenario"
}

for(id in positiveBBSIQ){
  RR1$cbmCondition[which(RR1$Id == id,)] <- "POSITIVE"
}

for(id in fiftyBBSIQ){
  RR1$cbmCondition[which(RR1$Id == id,)] <- "FIFTY_FIFTY"
}

###add combined condition####
RR1$condition <- NA
RR1[which(RR1$cbmCondition == "FIFTY_FIFTY" & RR1$primeCondition == "NEUTRAL"),]$condition <- "FIFTY_FIFTY&NEUTRAL"
RR1[which(RR1$cbmCondition == "FIFTY_FIFTY" & RR1$primeCondition == "ANXIETY"),]$condition <- "FIFTY_FIFTY&ANXIETY"
RR1[which(RR1$cbmCondition == "POSITIVE" & RR1$primeCondition == "NEUTRAL"),]$condition <- "POSITIVE&NEUTRAL"
RR1[which(RR1$cbmCondition == "POSITIVE" & RR1$primeCondition == "ANXIETY"),]$condition <- "POSITIVE&ANXIETY"
RR1[which(RR1$cbmCondition == "No Scenario" & RR1$primeCondition == "NEUTRAL"),]$condition <- "NEUTRAL&NEUTRAL"
RR1[which(RR1$cbmCondition == "No Scenario" & RR1$primeCondition == "ANXIETY"),]$condition <- "NEUTRAL&ANXIETY"

######anova vor differences in baseline RR#######
baselineRRData <- RR1[which(RR1$session == "PRE"),]
summary(aov(RRNegative ~ condition, data = baselineRRData))
summary(aov(RRPositive ~ condition, data = baselineRRData))
summary(aov(RRNegativeFoil ~ condition, data = baselineRRData))
summary(aov(RRPositiveFoil ~ condition, data = baselineRRData))

######Restructure from long to wide format##########
library(reshape2)
library(plyr)
RR_wide <- data.frame(matrix(NA, nrow = length(unique(RR1$Id)), ncol = 15))
colnames(RR_wide) <- c('Id','primeCondition','cbmCondition', 'pre_RRNegativeFoil', 'pre_RRPositiveFoil','pre_RRNegative','pre_RRPositive', 'session3_RRNegativeFoil', 'session3_RRPositiveFoil','session3_RRNegative','session3_RRPositive','session6_RRNegativeFoil', 'session6_RRPositiveFoil','session6_RRNegative','session6_RRPositive')
cnt <- 1

for(id in unique(RR1$Id)){
  RR_wide$Id[cnt] <- id
  if(length(RR1[which(RR1$session == "PRE" & RR1$Id == id),]$RRNegativeFoil) != 0){
    RR_wide$pre_RRNegativeFoil[cnt] <- RR1[which(RR1$session == "PRE" & RR1$Id == id),]$RRNegativeFoil
  }
  if(length(RR1[which(RR1$session == "SESSION3" & RR1$Id == id),]$RRNegativeFoil) != 0){
    RR_wide$session3_RRNegativeFoil[cnt] <- RR1[which(RR1$session == "SESSION3" & RR1$Id == id),]$RRNegativeFoil
  }
  if(length(RR1[which(RR1$session == "SESSION6" & RR1$Id == id),]$RRNegativeFoil) != 0){
    RR_wide$session6_RRNegativeFoil[cnt] <- RR1[which(RR1$session == "SESSION6" & RR1$Id == id),]$RRNegativeFoil
  }
  if(length(RR1[which(RR1$session == "PRE" & RR1$Id == id),]$RRPositiveFoil) != 0){
    RR_wide$pre_RRPositiveFoil[cnt] <- RR1[which(RR1$session == "PRE" & RR1$Id == id),]$RRPositiveFoil
  }
  if(length(RR1[which(RR1$session == "SESSION3" & RR1$Id == id),]$RRPositiveFoil) != 0){
    RR_wide$session3_RRPositiveFoil[cnt] <- RR1[which(RR1$session == "SESSION3" & RR1$Id == id),]$RRPositiveFoil
  }
  if(length(RR1[which(RR1$session == "SESSION6" & RR1$Id == id),]$RRPositiveFoil) != 0){
    RR_wide$session6_RRPositiveFoil[cnt] <- RR1[which(RR1$session == "SESSION6" & RR1$Id == id),]$RRPositiveFoil
  }
  if(length(RR1[which(RR1$session == "PRE" & RR1$Id == id),]$RRNegative) != 0){
    RR_wide$pre_RRNegative[cnt] <- RR1[which(RR1$session == "PRE" & RR1$Id == id),]$RRNegative
  }
  if(length(RR1[which(RR1$session == "SESSION3" & RR1$Id == id),]$RRNegative) != 0){
    RR_wide$session3_RRNegative[cnt] <- RR1[which(RR1$session == "SESSION3" & RR1$Id == id),]$RRNegative
  }
  if(length(RR1[which(RR1$session == "SESSION6" & RR1$Id == id),]$RRNegative) != 0){
    RR_wide$session6_RRNegative[cnt] <- RR1[which(RR1$session == "SESSION6" & RR1$Id == id),]$RRNegative
  }
  if(length(RR1[which(RR1$session == "PRE" & RR1$Id == id),]$RRPositive) != 0){
    RR_wide$pre_RRPositive[cnt] <- RR1[which(RR1$session == "PRE" & RR1$Id == id),]$RRPositive
  }
  if(length(RR1[which(RR1$session == "SESSION3" & RR1$Id == id),]$RRPositive) != 0){
    RR_wide$session3_RRPositive[cnt] <- RR1[which(RR1$session == "SESSION3" & RR1$Id == id),]$RRPositive
  }
  if(length(RR1[which(RR1$session == "SESSION6" & RR1$Id == id),]$RRPositive) != 0){
    RR_wide$session6_RRPositive[cnt] <- RR1[which(RR1$session == "SESSION6" & RR1$Id == id),]$RRPositive
  }
  cnt <- cnt + 1
}

######add prime and cbm condition##########
neutralPrimeRR <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(RR_wide$Id))
length(neutralPrimeRR)

anxietyPrimeRR <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "ANXIETY" & cbmPrime$currentSession != "PRE",)]),unique(RR_wide$Id))
length(anxietyPrimeRR)


for(id in neutralPrimeRR){
  RR_wide$primeCondition[which(RR_wide$Id == id,)] <- "NEUTRAL"
}

for(id in anxietyPrimeRR){
  RR_wide$primeCondition[which(RR_wide$Id == id,)] <- "ANXIETY"
}


neutralRR <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(RR_wide$Id))
length(neutralRR)

positiveRR <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "POSITIVE" & cbmPrime$currentSession != "PRE",)]),unique(RR_wide$Id))
length(positiveRR)

fiftyRR <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "FIFTY_FIFTY" & cbmPrime$currentSession != "PRE",)]),unique(RR_wide$Id))
length(fiftyRR)


for(id in neutralRR){
  RR_wide$cbmCondition[which(RR_wide$Id == id,)] <- "No Scenario"
}

for(id in positiveRR){
  RR_wide$cbmCondition[which(RR_wide$Id == id,)] <- "POSITIVE"
}

for(id in fiftyRR){
  RR_wide$cbmCondition[which(RR_wide$Id == id,)] <- "FIFTY_FIFTY"
}


#####check range for wrong entries########
table(RR_wide$pre_RRNegative)
table(RR_wide$session3_RRNegative)
table(RR_wide$session6_RRNegative)
table(RR_wide$pre_RRPositive)
table(RR_wide$session3_RRPositive)
table(RR_wide$session6_RRPositive)

#add new variables for experimental conditons in 0-1 format########
RR_wide$CBM_FIFTY_FIFTY <- NA
RR_wide$CBM_POSITIVE <- NA
RR_wide$CBM_NONE <- NA
RR_wide$I_ANXIOUS <- NA
RR_wide$I_NEUTRAL <- NA

RR_wide$I_ANXIOUS <- 0 
for (i in 1:nrow(RR_wide)) { if
  (RR_wide$primeCondition[i]=='ANXIETY') {
    (RR_wide$I_ANXIOUS[i] <- 1)
  }}
RR_wide$I_NEUTRAL <- 0 
for (i in 1:nrow(RR_wide)) { if
  (RR_wide$primeCondition[i]=='NEUTRAL') {
    (RR_wide$I_NEUTRAL[i] <- 1)
  }}
RR_wide$CBM_FIFTY_FIFTY <- 0 
for (i in 1:nrow(RR_wide)) { if
  (RR_wide$cbmCondition[i]=='FIFTY_FIFTY') {
    (RR_wide$CBM_FIFTY_FIFTY[i] <- 1)
  }}
RR_wide$CBM_POSITIVE <- 0 
for (i in 1:nrow(RR_wide)) { if
  (RR_wide$cbmCondition[i]=='POSITIVE') {
    (RR_wide$CBM_POSITIVE[i] <- 1)
  }}
RR_wide$CBM_NONE <- 0 
for (i in 1:nrow(RR_wide)) { if
  (RR_wide$cbmCondition[i]=='No Scenario') {
    (RR_wide$CBM_NONE[i] <- 1)
  }}

###merge with mean credibility#####
Credibility_order2 <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/cleaned/Credibility_order2.csv",header=T)
Credibility_order2$X <- NULL
Credibility_order2$logical <- NULL
Credibility_order2$recommendable <- NULL
Credibility_order2$session <-NULL
Credibility_order2$X.1 <- NULL
names(Credibility_order2)[names(Credibility_order2) == 'participantRSA'] <- 'Id'
RR_Cred <- merge(RR_wide,Credibility_order2, by = 'Id')

####identify and remove all rows with NA in all RR scores########
RR_NA <- subset(RR_Cred,is.na(RR_Cred$pre_RRNegative) & is.na(RR_Cred$pre_RRNegativeFoil) & is.na(RR_Cred$pre_RRPositive) & is.na(RR_Cred$pre_RRPositiveFoil) & is.na(RR_Cred$session3_RRNegative) & is.na(RR_Cred$session3_RRNegativeFoil) & is.na(RR_Cred$session3_RRPositive) & is.na(RR_Cred$session3_RRPositiveFoil) & is.na(RR_Cred$session6_RRNegative) & is.na(RR_Cred$session6_RRNegativeFoil) & is.na(RR_Cred$session6_RRPositive) & is.na(RR_Cred$session6_RRPositiveFoil))
length(RR_NA$Id)
RR_NA$Id
RR_ohneNA <- RR_Cred[!(RR_Cred$Id %in% RR_NA$Id),]
length(RR_ohneNA$Id)
RR_ohneNA$Id

######check for number of rr scores in each session (without NA)#####
sum(!is.na(RR_ohneNA$pre_RRNegative))
sum(!is.na(RR_ohneNA$session3_RRNegative))
sum(!is.na(RR_ohneNA$session6_RRNegative))
sum(!is.na(RR_ohneNA$pre_RRPositive))
sum(!is.na(RR_ohneNA$session3_RRPositive))
sum(!is.na(RR_ohneNA$session6_RRPositive))

#####merge with pre OASiS#######
OASIS_final <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/cleaned/OASIS_final.csv",header=T)
OASIS_final$X.1 <- NULL
OASIS_final$X <- NULL
OASIS_final$primeCondition <- NULL
OASIS_final$cbmCondition <- NULL
OASIS_final$session1_OASIS <- NULL
OASIS_final$session2_OASIS <- NULL
OASIS_final$session3_OASIS <- NULL
OASIS_final$session4_OASIS <- NULL
OASIS_final$session5_OASIS <- NULL
OASIS_final$session6_OASIS <- NULL
OASIS_final$session7_OASIS <- NULL
OASIS_final$session8_OASIS <- NULL
OASIS_final$post_OASIS <- NULL
OASIS_final$CBM_FIFTY_FIFTY <- NULL
OASIS_final$CBM_POSITIVE <- NULL
OASIS_final$CBM_NONE <- NULL
OASIS_final$I_ANXIOUS <- NULL
OASIS_final$I_NEUTRAL <- NULL
OASIS_final$credibility_mean <- NULL
OASIS_final$control_OASIS <- NULL
RR_final_OA <- merge(RR_ohneNA,OASIS_final, by = 'Id',all.x = T)
RR_final_OA$X.3 <- NULL
RR_final_OA$X.2 <- NULL
RR_final_OA$X.1 <- NULL
RR_final_OA$X <- NULL
View(RR_final_OA)

write.csv(RR_final_OA, "RR_final.csv")


##########Cleaning BBSIQ#############################
BBSIQ <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/BBSIQ_recovered_Mar_27_2018.csv")
cbmPrime <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/ParticipantExportDAO_recovered_Mar_27_2018.csv")

########recode 'prefer not to answer' (555) with NA######
BBSIQ$breath_flu[BBSIQ$breath_flu==555] <- NA
BBSIQ$breath_physically[BBSIQ$breath_physically==555] <- NA
BBSIQ$breath_suffocate[BBSIQ$breath_suffocate==555] <- NA
BBSIQ$chest_heart[BBSIQ$chest_heart==555] <- NA
BBSIQ$chest_indigestion[BBSIQ$chest_indigestion==555] <- NA
BBSIQ$chest_sore[BBSIQ$chest_sore==555] <- NA
BBSIQ$confused_cold[BBSIQ$confused_cold==555] <- NA
BBSIQ$confused_outofmind[BBSIQ$confused_outofmind==555] <- NA
BBSIQ$confused_work[BBSIQ$confused_work==555] <- NA
BBSIQ$dizzy_ate[BBSIQ$dizzy_ate==555] <- NA
BBSIQ$dizzy_ill[BBSIQ$dizzy_ill==555] <- NA
BBSIQ$dizzy_overtired[BBSIQ$dizzy_overtired==555] <- NA
BBSIQ$friend_helpful[BBSIQ$friend_helpful==555] <- NA
BBSIQ$friend_incompetent[BBSIQ$friend_incompetent==555] <- NA
BBSIQ$friend_moreoften[BBSIQ$friend_moreoften==555] <- NA
BBSIQ$heart_active[BBSIQ$heart_active==555] <- NA
BBSIQ$heart_excited[BBSIQ$heart_excited==555] <- NA
BBSIQ$heart_wrong[BBSIQ$heart_wrong==555] <- NA
BBSIQ$jolt_burglar[BBSIQ$jolt_burglar==555] <- NA
BBSIQ$jolt_dream[BBSIQ$jolt_dream==555] <- NA
BBSIQ$jolt_wind[BBSIQ$jolt_wind==555] <- NA
BBSIQ$lightheaded_eat[BBSIQ$lightheaded_eat==555] <- NA
BBSIQ$lightheaded_faint[BBSIQ$lightheaded_faint==555] <- NA
BBSIQ$lightheaded_sleep[BBSIQ$lightheaded_sleep==555] <- NA
BBSIQ$party_boring[BBSIQ$party_boring==555] <- NA
BBSIQ$party_hear[BBSIQ$party_hear==555] <- NA
BBSIQ$party_preoccupied[BBSIQ$party_preoccupied==555] <- NA
BBSIQ$shop_bored[BBSIQ$shop_bored==555] <- NA
BBSIQ$shop_concentrating[BBSIQ$shop_concentrating==555] <- NA
BBSIQ$shop_irritating[BBSIQ$shop_irritating==555] <- NA
BBSIQ$smoke_cig[BBSIQ$smoke_cig==555] <- NA
BBSIQ$smoke_food[BBSIQ$smoke_food==555] <- NA
BBSIQ$smoke_house[BBSIQ$smoke_house==555] <- NA
BBSIQ$urgent_bill[BBSIQ$urgent_bill==555] <- NA
BBSIQ$urgent_died[BBSIQ$urgent_died==555] <- NA
BBSIQ$urgent_junk[BBSIQ$urgent_junk==555] <- NA
BBSIQ$vision_glasses[BBSIQ$vision_glasses==555] <- NA
BBSIQ$vision_illness[BBSIQ$vision_illness==555] <- NA
BBSIQ$vision_strained[BBSIQ$vision_strained==555] <- NA
BBSIQ$visitors_bored[BBSIQ$visitors_bored==555] <- NA
BBSIQ$visitors_engagement[BBSIQ$visitors_engagement==555] <- NA
BBSIQ$visitors_outstay[BBSIQ$visitors_outstay==555] <- NA

#######identify duplicates########
x <- BBSIQ[which(duplicated(BBSIQ[,c('participantRSA','session')])==T),]
length(x$participantRSA)
intersect(x$participantRSA,crctId)

#######check ID 1338 and delete later pre session######
BBSIQ <- BBSIQ[-c(2630),]

######create new dataset with mean scores (Negative and other), include all possible items#######
BBSIQScore <- data.frame(matrix(NA, nrow = dim(BBSIQ)[1], ncol = 4))
colnames(BBSIQScore) <- c('ID', 'session', 'Negative_Ave', 'Other_Ave')
cnt <- 1
for(id in crctId){
  sessions <- BBSIQ$session[BBSIQ$participantRSA == id]
  for(session in sessions){
    BBSIQScore$ID[cnt] <- id
    BBSIQScore$session[cnt] <- session
    Neg1 <- BBSIQ$visitors_bored[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg2 <- BBSIQ$shop_irritating[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg3 <- BBSIQ$smoke_house[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg4 <- BBSIQ$friend_incompetent[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg5 <- BBSIQ$jolt_burglar[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg6 <- BBSIQ$party_boring[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg7 <- BBSIQ$urgent_died[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg8 <- BBSIQ$breath_suffocate[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg9 <- BBSIQ$chest_heart[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg10 <- BBSIQ$confused_outofmind[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg11 <- BBSIQ$dizzy_ill[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg12 <- BBSIQ$heart_wrong[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg13 <- BBSIQ$lightheaded_faint[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Neg14 <- BBSIQ$vision_illness[BBSIQ$participantRSA == id & BBSIQ$session == session]
    BBSIQScore$Negative_Ave[cnt] <- rowMeans(cbind(Neg1,Neg2,Neg3,Neg4,Neg5,Neg6,Neg7,Neg8,Neg9,Neg10,Neg11,Neg12,Neg13,Neg14),na.rm=T)
    
    Oth1 <- BBSIQ$visitors_engagement[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth2 <- BBSIQ$visitors_outstay[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth3 <- BBSIQ$shop_bored[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth4 <- BBSIQ$shop_concentrating[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth5 <- BBSIQ$smoke_cig[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth6 <- BBSIQ$smoke_food[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth7 <- BBSIQ$friend_helpful[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth8 <- BBSIQ$friend_moreoften[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth9 <- BBSIQ$jolt_dream[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth10 <- BBSIQ$jolt_wind[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth11 <- BBSIQ$party_hear[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth12 <- BBSIQ$party_preoccupied[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth13 <- BBSIQ$urgent_bill[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth14 <- BBSIQ$urgent_junk[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth15 <- BBSIQ$breath_flu[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth16 <- BBSIQ$breath_physically[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth17 <- BBSIQ$vision_glasses[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth18 <- BBSIQ$vision_strained[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth19 <- BBSIQ$lightheaded_eat[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth20 <- BBSIQ$lightheaded_sleep[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth21 <- BBSIQ$chest_indigestion[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth22 <- BBSIQ$chest_sore[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth23 <- BBSIQ$heart_active[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth24 <- BBSIQ$heart_excited[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth25 <- BBSIQ$confused_cold[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth26 <- BBSIQ$confused_work[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth27 <- BBSIQ$dizzy_ate[BBSIQ$participantRSA == id & BBSIQ$session == session]
    Oth28 <- BBSIQ$dizzy_overtired[BBSIQ$participantRSA == id & BBSIQ$session == session]
    
    BBSIQScore$Other_Ave[cnt] <- rowMeans(cbind(Oth1,Oth2,Oth3,Oth4,Oth5,Oth6,Oth7,Oth8,Oth9,Oth10,Oth11,Oth12,Oth13,Oth14,Oth15,Oth16,Oth17,Oth18,Oth19,Oth20,Oth21,Oth22,Oth23,Oth24,Oth25,Oth26,Oth27,Oth28),na.rm=T)
    cnt <- cnt + 1
  }
}

#BBSIQScore <- BBSIQScore[complete.cases(BBSIQScore), ]
#length(unique(BBSIQScore$ID))

#####add cbm condition and prime condition as column####
BBSIQScore$cbmCondition <- NA
BBSIQScore$primeCondition <- NA

neutralPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQScore$ID))
length(neutralPrimeBBSIQ)

anxietyPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "ANXIETY" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQScore$ID))
length(anxietyPrimeBBSIQ)


for(id in neutralPrimeBBSIQ){
  BBSIQScore$primeCondition[which(BBSIQScore$ID == id,)] <- "NEUTRAL"
}

for(id in anxietyPrimeBBSIQ){
  BBSIQScore$primeCondition[which(BBSIQScore$ID == id,)] <- "ANXIETY"
}


neutralBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQScore$ID))
length(neutralBBSIQ)

positiveBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "POSITIVE" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQScore$ID))
length(positiveBBSIQ)

fiftyBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "FIFTY_FIFTY" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQScore$ID))
length(fiftyBBSIQ)


for(id in neutralBBSIQ){
  BBSIQScore$cbmCondition[which(BBSIQScore$ID == id,)] <- "No Scenario"
}

for(id in positiveBBSIQ){
  BBSIQScore$cbmCondition[which(BBSIQScore$ID == id,)] <- "POSITIVE"
}

for(id in fiftyBBSIQ){
  BBSIQScore$cbmCondition[which(BBSIQScore$ID == id,)] <- "FIFTY_FIFTY"
}

######add combined conition as column######
BBSIQScore$condition <- NA
BBSIQScore[which(BBSIQScore$cbmCondition == "FIFTY_FIFTY" & BBSIQScore$primeCondition == "NEUTRAL"),]$condition <- "FIFTY_FIFTY&NEUTRAL"
BBSIQScore[which(BBSIQScore$cbmCondition == "FIFTY_FIFTY" & BBSIQScore$primeCondition == "ANXIETY"),]$condition <- "FIFTY_FIFTY&ANXIETY"
BBSIQScore[which(BBSIQScore$cbmCondition == "POSITIVE" & BBSIQScore$primeCondition == "NEUTRAL"),]$condition <- "POSITIVE&NEUTRAL"
BBSIQScore[which(BBSIQScore$cbmCondition == "POSITIVE" & BBSIQScore$primeCondition == "ANXIETY"),]$condition <- "POSITIVE&ANXIETY"
BBSIQScore[which(BBSIQScore$cbmCondition == "No Scenario" & BBSIQScore$primeCondition == "NEUTRAL"),]$condition <- "NEUTRAL&NEUTRAL"
BBSIQScore[which(BBSIQScore$cbmCondition == "No Scenario" & BBSIQScore$primeCondition == "ANXIETY"),]$condition <- "NEUTRAL&ANXIETY"

#####calculate anova for baseline differences in BBSIQ#####
baselineBBSIQData <- BBSIQScore[which(BBSIQScore$session == "PRE"),]
summary(aov(Negative_Ave ~ condition, data = baselineBBSIQData))
summary(aov(Other_Ave ~ condition, data = baselineBBSIQData))


######Restructure from long to wide format##############################
library(reshape2)
library(plyr)
names(BBSIQScore)[names(BBSIQScore) == 'ID'] <- 'Id'
BBSIQ_wide <- data.frame(matrix(NA, nrow = length(unique(BBSIQScore$Id)), ncol = 13))
colnames(BBSIQ_wide) <- c('Id','primeCondition','cbmCondition', 'pre_Negative_Ave', 'pre_Other_Ave','session3_Negative_Ave','session3_Other_Ave', 'session6_Negative_Ave', 'session6_Other_Ave','session8_Negative_Ave','session8_Other_Ave','post_Negative_Ave', 'post_Other_Ave')
cnt <- 1

for(id in unique(BBSIQScore$Id)){
  BBSIQ_wide$Id[cnt] <- id
  if(length(BBSIQScore[which(BBSIQScore$session == "PRE" & BBSIQScore$Id == id),]$Negative_Ave) != 0){
    BBSIQ_wide$pre_Negative_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "PRE" & BBSIQScore$Id == id),]$Negative_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "SESSION3" & BBSIQScore$Id == id),]$Negative_Ave) != 0){
    BBSIQ_wide$session3_Negative_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "SESSION3" & BBSIQScore$Id == id),]$Negative_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "SESSION6" & BBSIQScore$Id == id),]$Negative_Ave) != 0){
    BBSIQ_wide$session6_Negative_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "SESSION6" & BBSIQScore$Id == id),]$Negative_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "SESSION8" & BBSIQScore$Id == id),]$Negative_Ave) != 0){
    BBSIQ_wide$session8_Negative_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "SESSION8" & BBSIQScore$Id == id),]$Negative_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "POST" & BBSIQScore$Id == id),]$Negative_Ave) != 0){
    BBSIQ_wide$post_Negative_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "POST" & BBSIQScore$Id == id),]$Negative_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "PRE" & BBSIQScore$Id == id),]$Other_Ave) != 0){
    BBSIQ_wide$pre_Other_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "PRE" & BBSIQScore$Id == id),]$Other_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "SESSION3" & BBSIQScore$Id == id),]$Other_Ave) != 0){
    BBSIQ_wide$session3_Other_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "SESSION3" & BBSIQScore$Id == id),]$Other_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "SESSION6" & BBSIQScore$Id == id),]$Other_Ave) != 0){
    BBSIQ_wide$session6_Other_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "SESSION6" & BBSIQScore$Id == id),]$Other_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "SESSION8" & BBSIQScore$Id == id),]$Other_Ave) != 0){
    BBSIQ_wide$session8_Other_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "SESSION8" & BBSIQScore$Id == id),]$Other_Ave
  }
  if(length(BBSIQScore[which(BBSIQScore$session == "POST" & BBSIQScore$Id == id),]$Other_Ave) != 0){
    BBSIQ_wide$post_Other_Ave[cnt] <- BBSIQScore[which(BBSIQScore$session == "POST" & BBSIQScore$Id == id),]$Other_Ave
  }
  cnt <- cnt + 1
}

#####add prime conditon and cbm condition as column####
neutralPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQ_wide$Id))
length(neutralPrimeBBSIQ)

anxietyPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "ANXIETY" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQ_wide$Id))
length(anxietyPrimeBBSIQ)


for(id in neutralPrimeBBSIQ){
  BBSIQ_wide$primeCondition[which(BBSIQ_wide$Id == id,)] <- "NEUTRAL"
}

for(id in anxietyPrimeBBSIQ){
  BBSIQ_wide$primeCondition[which(BBSIQ_wide$Id == id,)] <- "ANXIETY"
}


neutralBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQ_wide$Id))
length(neutralBBSIQ)

positiveBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "POSITIVE" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQ_wide$Id))
length(positiveBBSIQ)

fiftyBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "FIFTY_FIFTY" & cbmPrime$currentSession != "PRE",)]),unique(BBSIQ_wide$Id))
length(fiftyBBSIQ)


for(id in neutralBBSIQ){
  BBSIQ_wide$cbmCondition[which(BBSIQ_wide$Id == id,)] <- "No Scenario"
}

for(id in positiveBBSIQ){
  BBSIQ_wide$cbmCondition[which(BBSIQ_wide$Id == id,)] <- "POSITIVE"
}

for(id in fiftyBBSIQ){
  BBSIQ_wide$cbmCondition[which(BBSIQ_wide$Id == id,)] <- "FIFTY_FIFTY"
}


######check range of mean scores for wrong entries#######
table(BBSIQ_wide$pre_Negative_Ave)
table(BBSIQ_wide$session3_Negative_Ave)
table(BBSIQ_wide$session6_Negative_Ave)
table(BBSIQ_wide$pre_Other_Ave)
table(BBSIQ_wide$session3_Other_Ave)
table(BBSIQ_wide$session6_Other_Ave)

######add new columns for experimental conditions in 0-1 format#####
BBSIQ_wide$CBM_FIFTY_FIFTY <- NA
BBSIQ_wide$CBM_POSITIVE <- NA
BBSIQ_wide$CBM_NONE <- NA
BBSIQ_wide$I_ANXIOUS <- NA
BBSIQ_wide$I_NEUTRAL <- NA

BBSIQ_wide$I_ANXIOUS <- 0 
for (i in 1:nrow(BBSIQ_wide)) { if
  (BBSIQ_wide$primeCondition[i]=='ANXIETY') {
    (BBSIQ_wide$I_ANXIOUS[i] <- 1)
  }}
BBSIQ_wide$I_NEUTRAL <- 0 
for (i in 1:nrow(BBSIQ_wide)) { if
  (BBSIQ_wide$primeCondition[i]=='NEUTRAL') {
    (BBSIQ_wide$I_NEUTRAL[i] <- 1)
  }}
BBSIQ_wide$CBM_FIFTY_FIFTY <- 0 
for (i in 1:nrow(BBSIQ_wide)) { if
  (BBSIQ_wide$cbmCondition[i]=='FIFTY_FIFTY') {
    (BBSIQ_wide$CBM_FIFTY_FIFTY[i] <- 1)
  }}
BBSIQ_wide$CBM_POSITIVE <- 0 
for (i in 1:nrow(BBSIQ_wide)) { if
  (BBSIQ_wide$cbmCondition[i]=='POSITIVE') {
    (BBSIQ_wide$CBM_POSITIVE[i] <- 1)
  }}
BBSIQ_wide$CBM_NONE <- 0 
for (i in 1:nrow(BBSIQ_wide)) { if
  (BBSIQ_wide$cbmCondition[i]=='No Scenario') {
    (BBSIQ_wide$CBM_NONE[i] <- 1)
  }}

#####merge with mean credibility#####
Credibility_order2 <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/cleaned/Credibility_order2.csv",header=T)
Credibility_order2$X <- NULL
Credibility_order2$logical <- NULL
Credibility_order2$recommendable <- NULL
Credibility_order2$session <-NULL
Credibility_order2$X.1 <- NULL
names(Credibility_order2)[names(Credibility_order2) == 'participantRSA'] <- 'Id'
BBSIQ_Cred <- merge(BBSIQ_wide,Credibility_order2, by = 'Id')

#####delete all rows where all BBSIQ Scores = NA#######
BBSIQ_NA <- subset(BBSIQ_Cred,is.na(BBSIQ_Cred$pre_Negative_Ave) & is.na(BBSIQ_Cred$pre_Other_Ave) & is.na(BBSIQ_Cred$session3_Negative_Ave) & is.na(BBSIQ_Cred$session3_Other_Ave) & is.na(BBSIQ_Cred$session6_Negative_Ave) & is.na(BBSIQ_Cred$session6_Other_Ave) & is.na(BBSIQ_Cred$session8_Negative_Ave) & is.na(BBSIQ_Cred$session8_Other_Ave) & is.na(BBSIQ_Cred$post_Negative_Ave) & is.na(BBSIQ_Cred$post_Other_Ave))
length(BBSIQ_NA$Id)
BBSIQ_NA$Id
BBSIQ_ohneNA <- BBSIQ_Cred[!(BBSIQ_Cred$Id %in% BBSIQ_NA$Id),]
length(BBSIQ_ohneNA$Id)
BBSIQ_ohneNA$Id

######check number of BBSIQ Scores in each session without NA#####
sum(!is.na(BBSIQ_ohneNA$pre_Negative_Ave))
sum(!is.na(BBSIQ_ohneNA$session3_Negative_Ave))
sum(!is.na(BBSIQ_ohneNA$session6_Negative_Ave))
sum(!is.na(BBSIQ_ohneNA$pre_Other_Ave))
sum(!is.na(BBSIQ_ohneNA$session3_Other_Ave))
sum(!is.na(BBSIQ_ohneNA$session6_Other_Ave))

######merge with pre OASiS######
OASIS_final <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/cleaned/OASIS_final.csv",header=T)
OASIS_final$X.1 <- NULL
OASIS_final$X <- NULL
OASIS_final$primeCondition <- NULL
OASIS_final$cbmCondition <- NULL
OASIS_final$session1_OASIS <- NULL
OASIS_final$session2_OASIS <- NULL
OASIS_final$session3_OASIS <- NULL
OASIS_final$session4_OASIS <- NULL
OASIS_final$session5_OASIS <- NULL
OASIS_final$session6_OASIS <- NULL
OASIS_final$session7_OASIS <- NULL
OASIS_final$session8_OASIS <- NULL
OASIS_final$post_OASIS <- NULL
OASIS_final$CBM_FIFTY_FIFTY <- NULL
OASIS_final$CBM_POSITIVE <- NULL
OASIS_final$CBM_NONE <- NULL
OASIS_final$I_ANXIOUS <- NULL
OASIS_final$I_NEUTRAL <- NULL
OASIS_final$credibility_mean <- NULL
OASIS_final$control_OASIS <- NULL
BBSIQ_final_OA <- merge(BBSIQ_ohneNA,OASIS_final, by = 'Id',all.x=T)
head(BBSIQ_final_OA)
BBSIQ_final_OA$X.2 <- NULL
BBSIQ_final_OA$X.1 <- NULL
BBSIQ_final_OA$X <- NULL
write.csv(BBSIQ_final_OA, "BBSIQ_final.csv")



#########Cleaning DASS-AS########
dass21 <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/DatensatzR34_RAW/M_3_27_2018/M_3_27_2018/DASS21_AS_recovered_Mar_27_2018.csv",header=T)

####only DASS-AS from screening#####
sbDass21 <- dass21[which(dass21$session == "ELIGIBLE"),]

####check number of participants from final dataset####
length(intersect(crctId, sbDass21$participantDAO))

######recode 'not prefer to answer' (-1) as NA#######
sbDass21$breathing[sbDass21$breathing==-1] <- NA
sbDass21$dryness[sbDass21$dryness==-1] <- NA
sbDass21$heart[sbDass21$heart==-1] <- NA
sbDass21$panic[sbDass21$panic==-1] <- NA
sbDass21$scared[sbDass21$scared==-1] <- NA
sbDass21$trembling[sbDass21$trembling==-1] <- NA
sbDass21$worry[sbDass21$worry==-1] <- NA

######calculate final scores with (mean*7)*2, include all possible items#####
AS_1 <- sbDass21$breathing
AS_2 <- sbDass21$dryness
AS_3 <- sbDass21$heart
AS_4 <- sbDass21$panic
AS_5 <- sbDass21$scared
AS_6 <- sbDass21$trembling
AS_7 <- sbDass21$worry
sbDass21$sum <- (rowMeans(cbind(AS_1,AS_2,AS_3,AS_4,AS_5,AS_6,AS_7),na.rm=T))*7
sbDass21 = sbDass21[order(sbDass21[,'participantDAO'],-sbDass21[,'sum']),]
sbDass21 = sbDass21[!duplicated(sbDass21$participantDAO),]
dassScore <- data.frame(matrix(NA, nrow = dim(sbDass21)[1], ncol = 3))
colnames(dassScore) <- c('Id', 'session', 'score')
cnt <- 1
for(id in crctId){
  sessions <- sbDass21$session[sbDass21$participantDAO == id]
  for(session in sessions){
    dassScore$Id[cnt] <- id
    dassScore$session[cnt] <- session
    AS_1 <- sbDass21$breathing[sbDass21$participantDAO == id & sbDass21$session == session]
    AS_2 <- sbDass21$dryness[sbDass21$participantDAO == id & sbDass21$session == session]
    AS_3 <- sbDass21$heart[sbDass21$participantDAO == id & sbDass21$session == session]
    AS_4 <- sbDass21$panic[sbDass21$participantDAO == id & sbDass21$session == session]
    AS_5 <- sbDass21$scared[sbDass21$participantDAO == id & sbDass21$session == session]
    AS_6 <- sbDass21$trembling[sbDass21$participantDAO == id & sbDass21$session == session]
    AS_7 <- sbDass21$worry[sbDass21$participantDAO == id & sbDass21$session == session]
    dassScore$score[cnt] <- ((rowMeans(cbind(AS_1,AS_2,AS_3,AS_4,AS_5,AS_6,AS_7),na.rm=T))*7)*2
    cnt <- cnt + 1  
  }
}
 
 
View(dassScore)

#####add columns for cbm condition and prime condition#####
dassScore$cbmCondition <- NA
dassScore$primeCondition <- NA

neutralPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(dassScore$Id))
length(neutralPrimeBBSIQ)

anxietyPrimeBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$prime == "ANXIETY" & cbmPrime$currentSession != "PRE",)]),unique(dassScore$Id))
length(anxietyPrimeBBSIQ)


for(id in neutralPrimeBBSIQ){
  dassScore$primeCondition[which(dassScore$Id == id,)] <- "NEUTRAL"
}

for(id in anxietyPrimeBBSIQ){
  dassScore$primeCondition[which(dassScore$Id == id,)] <- "ANXIETY"
}


neutralBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "NEUTRAL" & cbmPrime$currentSession != "PRE",)]),unique(dassScore$Id))
length(neutralBBSIQ)

positiveBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "POSITIVE" & cbmPrime$currentSession != "PRE",)]),unique(dassScore$Id))
length(positiveBBSIQ)

fiftyBBSIQ <- intersect(unique(cbmPrime$id[which(cbmPrime$cbmCondition == "FIFTY_FIFTY" & cbmPrime$currentSession != "PRE",)]),unique(dassScore$Id))
length(fiftyBBSIQ)


for(id in neutralBBSIQ){
  dassScore$cbmCondition[which(dassScore$Id == id,)] <- "No Scenario"
}

for(id in positiveBBSIQ){
  dassScore$cbmCondition[which(dassScore$Id == id,)] <- "POSITIVE"
}

for(id in fiftyBBSIQ){
  dassScore$cbmCondition[which(dassScore$Id == id,)] <- "FIFTY_FIFTY"
}

####add column for combined condition#####
dassScore$condition <- NA
dassScore[which(dassScore$cbmCondition == "FIFTY_FIFTY" & dassScore$primeCondition == "NEUTRAL"),]$condition <- "FIFTY_FIFTY&NEUTRAL"
dassScore[which(dassScore$cbmCondition == "FIFTY_FIFTY" & dassScore$primeCondition == "ANXIETY"),]$condition <- "FIFTY_FIFTY&ANXIETY"
dassScore[which(dassScore$cbmCondition == "POSITIVE" & dassScore$primeCondition == "NEUTRAL"),]$condition <- "POSITIVE&NEUTRAL"
dassScore[which(dassScore$cbmCondition == "POSITIVE" & dassScore$primeCondition == "ANXIETY"),]$condition <- "POSITIVE&ANXIETY"
dassScore[which(dassScore$cbmCondition == "No Scenario" & dassScore$primeCondition == "NEUTRAL"),]$condition <- "NEUTRAL&NEUTRAL"
dassScore[which(dassScore$cbmCondition == "No Scenario" & dassScore$primeCondition == "ANXIETY"),]$condition <- "NEUTRAL&ANXIETY"

dassScore <- dassScore[!(duplicated(dassScore[, c("Id", "condition")])),]

######calculate anova for baseline differences and descriptive statistics#####
summary(aov(score ~ condition, data = dassScore))
range(dassScore$score,na.rm=T)
mean(dassScore$score,na.rm=T)
sd(dassScore$score,na.rm=T)
aggregate( score ~ condition, dassScore, mean )
aggregate( score ~ condition, dassScore, sd )

write.csv(dassScore,"DASS-AS.csv")



##########################################################

#####split datasets by group - RR########
RR_final <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/cleaned/RR_final.csv",header=T)
databygroup <- split(RR_final, f = RR_final$cbmCondition)
databygroupPOS <- databygroup$POSITIVE
databygroup50 <- databygroup$FIFTY_FIFTY
databygroupNONE <- databygroup$`No Scenario`

databygroupPOS$X <- NULL
databygroupPOS$CBM_FIFTY_FIFTY <- NULL
databygroupPOS$CBM_POSITIVE <- NULL
databygroupPOS$CBM_NONE <- NULL

databygroup50$X <- NULL
databygroup50$CBM_FIFTY_FIFTY <- NULL
databygroup50$CBM_POSITIVE <- NULL
databygroup50$CBM_NONE <- NULL

databygroupNONE$X <- NULL
databygroupNONE$CBM_FIFTY_FIFTY <- NULL
databygroupNONE$CBM_POSITIVE <- NULL
databygroupNONE$CBM_NONE <- NULL

write.csv(databygroup50,"RR_Only_50.csv")
write.csv(databygroupPOS,"RR_Only_POS.csv")
write.csv(databygroupNONE,"RR_Only_NONE.csv")


########divide by group - BBSIQ#################
BBSIQ_final <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/cleaned/BBSIQ_final.csv",header=T)
databygroup <- split(BBSIQ_final, f = BBSIQ_final$cbmCondition)
databygroupPOS <- databygroup$POSITIVE
databygroup50 <- databygroup$FIFTY_FIFTY
databygroupNONE <- databygroup$`No Scenario`

databygroupPOS$X <- NULL
databygroupPOS$CBM_FIFTY_FIFTY <- NULL
databygroupPOS$CBM_POSITIVE <- NULL
databygroupPOS$CBM_NONE <- NULL

databygroup50$X <- NULL
databygroup50$CBM_FIFTY_FIFTY <- NULL
databygroup50$CBM_POSITIVE <- NULL
databygroup50$CBM_NONE <- NULL

databygroupNONE$X <- NULL
databygroupNONE$CBM_FIFTY_FIFTY <- NULL
databygroupNONE$CBM_POSITIVE <- NULL
databygroupNONE$CBM_NONE <- NULL

write.csv(databygroup50,"BBSIQ_Only_50.csv")
write.csv(databygroupPOS,"BBSIQ_Only_POS.csv")
write.csv(databygroupNONE,"BBSIQ_Only_NONE.csv")

#########divided by group - OASIS#############
OASIS_final <- read.csv("C:/Users/Nicola/Documents/Virginia/Data analysis/Credibility/Analysis Sonia/Nicola - R34/cleaned/OASIS_final.csv",header=T)
databygroup <- split(OASIS_final, f = OASIS_final$cbmCondition)
databygroupPOS <- databygroup$POSITIVE
databygroup50 <- databygroup$FIFTY_FIFTY
databygroupNONE <- databygroup$`No Scenario`

databygroupPOS$X <- NULL
databygroupPOS$CBM_FIFTY_FIFTY <- NULL
databygroupPOS$CBM_POSITIVE <- NULL
databygroupPOS$CBM_NONE <- NULL

databygroup50$X <- NULL
databygroup50$CBM_FIFTY_FIFTY <- NULL
databygroup50$CBM_POSITIVE <- NULL
databygroup50$CBM_NONE <- NULL

databygroupNONE$X <- NULL
databygroupNONE$CBM_FIFTY_FIFTY <- NULL
databygroupNONE$CBM_POSITIVE <- NULL
databygroupNONE$CBM_NONE <- NULL


write.csv(databygroup50,"OASIS_Only_50.csv")
write.csv(databygroupPOS,"OASIS_Only_POS.csv")
write.csv(databygroupNONE,"OASIS_Only_NONE.csv")


