# Cleaning script for R34 (MindTrails: Managing Anxiety) data for Sonia and Julie
library(plyr)
library(dplyr)
library(reshape2)

setwd("~/Documents/Projects/MindTrails/R34/consortDiagram/ManagingAnxiety_FINAL/Raw Data")

#------------------------------------------------------------------------------#
# "Participant" table                                                          #
#------------------------------------------------------------------------------#
participant <- read.csv("ParticipantExport_recovered_Feb_02_2019.csv", header = TRUE)
participant$participantID <- participant$id

# Find the column values
count(participant$active)
count(participant$admin)
count(participant$email_optout)
count(participant$increase30)
count(participant$over18)

# Replace column values with TRUE or FALSE
part1 <- mutate(participant, active = ifelse(active=="\001","TRUE","FALSE"))
part1 <- mutate(part1, admin = ifelse(admin=="\001","TRUE","FALSE"))
part1 <- mutate(part1, email_optout = ifelse(email_optout=="\001","TRUE","FALSE"))
part1 <- mutate(part1, increase30 = ifelse(increase30=="\001","TRUE","FALSE"))
part1 <- mutate(part1, over18 = ifelse(over18=="\001","TRUE","FALSE"))

# Remove admins, minors, scam accounts, and test accounts
part2 <- filter(part1, admin=="FALSE" & over18=="TRUE")
part3 <- filter(part2, participantID<20 | participantID>420)

# There was no variable to identify test accounts, so I manually searched for test IDs in the database 
# and removed accounts associated with PACTlab members or if they had the word "test" in their name/email
test <- c(1,2,4,5,441,450,538,540,578,610,624,718,767,753,775,847,848,926,971,1014,1020:1026,
          1031:1033,1038,1058,1187,1213,1215,1220:1226,1232,1263,1270,1288,1309,1338,1407,
          1486:1488,1490,1499,1500,1608,1631,1740,1767,1817:1819,1831,1899,1900,1968,1971) 

part4 <- part3[!part3$participantID%in%test,]
part5 <- part4[,c(4,5,9,16)]

length(unique(part4$participantID)) # There are no duplicates

# summary <- unique(part4) %>%
#   group_by(participantID) %>% 
#   summarise(count=n())
# summarySubset <- subset(summary, summary$count == 0)

IDlist <- c(part4$participantID) #1496 participants

clean_data <- "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/ManagingAnxiety_final/Clean Data"
write.csv(part4, file.path(clean_data, "ParticipantExport_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "Demographics" table                                                         #
#------------------------------------------------------------------------------#
dem <- read.csv("Demographic_recovered_Feb_02_2019.csv", header = TRUE)

# Fix ID numbers for three participants
levels(dem$participantRSA) <- c(levels(dem$participantRSA),"534","535","536")
dem$participantRSA[dem$participantRSA=="NvorEV/x1Rn9FC1yuqkfDjcDlSRj9b8Y3ji78LPNHI8vzrvOJdQjRTpAY+Mn/C8pToWmJmY4luS8P3Rfcfk/hcZGxUwzWpbkJjuKA0q8E4Y6IPxJmLdiO2zFYh8BJKEr4j8pidKyMooi2REL5hcNeJKPpkId4R0iBpLgPXfapA+XnRqHQftVZo+JL1TD8eP362JX7A0L90OxpqkuOp3lnEce2ox6vcc4fwTsvoWy+utocR+Fp7Pgct/wBIdjvKhSLJvabMK7Ffglk9OrAIXuxj77+n+sbsW4PoGc9a2UZu5D18WKGrUW6kdLTBCDqE/DyWjcLXEGWCwCwlTdRtPtjQ=="] <- "534"
dem$participantRSA[dem$participantRSA=="j5J6cs0Yromqio+zSNruW1qA1athdsLV4AQf6AnWnhVS2M+SYJ3bEgBG9GPQDZMLN4Ttqyk76HO+x1QXEdL/FtgkyyYy82aR4j5lYJ0IeamTIK5atLtT0MZJQqhhehLp5y97TvJjootnTj4OV+l0iaxAMKxzcIqadlZ7FzD5uWDTqJCzxX/Bny7V01irJfqHgEEpxNwwLmJttjgfr3ifWWv7JseWIgPPIH1spGFUWN127vCvKmZ3bmbYCLq119q5Nn11jKe+Oq2oSvv7HPczDb/knU3Uu7Y8Js01nK+VtX6kFYAACzwZcu6GN+KGuUUexlxMJXCaLBQvEdU3Iy7lVQ=="] <- "535"
dem$participantRSA[dem$participantRSA=="VS9LPKcqvwakVj2Orq/tFLN+JB9nmbaZ5kqg0gDXF7CQ4NKpey6kMSnEErlvr3UrnJKYgKGEAk3wi0zijUZDNS5+Nzys2L7ynDGXQoE61RO07m2dqMsxZGHgv8qadNtfIyKxpjYaNoblp0d3VJ8sSce08XBmVxMfr7nLbDwC60Zm1vsBajDqX073gc3U35cq+n4vNG5v1t1zPQFqeRvXsTHGc4naet1I8PeQEcLtQJN2daxdfgZplToaOzEW7KGhdzgYYRyTwWCvwh8wHUcdmV4dW7VG3oGLvz35wLYVkOlpZMgZ+5eAr9an4JDaXwR1e3CeHYJHRDxSGibqK3rl/Q=="] <- "536"

dem$participantID <- as.numeric(as.character(dem$participantRSA))

# Fix birth years
dem$birthYear[dem$birthYear==19019590] <- 1959
dem$birthYear[dem$birthYear==19011968] <- 1968
dem$birthYear[dem$birthYear==19001997] <- 1997
dem$birthYear[dem$birthYear==19001989] <- 1989
dem$birthYear[dem$birthYear==19001987] <- 1987
dem$birthYear[dem$birthYear==19001986] <- 1986
dem$birthYear[dem$birthYear==19001981] <- 1981
dem$birthYear[dem$birthYear==19001977] <- 1977
dem$birthYear[dem$birthYear==19001976] <- 1976
dem$birthYear[dem$birthYear==19001975] <- 1975
dem$birthYear[dem$birthYear==19001959] <- 1959
dem$birthYear[dem$birthYear==1972900] <- 1972
dem$birthYear[dem$birthYear==1900955] <- 1955
dem$birthYear[dem$birthYear==195100] <- 1951
dem$birthYear[dem$birthYear==2964] <- 1964
dem$birthYear[dem$birthYear==2222] <- NA
dem$birthYear[dem$birthYear==0] <- NA
dem$birthYear[dem$birthYear==97] <- 1997
dem$birthYear[dem$birthYear==92] <- 1992
dem$birthYear[dem$birthYear==90] <- 1990
dem$birthYear[dem$birthYear==89] <- 1989
dem$birthYear[dem$birthYear==87] <- 1987
dem$birthYear[dem$birthYear==86] <- 1986
dem$birthYear[dem$birthYear==84] <- 1984
dem$birthYear[dem$birthYear==82] <- 1982
dem$birthYear[dem$birthYear==81] <- 1981
dem$birthYear[dem$birthYear==80] <- 1980
dem$birthYear[dem$birthYear==77] <- 1977
dem$birthYear[dem$birthYear==72] <- 1972
dem$birthYear[dem$birthYear==70] <- 1970
dem$birthYear[dem$birthYear==64] <- 1964
dem$birthYear[dem$birthYear==61] <- 1961
dem$birthYear[dem$birthYear==58] <- 1958

# Remove everyone except the real participants
dem1 <- subset(dem, participantID %in% IDlist)

# Fix values
count(dem1$education)
dem1[dem1[,]==""] <- NA
dem1$education[dem1$education=="????"] <- NA
dem1$education[dem1$education=="Diploma di scuola superiore"] <- "High School Graduate"
dem1$education[dem1$education=="Un lycée"] <- "Some High School"

count(dem1$employmentStatus)
dem1$employmentStatus[dem1$employmentStatus=="????"] <- NA
dem1$employmentStatus[dem1$employmentStatus=="Étudiant"] <- "Student"
dem1$employmentStatus[dem1$employmentStatus=="Lavoro part-time"] <- "Working part-time"

count(dem1$ethnicity)
dem1$ethnicity[dem1$ethnicity=="??????????"] <- NA
dem1$ethnicity[dem1$ethnicity=="Inconnu"] <- "Unknown"
dem1$ethnicity[dem1$ethnicity=="Sconosciuto"] <- "Unknown"

count(dem1$gender)
dem1$gender[dem1$gender=="?"] <- NA
dem1$gender[dem1$gender=="Femmina"] <- "Female"
dem1$gender[dem1$gender=="Mâle"] <- "Male"

count(dem1$income)
dem1$income[dem1$income=="$ 5,000 a $ 11999"] <- "$5,000 through $11,999"
dem1$income[dem1$income=="$ 50,000??$ 74,999"] <- "$50,000 through $74,999"
dem1$income[dem1$income=="Moins de 5 000 $"] <- "Less than $5,000"

count(dem1$maritalStatus)
dem1$maritalStatus[dem1$maritalStatus=="??"] <- NA
dem1$maritalStatus[dem1$maritalStatus=="Single, ma attualmente fidanzato"] <- "Single, but currently engaged to be married"
dem1$maritalStatus[dem1$maritalStatus=="Unique"] <- "Single"

count(dem1$participateReason)
dem1$participateReason[dem1$participateReason=="??????????????"] <- NA
dem1$participateReason[dem1$participateReason=="Basta navigare sul web"] <- "Just surfing the web"
dem1$participateReason[dem1$participateReason=="Lien depuis un blog, un chat ou un fil de discussion"] <- "Link from blog, chat, or discussion thread"

count(dem1$race)
dem1$race[dem1$race=="?/????"] <- "NA"
dem1$race[dem1$race=="Bianco / origine europea"] <- "White/European origin"
dem1$race[dem1$race=="Blanc / origine européenne"] <- "White/European origin"

dem_dup <- dem1[(duplicated(dem1[, c("participantID")])),] # no duplicates

write.csv(dem1, file.path(clean_data, "Demographics_02_02_2019.csv"), row.names=FALSE)

dem_final <- subset(dem1,participantID %in% IDlist) #1308 participants

#------------------------------------------------------------------------------#
# "Recognition Ratings" table                                                  #
#------------------------------------------------------------------------------#

RR <- read.csv("RR_recovered_Feb_02_2019.csv", header = TRUE)
RR[68, "participantRSA"] <- 532 #Insert correct participantRSA
RR$participantID <- as.numeric(as.character(RR$participantRSA))

is.na(RR) <- RR == -1
RR1 <- subset(RR,participantID %in% IDlist)
rr <- RR1[(duplicated(RR1[, c("id","session")])),] #No duplicates

write.csv(RR1, file.path(clean_data, "RR_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "BBSIQ" table                                                                #
#------------------------------------------------------------------------------#

BBSIQ <- read.csv("BBSIQ_recovered_Feb_02_2019.csv", header = TRUE)
BBSIQ[68, "participantRSA"] <- 532 #Insert correct participantRSA
BBSIQ$participantID <- as.numeric(as.character(BBSIQ$participantRSA))
# BBSIQ <- BBSIQ %>%
#   select(participantID, id,participantRSA,everything())
# is.na(BBSIQ[,4:46]) <- BBSIQ[,4:46] == 555
BBSIQ1 <- subset(BBSIQ,participantID %in% IDlist)
bb <- BBSIQ1[(duplicated(BBSIQ1[, c("participantID","session")])),] #No duplicates

write.csv(BBSIQ1, file.path(clean_data, "BBSIQ_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "Task Log" table                                                             #
#------------------------------------------------------------------------------#
tasklog <- read.csv("TaskLog_02_02_2019.csv", header = TRUE)
tasklog$participantID <- tasklog$participantdao_id
tasklog1 <- subset(tasklog, participantID %in% IDlist)

write.csv(tasklog1, file.path(clean_data, "TaskLog_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "OASIS" table                                                                #
#------------------------------------------------------------------------------#
OA <- read.csv("OA_recovered_Feb_02_2019.csv", header = TRUE)
OA$participantID <- OA$participantDAO

# oa <- oa %>%
#   select(id, everything())
# is.na(oa[,2:7]) <- oa[,2:7] == 555

OA1 <- subset(OA,participantID %in% IDlist)
oa_dup <- OA1[(duplicated(OA1[, c("participantID","session")])),] #no duplicates

write.csv(OA1, file.path(clean_data, "OA_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "DASS AS" table                                                              #
#------------------------------------------------------------------------------#
dassa <- read.csv("DASS21_AS_recovered_Feb_02_2019.csv", header = TRUE)
dassa$participantID <- as.numeric(as.character(dassa$participantDAO))

is.na(dassa) <- dassa == -1; is.na(dassa) <- dassa == -2
dassa1 <- subset(dassa,participantID %in% IDlist)

#test 
DASS_pre <- filter(dassa1,session == "ELIGIBLE")
DASS_other <- filter(dassa1, session != "ELIGIBLE")

dassa_pre <-DASS_pre[!rev(duplicated(rev(DASS_pre$participantID))),]
dassa2 <- rbind(DASS_other, dassa_pre)
#

# dassadup <- dassa1[(duplicated(dassa1[, c("participantID","session")])),]
# dassa2 <-dassa1[!rev(duplicated(rev(dassa1$participantID))),]
# dassa2 <- dassa1[!rev(duplicated(dassa1[,c("participantID")])),]

write.csv(dassa2, file.path(clean_data, "DASS21_AS_02_02_2019.csv"), row.names=FALSE)

DASSA_final <- subset(dassa2,participantID %in% IDlist) #1537 participants

#------------------------------------------------------------------------------#
# "DASS DS" table                                                              #
#------------------------------------------------------------------------------#
dassd <- read.csv("DASS21_DS_recovered_Feb_02_2019.csv", header = TRUE)

dassd[68, "participantRSA"] <- 532 #Insert correct participantRSA
dassd$participantID <- as.numeric(as.character(dassd$participantRSA))
dassd <- dassd %>%
  select(participantID,participantRSA,id, everything())
is.na(dassd[,4:11]) <- dassd[,4:11] == -1
dassd1 <- subset(dassd,participantID %in% IDlist)
dassddup <- dassd1[(duplicated(dassd1[, c("participantID","session")])),] #NONE

write.csv(dassd1, file.path(clean_data, "DASS21_DS_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# Prepare data for analyses                                                    #
#------------------------------------------------------------------------------#

# Exclude participants with no DASS AS score or DASS AS score < 10
DASSA_final$DASS_sum <- rowSums(DASSA_final[,c(1,3,4,6,9,12,13)], na.rm = TRUE)
DASSA_final$DASS_A <- DASSA_final$DASS_sum*2
DASSA_elig <- filter(DASSA_final, session == "ELIGIBLE")

# Are there any less than 10? Yes, one participant (1644)
clau_dassa <- filter(DASSA_elig, DASS_A >= 10)
final1 <- inner_join(part5, clau_dassa, by = "participantID") #1421 participants

# Exclude participants with no Demographics data
final2 <- inner_join(final1, dem_final, by = "participantID") #1240 participants

# Exclude participants with no Task Log data
tasklog_IDs <- tasklog1$participantID
final3 <- subset(final2, participantID%in%tasklog_IDs)
length(unique(final3$participantID)) # 1240 participants = intersection bw DASSAS, DEM, and TASKLOG

#Exclude participants who have not completed PRE session
final4 <- filter(final3, current_session != "PRE") #901 participants

# Compare data
Julie_participants <- read.csv("10FebParticipants.csv", header = TRUE) # Read in Sonia's participants
Julie_participants$participantID <- Julie_participants$id

d1 <- as.data.frame(final4$participantID)
d2 <- as.data.frame(Julie_participants$participantID)
d1$participantID <- d1$'final4$participantID'
d2$participantID <- d2$'Julie_participants$participantID'
d3 <- full_join(d1,d2, by = "participantID")

no_pre <- c(9,11,12,14,17,421,425,434,491,503,598,1014,1038,1698,1767) #These Ps were not included in the original analyses but still in Sonia's data
test <- c(450,718,971,1014,1038,1213,1220,1222,1223,1224,1338,1740,1767,1817,1818,1819) #These Ps were found to actually be test accounts after the original analyses
no_dassa <- c(701,756,973,997,1112,1259,1290,1354,1662) #These Ps do not have DASSA ELIGIBLE data
d3$notes <- ifelse(d3$participantID > 1910, 'Not included in original dataset due to server error', 
            ifelse(d3$participantID %in% no_pre, 'No RR PRE session data, so they were excluded from original analyses anyways',
            ifelse(d3$participantID==532,'Originally thought to have no RR PRE data, but does',
            ifelse(d3$participantID %in% test, 'Test account found after original analyses were run', 
            ifelse(d3$participantID %in% no_dassa, 'No DASSA PRE data',
            ifelse(d3$participantID==1644, 'DASSA PRE score < 10',
            ifelse(d3$`final4$participantID`==d3$`Julie_participants$participantID`, 'IDs in both datasets',NA)))))))
write.csv(d3, file = "notes.csv", row.names=FALSE)
