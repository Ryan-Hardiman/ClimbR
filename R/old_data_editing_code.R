#Island_Climbing <- read_excel(paste0(here(),"/data/Island-Climbing.xlsx"), 
#                             sheet = "Ride the Tide")
#Island_Climbing$`Climb Grade`[Island_Climbing$`Climb Grade` == "Vs 4c"] <- "VS 4c"
#Island_Climbing$`Climb Grade`[Island_Climbing$`Climb Grade` == "Vs 5a"] <- "VS 5a"
#Island_Climbing$`Climb Grade`[Island_Climbing$`Climb Grade` == "Vs 4b"] <- "VS 4b"
#Island_Climbing$`Climb Grade`[Island_Climbing$`Climb Grade` == "Hs 4b"] <- "HS 4b"
#Island_Climbing$`Tide Height`[Island_Climbing$`Tide Height`=="HIgh"]<-"High"
#Island_Climbing$Latitide[Island_Climbing$Latitide == ""] <- NA
#Island_Climbing$Latitide <- na.locf(Island_Climbing$Latitide)
#Island_Climbing$Longitude[Island_Climbing$Longitude == ""] <- NA
#Island_Climbing$Longitude <- na.locf(Island_Climbing$Longitude)
#Island_Climbing <- Island_Climbing %>% drop_na(`Climb Name`)
#Island_Climbing$Longitude[Island_Climbing$`Name Of Area`=="The Sphinx"] <- -2.673698
#Island_Climbing$Longitude <- as.numeric(Island_Climbing$Longitude)
#Island_Climbing <- Island_Climbing%>% mutate(Date_Lead=Date...11)%>%select(-Date...11)
#Island_Climbing <- Island_Climbing%>% mutate(Date_Seconded=Date...13)%>%select(-Date...13)
##====
#Island_Climbing <- Island_Climbing %>% mutate(Overall_Grade_1 =str_match(Island_Climbing$`Climb Grade`,"E[1-9]{1,3}|[MVDHS]{1,3}"), #finds the first overall climb grade
#                                              Overall_Grade_2 =str_match(Island_Climbing$`Climb Grade`,"(?<=\\/)E[1-9]|(?<=\\/)[MVDHS]{1,3}"), #finds the second overall climb grade
#                                              Hardest_Move_1 =str_match(Island_Climbing$`Climb Grade`,"[1-9][a-c]{1}"), #finds the first hardest move
#                                              Hardest_Move_2 =str_match(Island_Climbing$`Climb Grade`,"(?<=\\/)[1-9][a-c]")) #finds the second hardest move
#
##Removing the General location
#Island_Climbing <- Island_Climbing %>% mutate(`General Location`=NULL)
#Island_Climbing <- Island_Climbing %>% mutate(`Climb Grade` = NULL)
#
##Saving as an RDS
#write_rds(Island_Climbing,file=paste0(here(),"/data/Island_Climbing.rds"))
#




#=======




