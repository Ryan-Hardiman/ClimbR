#Below we scrape tide data from seeker.gg
Website <- read_html("https://seeker.gg/Tides") %>% html_table(fill=TRUE)%>% pluck(1) #Grabs the first table from Seeker.gg

Times <- (Website %>% pluck(2))[-1]%>% hm() #Gets second column of website and removes the first element, Gives the high and low tide times

Tides <- gsub('.{1}$','',c((Website%>%pluck(3))[-1])) # removes the "m" from tide heights, Gives the high and low tide heights

Tide_States <- ifelse(Website$Today[-1]=="\u25BE","l","h")# Finds the tide state

Tide_Index<-match(min(abs(period_to_seconds(ClimbTime-Times))),abs(period_to_seconds(ClimbTime-Times))) #Finds the closest tide to when you are looking to climb

Tide_Index_Second<-match(min(abs(period_to_seconds(ClimbTime + ClimbDuration-Times[-Tide_Index]))),abs(period_to_seconds(ClimbTime+ClimbDuration-Times))) #Gets second closest time

ifelse(Tide_Index-Tide_Index_Second>1,"Must use All Seasons", "OK") #Notic this check!!!

Tide_Table <- data.frame(Time=Times, Tide=Tides, Tide_State=Tide_States) %>% mutate(Tide =  as.numeric(Tide))

Tide_Season <- ifelse(max(Tide_Table$Tide)-min(Tide_Table$Tide)>6,"Spring","Neap") #Returns the Tide Season


TideWave <- function(x){
  (Tide_Table$Tide[Tide_Index] - Tide_Table$Tide[max(1,Tide_Index + 1 %% 4)]) / 2 * cos(
    ifelse(Tide_Table$Tide_State[Tide_Index] == "h", 0 , pi) +pi * period_to_seconds(x) / period_to_seconds(Tide_Table$Time[max(1,Tide_Index + 1 %% 4)] -Tide_Table$Time[Tide_Index])
  ) + (Tide_Table$Tide[max(1,Tide_Index + 1 %% 4)] + Tide_Table$Tide[Tide_Index]) / 2}
