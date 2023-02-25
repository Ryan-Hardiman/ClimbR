Add_Lead <- function(ClimbName, ClimbDate){
  Island_Climbing$Lead[Island_Climbing$`Climb Name`==ClimbName] <- "Y"
  
  Island_Climbing$Date_Lead[Island_Climbing$`Climb Name`==ClimbName] <- ClimbDate
}

Add_Second <- function(ClimbName, ClimbDate){
  Island_Climbing$`2nd`[Island_Climbing$`Climb Name`==ClimbName] <- "Y"
  
  Island_Climbing$Date_Seconded[Island_Climbing$`Climb Name`==ClimbName] <- ClimbDate
}