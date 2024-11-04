#########Crab Team Network data on PAMA##########

#######Libraries
library(plyr) #ddply
library(lubridate) #month, day, year functions


trap.dat <- read.csv("~/Documents/GitHub/pama/Data/TrappingData.11.4.24.eg.csv", header = T)

trap.dat$IsCompromised <- as.factor(trap.dat$IsCompromised)
trap.dat.effort <- trap.dat[trap.dat$IsCompromised == "N",] # Remove compromised and effort only traps

effort <- aggregate(numeric(nrow(trap.dat.effort)), 
                    trap.dat.effort[c("SiteID", "SetTime", "TrapType", "TrapNumber")], 
                    length) #Produces list of unique traps

effort$SetTime <- as.POSIXlt(effort$SetTime, format = "%m/%d/%y %H:%M")

trapsbyyear <- ddply(effort, c(effort$SiteID, year(effort$SetTime), length()
  )
)