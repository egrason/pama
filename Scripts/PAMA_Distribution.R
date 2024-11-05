######################################################
#########Crab Team Network data on PAMA##########
######################################################

######################################################
#######Libraries
library(plyr) #ddply
library(lubridate) #month, day, year functions

######################################################
####### Data
trap.dat <- read.csv("~/Documents/GitHub/pama/Data/TrappingData.11.4.24.eg.csv", header = T)

trap.dat$IsCompromised <- as.factor(trap.dat$IsCompromised)
trap.dat.effort <- trap.dat[trap.dat$IsCompromised == "N",] # Remove compromised and effort only traps

######################################################
#######PAMA Abundance by site and Year for mapping

#DF of total Trap sets per year per site
effort <- aggregate(numeric(nrow(trap.dat.effort)), 
                    trap.dat.effort[c("SiteID", "SetTime", "TrapType", "TrapNumber")], 
                    length) #Produces list of unique traps

effort$EndTime <- as.POSIXlt(effort$EndTime, format = "%m/%d/%y %H:%M")
effort$Year <- year(effort$EndTime)
effort$EndTime <- as.character(effort$EndTime)

trapsbyyear <- ddply(effort, c("SiteID", "Year"), 
                               function(df) {
                                 return(
                                   c(
                                     trap.sets = length(df$SiteID)
                                   )
                                 )
                               }
  )


######DF of total PAMA sets per year per site
trap.pama <- trap.dat.effort[trap.dat.effort$Species == "PAMA",]
trap.pama$EndTime <- as.POSIXlt(trap.pama$EndTime, format = "%m/%d/%y %H:%M")
trap.pama$Year <- year(trap.pama$EndTime)
trap.pama$Month <- month(trap.pama$EndTime)

trap.pama$EndTime <- as.character(trap.pama$EndTime)

pama <- ddply(trap.pama, c("SiteID", "Year"),
              function(df) {
                return(
                  c(
                    total.pama = sum(df$TotalNumber)
                  )
                )
              }
)

pama.cpue <- merge(trapsbyyear, pama, 
                     by = ,
                     all.x = TRUE,
                     all.y = TRUE)

pama.cpue[is.na(pama.cpue)] <- 0
pama.cpue$CPUE <- (100*pama.cpue$total.pama/pama.cpue$trap.sets)

write.csv(pama.cpue, file = "Network PAMA CPUE.csv")

######################################################
#######PAMA Seasonal Abundance Patterns

######################
# Effort by Month
effort <- aggregate(numeric(nrow(trap.dat.effort)), 
                    trap.dat.effort[c("SiteID", "Month", "EndTime", "TrapType", "TrapNumber")], 
                    length) #Produces list of unique traps with x = number of unique species within the trap
effort$EndTime <- as.POSIXlt(effort$EndTime, format = "%m/%d/%y %H:%M")
effort$Year <- year(effort$EndTime)
effort$Month <- month(effort$EndTime)

effort$EndTime <- as.character(effort$EndTime)
monthly.effort <- ddply(effort, c("SiteID", "Month", "Year"),
                        function(df) {
                          return(
                            c(
                              trap.sets = length(df$SiteID)
                            )
                          )
                        }
)

######################
# PAMA total By month
trap.pama <- trap.dat.effort[trap.dat.effort$Species == "PAMA",]
trap.pama$EndTime <- as.POSIXlt(trap.pama$EndTime, format = "%m/%d/%y %H:%M")
trap.pama$Year <- year(trap.pama$EndTime)
trap.pama$Month <- month(trap.pama$EndTime)
trap.pama$EndTime <- as.character(trap.pama$EndTime)

pama.month <- ddply(trap.pama, c("SiteID", "Month", "Year"),
              function(df) {
                return(
                  c(
                    total.pama = sum(df$TotalNumber)
                  )
                )
              }
)

pama.month.CPUE <- merge(monthly.effort, pama.month, 
                     by = ,
                     all.x = TRUE,
                     all.y = TRUE)

pama.month.CPUE[is.na(pama.month.CPUE)] <- 0
pama.month.CPUE$CPUE <- (100*pama.month.CPUE$total.pama/pama.month.CPUE$trap.sets)

