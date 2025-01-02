######################################################
#########Crab Team Network data on PAMA##########
######################################################

######################################################
#######Libraries
library(plyr) #ddply
library(lubridate) #month, day, year functions
library(RColorBrewer)
library(viridis)
library(gridExtra)
library(forcats)
library(RColorBrewer)
library(ggplot2)


######################################################
####### Data
trap.dat <- read.csv("~/Documents/GitHub/pama/Data/TrappingData.11.7.24.eg.csv", header = T)

trap.dat$IsCompromised <- as.factor(trap.dat$IsCompromised)
trap.dat.effort <- trap.dat[trap.dat$IsCompromised == "N",] # Remove compromised and effort only traps

######################################################
#######PAMA Abundance by site and Year for mapping

#DF of total Trap sets per year per site
effort <- aggregate(numeric(nrow(trap.dat.effort)), 
                    trap.dat.effort[c("SiteID", "EndTime", "TrapType", "TrapNumber")], 
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


sites <- read.csv("~/Documents/GitHub/pama/Data/Sites.csv", header = T)
sites$SiteID <- sites$SiteNumber

pama.cpue.map <- merge(pama.cpue, sites[, c("LatitudeDD", "LongitudeDD", "SiteID")],
                   by = "SiteID", all.x = TRUE)

write.csv(pama.cpue.map, file = "Network PAMA CPUE.Map.csv")

###Plot PAMA Annual CPUE by year
pama.cpue.map$SiteID <- as.factor(pama.cpue.map$SiteID)
pama.cpue.plot <- pama.cpue.map[pama.cpue.map$SiteID %in% c("367",
                                                            "362", 
                                                           "529",
                                                           "528",
                                                           "378",
                                                           "386",
                                                           "599",
                                                           "516",
                                                           "133"), ]

pama.cpue.plot$SiteID = with(pama.cpue.plot, reorder(SiteID, CPUE, mean, decreasing = T))
pama.cpue.plot$Year <- as.factor(pama.cpue.plot$Year)

pdf("Catch by Year.pdf", height = 3, width = 6)
ggplot(data = pama.cpue.plot, aes(x = Year, y = log(CPUE), group = SiteID)) +
  geom_line(aes(color = SiteID)) +
  geom_point(aes(color = SiteID)) +
  theme_bw() + 
  guides(color = guide_legend(title = "Site")) +
  scale_color_brewer(palette = "Paired", name = "Site", labels = c("Drayton Harbor", 
                                                 "Post Point",
                                                  "Sharpe's Corner", 
                                                  "Davis Slough",
                                                  "Best Lagoon", 
                                                  "Alice Bay",
                                                  "Iverson Spit", 
                                                  "Shore Trail",
                                                  "Big Indian Slough"))
dev.off()

######################################################
#######PAMA Seasonal Abundance Patterns

######################
# Effort by Month
effort <- aggregate(numeric(nrow(trap.dat.effort)), 
                    trap.dat.effort[c("SiteID", "Month", "EndTime", "TrapType", "TrapNumber")], 
                    length) #Produces list of unique traps with x = number of unique species within the trap
effort$EndTime <- as.POSIXlt(effort$EndTime, format = "%m/%d/%y %H:%M")
effort$Year <- year(effort$EndTime)
effort$month <- month(effort$EndTime) #Derived from date and so true capture date

effort$EndTime <- as.character(effort$EndTime)
monthly.effort <- ddply(effort, c("SiteID", "month", "Year"),
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
trap.pama$month <- month(trap.pama$EndTime) #Derived from date and so true capture date
trap.pama$EndTime <- as.character(trap.pama$EndTime)

pama.month <- ddply(trap.pama, c("SiteID", "month", "Year"),
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

pama.month.cpue.site <- pama.month.CPUE[pama.month.CPUE$SiteID %in% c("367",
                                                            "362", 
                                                            "529",
                                                            "528",
                                                            "378",
                                                            "386",
                                                            "599",
                                                            "516",
                                                            "133"), ]

pama.month.cpue.site$Site2 <- factor(pama.month.cpue.site$SiteID, 
                                     levels = c("367",
                                                "362", 
                                                "529",
                                                "528",
                                                "378",
                                                "386",
                                                "599",
                                                "516",
                                                "133"))

new_labels <- c("367" = "Drayton Harbor",
                "362" = "Post Point", 
                "529" = "Alice Bay",
                "528" = "Shore Trail",
                "378" = "Big Indian Slough",
                "386" = "Sharpe's Corner",
                "599" = "Davis Slough",
                "516" = "Iverson Spit",
                "133" = "Best Lagoon")

pdf("Site by Season.pdf", height = 6, width = 7)
ggplot(data = pama.month.cpue.site, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() +
  facet_wrap(pama.month.cpue.site$Site2, 
             scales = "free",
             labeller = as_labeller(new_labels)) +
      scale_color_viridis_d() +
  xlab("Month") +
  guides(color=guide_legend("Year"))
dev.off()

###########
#Histogram of Photos by Class

Assignment <- seq(1:5)
Records <- c(66, 13, 19, 8, 44)
rating <- cbind(Confidence, Records)

pdf("Photo ID Assignment.pdf", width = 2.5, height = 2.5)
ggplot(data = rating, aes(x = Assignment, y = Records)) +
  geom_col() +
  theme_bw()
dev.off()

