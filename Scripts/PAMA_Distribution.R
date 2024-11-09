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


pama.month.367 <- pama.month.CPUE[pama.month.CPUE$SiteID == "367", ]
pama.month.362 <- pama.month.CPUE[pama.month.CPUE$SiteID == "362", ]
pama.month.378 <- pama.month.CPUE[pama.month.CPUE$SiteID == "378", ]
pama.month.529 <- pama.month.CPUE[pama.month.CPUE$SiteID == "529", ]
pama.month.528 <- pama.month.CPUE[pama.month.CPUE$SiteID == "528", ]
pama.month.386 <- pama.month.CPUE[pama.month.CPUE$SiteID == "386", ]
pama.month.599 <- pama.month.CPUE[pama.month.CPUE$SiteID == "599", ]
pama.month.516 <- pama.month.CPUE[pama.month.CPUE$SiteID == "516", ]
pama.month.133 <- pama.month.CPUE[pama.month.CPUE$SiteID == "133", ]


pama.month.cpue.site <- pama.month.CPUE[pama.month.CPUE$SiteID %in% c("367",
                                                            "362", 
                                                            "529",
                                                            "528",
                                                            "378",
                                                            "386",
                                                            "599",
                                                            "516",
                                                            "133"), ]

new_labels <- c("367" = "Drayton Harbor",
                "362" = "Post Point", 
                "529" = "Alice Bay",
                "528" = "Shore Trail",
                "378" = "Big Indian Slough",
                "386" = "Sharpe's Corner",
                "599" = "Davis Slough",
                "516" = "Iverson Spit",
                "133" = "Best Lagoon")

#new_labels2 <- c("4" = "April",
#                "5" = "May", 
#                "6" = "June",
#                "7" = "July",
#                "8" = "August",
#                "9" = "September")

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


#######

p367 <- ggplot(data = pama.month.367, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("Drayton") +
  scale_color_viridis_d()

p362 <- ggplot(data = pama.month.362, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("Post Point") +
  scale_color_viridis_d()

p529 <- ggplot(data = pama.month.529, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("529 Alice Bay") +
  scale_color_viridis_d()

p528 <- ggplot(data = pama.month.528, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("Shore Trail") +
  scale_color_viridis_d()

p378 <- ggplot(data = pama.month.378, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("Big Indian") +
  scale_color_viridis_d()

p386 <- ggplot(data = pama.month.386, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("Sharpes") +
  scale_color_viridis_d()

p599 <- ggplot(data = pama.month.599, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("Davis") +
  scale_color_viridis_d()

p516 <- ggplot(data = pama.month.516, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("Iverson") +
  scale_color_viridis_d()

p133 <- ggplot(data = pama.month.133, aes(x=month, y = CPUE, group = as.factor(Year))) +
  geom_line(aes(color = as.factor(Year))) +
  geom_point(aes(color = as.factor(Year))) +
  scale_shape_manual(values = 1:8) +
  theme_bw() + ggtitle("Best Lagoon") +
  scale_color_viridis_d()

grid.arrange(p367,
             p362,
             p529,
             p528,
             p378,
             p386,
             p599,
             p516,
             p133)