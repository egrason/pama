############################################################ 
###### Script for plotting and analyzing PAMA demographics
############################################################ 

###Libraries & Data
library(scales) #Alpha
library(lubridate) #month, day, year functions
library(beanplot)
library(plyr)
library(ggplot2)
library(gridExtra)

pama.size <- read.csv("~/Documents/GitHub/pama/Data/PAMA Specimen Measurements.csv", header = T)

pama.size$Sex <- as.factor(pama.size$Sex)
pama.size$Gravid <- as.factor(pama.size$Gravid)
pama.size$EffortType <- as.factor(pama.size$EffortType)
pama.size$WaterBody <- as.factor(pama.size$WaterBody)
pama.size$Gear <- as.factor(pama.size$Gear)

pama.size$CollectionDate <- as.POSIXlt(pama.size$CollectionDate, format = "%m/%d/%y")
pama.size$CollectionDate <- as.POSIXct(pama.size$CollectionDate, format = "%m/%d/%y")


pama.size.crabteam <- pama.size[pama.size$EffortType == "Crab Team", ]
pama.size.synoptic <- pama.size[pama.size$EffortType == "Synoptic", ]
pama.size.opp <- pama.size[pama.size$EffortType == "Opportunistic", ]
pama.trap.syn <- pama.size[pama.size$WaterBody != "Lummi Bay", ]
pama.females <- pama.size[pama.size$Sex == "Female", ]


############################################################ 
###### Allometry


### CL v TL
pdf("Allometry.pdf", height = 6, width = 3)
par(las = 1, mfrow = c(3,1), mar = c(4, 4, 1, 1), oma = c(1, 1, 0, 0))
with(pama.size, 
     plot(CL, TL,
          pch = c(1, 2)[as.factor(Sex)],
          col = alpha(c("red", "blue")[as.factor(Sex)], 0.6),
          ylab = "Total Length (mm)",
          xlab = "Carapace Length (mm)",
          ))
text(4.5, 60, expression("Adj. r"^2*" = 0.97"))
allom.leg <- c("Female", "Male")
legend("bottomright", allom.leg, pch = c(1,2), col = c("red", "blue"))

### CL v GVTL

with(pama.size, 
     plot(CL, GVTL,
          pch = c(1, 2)[as.factor(Sex)],
          col = alpha(c("red", "blue")[as.factor(Sex)], 0.6),
          ylab = "Vazquez Total Length (mm)",
          xlab = "Carapace Length (mm)"
     ))
text(4.5, 45, expression("Adj. r"^2*" = 0.97"))

### TL v GVTL
with(pama.size, 
     plot(TL, GVTL,
          pch = c(1, 2)[as.factor(Sex)],
          col = alpha(c("red", "blue")[as.factor(Sex)], 0.6),
          ylab = "Vazquez Total Length (mm)",
          xlab = "Total Length (mm)"
     ))
text(22, 45, expression("Adj. r"^2*" = 0.99"))
dev.off()


############Models

### CL v TL
lm.cltl <- lm(TL~CL*Sex, data = pama.size)
summary(lm.cltl)

### CL v GVTL
lm.clgvtl <- lm(GVTL ~ CL*Sex, data = pama.size)
summary(lm.clgvtl)

### TL v GVTL
lm.tlgvtl <- lm(GVTL ~ TL*Sex, data = pama.size)
summary(lm.tlgvtl)


############Log Plot
with(pama.size, 
     plot(log(GVTL), log(CL),
          pch = c(1, 2)[as.factor(Sex)],
          col = alpha(c("red", "blue")[as.factor(Sex)], 0.6),
          ylab = "Ln Vazquez Total Length (mm)",
          xlab = "Ln Carapace Length (mm)"
     ))

lm.clgvtl <- lm(log(GVTL) ~ (log(CL))*SexFactor, data = pama.size)
summary(lm.clgvtl)

pdf("VTL.TL.pdf", width = 5, height = 5)
par(las = 1)
with(pama.size, 
     plot(log(GVTL), log(CL),
          pch = c(1, 2, 1)[as.factor(SexFactor)],
          col = alpha(c("red", "blue", "orange")[as.factor(SexFactor)], 0.6),
          ylab = "Ln Vazquez Total Length (mm)",
          xlab = "Ln Carapace Length (mm)"
     ))
leg.names <- c("Gravid Females", "Non-Gravid Females", "Males")
legend("topleft", leg.names, col = c("red", "orange", "blue"), pch = c(1, 1, 2))
dev.off()


############################################################ 
###### Size by capture date

pama.size$CollectionDate <- as.POSIXlt(pama.size$CollectionDate, format = "%m/%d/%y")

#All captures by date and size
plot(pama.size$CollectionDate, pama.size$CL,
     pch = c(1, 2)[as.factor(pama.size$Sex)],
     col = alpha(c("red", "blue")[as.factor(pama.size$Sex)], 0.6),
     xlab = "Collection Date",
     ylab = "Carapace Length (mm)")

#Crab Team only
plot(pama.size.crabteam$CollectionDate, pama.size.crabteam$CL,
     pch = c(1, 2)[as.factor(pama.size.crabteam$Sex)],
     col = alpha(c("red", "blue")[as.factor(pama.size.crabteam$Sex)], 0.6),
     xlab = "Collection Date",
     ylab = "Carapace Length (mm)")

pama.crabteam.21 <- pama.size.crabteam[pama.size.crabteam$Year == "2021", ]
pama.crabteam.22 <- pama.size.crabteam[pama.size.crabteam$Year == "2022", ]
pama.crabteam.2122 <- pama.size.crabteam[pama.size.crabteam$Year != "2020", ]


par(las = 1, mfrow = c(2,1))
plot(pama.crabteam.21$CollectionDate, pama.crabteam.21$CL,
     pch = c(1, 2)[as.factor(pama.crabteam.21$Sex)],
     col = alpha(c("red", "blue")[as.factor(pama.crabteam.21$Sex)], 0.6),
     xlab = "Collection Date",
     ylab = "Carapace Length (mm)"
     )

plot(pama.crabteam.22$CollectionDate, pama.crabteam.22$CL,
     pch = c(1, 2)[as.factor(pama.crabteam.22$Sex)],
     col = alpha(c("red", "blue")[as.factor(pama.crabteam.22$Sex)], 0.6),
     xlab = "Collection Date",
     ylab = "Carapace Length (mm)")

plot(pama.crabteam.2122$CollectionDate, pama.crabteam.2122$CL,
     pch = c(1, 2)[as.factor(pama.crabteam.2122$Sex)],
     col = alpha(c("red", "blue")[as.factor(pama.crabteam.2122$Sex)], 0.6),
     xlab = "Collection Date",
     ylab = "Carapace Length (mm)")

###### Size by Water Body

pama.size.crabteam$CollectionDate <- ymd(pama.size.crabteam$CollectionDate)
ggplot(pama.size.crabteam, aes(x = CollectionDate, y = CL, group = Sex)) +
  geom_point(aes(color = Sex)) +
  scale_x_date(date_labels = "%Y-%M-%D") +
  theme_bw() +
  facet_wrap(pama.size.crabteam$WaterBody)

pama.size.crabteam$SiteName <- as.factor(pama.size.crabteam$SiteName)
pama.founder.size <- subset(pama.size.crabteam, SiteName == "BestLagoon" | SiteName == "IversonSpit" | SiteName == "SharpesCorner")
ggplot(pama.size.crabteam, aes(x = CollectionDate, y = CL, group = Sex)) +
  geom_point(aes(color = Sex)) +
  theme_bw() +
  facet_wrap(pama.size.crabteam$SiteName) +
  xlab("Capture Date") +
  ylab("Carapace Length (mm)")

ggplot(pama.founder.size, aes(x = SiteName, y = CL, group = Sex)) +
  geom_point(aes(color = Sex)) +
  theme_bw() +
  xlab("Site") +
  ylab("Carapace Length (mm)")

########################################
#OVIGERY

# All females ovigery and date of capture
pama.females <- pama.size[pama.size$Sex == "Female", ]
plot(pama.females$CollectionDate, pama.females$CL,
     pch = c(2, 16)[as.factor(pama.females$Gravid)],
     col = alpha("red", 0.6),
     ylab = "Collection Date",
     xlab = "Carapace Length (mm)")

#Seasonality of ovigery in crab team females
crabteam.females <- pama.females[pama.females$EffortType == "Crab Team", ]

female.ovigery <- as.data.frame(table(crabteam.females$Gravid, crabteam.females$Month))
colnames(female.ovigery) <- c("Gravid", "Month", "Total")

pdf("Ovigery Eggs.pdf", width = 4, height = 6)
p1 <- ggplot(female.ovigery, aes(x = Month, y = Total, fill = Gravid)) +
  geom_col() +
  scale_fill_grey() +
  theme_bw() +
  ylab("Number of Females Captured")

pama.gravid <- pama.females[pama.females$Gravid == "Yes", ]
pama.gravid <- pama.gravid[pama.gravid$Month != "5.5",]
gravid.eggs <- as.data.frame(table(pama.gravid$EggsEyes, pama.gravid$Month))
colnames(gravid.eggs) <- c("EggsEyes", "Month", "Total")

p2 <- ggplot(gravid.eggs, aes(x = Month, y = Total, fill = EggsEyes)) +
  geom_col(position = "fill") +
  scale_fill_grey(name = "Eyes") +
  theme_bw() +
  ylab("Proportion of Gravid Females")

grid.arrange(p1, p2, nrow = 2)
dev.off()

#Size of females based on ovigery (beanplot)
beanplot(pama.females$CL ~ pama.females$Gravid, 
         ll = 0.04, 
         col = "gray",
         main = "Gravid",
         ylab = "Carapace Length (mm)")

################################
#Gear Investigations 
#Size of shrimp based on capture method (excluding seining)
pdf("Size by Capture Method.pdf", height = 5)
par(las = 1)
beanplot(CL ~ EffortType, data = pama.trap.syn, 
         ll = 0.07, 
         col = "gray",
         ylab = "Carapace Length (mm)",
         xaxt = "n")
axis(1, at = c(1:3), 
     labels = c("Crab Team\nTrapping", 
                "Dispersed\nTrapping", 
                "Synoptic\nDip Net"),
     lwd = 0,
     line = 1)
dev.off()

beanplot(CL ~ Gear, data = pama.size )
