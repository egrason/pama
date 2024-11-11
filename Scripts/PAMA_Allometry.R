############################################################ 
###### Script for plotting and analyzing PAMA demographics
############################################################ 

###Libraries & Data
library(scales) #Alpha
library(lubridate) #month, day, year functions
library(beanplot)
library(plyr)
library(ggplot2)

pama.size <- read.csv("~/Documents/GitHub/pama/Data/PAMA Specimen Measurements.csv", header = T)

pama.size$Sex <- as.factor(pama.size$Sex)
pama.size$Gravid <- as.factor(pama.size$Gravid)
pama.size$EffortType <- as.factor(pama.size$EffortType)
pama.size$WaterBody <- as.factor(pama.size$WaterBody)

pama.size$CollectionDate <- as.POSIXlt(pama.size$CollectionDate, format = "%m/%d/%y")

pama.size.crabteam <- pama.size[pama.size$EffortType == "Crab Team", ]
pama.size.synoptic <- pama.size[pama.size$EffortType == "Synoptic", ]
pama.size.opp <- pama.size[pama.size$EffortType == "Opportunistic", ]
pama.trap.syn <- pama.size[pama.size$WaterBody != "Lummi Bay", ]
pama.females <- pama.size[pama.size$Sex == "Female", ]


############################################################ 
###### Allometry


### CL v TL

par(las = 1)
with(pama.size, 
     plot(CL, TL,
          pch = c(1, 2)[as.factor(Sex)],
          col = alpha(c("red", "blue")[as.factor(Sex)], 0.6),
          ylab = "Total Length (mm)",
          xlab = "Carapace Length (mm)",
          ))
text(4, 60, expression("Adj. r"^2*" = 0.97"))

lm.cltl <- lm(TL~CL*Sex, data = pama.size)
summary(lm.cltl)

### CL v GVTL

par(las = 1)
with(pama.size, 
     plot(CL, GVTL,
          pch = c(1, 2)[as.factor(Sex)],
          col = alpha(c("red", "blue")[as.factor(Sex)], 0.6),
          ylab = "Guadelupe-Vazquez Total Length (mm)",
          xlab = "Carapace Length (mm)"
     ))
text(4, 45, expression("Adj. r"^2*" = 0.97"))

lm.clgvtl <- lm(GVTL ~ CL*Sex, data = pama.size)
summary(lm.clgvtl)

with(pama.size, 
     plot(log(GVTL), log(CL),
          pch = c(1, 2)[as.factor(Sex)],
          col = alpha(c("red", "blue")[as.factor(Sex)], 0.6),
          ylab = "Ln Vazquez Total Length (mm)",
          xlab = "Ln Carapace Length (mm)"
     ))

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

lm.clgvtl <- lm(log(GVTL) ~ (log(CL))*SexFactor, data = pama.size)
summary(lm.clgvtl)

### TL v GVTL
par(las = 1)
with(pama.size, 
     plot(TL, GVTL,
          pch = c(1, 2)[as.factor(Sex)],
          col = alpha(c("red", "blue")[as.factor(Sex)], 0.6),
          ylab = "Guadelupe-Vazquez Total Length (mm)",
          xlab = "Total Length (mm)"
     ))
text(20, 45, expression("Adj. r"^2*" = 0.99"))

lm.tlgvtl <- lm(GVTL ~ TL*Sex, data = pama.size)
summary(lm.tlgvtl)


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


# All females ovigery and date of capture
pama.females <- pama.size[pama.size$Sex == "Female", ]
plot(pama.females$CollectionDate, pama.females$CL,
     pch = c(2, 16)[as.factor(pama.size.crabteam$Gravid)],
     col = alpha("red", 0.6),
     ylab = "Collection Date",
     xlab = "Carapace Length (mm)")

#Seasonality of ovigery in crab team females
crabteam.females <- pama.females[pama.females$EffortType == "Crab Team", ]

female.ovigery <- as.data.frame(table(crabteam.females$Gravid, crabteam.females$Month))
colnames(female.ovigery) <- c("Gravid", "Month", "Total")

pdf("Female Ovigery.pdf", width = 5, height = 4)
ggplot(female.ovigery, aes(x = Month, y = Total, fill = Gravid)) +
  geom_col() +
  scale_fill_grey() +
  theme_bw() +
  ylab("Number of Females Captured")
dev.off()

#Size of females based on ovigery (beanplot)
beanplot(pama.females$CL ~ pama.females$Gravid, 
         ll = 0.04, 
         col = "gray",
         main = "Gravid",
         ylab = "Carapace Length (mm)")

pama.gravid <- pama.females[pama.females$Gravid == "Yes", ]


#Size of shrimp based on capture method
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
