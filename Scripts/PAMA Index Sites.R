############################################################ 
###### Plotting and Drayton Harbor Index Site Abundance 2021
############################################################ 

###Libraries & Data
library(lubridate) #month, day, year functions

WDFW.Drayton.dat <- read.csv("~/Documents/GitHub/pama/Data/WDFW.PAMA 2021 -2022.csv", header = T)
WDFW.Drayton.dat$Index <- as.factor(WDFW.Drayton.dat$Index)
WDFW.Drayton.dat$Date_Checked <- as.POSIXlt(WDFW.Drayton.dat$Date_Checked, format = "%m/%d/%y")
WDFW.Drayton.dat$Month <- month(WDFW.Drayton.dat$Date_Checked)


index.dat <- WDFW.Drayton.dat[which(WDFW.Drayton.dat$Index %in% c("Downstream","Mid", "Upstream")) ,] 
index.dat <- index.dat[index.dat$Year == "2021", ]

index.Apr <- index.dat[index.dat$Month == "4", ]
index.May <- index.dat[index.dat$Month == "5", ]
index.Jun <- index.dat[index.dat$Month == "6", ]
index.Jul <- index.dat[index.dat$Month == "7", ]
index.Aug <- index.dat[index.dat$Month == "8", ]
index.Sep <- index.dat[index.dat$Month == "9", ]
index.Oct <- index.dat[index.dat$Month == "10", ]


#########Create Plot
par(mfrow = c(6,1), mar = c(0.1,3,1,1), las = 1, oma = c(5, 4,0,0))
plot(index.Apr$DistanceMouth, index.Apr$PAMA,
     ylim = c(0, 40),
     pch = 16,
     xaxt = "n",
     col = c("purple", "orange")[as.factor(index.Apr$Site_Name)])
text(1250, 35, "April")
abline(v = c(500, 1000, 1500, 2000), lty = 2, col = "gray")
legend.names <- c("California Creek", "Dakota Creek")
legend(2000, 30, legend.names, col = c("purple", "orange"), pch = 16, cex = 0.75)
plot(index.May$DistanceMouth, index.May$PAMA,
     ylim = c(0, 40),
     pch = 16,
     xaxt = "n",
     col = c("purple", "orange")[as.factor(index.Jun$Site_Name)])
text(1250, 35, "May")
abline(v = c(500, 1000, 1500, 2000), lty = 2, col = "gray")
plot(index.Jun$DistanceMouth, index.Jun$PAMA,
     ylim = c(0, 40),
     pch = 16,     
     xaxt = "n",
     col = c("purple", "orange")[as.factor(index.Jul$Site_Name)])
text(1250, 35, "June")
abline(v = c(500, 1000, 1500, 2000), lty = 2, col = "gray")
plot(index.Jul$DistanceMouth, index.Jul$PAMA,
     ylim = c(0, 40),
     pch = 16,
     xaxt = "n",
     col = c("purple", "orange")[as.factor(index.Jul$Site_Name)])
text(1250, 35, "July")
abline(v = c(500, 1000, 1500, 2000), lty = 2, col = "gray")
mtext("Shrimp per Trap", side = 2, las = 3, line = 3)
plot(index.Aug$DistanceMouth, index.Aug$PAMA,
     ylim = c(0, 40),
     pch = 16,
     xaxt = "n",
     col = c("purple", "orange")[as.factor(index.Aug$Site_Name)])
text(1250, 35, "August")
abline(v = c(500, 1000, 1500, 2000), lty = 2, col = "gray")
plot(index.Sep$DistanceMouth, index.Sep$PAMA,
     ylim = c(0, 40),
     pch = 16,
     col = c("purple", "orange")[as.factor(index.Sep$Site_Name)])
text(1250, 35, "September")
abline(v = c(500, 1000, 1500, 2000), lty = 2, col = "gray")
mtext("Distance from Mouth (m)", line = -14)
