###### Script for plotting and analyzing PAMA Size comparisons.

###Libraries
library(scales) #Alpha

pama.size <- read.csv("~/Documents/GitHub/pama/PAMA Specimen Measurements.csv", header = T)

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
