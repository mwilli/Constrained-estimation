##Make tables for NASS Example##
getwd()
setwd("NASS Example")
load("NASS_Example_Estimates.RData")


####Linear#####
#
#
#
#
###############

##Default##
##No restrictions##
Qm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Qm) <- c("Num","Den","Ratio")
rownames(Qm) <- 1:20
Qm[,1:2] <- matrix(Q,ncol = 2, nrow = 20)
Qm[,3] <- Qm[,1]/Qm[,2]

Pm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Pm) <- c("Num","Den","Ratio")
rownames(Pm) <- 1:20
Pm[,1:2] <- matrix(P$x,ncol = 2, nrow = 20)
Pm[,3] <- Pm[,1]/Pm[,2]

Dm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Dm) <- c("Num","Den","Ratio")
rownames(Dm) <- 1:20
Dm[,1:2] <- matrix(D$x,ncol = 2, nrow = 20)
Dm[,3] <- Dm[,1]/Dm[,2]

##User Intervention##
##Some fixed##
Q0m <- matrix(NA, ncol = 3, nrow = 20)
colnames(Q0m) <- c("Num","Den","Ratio")
rownames(Q0m) <- 1:20
Q0m[,1:2] <- matrix(Q0,ncol = 2, nrow = 20)
Q0m[,3] <- Q0m[,1]/Q0m[,2]

P0m <- matrix(NA, ncol = 3, nrow = 20)
colnames(P0m) <- c("Num","Den","Ratio")
rownames(P0m) <- 1:20
P0m[,1:2] <- matrix(P0$x,ncol = 2, nrow = 20)
P0m[,3] <- P0m[,1]/P0m[,2]

D0m <- matrix(NA, ncol = 3, nrow = 20)
colnames(D0m) <- c("Num","Den","Ratio")
rownames(D0m) <- 1:20
D0m[,1:2] <- matrix(D0$x,ncol = 2, nrow = 20)
D0m[,3] <- D0m[,1]/D0m[,2]

##Path 1##
#Some ratios set
QRm <- matrix(NA, ncol = 3, nrow = 20)
colnames(QRm) <- c("Num","Den","Ratio")
rownames(QRm) <- 1:20
QRm[,1:2] <- matrix(QR,ncol = 2, nrow = 20)
QRm[,3] <- QRm[,1]/QRm[,2]

PRm <- matrix(NA, ncol = 3, nrow = 20)
colnames(PRm) <- c("Num","Den","Ratio")
rownames(PRm) <- 1:20
PRm[,1:2] <- matrix(PR$x,ncol = 2, nrow = 20)
PRm[,3] <- PRm[,1]/PRm[,2]

DRm <- matrix(NA, ncol = 3, nrow = 20)
colnames(DRm) <- c("Num","Den","Ratio")
rownames(DRm) <- 1:20
DRm[,1:2] <- matrix(DR$x,ncol = 2, nrow = 20)
DRm[,3] <- DRm[,1]/DRm[,2]

#User Priority Weighting#
QRWm <- matrix(NA, ncol = 3, nrow = 20)
colnames(QRWm) <- c("Num","Den","Ratio")
rownames(QRWm) <- 1:20
QRWm[,1:2] <- matrix(QR.w,ncol = 2, nrow = 20)
QRWm[,3] <- QRWm[,1]/QRWm[,2]

PRWm <- matrix(NA, ncol = 3, nrow = 20)
colnames(PRWm) <- c("Num","Den","Ratio")
rownames(PRWm) <- 1:20
PRWm[,1:2] <- matrix(PR.w$x,ncol = 2, nrow = 20)
PRWm[,3] <- PRWm[,1]/PRWm[,2]

DRWm <- matrix(NA, ncol = 3, nrow = 20)
colnames(DRWm) <- c("Num","Den","Ratio")
rownames(DRWm) <- 1:20
DRWm[,1:2] <- matrix(DR.w$x,ncol = 2, nrow = 20)
DRWm[,3] <- DRWm[,1]/DRWm[,2]

##Path 2##
#All ratios set
QRRm <- matrix(NA, ncol = 3, nrow = 20)
colnames(QRRm) <- c("Num","Den","Ratio")
rownames(QRRm) <- 1:20
QRRm[,1:2] <- matrix(QR.R,ncol = 2, nrow = 20)
QRRm[,3] <- QRRm[,1]/QRRm[,2]

PRRm <- matrix(NA, ncol = 3, nrow = 20)
colnames(PRRm) <- c("Num","Den","Ratio")
rownames(PRRm) <- 1:20
PRRm[,1:2] <- matrix(PR.R$x,ncol = 2, nrow = 20)
PRRm[,3] <- PRRm[,1]/PRRm[,2]

DRRm <- matrix(NA, ncol = 3, nrow = 20)
colnames(DRRm) <- c("Num","Den","Ratio")
rownames(DRRm) <- 1:20
DRRm[,1:2] <- matrix(DR.R$x,ncol = 2, nrow = 20)
DRRm[,3] <- DRRm[,1]/DRRm[,2]

####Nonlinear####
##D and R to get
#
#
#################

##Default##
##No restrictions##
Qnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Qnlm) <- c("Num","Den","Ratio")
rownames(Qnlm) <- 1:20
Qnlm[,2:3] <- matrix(Q.nl$x,ncol = 2, nrow = 20)
Qnlm[,1] <- Qnlm[,2]*Qnlm[,3]

Pnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Pnlm) <- c("Num","Den","Ratio")
rownames(Pnlm) <- 1:20
Pnlm[,2:3] <- matrix(P.nl$x,ncol = 2, nrow = 20)
Pnlm[,1] <- Pnlm[,2]*Pnlm[,3]

Dnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Dnlm) <- c("Num","Den","Ratio")
rownames(Dnlm) <- 1:20
Dnlm[,2:3] <- matrix(D.nl$x,ncol = 2, nrow = 20)
Dnlm[,1] <- Dnlm[,2]*Dnlm[,3]

##User Intervention##
##Some fixed
Q0nlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Q0nlm) <- c("Num","Den","Ratio")
rownames(Q0nlm) <- 1:20
Q0nlm[,2:3] <- matrix(Q0.nl$x,ncol = 2, nrow = 20)
Q0nlm[,1] <- Q0nlm[,2]*Q0nlm[,3]

P0nlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(P0nlm) <- c("Num","Den","Ratio")
rownames(P0nlm) <- 1:20
P0nlm[,2:3] <- matrix(P0.nl$x,ncol = 2, nrow = 20)
P0nlm[,1] <- P0nlm[,2]*P0nlm[,3]

D0nlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(D0nlm) <- c("Num","Den","Ratio")
rownames(D0nlm) <- 1:20
D0nlm[,2:3] <- matrix(D0.nl$x,ncol = 2, nrow = 20)
D0nlm[,1] <- D0nlm[,2]*D0nlm[,3]

##Path 1##
##Some fixed and some ratios fixed
Q02nlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Q02nlm) <- c("Num","Den","Ratio")
rownames(Q02nlm) <- 1:20
Q02nlm[,2:3] <- matrix(Q02.nl$x,ncol = 2, nrow = 20)
Q02nlm[,1] <- Q02nlm[,2]*Q02nlm[,3]

P02nlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(P02nlm) <- c("Num","Den","Ratio")
rownames(P02nlm) <- 1:20
P02nlm[,2:3] <- matrix(P02.nl$x,ncol = 2, nrow = 20)
P02nlm[,1] <- P02nlm[,2]*P02nlm[,3]

D02nlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(D02nlm) <- c("Num","Den","Ratio")
rownames(D02nlm) <- 1:20
D02nlm[,2:3] <- matrix(D02.nl$x,ncol = 2, nrow = 20)
D02nlm[,1] <- D02nlm[,2]*D02nlm[,3]

##Some fixed, some ratios, and priority weights
Q02wnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Q02wnlm) <- c("Num","Den","Ratio")
rownames(Q02wnlm) <- 1:20
Q02wnlm[,2:3] <- matrix(Q02.nl.w$x,ncol = 2, nrow = 20)
Q02wnlm[,1] <- Q02wnlm[,2]*Q02wnlm[,3]

P02wnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(P02wnlm) <- c("Num","Den","Ratio")
rownames(P02wnlm) <- 1:20
P02wnlm[,2:3] <- matrix(P02.nl.w$x,ncol = 2, nrow = 20)
P02wnlm[,1] <- P02wnlm[,2]*P02wnlm[,3]

D02wnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(D02wnlm) <- c("Num","Den","Ratio")
rownames(D02wnlm) <- 1:20
D02wnlm[,2:3] <- matrix(D02.nl.w$x,ncol = 2, nrow = 20)
D02wnlm[,1] <- D02wnlm[,2]*D02wnlm[,3]

##Path 2##
##Some fixed and all ratios fixed
Q0Rnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(Q0Rnlm) <- c("Num","Den","Ratio")
rownames(Q0Rnlm) <- 1:20
Q0Rnlm[,2:3] <- matrix(Q0R.nl$x,ncol = 2, nrow = 20)
Q0Rnlm[,1] <- Q0Rnlm[,2]*Q0Rnlm[,3]

P0Rnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(P0Rnlm) <- c("Num","Den","Ratio")
rownames(P0Rnlm) <- 1:20
P0Rnlm[,2:3] <- matrix(P0R.nl$x,ncol = 2, nrow = 20)
P0Rnlm[,1] <- P0Rnlm[,2]*P0Rnlm[,3]

D0Rnlm <- matrix(NA, ncol = 3, nrow = 20)
colnames(D0Rnlm) <- c("Num","Den","Ratio")
rownames(D0Rnlm) <- 1:20
D0Rnlm[,2:3] <- matrix(D0R.nl$x,ncol = 2, nrow = 20)
D0Rnlm[,1] <- D0Rnlm[,2]*D0Rnlm[,3]


####HeatMaps of Tables#####
library(gplots)#for bluered()
#heatmaps for export#
#NASS data#
NASS <- dat[,-1]


pdf(file = "NASS_HeatMap_linear.pdf", onefile = TRUE, height = 6, width = 4)
par(las = 1)#makes axes labels horizontal

#log scale here#
bwz.lim <- log(c(min(NASS), max(NASS)))
nbw <- 100
#generate 101 values in the grayscale
#assign cell colors based on value of the data
#transpose and reverse row order to get correct y axis orientation
image(y = 1:20, x = 1:3,log(t(NASS[20:1,])), col = gray((nbw:0)/nbw), zlim = bwz.lim,
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)

##Default Settings##
#Calculated % change
dNass <- Dm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

##User Intervention##
#Calculated % change
dNass <- D0m-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)
#add margin notes to indicate user actions
axis(side = 4, at = (20:1)[fixl[fixl <= 20]], labels = rep("ND",5), 
		tick = FALSE, line = -0.8)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

##Path 1##
#Calculated % change
dNass <- DRm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)
#add margin notes to indicate user actions
axis(side = 4, at = (20:1)[fixl[fixl <= 20]], labels = rep("ND",5), 
		tick = FALSE, line = -0.8)
axis(side = 4, at = (20:1)[redl], labels = rep("R",3), 
		tick = FALSE, line = -0.8)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

#User Priority Weights#
#Calculated % change
dNass <- DRWm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)
#add margin notes to indicate user actions
lab.ax <- rep("", 20)
lab.ax[fixl[fixl <= 20]] <- "ND"
lab.ax[redl] <- "R"
axis(side = 4, at = (20:1), labels = lab.ax, tick = FALSE, line = -0.8)
axis(side = 4, at = (20:1)[rwpl], 
labels = rep(expression(alpha),7), tick = FALSE, line = -0.2)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

##Path 2##
#Calculated % change
dNass <- DRRm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)
#add margin notes to indicate user actions
lab.ax <- rep("R", 20)
lab.ax[fixl[fixl <= 20]] <- "ND"
axis(side = 4, at = (20:1), labels = lab.ax, tick = FALSE, line = -0.8)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

dev.off() #close pdf connection



#####Nonlinear######
getwd()
pdf(file = "NASS_HeatMap_nonlinear.pdf", onefile = TRUE, height = 6, width = 4)
par(las = 1)#makes axes labels horizontal

#data in log scale - same as linear#
bwz.lim <- log(c(min(NASS),max(NASS)))
nbw <- 100
image(y = 1:20, x = 1:3,log(t(NASS[20:1,])), col = gray((nbw:0)/nbw), zlim = bwz.lim,
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)

##Default Settings##
#Calculated % change
dNass <- Dnlm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

##User Intervention##
#Calculated % change
dNass <- D0nlm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

#this is what we want
zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)
#add margin notes to indicate user actions
axis(side = 4, at = (20:1)[fixl[fixl <= 20]], labels = rep("ND",5), 
		tick = FALSE, line = -0.8)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

##Path 1##
#Calculated % change
dNass <- D02nlm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)
#add margin notes to indicate user actions
axis(side = 4, at = (20:1)[fixl[fixl <= 20]], labels = rep("ND",5), 
		tick = FALSE, line = -0.8)
axis(side = 4, at = (20:1)[redl], labels = rep("R",3), 
		tick = FALSE, line = -0.8)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

#User Priority Weights#
#Calculated % change
dNass <- D02wnlm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)
#add margin notes to indicate user actions
lab.ax <- rep("", 20)
lab.ax[fixl[fixl <= 20]] <- "ND"
lab.ax[redl] <- "R"
axis(side = 4, at = (20:1), labels = lab.ax, tick = FALSE, line = -0.8)
axis(side = 4, at = (20:1)[rwpl], 
labels = rep(expression(alpha),7), tick = FALSE, line = -0.2)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

##Path 2##
#Calculated % change
dNass <- D0Rnlm-NASS
dNass.sc <- dNass
dNass.sc[,1] <- dNass[,1]/NASS[,1]
dNass.sc[,2] <- dNass[,2]/NASS[,2]
dNass.sc[,3] <- dNass[,3]/NASS[,3]
dNass.sc <- dNass.sc*100

zlim <- max(abs(dNass.sc))
zint <- ceiling(zlim*2)
#generate even values in red and blue and
#make zlim symmetric to keep assign white to values of 0
image(y = 1:20, x = 1:3,t(dNass.sc[20:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:20, labels = 20:1)
axis(side = 3, at = 1:3, labels = c("Num", "Den", "Ratio"), tick = FALSE, cex.axis = 1.5)
#add margin notes to indicate user actions
lab.ax <- rep("R", 20)
lab.ax[fixl[fixl <= 20]] <- "ND"
axis(side = 4, at = (20:1), labels = lab.ax, tick = FALSE, line = -0.8)

#plot +,- to show sign of change
sym.dNass <- matrix(NA, ncol = 4, nrow = 6)
sym.dNass <- ifelse(dNass.sc> 0, "+", sym.dNass)
sym.dNass <- ifelse(dNass.sc < 0, "-", sym.dNass)
xsym <- rep(1:3, each = 20)
ysym <- rep(20:1, times = 3)
points(x = xsym, y = ysym, pch = sym.dNass, col = "white", cex = 2)

dev.off()