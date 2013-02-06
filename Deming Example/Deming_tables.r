##For Deming Example##
#load output and make tables#
#make heat maps#
getwd()
setwd("Deming Example")
load("Deming_out.RData")

###Estimates Tables####
#
#
#
#####

#quadratic
QBm <- matrix(QB, ncol = 4, nrow = 6, byrow = TRUE)
colnames(QBm) <- 1:4
rownames(QBm) <- 1:6

QBms <- matrix(QB.s, ncol = 4, nrow = 6, byrow = TRUE)
colnames(QBms) <- 1:4
rownames(QBms) <- 1:6

QBmw <- matrix(QB.w, ncol = 4, nrow = 6, byrow = TRUE)
colnames(QBmw) <- 1:4
rownames(QBmw) <- 1:6

#deviance

PBm <- matrix(PB$x, ncol = 4, nrow = 6, byrow = TRUE)
colnames(PBm) <- 1:4
rownames(PBm) <- 1:6

PBms <- matrix(PB.s$x, ncol = 4, nrow = 6, byrow = TRUE)
colnames(PBms) <- 1:4
rownames(PBms) <- 1:6

PBmw <- matrix(PB.w$x, ncol = 4, nrow = 6, byrow = TRUE)
colnames(PBmw) <- 1:4
rownames(PBmw) <- 1:6

#discrimination measure

DBm <- matrix(DB$x, ncol = 4, nrow = 6, byrow = TRUE)
colnames(DBm) <- 1:4
rownames(DBm) <- 1:6

DBms <- matrix(DB.s$x, ncol = 4, nrow = 6, byrow = TRUE)
colnames(DBms) <- 1:4
rownames(DBms) <- 1:6

DBmw <- matrix(DB.w$x, ncol = 4, nrow = 6, byrow = TRUE)
colnames(DBmw) <- 1:4
rownames(DBmw) <- 1:6


#############HeatMaps/Images for Tables##############
#
#
#
#
######

library(gplots)#for bluered()
Dem <- matrix(y, ncol = 4, nrow = 6, byrow = TRUE)
colnames(Dem) <- 1:4
rownames(Dem) <- 1:6

#open connection to pdf to send output
pdf(file = "Deming_HeatMap.pdf", onefile = TRUE, height = 6, width = 4)

#plot the data in grayscale
#set min/max for black/white limit
bwz.lim <- c(min(Dem, DBm,DBms, DBmw),max(Dem, DBm,DBms, DBmw))
nbw <- 100
#creates 100 values along grayscale
#selects one for each cell based on values
#y-axis is reversed - hence the 6:1 and the transpose of Dem
image(y = 1:6, x = 1:4,t(Dem[6:1,]), col = gray((nbw:0)/nbw), zlim = bwz.lim,
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
#label axes
axis(side = 2, at = 1:6, labels = 6:1, tick = FALSE, cex.axis = 2)
axis(side = 3, at = 1:4, labels = 1:4, tick = FALSE, cex.axis = 2)

##plot the adjustment in red/white/blue for Discrimination Information##
#Default
dDem <- DBm-Dem
zlim <- max(abs(dDem))
zint <- ceiling(zlim*2)
#we want to make 0 values displayed as white
#to do that we generate the same number of red and blue values to select from
#and we set the "zlim" to be symmetric

image(y = 1:6, x = 1:4,t(dDem[6:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:6, labels = 6:1, tick = FALSE, cex.axis = 2)
axis(side = 3, at = 1:4, labels = 1:4, tick = FALSE, cex.axis = 2)

#now we add the +,-
sym.dDem <- matrix(NA, ncol = 4, nrow = 6)
sym.dDem <- ifelse(dDem > 0, "+", sym.dDem)
sym.dDem <- ifelse(dDem < 0, "-", sym.dDem)
xsym <- rep(1:4, each = 6)
ysym <- rep(6:1, times = 4)
#this just gives a vector argument for which symbol to use for "pch"
points(x = xsym, y = ysym, pch = sym.dDem, col = "white", cex = 3)

#Points Set
dDem <- DBms-Dem
zlim <- max(abs(dDem))
zint <- ceiling(zlim*2)
image(y = 1:6, x = 1:4,t(dDem[6:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:6, labels = 6:1, tick = FALSE, cex.axis = 2)
axis(side = 3, at = 1:4, labels = 1:4, tick = FALSE, cex.axis = 2)

sym.dDem <- matrix(NA, ncol = 4, nrow = 6)
sym.dDem <- ifelse(dDem > 0, "+", sym.dDem)
sym.dDem <- ifelse(dDem < 0, "-", sym.dDem)
xsym <- rep(1:4, each = 6)
ysym <- rep(6:1, times = 4)
points(x = xsym, y = ysym, pch = sym.dDem, col = "white", cex = 3)

dDem <- DBmw-Dem
zlim <- max(abs(dDem))
zint <- ceiling(zlim*2)
image(y = 1:6, x = 1:4,t(dDem[6:1,]), col = bluered(zint)[zint:1], zlim = c(-zlim,zlim),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(side = 2, at = 1:6, labels = 6:1, tick = FALSE, cex.axis = 2)
axis(side = 3, at = 1:4, labels = 1:4, tick = FALSE, cex.axis = 2)

sym.dDem <- matrix(NA, ncol = 4, nrow = 6)
sym.dDem <- ifelse(dDem > 0, "+", sym.dDem)
sym.dDem <- ifelse(dDem < 0, "-", sym.dDem)
xsym <- rep(1:4, each = 6)
ysym <- rep(6:1, times = 4)
points(x = xsym, y = ysym, pch = sym.dDem, col = "white", cex = 3)

dev.off() #close pdf connection



