#Deming 1940 Example for Benchmarking#
#check and set working directory
#getwd()
#setwd("Deming Example")
#load benchmark functions from file#
source("benchmark_functions_nl.r")

#import data
dat <- read.csv(file = "Deming_1940_data_stacked.csv")
marg <- read.csv(file = "Deming_1940_data_margin.csv")

#construct A using model matrix shortcut (no intercept)
#default removes Age 1 total as redundant

X <- model.matrix(~ as.factor(State) + as.factor(Age) -1, data = dat)
A <- t(X)

#construct the target vector q
Ms <- matrix(marg$Mi., ncol = 1)
Ma <- marg$M.j; Ma <- matrix(Ma[!is.na(Ma)], ncol = 1)
Mb <- matrix(c(Ms,Ma[-1]),ncol = 1)
q <- Mb

#set y - the unconstrained values
y <- matrix(dat$Count, ncol = 1)
n <- length(y)

###Generate Estimates###
###regular `optimal' - no  additional user input###
#quadratic
Wq <- Diagonal(x = 1/y, n = n)
Wqi <- solve(Wq)

QB <- Quad_Ben(y,A,q,Wqi)

#poisson and discrimination
Wd <- Diagonal(x = 1, n = n)
Wdi <- solve(Wd)

PB <- PoisDev_Ben(y,A,q,Wdi, y, mxit = 15, tol = 1e-5)

lam0 <- q*0 #starting value for lagrange multipliers lam
DB <- Discr_Ben(y,A,q,Wdi,lam0,mxit = 50, tol = 1e-8)


##Alternatively, use the general procedure##
QB.gen <- Gen_Dist_NL(y,g.lin, Dg.lin, q, Wqi, x0 = y, lam0 = lam0,
	mxit = c(15,15), tol = c(1e-5,1e-5), h = h.Q, hp = hp.Q)

PB.gen <- Gen_Dist_NL(y,g.lin, Dg.lin, q, Wdi, x0 = y, lam0 = lam0,
	mxit = c(15,15), tol = c(1e-5,1e-5), h = h.P, hp = hp.P)

DB.gen <- Gen_Dist_NL(y,g.lin, Dg.lin, q, Wdi, x0 = y, lam0 = lam0,
	mxit = c(15,15), tol = c(1e-8,1e-8), h = h.D, hp = hp.D)
	#shrink tolerance a little
#Two more#
HB.gen <- Gen_Dist_NL(y,g.lin, Dg.lin, q, Wdi, x0 = y, lam0 = lam0,
	mxit = c(15,15), tol = c(1e-8,1e-8), h = h.H, hp = hp.H)

AQB.gen <- Gen_Dist_NL(y,g.lin, Dg.lin, q, Wdi, x0 = y, lam0 = lam0,
	mxit = c(15,15), tol = c(1e-8,1e-8), h = h.AQ, hp = hp.AQ)


###User sets two points y[9] = 1516, y[20] = 160###
library(MASS)
y.s <- y
y.s[9] <- 1516
y.s[20] <- 160

#quadratic
Wqi.0 <- Wqi #since diagonal it's ok
Wqi.0[9,] <- Wqi.0[,9] <- Wqi.0[20,] <- Wqi.0[,20] <- 0

QB.s <- Quad_Ben(y.s,A,q,Wqi.0)

#Poisson and Discrimination
Wdi.0 <- Wdi
Wdi.0[9,] <- Wdi.0[,9] <- Wdi.0[20,] <- Wdi.0[,20] <- 0

PB.s <- PoisDev_Ben(y.s,A,q,Wdi.0, y.s, mxit = 15, tol = 1e-5)
DB.s <- Discr_Ben(y.s,A,q,Wdi.0,lam0, mxit = 50, tol = 1e-8)


###User modifies weights###
w <- rep(c(5,5,1,1), times = 6)
Wl <- Wr <- Diagonal(x = sqrt(w), n = n)

#quadratic
Wqw <- Wl%*%Wq%*%Wr
Wqwi <- solve(Wqw)

QB.w <- Quad_Ben(y,A,q,Wqwi)

#Poisson and Discrimination
Wdw <- Wl%*%Wd%*%Wr
Wdwi <- solve(Wdw)

PB.w <- PoisDev_Ben(y,A,q,Wdwi, y, mxit = 15, tol = 1e-5)
lam0 <- q*0
DB.w <- Discr_Ben(y,A,q,Wdwi,lam0, mxit = 50, tol = 1e-8)

#####Calculate Distances#####

#Default table#
Dist.def5 <- matrix(NA, ncol = 5, nrow = 5)
colnames(Dist.def5) <- c("Quad", "Pois", "Discr", "Hellg", "AQuad")
rownames(Dist.def5) <- c("Quad", "Pois", "Discr", "Hellg", "AQuad")

#Quadratic
Dist.def5[1,1] <- Quad_Dist(QB,y,Wq)
Dist.def5[2,1] <- Quad_Dist(PB$x,y,Wq)
Dist.def5[3,1] <- Quad_Dist(DB$x,y,Wq)
Dist.def5[4,1] <- Quad_Dist(HB.gen$x,y,Wq)
Dist.def5[5,1] <- Quad_Dist(AQB.gen$x,y,Wq)

#Poisson
Dist.def5[1,2] <- PoisDev_Dist(QB,y,Wd)
Dist.def5[2,2] <- PoisDev_Dist(PB$x,y,Wd)
Dist.def5[3,2] <- PoisDev_Dist(DB$x,y,Wd)
Dist.def5[4,2] <- PoisDev_Dist(HB.gen$x,y,Wd)
Dist.def5[5,2] <- PoisDev_Dist(AQB.gen$x,y,Wd)

#Discr
Dist.def5[1,3] <- Discr_Dist(QB,y,Wd)
Dist.def5[2,3] <- Discr_Dist(PB$x,y,Wd)
Dist.def5[3,3] <- Discr_Dist(DB$x,y,Wd)
Dist.def5[4,3] <- Discr_Dist(HB.gen$x,y,Wd)
Dist.def5[5,3] <- Discr_Dist(AQB.gen$x,y,Wd)

#Hellg
Dist.def5[1,4] <- Hellg_Dist(QB,y,Wd)
Dist.def5[2,4] <- Hellg_Dist(PB$x,y,Wd)
Dist.def5[3,4] <- Hellg_Dist(DB$x,y,Wd)
Dist.def5[4,4] <- Hellg_Dist(HB.gen$x,y,Wd)
Dist.def5[5,4] <- Hellg_Dist(AQB.gen$x,y,Wd)

#Alter. Quad
Dist.def5[1,5] <- Alt_Quad_Dist(QB,y,Wd)
Dist.def5[2,5] <- Alt_Quad_Dist(PB$x,y,Wd)
Dist.def5[3,5] <- Alt_Quad_Dist(DB$x,y,Wd)
Dist.def5[4,5] <- Alt_Quad_Dist(HB.gen$x,y,Wd)
Dist.def5[5,5] <- Alt_Quad_Dist(AQB.gen$x,y,Wd)

#looks good. minimum values on the diagonal!

#y.s (can us regular W because of 0's in distances)
Dist.s <- matrix(NA, ncol = 3, nrow = 3)
colnames(Dist.s) <- c("Quad", "Pois", "Discr")
rownames(Dist.s) <- c("Quad", "Pois", "Discr")
#Quadratic
Dist.s[1,1] <- Quad_Dist(QB.s,y.s,Wq)
Dist.s[2,1] <- Quad_Dist(PB.s$x,y.s,Wq)
Dist.s[3,1] <- Quad_Dist(DB.s$x,y.s,Wq)

#Poisson
Dist.s[1,2] <- PoisDev_Dist(QB.s,y.s,Wd)
Dist.s[2,2] <- PoisDev_Dist(PB.s$x,y.s,Wd)
Dist.s[3,2] <- PoisDev_Dist(DB.s$x,y.s,Wd)

#Discr
Dist.s[1,3] <- Discr_Dist(QB.s,y.s,Wd)
Dist.s[2,3] <- Discr_Dist(PB.s$x,y.s,Wd)
Dist.s[3,3] <- Discr_Dist(DB.s$x,y.s,Wd)


#Ww (new weights)
Dist.w <- matrix(NA, ncol = 3, nrow = 3)
colnames(Dist.w) <- c("Quad", "Pois", "Discr")
rownames(Dist.w) <- c("Quad", "Pois", "Discr")
#Quadratic
Dist.w[1,1] <- Quad_Dist(QB.w,y,Wqw)
Dist.w[2,1] <- Quad_Dist(PB.w$x,y,Wqw)
Dist.w[3,1] <- Quad_Dist(DB.w$x,y,Wqw)

#Poisson
Dist.w[1,2] <- PoisDev_Dist(QB.w,y,Wdw)
Dist.w[2,2] <- PoisDev_Dist(PB.w$x,y,Wdw)
Dist.w[3,2] <- PoisDev_Dist(DB.w$x,y,Wdw)

#Discr
Dist.w[1,3] <- Discr_Dist(QB.w,y,Wdw)
Dist.w[2,3] <- Discr_Dist(PB.w$x,y,Wdw)
Dist.w[3,3] <- Discr_Dist(DB.w$x,y,Wdw)


getwd()
#save workspace for use in plots and tables
save.image(file = "Deming_out.RData")


