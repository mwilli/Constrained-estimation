#NASS-like Example with linear and nonlinear constraints

#N is numerate, D is denominate, R = rate or ratio R = N/D
#constraints are on totals for N and D
#linear versions have N and D as data
##constraining R - need to augment A
#nonlinear constraints have D and Y
##constraining R - now use W_0

#simulate N,D,R - 20 sets#
##uncomment to create new data
##otherwise import below

n <- 20
#simulate D (acres or sows)
## between 50 and 1000 #wider range
#D <- 1/(runif(n, min = 1/1000, max = 1/50))

#simulate R (yield or cash rent or litter rate)
## narrower range (20 and 50)
#R <- rnorm(n, mean = 35, sd = 10) #make sure R > 0

#N <- R*D


#dat <- data.frame(Num = N, Den = D, Ratio = R)
#save
getwd()
setwd("NASS Example")
#write.csv(dat, file = "NASS_table.csv")
dat <- read.csv(file = "NASS_table.csv")

#now check totals
#observed
sTot <- c(sum(dat$Num), sum(dat$Den))
SR <- sTot[1]/sTot[2]
#set target
PTot <- sTot*c(.95, 1.03)
PR <- PTot[1]/PTot[2]

#load benchmarking functions#
source("benchmark_functions_nl.r")
library(MASS)

##############linear#############
#
#
#
#
#
##############

###Default Setting###
q <- matrix(PTot, nrow = 2)
grp <- c(rep(1,20), rep(2,20))
A <- t(model.matrix(~as.factor(grp)-1))
rownames(A) <- c("Num", "Den")

#stack data
y <- matrix(c(dat$Num,dat$Den), ncol = 1, nrow = 20*2)

#Quadratic
Wq <- Diagonal(x = 1/y, n = 20*2)
Wqi <- solve(Wq)

Q <- Quad_Ben(y,A,q,Wqi)
dQ <- Quad_Dist(Q,y,Wq)

#Poisson and Discrimination
Wd <- Diagonal(x = 1, n = 20*2)
Wdi <- solve(Wd)

P <- PoisDev_Ben(y,A,q,Wdi, x0 = y, mxit = 15, tol = 1e-8)
D <- Discr_Ben(y,A,q,Wdi,lam0 = q*0, mxit = 15, tol = 1e-8)

##User Intervention##
##Fix / Protect 5 or smaller ones###
#
#1,3,10,12,14
fixl <- c(c(1,3,10,12,14), c(1,3,10,12,14)+20)

#Quadratic#
Wq0 <- as.matrix(Wq)
Wq0[fixl,] <- Wq0[,fixl] <- 0
Wq0i <- ginv(Wq0) #not implemented for sparse matrices
Wq0i <- as(Wq0i, Class = "CsparseMatrix")#coerce to a sparse matrix

Q0 <- Quad_Ben(y,A,q,Wq0i)

#Poisson and Discrimination#
Wd0 <- as.matrix(Wd)
Wd0[fixl,] <- Wd0[,fixl] <- 0
Wd0i <- ginv(Wd0)
Wd0i <- as(Wd0i, Class = "CsparseMatrix")

P0 <- PoisDev_Ben(y,A,q,Wd0i, x0 = y, mxit = 20, tol = 1e-8)
D0 <- Discr_Ben(y,A,q,Wd0i,lam0 = q*0, mxit = 20, tol = 1e-8)

##Path 1###
##Set R's for some##
#set R is 2,5,16
#reducing  y and W and changing A
redl <- c(2,5,16)
R.r <- round(dat$Ratio[redl])
A.r <- A
A.r[1,redl+20] <- R.r
A.r[1,redl] <- 0 #could shrink dimension, but nice to keep y and W same#


##need to recalculate N based on new D and set R
##By construction the N part of y is ignored for redl set#
QR <- Quad_Ben(y,A.r,q,Wq0i)
QR[1:20] <- QR[1:20]*t(A.r[1,1:20]) + QR[21:40]*t(A.r[1,21:40])
PR <- PoisDev_Ben(y,A.r,q,Wd0i, x0 = y, mxit = 20, tol = 1e-8)
PR$x[1:20] <- PR$x[1:20]*t(A.r[1,1:20]) + PR$x[21:40]*t(A.r[1,21:40])
DR <- Discr_Ben(y,A.r,q,Wd0i,lam0 = q*0, mxit = 20, tol = 1e-8)
DR$x[1:20] <- DR$x[1:20]*t(A.r[1,1:20]) + DR$x[21:40]*t(A.r[1,21:40])

#Both should now give correct results:
#A%*%QR
#A.r%*%QR

##Set reweighting with alpha = 1/5##
#reweight consistently for N and D.
rwpl <- c(2,16,5,11,8,9,13)
w <- rep(1,40)
w[c(rwpl, rwpl + 20)] <- 1/5
Wl <- Wr <- Diagonal(x = sqrt(w), n = 40)

Wq0w <- as.matrix(Wl%*%Wq0%*%Wr)
Wq0wi <- as(ginv(Wq0w), Class = "CsparseMatrix")

Wd0w <- as.matrix(Wl%*%Wd0%*%Wr)
Wd0wi <- as(ginv(Wd0w), Class = "CsparseMatrix")

##need to recalculate N based on new D and set R
QR.w <- Quad_Ben(y,A.r,q,Wq0wi)
QR.w[1:20] <- QR.w[1:20]*t(A.r[1,1:20]) + QR.w[21:40]*t(A.r[1,21:40])
PR.w <- PoisDev_Ben(y,A.r,q,Wd0wi, x0 = y, mxit = 20, tol = 1e-8)
PR.w$x[1:20] <- PR.w$x[1:20]*t(A.r[1,1:20]) + PR.w$x[21:40]*t(A.r[1,21:40])
DR.w <- Discr_Ben(y,A.r,q,Wd0wi,lam0 = q*0, mxit = 20, tol = 1e-8)
DR.w$x[1:20] <- DR.w$x[1:20]*t(A.r[1,1:20]) + DR.w$x[21:40]*t(A.r[1,21:40])
DRA.w  <- Quad_Ben(DR.w$x,A.r,q,Wd0wi)
DRA.w[1:20] <- DRA.w[1:20]*t(A.r[1,1:20]) + DRA.w[21:40]*t(A.r[1,21:40])

#Both should now give correct results:
#A%*%QR.w
#A.r%*%QR.w


##Path 2###
##Set R's for all
Redl <- (1:20)[-c(1,3,10,12,14)]
R.R <- round(dat$Ratio[Redl])
A.R <- A
A.R[1,Redl+20] <- R.R
A.R[1,Redl] <- 0 #could shrink dimension, but nice to keep y and W same#

##need to recalculate N based on new D and set R
QR.R <- Quad_Ben(y,A.R,q,Wq0i)
QR.R[1:20] <- QR.R[1:20]*t(A.R[1,1:20]) + QR.R[21:40]*t(A.R[1,21:40])
PR.R <- PoisDev_Ben(y,A.R,q,Wd0i, x0 = y, mxit = 20, tol = 1e-8)
PR.R$x[1:20] <- PR.R$x[1:20]*t(A.R[1,1:20]) + PR.R$x[21:40]*t(A.R[1,21:40])
DR.R <- Discr_Ben(y,A.R,q,Wd0i,lam0 = q*0, mxit = 20, tol = 1e-8)
DR.R$x[1:20] <- DR.R$x[1:20]*t(A.R[1,1:20]) + DR.R$x[21:40]*t(A.R[1,21:40])
DRA.R  <- Quad_Ben(DR.R$x,A.R,q,Wd0i)
DRA.R[1:20] <- DRA.R[1:20]*t(A.R[1,1:20]) + DRA.R[21:40]*t(A.R[1,21:40])

#Both should now give correct results:
#A%*%QR.R
#A.R%*%QR.R


#####NonLinear######
#
#
#
#
#
####################
#source("benchmark_functions_nl.r")

#use D and R instead of N and D

#create nonlinear function and derivative function

#q is now g(x) = sum(D), sum(R*D)
g <- function(x){#x is even length
n <- floor(length(x)/2)
g <- matrix(NA, nrow = 2)
g[1] <- sum(x[1:n]*x[(n+1):(2*n)])
g[2] <- sum(x[1:n])
return(g)
}

D.g <- function(x){
n <- floor(length(x)/2)
D <- matrix(0, ncol = 2, nrow = 2*n)
D[1:n,2] <- 1
D[1:n,1] <- x[(n+1):(2*n)]
D[(n+1):(2*n),1] <- x[1:n]
return(D)
}

#stack the y values
y.nl <- c(dat$Den, dat$Ratio)
Wq.nl <- Diagonal(x = 1/y.nl, n = 40) #make sure 1/y
Wq.nli <- solve(Wq.nl)
Wd.nl <- Wd #just Identity
Wd.nli <- Wdi #just Identity

##Default##
#No Restrictions#
Q.nl <- Quad_Ben_NL(y.nl,g,D.g,q,Wq.nli, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )
P.nl <- PoisDev_Ben_NL(y.nl,g,D.g,q,Wd.nli, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )
D.nl <- Discr_Ben_NL(y.nl,g,D.g,q,Wd.nli, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )

#alternatively use general functions
#Q.nl.2 <- Gen_Dist_NL(y.nl,g,D.g,q,Wq.nli, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8), h = h.Q, hp = hp.Q )
#P.nl.2 <- Gen_Dist_NL(y.nl,g,D.g,q,Wd.nli, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8), h = h.P, hp = hp.P )
#D.nl.2 <- Gen_Dist_NL(y.nl,g,D.g,q,Wd.nli, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8), h = h.D, hp = hp.D )


##User interaction##
#Some D, R  pairs fixed#
fixl <- c(c(1,3,10,12,14), c(1,3,10,12,14)+20)

#Quadratic
Wq0.nl <- as.matrix(Wq.nl)
Wq0.nl[fixl,] <- Wq0.nl[,fixl] <- 0
Wq0i.nl <- ginv(Wq0.nl)
Wq0i.nl <- as(Wq0i.nl, Class = "CsparseMatrix")

Q0.nl <- Quad_Ben_NL(y.nl,g,D.g,q,Wq0i.nl, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )

#Poisson and Discrimination
Wd0.nl <- as.matrix(Wd.nl)
Wd0.nl[fixl,] <- Wd0.nl[,fixl] <- 0
Wd0i.nl <- ginv(Wd0.nl)
Wd0i.nl <- as(Wd0i.nl, Class = "CsparseMatrix")

P0.nl <- PoisDev_Ben_NL(y.nl,g,D.g,q,Wd0i.nl, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )
D0.nl <- Discr_Ben_NL(y.nl,g,D.g,q,Wd0i.nl, x0 = y.nl, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )

##Path 1##
#In Addition some extra R's fixed#
#round these r's
y.nlr <- y.nl
y.nlr[redl+20] <- round(y.nlr[redl+20])

#Quadratic
Wq02.nl <- diag(c(1/y.nlr))
Wq02.nl[redl+20,] <- Wq02.nl[,redl+20] <-0
Wq02.nl[fixl,] <- Wq02.nl[,fixl] <- 0
Wq02i.nl <- ginv(Wq02.nl)
Wq02i.nl <- as(Wq02i.nl, Class = "CsparseMatrix")

Q02.nl <- Quad_Ben_NL(y.nlr,g,D.g,q,Wq02i.nl, x0 = y.nlr, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )

#Poisson and Discrimination
Wd02.nl <- Wd0.nl
Wd02.nl[redl+20,] <- Wd02.nl[,redl+20] <- 0
Wd02i.nl <- ginv(Wd02.nl)
Wd02i.nl <- as(Wd02i.nl, Class = "CsparseMatrix")

P02.nl <- PoisDev_Ben_NL(y.nlr,g,D.g,q,Wd02i.nl, x0 = y.nlr, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )
D02.nl <- Discr_Ben_NL(y.nlr,g,D.g,q,Wd02i.nl, x0 = y.nlr, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )

##Fix some R's and add scaling##
rwpl <- c(2,16,5,11,8,9,13)
w <- rep(1,40)
w[c(rwpl, rwpl + 20)] <- 1/5
Wl <- Wr <- Diagonal(x = sqrt(w), n = 40)

#Quadratic
Wq02.nl.w <- as.matrix(Wl%*%Wq02.nl%*%Wr)
Wq02.nl.wi <- as(ginv(Wq02.nl.w), Class = "CsparseMatrix")

Q02.nl.w <- Quad_Ben_NL(y.nlr,g,D.g,q,Wq02.nl.wi, x0 = y.nlr, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )

#Poisson and Discrimination
Wd02.nl.w <- as.matrix(Wl%*%Wd02.nl%*%Wr)
Wd02.nl.wi <- as(ginv(Wd02.nl.w), Class = "CsparseMatrix")

P02.nl.w <- PoisDev_Ben_NL(y.nlr,g,D.g,q,Wd02.nl.wi, x0 = y.nlr, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )
D02.nl.w <- Discr_Ben_NL(y.nlr,g,D.g,q,Wd02.nl.wi, x0 = y.nlr, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )

##Path 2##
##In Addition fix all R's##
y.nlR <- y.nlr
y.nlR[(21:40)[-c(1,3,10,12,14)]] <- round(y.nlR[(21:40)[-c(1,3,10,12,14)]])

#Quadratic
Wq0R.nl <- diag(c(1/y.nlR))
Wq0R.nl[(21:40),] <- Wq0R.nl[,(21:40)] <- 0
Wq0R.nl[fixl,] <- Wq0R.nl[,fixl] <- 0
Wq0Ri.nl <- ginv(Wq0R.nl)
Wq0Ri.nl <- as(Wq0Ri.nl, Class = "CsparseMatrix")

Q0R.nl <- Quad_Ben_NL(y.nlR,g,D.g,q,Wq0Ri.nl, x0 = y.nlR, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )

#Poisson and Discrimination#
Wd0R.nl <- Wd0.nl
Wd0R.nl[(21:40),] <- Wd0R.nl[,(21:40)] <- 0
Wd0Ri.nl <- ginv(Wd0R.nl)
Wd0Ri.nl <- as(Wd0Ri.nl, Class = "CsparseMatrix")

P0R.nl <- PoisDev_Ben_NL(y.nlR,g,D.g,q,Wd0Ri.nl, x0 = y.nlR, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )
D0R.nl <- Discr_Ben_NL(y.nlR,g,D.g,q,Wd0Ri.nl, x0 = y.nlR, lam0 = q*0, mxit = c(20,20), tol = c(1e-8, 1e-8) )


####################
#need to save and then polish up and export#
getwd()
save.image(file = "NASS_Example_Estimates.RData")


