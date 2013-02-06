#Benchmark methods
#functions with arguments
#use notation and convensions consistent with Model-Assisted Notes
require(Matrix)

##Quadratic
Quad_Ben <- function(y,A,q,Wi){#try to get A and Wi as sparse
x <- as.matrix(y + Wi%*%t(A)%*%solve(A%*%Wi%*%t(A))%*%(q-A%*%y))
return(x)
}

Quad_Dist <- function(x,y,W){
d <- as.matrix(t(x-y)%*%W%*%(x-y))
return(d)
}

Quad_Ben_NL <- function(y,g,Dg,q,Wi, x0, lam0, mxit, tol){
xnew <- x0
lnew <- lam0

sumj <- 0
#outer loop for x
for( i in 1:mxit[1]){
#lambda step
#inner loop
for(j in 1:mxit[2]){
lold <- lnew
xl <- as.matrix(y + Wi%*%Dg(xnew)%*%lnew)
lnew <- lnew + solve(t(Dg(xnew))%*%Wi%*%Dg(xl) )%*%(q - g(xl))
Lj <- mean((lnew - lold)^2)
Lj
if(Lj <= tol[2]){break}
}#end inner loop
if(j >= mxit[2]){cat("Max iterations reached for inner loop\n")}
sumj <- sumj + j

#xstep
xold <- xnew
xnew <- as.matrix(y + Wi%*%Dg(xnew)%*%lnew)
Li <- mean((xnew - xold)^2)
if(Li <= tol[1]){break}
}#end outer loop
if(i >= mxit[1]){cat("Max iterations reached for outer loop\n")}
#check how close to zero
value <- sum((q - g(xnew))^2)

return(list(x = xnew, lam = lnew,
		value = value, Itr = c(i,sumj), Conv = list(L=c(Li, Lj),tol = tol) ))
}


##Poisson (logliner model) Deviance
PoisDev_Ben <- function(y,A,q,Wdi,x0, mxit, tol){
nx <- length(x0)
xnew <- x0

for(i in 1:mxit){
dx <- Diagonal(x = xnew, n = nx)
xold <- xnew
xnew <- as.matrix(y + Wdi%*%dx%*%t(A)%*%solve(A%*%Wdi%*%dx%*%t(A))%*%(q-A%*%y))
L2 <- mean((xnew - xold)^2)
if(L2 <= tol){break}
}
if(i >= mxit){cat("Max iterations reached\n")}

value <- sum((q - A%*%xnew)^2)

return(list(x = xnew, value = value, Itr = i, Conv = list(L2=L2,tol = tol) ))
}


PoisDev_Ben_NL <- function(y,g,Dg,q,Wdi, x0, lam0, mxit, tol){
xnew <- x0
n <- length(xnew)
xd <- Diagonal(x = xnew, n=n)
lnew <- lam0

sumj <- 0
#outer loop for x
for( i in 1:mxit[1]){
#lambda step
#inner loop
for(j in 1:mxit[2]){
lold <- lnew
xl <- y + Wdi%*%xd%*%Dg(xnew)%*%lnew
lnew <- lnew + solve(t(Dg(xnew))%*%Wdi%*%xd%*%Dg(xl) )%*%(q - g(xl))
#Lj <- mean((lnew - lold)^2)
if(Lj <= tol[2]){break}
}#end inner loop
if(j >= mxit[2]){cat("Max iterations reached for inner loop\n")}
sumj <- sumj + j

#xstep
xold <- xnew
xnew <- as.matrix(y + xd%*%Wdi%*%Dg(xnew)%*%lnew)
xd <- Diagonal(x = xnew, n=n)
Li <- mean((xnew - xold)^2)
if(Li <= tol[1]){break}
}#end outer loop
if(i >= mxit[1]){cat("Max iterations reached for outer loop\n")}
#check how close to zero
value <- sum((q - g(xnew))^2)

return(list(x = xnew, lam = lnew,
		value = value, Itr = c(i,sumj), Conv = list(L=c(Li, Lj),tol = tol) ))
}



PoisDev_Dist <- function(x,y,W){
d <- as.matrix(sum(W%*%(y*log(y/x) - y + x  )))
return(d)
}

##Discrimination Measure
Discr_Ben <- function(y,A,q,Wdi, lam0, mxit, tol){
ny <- length(y)
lnew <- lam0
for(i in 1:mxit){
lold <- lnew
Expy <- as.matrix(exp(Wdi%*%t(A)%*%lnew)*y)
DExpy <- Diagonal(x = Expy, n = ny)
h <- A%*%Expy
lnew <- lnew + as.matrix(solve(A%*%Wdi%*%DExpy%*%t(A))%*%(q - h))
L2 <- mean((lnew - lold)^2)
if(L2 <= tol){break}
}
if(i >= mxit){cat("Max iterations reached\n")}
Expy <- as.matrix(exp(Wdi%*%t(A)%*%lnew)*y)
x <- Expy
value <- sum((q - A%*%x)^2)
return(list(x = x, lam = lnew, value = value, Itr = i, Conv = list(L2=L2,tol = tol) ))
}

Discr_Ben_NL <- function(y,g,Dg,q,Wdi, x0, lam0, mxit, tol){
xnew <- x0
n <- length(xnew)
lnew <- lam0

sumj <- 0
#outer loop for x
for( i in 1:mxit[1]){
#lambda step
#inner loop
for(j in 1:mxit[2]){
lold <- lnew
xl <- as.matrix(exp(Wdi%*%Dg(xnew)%*%lnew)*y)
dxl <- Diagonal(x = xl, n = n)
lnew <- lnew + solve(t(Dg(xnew))%*%Wdi%*%dxl%*%Dg(xl) )%*%(q - g(xl))
Lj <- mean((lnew - lold)^2)
if(Lj <= tol[2]){break}
}#end inner loop
if(j >= mxit[2]){cat("Max iterations reached for inner loop\n")}
sumj <- sumj + j

#xstep
xold <- xnew
xnew <- as.matrix(exp(Wdi%*%Dg(xnew)%*%lnew)*y)
xd <- Diagonal(x = xnew, n=n)
Li <- mean((xnew - xold)^2)
if(Li <= tol[1]){break}
}#end outer loop
if(i >= mxit[1]){cat("Max iterations reached for outer loop\n")}
#check how close to zero
value <- sum((q - g(xnew))^2)

return(list(x = xnew, lam = lnew,
		value = value, Itr = c(i,sumj), Conv = list(L=c(Li, Lj),tol = tol) ))
}

Discr_Dist <- function(x,y,W){
d <- as.matrix(sum(W%*%(x*log(x/y) - x + y  )))
return(d)
}

#other distance not included above
Hellg_Dist <- function(x,y,Wd){
z <- sqrt(x) - sqrt(y)
d <- as.matrix(t(z)%*%Wd%*%z)
return(d)
}

Alt_Quad_Dist <- function(x,y,Wd){
z <- (x - y)/sqrt(x)
d <- as.matrix(t(z)%*%Wd%*%z)
return(d)
}

####general solver - deviance functions are arguments####
Gen_Dist_NL <- function(y,g,Dg,q,W, x0, lam0, mxit, tol,h,hp){
xnew <- x0
n <- length(xnew)
lnew <- lam0

sumj <- 0
#outer loop for x
for( i in 1:mxit[1]){
#lambda step
#inner loop
for(j in 1:mxit[2]){
lold <- lnew
Dgnew <- Dg(xnew)
hxl <- h(as.matrix(W%*%Dgnew%*%lnew),y)
hpxl <- hp(as.matrix(W%*%Dgnew%*%lnew),y)
dhpxl <- Diagonal(x = hpxl, n = n)
lnew <- lnew + solve(t(Dg(xnew))%*%W%*%dhpxl%*%Dg(hxl) )%*%(q - g(hxl))
Lj <- mean((lnew - lold)^2)

if(Lj <= tol[2]){break}
}#end inner loop
if(j >= mxit[2]){cat("Max iterations reached for inner loop\n")}
sumj <- sumj + j

#xstep
xold <- xnew
xnew <- h(as.matrix(W%*%Dg(xnew)%*%lnew),y)
xd <- Diagonal(x = xnew, n=n)
Li <- mean((xnew - xold)^2)
if(Li <= tol[1]){break}
}#end outer loop
if(i >= mxit[1]){cat("Max iterations reached for outer loop\n")}
#check how close to zero
value <- sum((q - g(xnew))^2)

return(list(x = xnew, lam = lnew,
		value = value, Itr = c(i,sumj), Conv = list(L=c(Li, Lj),tol = tol) ))
}


#linear expecting A to be set
g.lin <- function(x,M = A){M%*%x}
Dg.lin <- function(x,M = A){t(M)}


#Quadratic distance
h.Q <- function(u,y){u + y}
hp.Q <- function(u,y){u*0 + 1}
d1.Q <- function(x,y){x - y}

#Poisson deviance
h.P <- function(u,y){y/(1-u)}
hp.P <- function(u,y){y/(1-u)^2}
d1.P <- function(x,y){1 - y/x}

#Discrimination information
h.D <- function(u,y){y*exp(u)}
hp.D <- function(u,y){y*exp(u)}
d1.D <- function(x,y){log(x/y)}

#Hellinger distance
h.H <- function(u,y){y*(1-u)^{-2}}
hp.H <- function(u,y){2*y*(1-u)^{-3}}
d1.H <- function(x,y){1 - sqrt(y/x)}

#Alternative Quadratic distance
h.AQ <- function(u,y){y/sqrt(1-u)}
hp.AQ <- function(u,y){y/(2*(1-u)^(3/2))}
d1.AQ <- function(x,y){1 - (y/x)^2}

