#implements blockwise group fused lasso of bleakly
cumsumsh = function(t){
return(cumsum(t)[-length(t)])
}
GFLasso <- function (Y, lambda, startpoint = NULL){

#might be faster to work with betad, really.
Ymeans = colMeans(Y)
p = ncol(Y)
n = nrow(Y)
Y = Y - rep(Ymeans, each = n)
d = rep(1, n-1)
activeset = NULL
beta = matrix(0, nrow(Y) - 1, ncol(Y))#rep(0, length(Y))
C = -apply(Y[1:(n-1),], 2, cumsum) * d
maxiter = 10000
maxiter2 = 10000
for (iter in 1:maxiter){
cat(iter, ", A=", activeset, "\n")
for (iter2 in 1:maxiter2){
betaold = beta
#ivar = 1
if (length(activeset) == 0) break
for (var in activeset){
btmp = beta * d; btmp[var, ] = 0
Svar = C[var,] -   d[var] *apply(( rbind(0,apply(btmp, 2, cumsum)) - drop( ((n-1): 1) %*% btmp / n)), 2, function(t) -sum(t[1:var]) + sum(t) * var/n) #need to incorporate d
beta[var,] = Svar * n/(var * (n - var) * d[var]^2) *max(0, 1- lambda/sqrt(sum(Svar^2)))
#ivar = ivar +1
}
if ((max(abs(betaold - beta)) -> err) < 1e-3) break
cat("[",iter2, ":" , err, "]")
if (maxiter2 == iter2) print ("Out of max iterations! (Inner loop)")
}
cat("\n")
activeset = activeset[rowSums(abs(beta[activeset,, drop=F] )) != 0]

#check KKT

Ssq = replace(rowSums( (C -   d * apply(( rbind(0,apply(beta*d, 2, cumsum)) - drop( ((n-1): 1) %*% (beta*(d / n)))),2, function(t) -cumsumsh(t) + sum(t) * (1:(n-1))/n))^2), activeset, 0)

candidate = which.max(Ssq); M = max(Ssq)

if (M >= lambda^2){
activeset = c(activeset, candidate)
} else {
return(scale(apply(rbind(0,beta*d),2, cumsum), scale=F) + rep(Ymeans, each=n) )
}
}
print("Out of max iterations! (Outer loop)")
invisible(scale(apply(rbind(0,beta*d),2, cumsum), scale=F) + rep(Ymeans, each=n))
}

