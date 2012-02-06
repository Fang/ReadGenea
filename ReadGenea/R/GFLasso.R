#implements blockwise group fused lasso of bleakly
cumsumsh = function(t){
return(cumsum(t)[-length(t)])
}


#can this be done sparsely for speed?


GFLasso <- function (Y, lambda, startpoint = NULL, trace = T){

#might be faster to work with betad, really.
Ymeans = colMeans(Y)
p = ncol(Y)
n = as.double(nrow(Y))
Y = Y - rep(Ymeans, each = n)
d = sqrt(n / (as.double(1:n) * (n- as.double(1:n))))[-n]#rep(1, n-1)

beta = startpoint
if (is.null(startpoint)) beta = Matrix(0, nrow(Y) - 1, ncol(Y))#rep(0, length(Y))

betasparse = as.matrix(matrix(apply(beta,2, removeZero), ncol=p))

activeset = which(rowSums(abs(beta)) != 0)

C = -apply(Y[1:(n-1),], 2, cumsum) * d
maxiter = 10000
maxiter2 = 10000
for (iter in 1:maxiter){
if (trace) cat(iter, ", A=", activeset, "\n")
for (iter2 in 1:maxiter2){
betaold = betasparse
#ivar = 1
if (length(activeset) == 0) break
activepool = 1:length(activeset)
if (length(activeset) > 1) activepool = sample(activepool)
for (var in activepool){
activevar = activeset[var]
btmp = betasparse * d[activeset]; btmp[var, ] = 0
#cumsums = scale(matrix(apply(btmp,2, cumsum), ncol = p), scale=F, center =  drop( (n-activeset) %*% btmp / n))


Svar = C[activevar,] +  d[activevar] * colSums(rbind( -drop( (n-activeset) %*% btmp / n), btmp) *  pmax(activevar - c(0,activeset), 0 ))
#C[var,] -   d[var] *apply(   (    scale(rbind(0,apply(btmp, 2, cumsum)) ,scale=F, center= drop( ((n-1): 1) %*% btmp / n))), 2, function(t) -sum(t[1:var]) + sum(t) * var/n) 
betasparse[var,] = Svar * n/(activevar * (as.double(n) - activevar) * d[activevar]^2) *max(0, 1- lambda/sqrt(sum(Svar^2)))
#ivar = ivar +1
}
if ((max(abs(betaold - betasparse)) -> err) < 1e-3) break
if (trace) cat("[",iter2, ":" , err, "]")
if (maxiter2 == iter2) print ("Out of max iterations! (Inner loop)")
}
if (trace) cat("\n")
beta[activeset,] = betasparse
activeset = activeset[rowSums(abs(betasparse )) != 0]
betasparse = betasparse[rowSums(abs(betasparse )) != 0,, drop=F]

#check KKT

Ssq = replace(rowSums( (C -   d * apply(  scale( rbind(0,apply(beta*d, 2, cumsum)), scale = F, center =  drop( ((n-1): 1) %*% (beta*(d / n)))),2, function(t) -cumsumsh(t)))^2), activeset, 0) # need a speedup
#Ssq = replace(rowSums( (C -   d * apply(( rbind(0,apply(beta*d, 2, cumsum)) - drop( ((n-1): 1) %*% (beta*(d / n)))),2, function(t) -cumsumsh(t) + sum(t) * (1:(n-1))/n))^2), activeset, 0) # need a speedup

candidate = which.max(Ssq); M = max(Ssq) # warning, this can cause possible problems in case of ties in some applications.
if (trace) print(M)

if (M >= lambda^2){
activeset = c(activeset, candidate)
betasparse = rbind(betasparse,0)
} else {
return(scale(rbind(0,apply((beta*d),2, cumsum)), scale=F) + rep(Ymeans, each=n) )
}
}
print("Out of max iterations! (Outer loop)")
invisible(scale(rbind(0,apply((beta*d),2, cumsum)), scale=F) + rep(Ymeans, each=n))
}

