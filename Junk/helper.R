CONSTtesting = FALSE

#some helper functions
cdiag = function(x = 1,nrow,ncol){
	diag(as.vector(x) -> y,nrow = length(y))
}

getSort <- function(X){
  sortcode = apply(X,2,order)
#  sortcode = X
#  for (i in 1:ncol(X)){
#    sortcode[,i] = order(X[,i])
#  }
  sortcode
}



sparseoverlap = function(B, SIGMA=diag(dim(B)[1])){
  for (i in 1:(dim(B)[1])){
    if (0 < sum(B[i,]^2) -> aver){
      B[i,] = B[i,] / aver
    }
  }
  temp = t(B) %*% solve(SIGMA,B)
  max(svd(t(temp) %*% temp)$d)
}

# positive constrained version of lars
"larspos" <-
  function(x, y, type = c("lasso", "lar", "forward.stagewise","stepwise"), trace = FALSE,
           normalize=TRUE, intercept=TRUE, Gram, 
           eps = 1e-08,  max.steps, use.Gram = TRUE)
#           eps = .Machine$double.eps,  max.steps, use.Gram = TRUE)
#too much spurious accuracy is dangerous
{
### program automatically centers and standardizes predictors by default.
###
### Original program by Brad Efron September 2001
### Recoded by Trevor Hastie November 2001
### Computational efficiency December 22, 2001
### Bug fixes and singularities February 2003
### Conversion to R April 2003
### stepwise and non-standardize options added May 2007
### Copyright Brad Efron and Trevor Hastie
  call <- match.call()
  type <- match.arg(type)
  TYPE <- switch(type,
                 lasso = "LASSO",
                 lar = "LAR",
                 forward.stagewise = "Forward Stagewise",
                 stepwise = "Forward Stepwise")
  if(trace)
    cat(paste(TYPE, "sequence\n"))
  
  nm <- dim(x)
  n <- nm[1]
  m <- nm[2]
  im <- inactive <- seq(m)
  one <- rep(1, n)
  vn <- dimnames(x)[[2]]	
### Center x and y, and scale x, and save the means and sds
  if(intercept){
    meanx <- drop(one %*% x)/n
    x <- scale(x, meanx, FALSE)	# centers x
    mu <- mean(y)
    y <- drop(y - mu)
  }
  else {
    meanx <- rep(0,m)
    mu <- 0
    y <- drop(y)
  }
  if(normalize){
    normx <- sqrt(drop(one %*% (x^2)))
    nosignal<-normx/sqrt(n) < eps
    if(any(nosignal))# ignore variables with too small a variance
      {
        ignores<-im[nosignal]
        inactive<-im[-ignores]
        normx[nosignal]<-eps*sqrt(n)
        if(trace)
          cat("LARS Step 0 :\t", sum(nosignal), "Variables with Variance < eps; dropped for good\n")	#
      }
    else ignores <- NULL #singularities; augmented later as well
    names(normx) <- NULL
    x <- scale(x, FALSE, normx)	# scales x
  }
  else {
    normx <- rep(1,m)
    ignores <- NULL
  }
  if(use.Gram & missing(Gram)) {
    if(m > 500 && n < m)
      cat("There are more than 500 variables and n<m;\nYou may wish to restart and set use.Gram=FALSE\n"
          )
    if(trace)
      cat("Computing X'X .....\n")
    Gram <- t(x) %*% x	#Time saving
  }
  Cvec <- drop(t(y) %*% x)
  ssy <- sum(y^2)	### Some initializations
  residuals <- y
  if(missing(max.steps))
    max.steps <- 8*min(m, n-intercept)
  beta <- matrix(0, max.steps + 1, m)	# beta starts at 0
  lambda=double(max.steps)
  Gamrat <- NULL
  arc.length <- NULL
  R2 <- 1
  RSS <- ssy
  first.in <- integer(m)
  active <- NULL	# maintains active set
  actions <- as.list(seq(max.steps))	
                                        # a signed index list to show what comes in and out
  drops <- FALSE	# to do with type=="lasso" or "forward.stagewise"
  Sign <- NULL	# Keeps the sign of the terms in the model
  R <- NULL	###
### Now the main loop over moves
###
  k <- 0
  while((k < max.steps) & (length(active) <  min(m - length(ignores),n-intercept)) )
    {
      action <- NULL
      C <- Cvec[inactive]	#
### identify the largest nonactive gradient
      Cmax <- max(C)
      if(Cmax<eps*100){ # the 100 is there as a safety net
        if(trace)cat("Max |corr| = 0; exiting...\n")
        #print("FINISHED BY COMPLETION")
        break
      }
      k <- k + 1
      lambda[k]=Cmax
### Check if we are in a DROP situation
      if(!any(drops)) {
        new <- C >= Cmax - eps
        C <- C[!new]	# for later
        new <- inactive[new]	# Get index numbers
### We keep the choleski R  of X[,active] (in the order they enter)
        for(inew in new) {
          if(use.Gram) {
            R <- updateR(Gram[inew, inew], R, drop(Gram[
                                                        inew, active]), Gram = TRUE,eps=eps)
          }
          else {
            R <- updateR(x[, inew], R, x[, active], Gram
                         = FALSE,eps=eps)
          }
          if(attr(R, "rank") == length(active)) {
            ##singularity; back out
            nR <- seq(length(active))
            R <- R[nR, nR, drop = FALSE]
            attr(R, "rank") <- length(active)
            ignores <- c(ignores, inew)
            action <- c(action,  - inew)
            if(trace)
              cat("LARS Step", k, ":\t Variable", inew, 
                  "\tcollinear; dropped for good\n")	#
          }
          else {
            if(first.in[inew] == 0)
              first.in[inew] <- k
            active <- c(active, inew)
            Sign <- c(Sign, sign(Cvec[inew]))
            action <- c(action, inew)
            if(trace)
              cat("LARS Step", k, ":\t Variable", inew, 
                  "\tadded\n")	#
          }
        }
      }
      else action <-  - dropid
      Gi1 <- backsolve(R, backsolvet(R, Sign))	
### Now we have to do the forward.stagewise dance
### This is equivalent to NNLS
      dropouts<-NULL
      A <- 1/sqrt(sum(Gi1 * Sign))
      w <- A * Gi1	# note that w has the right signs
      if(!use.Gram) u <- drop(x[, active, drop = FALSE] %*% w)	###
### Now we see how far we go along this direction before the
### next competitor arrives. There are several cases
###
### If the active set is all of x, go all the way
      if( (length(active) >=  min(n-intercept, m - length(ignores) ) )|type=="stepwise") {
        gamhat <- Cmax/A
      }
      else {
        if(use.Gram) {
          a <- drop(w %*% Gram[active,  - c(active,ignores), drop = FALSE])
        }
        else {
          a <- drop(u %*% x[,  - c(active, ignores), drop=FALSE])
        }
        gam <- c((Cmax - C)/(A - a))#, (Cmax + C)/(A + a))	
### Any dropouts will have gam=0, which are ignored here
        gamhat <- min(gam[gam > eps], Cmax/A)	
      }
      if(type == "lasso") {
        dropid <- NULL
        b1 <- beta[k, active]	# beta starts at 0
        z1 <-  - b1/w
        zmin <- min(z1[z1 > eps], gamhat)
        if(zmin < gamhat) {
          gamhat <- zmin
          drops <- z1 == zmin
        }
        else drops <- FALSE
      }
      beta[k + 1,  ] <- beta[k,  ]
      beta[k + 1, active] <- beta[k + 1, active] + gamhat * w
      if(use.Gram) {
        Cvec <- Cvec - gamhat * Gram[, active, drop = FALSE] %*% w
      }
      else {
        residuals <- residuals - gamhat * u
        Cvec <- drop(t(residuals) %*% x)
      }
      Gamrat <- c(Gamrat, gamhat/(Cmax/A))
      arc.length <- c(arc.length, gamhat)	
### Check if we have to drop any guys
      if(type == "lasso" && any(drops)) {
        dropid <- seq(drops)[drops]	
                                        #turns the TRUE, FALSE vector into numbers
        for(id in rev(dropid)) {
          if(trace)
            cat("Lasso Step", k+1, ":\t Variable", active[
                                                        id], "\tdropped\n")
          R <- downdateR(R, id)
        }
        dropid <- active[drops]	# indices from 1:m
        beta[k+1,dropid]<-0  # added to make sure dropped coef is zero
        active <- active[!drops]
        Sign <- Sign[!drops]
      }
      if(!is.null(vn))
        names(action) <- vn[abs(action)]
      actions[[k]] <- action
      inactive <- im[ - c(active, ignores)]
      if(type=="stepwise")Sign=Sign*0
    }
  beta <- beta[seq(k + 1), ,drop=FALSE ]	#
  lambda=lambda[seq(k)]
  dimnames(beta) <- list(paste(0:k), vn)	### Now compute RSS and R2
  if(trace)
    cat("Computing residuals, RSS etc .....\n")
  residuals <- y - x %*% t(beta)
  beta <- scale(beta, FALSE, normx)
  RSS <- apply(residuals^2, 2, sum)
  R2 <- 1 - RSS/RSS[1]
  actions=actions[seq(k)]
  netdf=sapply(actions,function(x)sum(sign(c(x,0))))
  df=cumsum(netdf)### This takes into account drops
  if(intercept)df=c(Intercept=1,df+1)
  else df=c(Null=0,df)
  rss.big=rev(RSS)[1]
  df.big=n-rev(df)[1]
  if(rss.big<eps|df.big<eps)sigma2=NaN
  else
    sigma2=rss.big/df.big
  Cp <- RSS/sigma2 - n + 2 * df
  attr(Cp,"sigma2")=sigma2
  attr(Cp,"n")=n
  object <- list(call = call, type = TYPE, df=df, lambda=lambda,R2 = R2, RSS = RSS, Cp = Cp, 
                 actions = actions[seq(k)], entry = first.in, Gamrat = Gamrat, 
                 arc.length = arc.length, Gram = if(use.Gram) Gram else NULL, 
                 beta = beta, mu = mu, normx = normx, meanx = meanx)
  class(object) <- "lars"
  object
}

nn = 100
pp = 50
dd = 10
genXX = function(nn, pp,ran=runif(1,-1,1)){
  V = matrix(1:(pp*pp),nrow=pp)
  for (i in 1:pp){
    for (j in 1:pp){
      V[i,j] = ran^(abs(i - j))
      }
  }
  cV = chol(V)
  XX = matrix(1:(pp*nn),ncol = pp)
  for (i in 1:nn){
    XX[i,] = t(cV) %*% rnorm(pp)
  }
  XX
}

genBetas = function(rho = 0,repeats = 0.4,dd,pp,blocks = 10,peak = 5){
  lk = sample(1:peak,blocks,replace = TRUE)
  jk = sample(1:pp,blocks,replace = TRUE)
  V = matrix(1:(dd*dd),nrow=dd)
  for (i in 1:dd){
    for (j in 1:dd){
      V[i,j] = rho^(abs(i - j))
      }
  }
  cV = chol(V)
  cik = matrix(1:(blocks*dd),nrow = dd)
  for (k in 1:blocks){
    cik[,k] = t(t(cV) %*% rnorm(dd))
    for (i in 1:dd){
      if (runif(1) < repeats){cik[i,k] = 0}
    }
  }
  gjk = matrix(1:(blocks*pp),nrow = pp)
  for (k in 1:blocks){
    for (j in 1:pp){
      gjk[j,k] = max(0,lk[k] - abs(j - jk[k]))^2
    }
    gjk[,k] = gjk[,k] / sum(gjk[,k])
  }
  betas = t(cik %*% t(gjk))
}

#sigma = 0.5
genNoise = function(sigma,nn,dd,entry=rep(1,dd)){
  matrix(sigma * rnorm(nn*dd) * rep(entry,each=nn),ncol = dd)
}

genData = function(nn = 100, pp = 50 , dd = 10, ran =runif(1,-1,1), blocks = 10,peak=1, rho = 0.5, repeats = 0.4, sigma = 0.5,positive=F){
  X = genXX(nn, pp,ran)
  betas = genBetas(rho,repeats,dd,pp,blocks,peak)
  if (positive) {betas = abs(betas)}
  noise = genNoise(sigma,nn,dd)
  betas = betas / sqrt(var(as.vector(X %*% betas)))
  Y = X%*% betas + noise
  object <- list(call = call, beta = betas, X = X, Y=Y, noise = noise, p = pp, d = dd, n = nn, sigma = sigma, designcor = ran)
  class(object) <- "dataset"
  object
}

regenData = function(dataset, nn=dataset$n, designcor = dataset$designcor, sigma = dataset$sigma){
  object = dataset
  pp = object$p
  dd = object$d
  XX = genXX(nn,pp,designcor)
  noise = genNoise(sigma,nn,dd)
  YY = XX %*% object$beta + noise
  object$call = call
  object$X = XX
  object$Y = YY
  object$noise = noise
  object$n = nn
  object$sigma = sigma
  object$designcor = designcor
  object
}


##NOTES: defining a new sparse format for step functions in many dims
#
#class <- "multistep"
#$params 
#index   number of breaks    intercept
#$values 
#coefchaincount breakpoint delta previous
#$intercept 
#$ranges
#min max

# * -> used to get Y's out of it
"*.multistep" <- function(X, obj){
  if (!inherits(obj,"multistep")){
    return ("*.multistep"(obj,X))
  }
  pp <- nrow(obj$params)
  if (is.null(dim(X))){
    X <- matrix(X,ncol = pp)
  }
  nn <- nrow(X)
  addup <- function(vect){
    i = vect[1]
    if (obj$params[i,2] == 0){
      return(rep(0,length(vect)-1))
    } else {
      matchtable = obj$values[obj$params[i,1]:(obj$params[i,1]+obj$params[i,2]-1),2:3,drop = FALSE]
      res = sapply(vect[-1], function(x)  sum(matchtable[matchtable[,1] <= x,2])) + obj$params[i,3]
    }
    res
  }
  results = rowSums(matrix(apply(rbind(1:pp,X),2,function(x) addup(x)),nrow=nn)) + obj$intercept
  results
}

#the same as *, but does linear interpolation
"&.multistep" <- function(X, obj){
  if (!inherits(obj,"multistep")){
    return ("&.multistep"(obj,X))
  }
  pp <- nrow(obj$params)
  if (is.null(dim(X))){
    X <- matrix(X,ncol = pp)
  }
  nn <- nrow(X)
  addup <- function(vect){
    i = vect[1]
    if (obj$params[i,2] == 0){
      return(rep(0,length(vect)-1))
    } else {
      matchtable = rbind(c(obj$range[i,1],0),obj$values[obj$params[i,1]:(obj$params[i,1]+obj$params[i,2]-1),2:3,drop = FALSE])
      matchtable[,2] = cumsum(matchtable[,2])
      res = approx(matchtable, xout=vect[-1],rule=2)$y + obj$params[i,3]
    }
    res
  }
  results = rowSums(matrix(apply(rbind(1:pp,X),2,function(x) addup(x)),nrow=nn)) + obj$intercept
  results
}

max.multistep <- function(obj, na.rm){
  individualmax <- function(vect){
    if (vect[2] == 0){
      return(0)
    } else {
      return( sum(obj$values[vect[1]:(vect[1]+vect[2]-1), 3]) + vect[3])
    }
  }
  res <- drop(apply(obj$params,1,individualmax))
  res
}
min.multistep <- function(obj, na.rm){
  res <- drop(obj$params[,3])
  res
}
dim.multistep <- function(obj){
  res <- drop(obj$params[,2])
}

#pull out a coefchain
as.vector.multistep <- function(obj, mode){
  coefchain = replace(rep(0, (obj$n - 1)*nrow(obj$params)), obj$values[,1], obj$values[,3])
  coefchain
}

#turns multistep into 'fits'
as.array.multistep <- function( obj, dims = 1:nrow(obj$param), explode = FALSE, signs = NULL){
  coefchain = replace(rep(0, (obj$n - 1)*nrow(obj$params)), obj$values[,1], obj$values[,3])
  if (!explode){
  result = apply(rbind(obj$params[dims,3],matrix(coefchain, obj$n - 1))[,dims], 2, cumsum)
  } else {
    if (is.null(signs)){
      result = scale(apply(rbind(0, cbind(matrix(pmax(coefchain,0), obj$n-1)[,dims],apply(-matrix(pmin(coefchain, 0), obj$n-1)[,dims],2,rev))),2,cumsum), center=TRUE,scale=FALSE)
    } else {
      if (length(signs) == 1) signs = rep(signs, length(dims))
      #okay, we should explode or flip the appropriate signs.
      result1 = scale(apply(rbind(0, cbind(matrix(pmax(coefchain,0), obj$n-1)[,dims],apply(-matrix(pmin(coefchain, 0), obj$n-1)[,dims],2,rev))),2,cumsum), center=TRUE,scale=FALSE)
      result = apply(rbind(obj$params[dims,3],matrix(coefchain, obj$n - 1))[,dims], 2, cumsum)
      for (i in dims){
        if (signs[i] == -1){
          result[,i] = rev(result[,i])
        } else if (signs[i] == 0){
          result[,i] = result1[,i]
          result = cbind(result, result1[,i + length(dims)])
        }
      }
    }
  }
  result
}


multistep <- function(coefchain,X=NULL,intercept=0,sortedX = apply(X,2,sort),names = NULL, pinters=NULL,...){
  pp <- ncol(sortedX)
  nn <- nrow(sortedX)
  coefchainm <- matrix(coefchain, ncol=pp)
  sparsitycounts <- drop(apply(coefchainm, 2, function(x) sum(x !=0)))
  if (is.null(pinters)) {pinters <- drop(apply(coefchainm, 2, function(x) -sum(cumsum(x))/nn))}
  params <- cbind(c(1,cumsum(sparsitycounts)+1)[1:pp], sparsitycounts, pinters)
  coefchaincounts <- which(coefchain != 0)
  breakpoints <- as.vector(sortedX[-1,])[coefchaincounts]
  previous <- as.vector(sortedX[-nn,])[coefchaincounts]
  delta <- coefchain[coefchaincounts]
  values <- cbind(coefchaincounts, breakpoints, delta, previous)
  if (!is.null(names)){
    dimnames(params)[[1]] = names
  }
  obj <- list(params = params, values = values, intercept = intercept, range = cbind(sortedX[1,] , sortedX[nn,]),n=nn, ...)
  class(obj) <- "multistep"
  obj
}

#combine two or more multistep fits, or scale
#default is average
combmulti <- function(multisteplist, scales = rep(1/length(multisteplist), length(multisteplist)), X = NULL, sortedX=apply(X,2,sort)){
  newcoefchain = apply(sapply(multisteplist,as.vector) ,1,function(x) sum(x * scales))
  newintercept = sum(scales * sapply(multisteplist, function(x) x$intercept))
  newpinters = apply(sapply(multisteplist, function(x) min(x)), 1, function(x) sum(x*scales) )
  tempmultistep = multistep(newcoefchain, intercept=newintercept, sortedX = sortedX, pinters = newpinters, names = dimnames(multisteplist[[1]]$params)[[1]])
#re-intercept to give correct weighted means against the first set of weights

#copy over to get the remaining fields in the original
  result = multisteplist[[1]]
  result$params = tempmultistep$params
  result$values = tempmultistep$values
  result$intercept = tempmultistep$intercept
  result

}

#multistepvector(obj = NULL){
#  if (is.null(obj)){
#    params = NULL
#    values = NULL
#    intercept = NULL
#    range = NULL
#    n = NULL
#    lengths = NULL
#  } else {
#    params = obj$params
#    values = obj$values
#    intercept = obj$intercept
#    range = obj$range
#    n = obj$n
#    lengths = nrow(values)
#  }
#  obj <- list(params = params, values = values, intercept = intercept, range = cbind(sortedX[1,] , sortedX[nn,]),n=nn, lengths = lengths)
#  class(obj) <- "multistepvector"
#  obj
#}
#

#gets 'row and column' for a value
modrow <- function(n, rows){
  c( (n -1)%/% rows + 1, (n-1) %% rows +1 )
}
  
#add um adding, allow obj = NULL
  #needs a bit smarter code using plot.window. Too busy to do this
plot.multistep <- function(obj = NULL, X=NULL, Y = NULL, testmatrix = NULL, shifts = rep(0, length(testmatrix)), dims = 1:max(nrow(obj$param), ncol(X), length(testmatrix)) ,newplot=FALSE, ylimit = cbind(min(min(obj),max(obj)), max(max(obj), min(obj))), grid = TRUE, add = FALSE, titles = !add,...){
  if (newplot) {dev.new()}
#    sortcode <- getSort(X)
#    sortedX <- apply(X,2,sort)
#    sortedY <- apply(sortcode,2, function(x) Y[x])
  pp = length(testmatrix)#nrow(obj$params)
  numplots = length(dims)
  if (!add){
  if (grid) op <- par(mar = c(2,2,1,0) + 0.1,mfrow = c((ceiling(sqrt(numplots))) -> width,ceiling(numplots/width)))
  } else {
    numberrows = par()$mfrow[2]
    op = par()
  }
  for (idim in 1:length(dims)){
#    coefcut = coefchain[((i-1) * (nn-1) +1):(i *(nn-1))]
#    sparse = union((1:(nn-1))[abs(coefcut )>0.001],nn-1)
#    sparsecoefs = coefcut[sparse]
#    baseline = -(1/nn) * sparsecoefs %*% (nn-sparse)
##    coefcut = coefchain[(coefchain > (i-1)*(nn-1))&(coefchain < i*(nn-1))]
#    knots = sortedX[sparse +1,i]
#    numknots = length(knots)
#    ploty = rep(baseline, numknots+1)
#    for (j in 1:numknots){
#      ploty = ploty + c(rep(0,j), rep(sparsecoefs[j], numknots + 1 - j))
#    }
#   # ploty = rep(baseline, 2 * numknots+1)
#   # for (j in 1:numknots){
#   #   ploty = ploty + c(rep(0,j*2), rep(sparsecoefs[j], 2*numknots + 1 - 2*j))
#   # }
#   # print(y)
#   # plotx = c(sortedX[1,i],rep(knots,each=2))
#    #plot(plotx,ploty,type="l",ylim = c(-5,5))
#    plotx = c(sortedX[1,i],knots)
    if (add &grid) par(mfg = modrow(idim, numberrows))
    if (!is.null(obj)){
      if (dim(obj)[dims[idim]] == 0){
        plotdata = NULL
      } else {
        plotdata = obj$values[obj$params[dims[idim],1]:(obj$params[dims[idim],1] + obj$params[dims[idim],2] - 1),2:3, drop=FALSE]
        plotdata[,2] = cumsum(plotdata[,2]) + obj$params[dims[idim],3]
      }
      plotdata = rbind(c(obj$range[dims[idim],1], min(obj)[dims[idim]]), plotdata, c(obj$range[dims[idim],2], max(obj)[dims[idim]]))
      if (!add){
        plot(plotdata,type="s", ann=F,frame.plot=F,axes=T, ylim = ylimit, ...)#,ann = F, axes = F)
      } else {
        lines(plotdata, type="s",...)
      }
    }
    if (titles){
      if (is.null(dimnames(obj$params)[[1]][dims[idim]])){
        title(main = dims[idim])
      } else {
        title(main = dimnames(obj$params)[[1]][dims[idim]])
      }
    }

    if (!(is.null(X))){
      if (!is.null(obj)){
        points(X[,dims[idim]],Y,pch=".")
      } else {
        points(X[,dims[idim]],Y,pch=".",...)
      }
    }
    if (!(is.null(testmatrix))){
      if (!is.null(obj)){
        lines(sort(X[,dims[idim]]), sort(genSimu(X, c(rep(0,dims[idim]-1), testmatrix[dims[idim]], rep(0, pp - dims[idim])), shifts, isnormal)),col="red")
      } else {
        lines(sort(X[,dims[idim]]), sort(genSimu(X, c(rep(0,dims[idim]-1), testmatrix[dims[idim]], rep(0, pp - dims[idim])), shifts, isnormal)),col="red",...)
      }
    }
  }
  if (grid) invisible(op) # return the old pars, don't reset!
}

cv2.liso <- function (x, y, K = 10, fraction = seq(from = 0, to = 1, length = 100), trace = TRUE, plot.it = FALSE, se = TRUE, ...) {
    all.folds <- cv.folds(length(y), K)
    residmat <- matrix(0, length(fraction), K)
    for (i in seq(K)) {
        omit <- all.folds[[i]]
        fit <- liso(x[-omit, , drop = FALSE], y[-omit], trace = trace, 
            ...)
        fit <- predict(fit, x[omit, , drop = FALSE], mode = "fraction", 
            s = fraction)$fit
        if (length(omit) == 1) 
            fit <- matrix(fit, nrow = 1)
        residmat[, i] <- apply((y[omit] - fit)^2, 2, mean)
        if (trace) 
            cat("\n CV Fold", i, "\n\n")
    }
    cv <- apply(residmat, 1, mean)
    cv.error <- sqrt(apply(residmat, 1, var)/K)
    object <- list(fraction = fraction, cv = cv, cv.error = cv.error, residmat = residmat, optimfraction = fraction[which.min(cv)])
    if (plot.it) 
        plotCVLars(object, se = se)
    invisible(object)
}


"predict.liso" <- function(object, newx, s, type = c("fit", "coefficients"), mode = c("step", "fraction", "norm","lambda"), ...)
{
  sortedX = apply(object$x,2,sort)
  mode <- match.arg(mode)
  type <- match.arg(type)
  if(missing(newx) & type == "fit") {
    warning("Type=fit with no newx argument; type switched to coefficients"
            )
    type <- "coefficients"
  }
  betas <- object$beta
  actions <- object$actions
  k <- nrow(betas)
  p <- object$dims[2]
  n <- object$dims[1]
  steps <- seq(k)
  if(missing(s)) {
    s <- steps
    mode <- "step"
  }
  sbeta <- switch(mode,
                  step = {
                    if(any(s < 0) | any(s > k))
                      stop("Argument s out of range")
                    steps
                  }
                  ,
                  fraction = {
                    if(any(s > 1) | any(s < 0))
                      stop("Argument s out of range")
                    nbeta <- drop(abs(betas[,(n+1):(2*n)]) %*% rep(1, n))
                    nbeta/nbeta[k]
                  }
                  ,
                  norm = {
                    nbeta <- drop(abs(betas[,(n+1):(2*n)]) %*% rep(1, n))
                    if(any(s > nbeta[k]) | any(s < 0))
                      stop("Argument s out of range")
                    nbeta
                  }
                  ,
                  lambda={
                    lambdas=object$lambda
                    s[s>max(lambdas)]=max(lambdas)
                    s[s<0]=0
                    c(lambdas,0)
                  }
                  )

  sfrac <- (s - sbeta[1])/(sbeta[k] - sbeta[1])
   sbeta <- (sbeta - sbeta[1])/(sbeta[k] - sbeta[1])
  usbeta<-unique(sbeta)
  useq<-match(usbeta,sbeta)
  sbeta<-sbeta[useq]
  betas<-betas[useq,,drop=FALSE]
  coord <- approx(sbeta, seq(sbeta), sfrac)$y
  left <- floor(coord)
  right <- ceiling(coord)
  stepfits <- list()
  for (i in 1:length(left)){
    if (left[i] == right[i]){
      midbeta <- betas[left[i],]
    } else {
      lbeta <- betas[left[i],]
      rbeta <- betas[right[i],]
      additions = actions[[left[i]]]
      if (right[i] < k) subtractions = -actions[[right[i]]]
      if (sum(additions > 0) != 0){
        mergestring <- c(lbeta[1:n][lbeta[1:n] != 0], additions[additions >0])
        lbeta[1:length(mergestring)] = mergestring
      } else {
        mergestring = lbeta[1:n][lbeta[1:n]!=0]
      }
      if (length(mergestring) > sum(rbeta[1:n] !=0)){
        rbeta[1:length(mergestring)] = mergestring
        temp = rep(0,n)
        temp[-na.omit(match(subtractions, mergestring))] = rbeta[(n+1):(2*n - length(na.omit(match(subtractions, mergestring))))]
        rbeta[(n+1):(2*n)] = temp
      }
      midbeta <- ((sbeta[right[i]] - sfrac[i]) * lbeta + (sfrac[i] -
sbeta[left[i]]) * rbeta)/(sbeta[right[i]] - sbeta[left[i]])       
      midbeta[1:n] = lbeta[1:n]
    }
    #now we form the appropiate multistep thing
    coefchain = replace(rep(0, (n-1)*p), midbeta[1:n], midbeta[(n+1):(2*n)][midbeta[1:n] != 0])
    stepfits[[i]] = multistep(coefchain,NULL,object$mu,sortedX)
  }

#  newbetas[left == right,  ] <- betas[left[left == right],  ]
robject <- switch(type,
                    coefficients = list(s = s, fraction = sfrac, mode = mode, 
                      coefficients = stepfits),
                    fit = list(s = s, fraction = sfrac, mode = mode, fit = sapply(stepfits, function(x) x * newx)))
  robject
}


"lisohock" <-  function(x, y, trace = FALSE,
           normalize=FALSE, intercept=TRUE, Gram, 
           eps = 1e-08,  max.steps, use.Gram = FALSE, q = 3){
#           eps = .Machine$double.eps,  max.steps, use.Gram = TRUE)
#too much spurious accuracy is dangerous{
### program automatically centers and standardizes predictors by default.
###
### Original program by Brad Efron September 2001
### Recoded by Trevor Hastie November 2001
### Computational efficiency December 22, 2001
### Bug fixes and singularities February 2003
### Conversion to R April 2003
### stepwise and non-standardize options added May 2007
### Copyright Brad Efron and Trevor Hastie
  nm <- dim(x)
  n <- nm[1]
  sortcode = getSort(x) 
#  diffs = apply(sortcode,2,function(x) X[x])[-1,] - apply(sortcode,2,function(x) X[x])[-n,]
  Xs = NULL
Xs[[1]] = x#apply(sortcode,2,function(x) X[x])
if (q > 1){
  for (i in 2:q){
    Xs[[i]] = Xs[[i-1]] * Xs[[1]]
  }
}
sortedXs = NULL
sortedXs[[1]] = apply(x,2,sort)
if (q > 1){
  for (i in 2:q){
    sortedXs[[i]] = sortedXs[[i-1]] * sortedXs[[1]]
  }
}

  applyperm = function(X,sortcode){
    cols = ncol(X)
    for (i in 1:cols){
      X[,i] = X[sortcode[,i],i]
    }
    X
  }

  monocor <- function(u,sorts){
    #get y'x in the case of monotone stuff. Y must have mean 0!
    if (is.null(dim(sorts))){
      sorts = matrix(sorts, ncol = 1)
    }
#    n <- length(u)
#    print(Xs[[1]])
    sorted = apply(Xs[[1]], 2, sort)
    temp1 = NULL
    for (i in 1:q){
    temp1[[i]] = applyperm(u * Xs[[i]], sorts)
    temp1[[i]] = apply(temp1[[i]],2,function(x) rev(cumsum(rev(x))))
    }
    cormatrix = apply(sorts,2, function(x) rev(cumsum(rev(u[x]))))[-1,]*sortedXs[[q]][-n,] * (-1)^q
    if (q > 1){
      for (i in 1:(q-1)){
        cormatrix[1:(n-1),] = cormatrix[1:(n-1),] + temp1[[i]][-1,] * choose(q,i) * (-1)^(q-i) * sortedXs[[q-i]][-n,]#apply(sorts[-n,],2,function(x) Xs[[q-i]][x])
      }
    }
    cormatrix[1:(n-1),]  = cormatrix[1:(n-1),] + temp1[[q]][-1,]
    if (q > 1){
      cormatrix = rbind(cormatrix, matrix(0, q-1,p))
      for (i in 1:(q-1)){
        cormatrix[n-1+i,] = colSums(apply(sorts,2,function(x) u[x]) * (sorted - matrix(rep(sorted[1,], each =n),n,p))^i)
      }
    }
    cormatrix
  }
  extractx <- function(indexlist){
    #get the real x values for this lot
    result = matrix(nrow=n, ncol=length(indexlist))
    for (i in 1:length(indexlist)){
      index = indexlist[i]
      pnum = ceiling(index/(n + q -2))
      rnum = index - (n + q - 2)*(pnum-1)
      if (rnum < n){
        result[,i] = (x[,pnum] - x[sortcode[rnum,pnum],pnum])^q
        result[,i] = (result[,i] + abs(result[,i]))/2
      } else { 
        result[,i] = (x[,pnum] - min(x[,pnum]))^(rnum - n + 1)
      }
    }
    result = result - matrix(rep(apply(result,2,mean),each = n),nrow = n)
  }
    
  call <- match.call()
  TYPE = "LASSO"
  type = "lasso"
  if(trace)
    cat(paste(TYPE, "sequence\n"))
  p <- nm[2]
  m <- nm[2] * (n+q-2)
  im <- inactive <- seq(m)
  one <- rep(1, n)
  vn <- dimnames(x)[[2]]	
### Center x and y, and scale x, and save the means and sds
  if(intercept){
    mu <- mean(y)
    y <- drop(y - mu)
  }
  else {
    mu <- 0
    y <- drop(y)
  }
  meanx <- rep(0,m)
  if(normalize){
    normx <- sqrt(drop(one %*% (x^2)))
    nosignal<-normx/sqrt(n) < eps
    if(any(nosignal))# ignore variables with too small a variance
      {
        ignores<-im[nosignal]
        inactive<-im[-ignores]
        normx[nosignal]<-eps*sqrt(n)
        if(trace)
          cat("LARS Step 0 :\t", sum(nosignal), "Variables with Variance < eps; dropped for good\n")	#
      }
    else ignores <- NULL #singularities; augmented later as well
    names(normx) <- NULL
    x <- scale(x, FALSE, normx)	# scales x
  }
  else {
    normx <- rep(1,m)
    ignores <- NULL
  }
  if(use.Gram & missing(Gram)) {
    if(m > 500 && n < m)
      cat("There are more than 500 variables and n<m;\nYou may wish to restart and set use.Gram=FALSE\n"
          )
    if(trace)
      cat("Computing X'X .....\n")
    Gram <- t(x) %*% x	#Time saving
  }
  Cvec <- as.vector(monocor(y, sortcode))#drop(t(y) %*% x)
  ssy <- sum(y^2)	### Some initializations
  residuals <- y
  if(missing(max.steps))
    max.steps <- 8*min(m, n-intercept)
  beta <- matrix(0, max.steps + 1, m)	# beta starts at 0
  lambda=double(max.steps)
  Gamrat <- NULL
  arc.length <- NULL
  R2 <- 1
  RSS <- ssy
  first.in <- integer(m)
  active <- NULL	# maintains active set
  actions <- as.list(seq(max.steps))	
                                        # a signed index list to show what comes in and out
  drops <- FALSE	# to do with type=="lasso" or "forward.stagewise"
  Sign <- NULL	# Keeps the sign of the terms in the model
  R <- NULL	###
### Now the main loop over moves
###
  k <- 0
  while((k < max.steps))# & (length(active) <  min(m - length(ignores),n-intercept)) )
    {
      action <- NULL
      C <- Cvec[inactive]	#
### identify the largest nonactive gradient
      Cmax <- max(C)
      print(Cmax)
      if(Cmax<eps*100){ # the 100 is there as a safety net
        if(trace)cat("Max |corr| = 0; exiting...\n")
        break
      }
      k <- k + 1
      lambda[k]=Cmax
### Check if we are in a DROP situation
      if(!any(drops)) {
        new <- C >= Cmax - eps
        C <- C[!new]	# for later
        new <- inactive[new]	# Get index numbers
### We keep the choleski R  of X[,active] (in the order they enter)
        for(inew in new) {
          if(use.Gram) {
            R <- updateR(Gram[inew, inew], R, drop(Gram[
                                                        inew, active]), Gram = TRUE,eps=eps)
          }
          else {
            R <- updateR(extractx(inew), R, extractx(active), Gram
                         = FALSE,eps=eps)
          }
          if(attr(R, "rank") == length(active)) {
            ##singularity; back out
            nR <- seq(length(active))
            R <- R[nR, nR, drop = FALSE]
            attr(R, "rank") <- length(active)
            ignores <- c(ignores, inew)
            action <- c(action,  - inew)
            if(trace)
              cat("LARS Step", k, ":\t Variable", inew, 
                  "\tcollinear; dropped for good\n")	#
          }
          else {
            if(first.in[inew] == 0)
              first.in[inew] <- k
            active <- c(active, inew)
            Sign <- c(Sign, sign(Cvec[inew]))
            action <- c(action, inew)
            if(trace)
              cat("LARS Step", k, ":\t Variable", inew, 
                  "\tadded\n")	#
          }
        }
      }
      else action <-  - dropid
      Gi1 <- backsolve(R, backsolvet(R, Sign))	
### Now we have to do the forward.stagewise dance
### This is equivalent to NNLS
      dropouts<-NULL
      A <- 1/sqrt(sum(Gi1 * Sign))
      w <- A * Gi1	# note that w has the right signs
      if(!use.Gram) u <- drop(extractx(active) %*% w)	###
### Now we see how far we go along this direction before the
### next competitor arrives. There are several cases
###
### If the active set is all of x, go all the way
##THIS DOES NOT WORK
      if( (length(active) >=  min( m - length(ignores) ) )|type=="stepwise") {
        gamhat <- Cmax/A
      }
      else {
        if(use.Gram) {
          a <- drop(w %*% Gram[active,  - c(active,ignores), drop = FALSE])
        }
        else {
#          a <- drop(u %*% x[,  - c(active, ignores), drop=FALSE])
	  a <- as.vector(monocor(u, sortcode))[-c(active,ignores)]
#print(u)
#print(residuals)
#print(monocor(u,sortcode))
        }
        gam <- c((Cmax - C)/(A - a))#, (Cmax + C)/(A + a))	
### Any dropouts will have gam=0, which are ignored here
        gamhat <- min(gam[gam > eps], Cmax/A)	
      }
      if(type == "lasso") {
        dropid <- NULL
        b1 <- beta[k, active]	# beta starts at 0
        z1 <-  - b1/w
        zmin <- min(z1[z1 > eps], gamhat)
        if(zmin < gamhat) {
          gamhat <- zmin
          drops <- z1 == zmin
        }
        else drops <- FALSE
      }
      beta[k + 1,  ] <- beta[k,  ]
      beta[k + 1, active] <- beta[k + 1, active] + gamhat * w
      if(use.Gram) {
        Cvec <- Cvec - gamhat * Gram[, active, drop = FALSE] %*% w
      }
      else {
        residuals <- residuals - gamhat * u
        Cvec <- as.vector(monocor(residuals, sortcode))#drop(t(residuals) %*% x)
      }
      Gamrat <- c(Gamrat, gamhat/(Cmax/A))
      arc.length <- c(arc.length, gamhat)	
### Check if we have to drop any guys
      if(type == "lasso" && any(drops)) {
        dropid <- seq(drops)[drops]	
                                        #turns the TRUE, FALSE vector into numbers
        for(id in rev(dropid)) {
          if(trace)
            cat("Lasso Step", k+1, ":\t Variable", active[
                                                        id], "\tdropped\n")
          R <- downdateR(R, id)
        }
        dropid <- active[drops]	# indices from 1:m
        beta[k+1,dropid]<-0  # added to make sure dropped coef is zero
        active <- active[!drops]
        Sign <- Sign[!drops]
      }
      if(!is.null(vn))
        names(action) <- vn[abs(action)]
      actions[[k]] <- action
      inactive <- im[ - c(active, ignores)]
      if(type=="stepwise")Sign=Sign*0
    }
  beta <- beta[seq(k + 1), ,drop=FALSE ]	#
  lambda=lambda[seq(k)]
  dimnames(beta) <- list(paste(0:k), vn)	### Now compute RSS and R2
  if(trace)
    cat("Computing residuals, RSS etc .....\n")
  residuals <- y - cbind(rep(0,n),apply(beta[-1,],1, function(x) extractx(which(x != 0)) %*% (x[x !=0])))
#print(residuals)
  beta <- scale(beta, FALSE, normx)
  RSS <- apply(residuals^2, 2, sum)
  R2 <- 1 - RSS/RSS[1]
  actions=actions[seq(k)]
  netdf=sapply(actions,function(x) sum(sign(c(x,0))))
  df=cumsum(netdf)### This takes into account drops
  if(intercept)df=c(Intercept=1,df+1)
  else df=c(Null=0,df)
  rss.big=rev(RSS)[1]
  df.big=n-rev(df)[1]
  if(rss.big<eps|df.big<eps)sigma2=NaN
  else
    sigma2=rss.big/df.big
  Cp <- RSS/sigma2 - n + 2 * df
  attr(Cp,"sigma2")=sigma2
  attr(Cp,"n")=n
  object <- list(call = call, type = TYPE, df=df, lambda=lambda,R2 = R2, RSS = RSS, Cp = Cp, 
                 actions = actions[seq(k)], entry = first.in, Gamrat = Gamrat, 
                 arc.length = arc.length, Gram = if(use.Gram) Gram else NULL, 
                 beta = beta, mu = mu, normx = normx, meanx = meanx)
  class(object) <- "knotso"
  object
}


liso.extractx <- function(indexlist, sortcode=getSort(X)){
  if (is.null(indexlist)) return (NULL)
  n = nrow(sortcode)
#get the real x values for this lot, zero gives zero
  result = matrix(nrow=n, ncol=length(indexlist))
  for (i in 1:length(indexlist)){
    index = indexlist[i]
    if (index == 0){
      result[,i] = rep(0, n)
    } else {
      pnum = ceiling(index/(n-1))
      rnum = index - (n-1)*(pnum-1)
      result[,i] = replace(rep(0,n),sortcode[,pnum][1:rnum],-1) + rnum/n
    }
  }
  result
}

liso.monocorr <- function(u, sorts){
    #get y'x in the case of monotone stuff. Y must have mean 0!
    if (is.null(dim(sorts))){
      sorts = matrix(sorts, ncol = 1)
    }
    n <- length(u)
    sorted <- apply(sorts,2,function(x) u[x])
    cormatrix <- apply(sorted,2, function(x) -(cumsum(x)))[-n,]
    cormatrix
  }


############################################################################
############################################################################
############################################################################
#AMENDED LARS ALG
############################################################################
############################################################################
"lars" <-
  function(x, y, type = c("lasso", "lar", "forward.stagewise","stepwise"), trace = FALSE,
           normalize=TRUE, intercept=TRUE, Gram, 
           eps = .Machine$double.eps,  max.steps, min.lambda = -1, use.Gram = TRUE, monotone = FALSE)
{
### program automatically centers and standardizes predictors by default.
###
### Original program by Brad Efron September 2001
### Recoded by Trevor Hastie November 2001
### Computational efficiency December 22, 2001
### Bug fixes and singularities February 2003
### Conversion to R April 2003
### stepwise and non-standardize options added May 2007
### Copyright Brad Efron and Trevor Hastie
  call <- match.call()
  type <- match.arg(type)
  TYPE <- switch(type,
                 lasso = "LASSO",
                 lar = "LAR",
                 forward.stagewise = "Forward Stagewise",
                 stepwise = "Forward Stepwise")
  if(trace)
    cat(paste(TYPE, "sequence\n"))
  Cmax = Inf
  nm <- dim(x)
  n <- nm[1]
  m <- nm[2]
  im <- inactive <- seq(m)
  one <- rep(1, n)
  vn <- dimnames(x)[[2]]	
### Center x and y, and scale x, and save the means and sds
  if(intercept){
    meanx <- drop(one %*% x)/n
    x <- scale(x, meanx, FALSE)	# centers x
    mu <- mean(y)
    y <- drop(y - mu)
  }
  else {
    meanx <- rep(0,m)
    mu <- 0
    y <- drop(y)
  }
  if(normalize){
    normx <- sqrt(drop(one %*% (x^2)))
    nosignal<-normx/sqrt(n) < eps
    if(any(nosignal))# ignore variables with too small a variance
      {
        ignores<-im[nosignal]
        inactive<-im[-ignores]
        normx[nosignal]<-eps*sqrt(n)
        if(trace)
          cat("LARS Step 0 :\t", sum(nosignal), "Variables with Variance < eps; dropped for good\n")	#
      }
    else ignores <- NULL #singularities; augmented later as well
    names(normx) <- NULL
    x <- scale(x, FALSE, normx)	# scales x
  }
  else {
    normx <- rep(1,m)
    ignores <- NULL
  }
  if(use.Gram & missing(Gram)) {
    if(m > 500 && n < m)
      cat("There are more than 500 variables and n<m;\nYou may wish to restart and set use.Gram=FALSE\n"
          )
    if(trace)
      cat("Computing X'X .....\n")
    Gram <- t(x) %*% x	#Time saving
  }
  Cvec <- drop(t(y) %*% x)
  ssy <- sum(y^2)	### Some initializations
  residuals <- y
  if(missing(max.steps))
    max.steps <- 8*min(m, n-intercept)
  beta <- matrix(0, max.steps + 1, m)	# beta starts at 0
  lambda=double(max.steps)
  Gamrat <- NULL
  arc.length <- NULL
  R2 <- 1
  RSS <- ssy
  first.in <- integer(m)
  active <- NULL	# maintains active set
  collinears <- NULL
  actions <- as.list(seq(max.steps))	
  collinearities <- as.list(rep(integer(0),max.steps))	
                                        # a signed index list to show what comes in and out
  drops <- FALSE	# to do with type=="lasso" or "forward.stagewise"
  Sign <- NULL	# Keeps the sign of the terms in the model
  R <- NULL	###
### Now the main loop over moves
###
  k <- 0
  while((k < max.steps) && (Cmax >= min.lambda) && (length(active) < min(m - length(ignores),n-intercept)) )
    {
      action <- NULL
      C <- Cvec[inactive]	#
### identify the largest nonactive gradient
      if (monotone){
        Cmax <- max((C))
      } else {
        Cmax <- max(abs(C))
      }
      if(Cmax<eps*100){ # the 100 is there as a safety net
        if(trace)cat("Max |corr| = 0; exiting...\n")
        break
      }
      k <- k + 1
      lambda[k]=Cmax
### Check if we are in a DROP situation
      if(!any(drops)) {
        if (monotone){
          new <- (C >= (Cmax -eps))
        } else {
          new <- abs(C) >= Cmax - eps
        }
        C <- C[!new]	# for later
        new <- inactive[new]	# Get index numbers
### We keep the choleski R  of X[,active] (in the order they enter)
        for(inew in new) {
          if(use.Gram) {
            R <- updateR(Gram[inew, inew], R, drop(Gram[
                                                        inew, active]), Gram = TRUE,eps=eps)
          }
          else {
            R <- updateR(x[, inew], R, x[, active], Gram
                         = FALSE,eps=eps)
          }
          if(attr(R, "rank") == length(active)) {
            ##singularity; back out
            nR <- seq(length(active))
            R <- R[nR, nR, drop = FALSE]
            attr(R, "rank") <- length(active)
            collinears <- c(collinears, inew)
            #action <- c(action,  - inew)
            if(trace)
              cat("LARS Step", k, ":\t Variable", inew, 
                  "\tcollinear\n")	#
          }
          else {
            if(first.in[inew] == 0)
              first.in[inew] <- k
            active <- c(active, inew)
            Sign <- c(Sign, sign(Cvec[inew]))
            action <- c(action, inew)
            if(trace)
              cat("LARS Step", k, ":\t Variable", inew, 
                  "\tadded\n")	#
          }
        }
      }
      else action <-  - dropid
      repeat{
        checkunignore = ((type == "lasso") && (length(collinears) > 0) && any(drops))
        Gi1 <- backsolve(R, backsolvet(R, Sign))
### Now we have to do the forward.stagewise dance
### This is equivalent to NNLS
        dropouts<-NULL
        if(type == "forward.stagewise") {
          directions <- Gi1 * Sign
          if(!all(directions > 0)) {
            if(use.Gram) {
              nnls.object <- nnls.lars(active, Sign, R, 
                  directions, Gram[active, active], trace = 
                  trace, use.Gram = TRUE,eps=eps)
            }
            else {
              nnls.object <- nnls.lars(active, Sign, R, 
                  directions, x[, active], trace = trace, 
                  use.Gram = FALSE,eps=eps)
            }
            positive <- nnls.object$positive
            dropouts <-active[-positive]
            action <- c(action, -dropouts)
            active <- nnls.object$active
            Sign <- Sign[positive]
            Gi1 <- nnls.object$beta[positive] * Sign
            R <- nnls.object$R
            C <- Cvec[ - c(active, ignores, collinears)]
          }
        }
        A <- 1/sqrt(sum(Gi1 * Sign))
        w <- A * Gi1	# note that w has the right signs
        if(!use.Gram) u <- drop(x[, active, drop = FALSE] %*% w)	###
### Now we see how far we go along this direction before the
### next competitor arrives. There are several cases
###
### If the active set is all of x, go all the way
        if( (length(active) >=  min(n-intercept, m - length(ignores) - length(collinears) ) )|type=="stepwise") {
          checkunignore = FALSE
          gamhat <- Cmax/A
        }
        else {
          if(use.Gram) {
            a <- drop(w %*% Gram[active,  - c(active,ignores, collinears), drop = FALSE])
            acollinears <- drop(w %*% Gram[active,  collinears, drop = FALSE])
          }
          else {
            a <- drop(u %*% x[,  - c(active, ignores, collinears), drop=FALSE])
            acollinears <- drop(u %*% x[,collinears,drop=FALSE])
          }
          if (checkunignore){
            checkunignore = FALSE
            toberemoved = NULL
### check which ones we need to consider - the ones we are leaving the faces of
            for (i in which(abs(abs(acollinears) - abs(A)) > eps)){
              if(trace)
                cat("Variable", collinears[i], 
                  "\tunder consideration for re-adding...\n")
              if (abs(acollinears[i]) >  abs(A)) {
### adding this one will break sign, so we're hitting a drop situation for this 
                if (trace)
                  cat("Variable", collinears[i], 
                  "\tremoved from collinear variables\n")
                toberemoved = c(toberemoved, i)
              } else {
                if(use.Gram) {
                  newR <- updateR(Gram[collinears[i], collinears[i]], R, drop(Gram[
                                                        collinears[i], active]), Gram = TRUE,eps=eps)
                } else {
                  newR <- updateR(x[, collinears[i]], R, x[, active], Gram
                          = FALSE,eps=eps)
                }
                if(attr(newR, "rank") == length(active)) {
## THIS SHOULD NEVER HAPPEN!!!!!! (Unless I guess we are now collinear with something else that got added back, in which case, leave it as it is.)
                  if(trace)
                    cat("Variable", collinears[i], "is still collinear\n")	#
                }
                else {
                  R <- newR
                  if(first.in[collinears[i]] == 0)
                    first.in[collinears[i]] <- k
                  active <- c(active, collinears[i])
                  Sign <- c(Sign, sign(Cvec[collinears[i]]))
                  action <- c(action, collinears[i])
                  if(trace)
                    cat("Variable", collinears[i], 
                          "\tadded\n")	
                  toberemoved = c(toberemoved, i)
                  checkunignore = TRUE
                }
              }
            }
            if (length(toberemoved) != 0)
              collinears <- collinears[-toberemoved]
          }
          if (checkunignore == FALSE){
            if (monotone){
              gam <- c((Cmax - C)/(A-a))
            } else {
              gam <- c((Cmax - C)/(A - a), (Cmax + C)/(A + a))	
            }
### Any dropouts will have gam=0, which are ignored here
            gamhat <- min(gam[gam > eps], Cmax/A, na.rm=T)###be wary of this	
#	if (is.nan(gamhat)){gamhat = eps}
          } else {
            C <- Cvec[-c(active,ignores,collinears)]
          }
        }
        if (checkunignore == FALSE){
          break
        }
      }
      if(type == "lasso") {
        dropid <- NULL
        b1 <- beta[k, active]	# beta starts at 0
        z1 <-  - b1/w
        zmin <- min(z1[z1 > eps], gamhat)
        if(zmin < gamhat) {
          gamhat <- zmin
          drops <- z1 == zmin
        }
        else drops <- FALSE
      }
      beta[k + 1,  ] <- beta[k,  ]
      beta[k + 1, active] <- beta[k + 1, active] + gamhat * w
      if(use.Gram) {
        Cvec <- Cvec - gamhat * Gram[, active, drop = FALSE] %*% w
      }
      else {
        residuals <- residuals - gamhat * u
        Cvec <- drop(t(residuals) %*% x)
      }
      Gamrat <- c(Gamrat, gamhat/(Cmax/A))
      arc.length <- c(arc.length, gamhat)	
### Check if we have to drop any guys
      if(type == "lasso" && any(drops)) {
        dropid <- seq(drops)[drops]	
                                        #turns the TRUE, FALSE vector into numbers
        for(id in rev(dropid)) {
          if(trace)
            cat("Lasso Step", k+1, ":\t Variable", active[
                                                        id], "\tdropped\n")
          R <- downdateR(R, id)
        }
        dropid <- active[drops]	# indices from 1:m
        beta[k+1,dropid]<-0  # added to make sure dropped coef is zero
        active <- active[!drops]
        Sign <- Sign[!drops]
      }
      if(!is.null(vn))
        names(action) <- vn[abs(action)]
      actions[[k]] <- action
      collinearities[[k]] <- collinears
      inactive <- im[ - c(active, ignores, collinears)]
      if(type=="stepwise")Sign=Sign*0
    }
  beta <- beta[seq(k + 1), ,drop=FALSE ]	#
  lambda=lambda[seq(k)]
  dimnames(beta) <- list(paste(0:k), vn)	### Now compute RSS and R2
  if(trace)
    cat("Computing residuals, RSS etc .....\n")
  residuals <- y - x %*% t(beta)
  beta <- scale(beta, FALSE, normx)
  RSS <- apply(residuals^2, 2, sum)
  R2 <- 1 - RSS/RSS[1]
  actions=actions[seq(k)]
  collinearities = collinearities[seq(k)]
  netdf=sapply(actions,function(x)sum(sign(c(x,0))))
  df=cumsum(netdf)### This takes into account drops
  if(intercept)df=c(Intercept=1,df+1)
  else df=c(Null=0,df)
  rss.big=rev(RSS)[1]
  df.big=n-rev(df)[1]
  if(rss.big<eps|df.big<eps)sigma2=NaN
  else
    sigma2=rss.big/df.big
  Cp <- RSS/sigma2 - n + 2 * df
  attr(Cp,"sigma2")=sigma2
  attr(Cp,"n")=n
  object <- list(call = call, type = TYPE, df=df, lambda=lambda,R2 = R2, RSS = RSS, Cp = Cp, 
                 actions = actions[seq(k)], entry = first.in, Gamrat = Gamrat, 
                 arc.length = arc.length, Gram = if(use.Gram) Gram else NULL, collinearities = collinearities, 
                 beta = beta, mu = mu, normx = normx, meanx = meanx)
  class(object) <- "lars"
  object
}


#Lasso-ISOtone lars type algorithm
"liso" <-
  function(x, y, trace = TRUE,
           normalize=FALSE, intercept=TRUE, Gram, 
           eps = 1e-10,  max.steps, min.lambda = -1, use.Gram = FALSE, monotone = TRUE)
#           eps = .Machine$double.eps,  max.steps, use.Gram = TRUE)
#too much spurious accuracy is dangerous - the updateR alg's detection of singularities is unreliable
{
### program automatically centers and standardizes predictors by default.
###
### Original program by Brad Efron September 2001
### Recoded by Trevor Hastie November 2001
### Computational efficiency December 22, 2001
### Bug fixes and singularities February 2003
### Conversion to R April 2003
### stepwise and non-standardize options added May 2007
### Copyright Brad Efron and Trevor Hastie
  Cmax = Inf
  sortcode = getSort(x) 
  monocor <- function(u,sorts){
    liso.monocorr(u,sorts)
  }
  extractx <- function(indexlist) {
    liso.extractx(indexlist, sortcode)
  }
    
  call <- match.call()
  TYPE = "LASSO"
  type = "lasso"
  if(trace)
    cat(paste(TYPE, "sequence\n"))
  nm <- dim(x)
  n <- nm[1]
  p <- nm[2]
  m <- nm[2] * (n-1)
  im <- inactive <- seq(m)
  one <- rep(1, n)
  vn <- dimnames(x)[[2]]	
### Center x and y, and scale x, and save the means and sds
  if(intercept){
    mu <- mean(y)
    y <- drop(y - mu)
  }
  else {
    mu <- 0
    y <- drop(y)
  }
#  meanx <- rep(0,m)
  if(normalize){
    normx <- sqrt(drop(one %*% (x^2)))
    nosignal<-normx/sqrt(n) < eps
    if(any(nosignal))# ignore variables with too small a variance
      {
        ignores<-im[nosignal]
        inactive<-im[-ignores]
        normx[nosignal]<-eps*sqrt(n)
        if(trace)
          cat("LARS Step 0 :\t", sum(nosignal), "Variables with Variance < eps; dropped for good\n")	#

      }
    else ignores <- NULL #singularities; augmented later as well
    names(normx) <- NULL
    x <- scale(x, FALSE, normx)	# scales x
  }
  else {
    ignores <- NULL
  }
  if(use.Gram & missing(Gram)) {
    if(m > 500 && n < m)
      cat("There are more than 500 variables and n<m;\nYou may wish to restart and set use.Gram=FALSE\n"
          )
    if(trace)
      cat("Computing X'X .....\n")
    Gram <- t(x) %*% x	#Time saving
  }
  Cvec <- as.vector(monocor(y, sortcode))#drop(t(y) %*% x)
  ssy <- sum(y^2)	### Some initializations
  residuals <- y
  if(missing(max.steps))
    max.steps <- 8*min(m, n-intercept)
  beta <- matrix(0, max.steps + 1, 2*n)	# beta starts at 0
  # New beta format - (coefcounts ...padding 0's up until nn, padding 0's until 3*nn)
  lambda=double(max.steps)
  Gamrat <- NULL
  arc.length <- NULL
  R2 <- 1
  RSS <- ssy
  #first.in <- integer(m)
  active <- NULL	# maintains active set
  collinears <- NULL
  actions <- as.list(seq(max.steps))	
  collinearities <- as.list(rep(integer(0),max.steps))	
                                        # a signed index list to show what comes in and out
  drops <- FALSE	# to do with type=="lasso" or "forward.stagewise"
  Sign <- NULL	# Keeps the sign of the terms in the model
  R <- NULL	###
  activex <- NULL
### Now the main loop over moves
###
  k <- 0
  while((k < max.steps) & (Cmax >= min.lambda) & (length(active) < min(m - length(ignores),n-intercept)) )
    {
      action <- NULL
      C <- Cvec[inactive]	#
### identify the largest nonactive gradient
      if (monotone){
        Cmax <- max(C)
      } else {
        Cmax <- max(abs(C))
      }
      if(Cmax<eps*100){ # the 100 is there as a safety net
        if(trace)cat("Max |corr| = 0; exiting...\n")
        #print("FINISHED BY COMPLETION")
        break
      }
      k <- k + 1
      lambda[k]=Cmax
### Check if we are in a DROP situation
      if(!any(drops)) {
        if (monotone){
          new <- (C >= (Cmax - eps))
        } else {
          new <- abs(C) >= Cmax - eps
        }
        C <- C[!new]	# for later
        new <- inactive[new]	# Get index numbers
### We keep the choleski R  of X[,active] (in the order they enter)
        for(inew in new) {
          if(use.Gram) {
            R <- updateR(Gram[inew, inew], R, drop(Gram[
                                                        inew, active]), Gram = TRUE,eps=eps)
          }
          else {
            R <- updateR(extractx(inew) -> inewx, R, activex, Gram
                         = FALSE,eps=eps)
          }
          if(attr(R, "rank") == length(active)) {
            ##singularity; back out
            nR <- seq(length(active))
            R <- R[nR, nR, drop = FALSE]
            attr(R, "rank") <- length(active)
            collinears <- c(collinears, inew)
            #action <- c(action,  - inew)
            if(trace)
              cat("LARS Step", k, ":\t Variable", inew, 
                  "\tcollinear\n")	#
          }
          else {
           # if(first.in[inew] == 0)
           #   first.in[inew] <- k
            active <- c(active, inew)
            activex <- cbind(activex, inewx)
            Sign <- c(Sign, sign(Cvec[inew]))
            action <- c(action, inew)
            if(trace)
              cat("LARS Step", k, ":\t Variable", inew, 
                  "\tadded\n")	#
          }
        }
      }
      else {
        action <-  - dropid
#        collinears = c(collinears, dropid)
      }
      repeat{
        checkunignore = ((type == "lasso") && (length(collinears) > 0) && any(drops))
        Gi1 <- backsolve(R, backsolvet(R, Sign))
### Now we have to do the forward.stagewise dance
### This is equivalent to NNLS
        dropouts<-NULL
        A <- 1/sqrt(sum(Gi1 * Sign))
        w <- A * Gi1	# note that w has the right signs
        if(!use.Gram) u <- drop(activex %*% w)	###
### Now we see how far we go along this direction before the
### next competitor arrives. There are several cases
###
### If the active set is all of x, go all the way
        if( (length(active) >=  min(n-intercept, m - length(ignores) - length(collinears) ) )|type=="stepwise") {
          checkunignore = FALSE
          gamhat <- Cmax/A
        }
        else {
          if(use.Gram) {
            a <- drop(w %*% Gram[active,  - c(active,ignores), drop = FALSE])
          }
          else {
            afull <- a <- as.vector(monocor(u, sortcode))
            acollinears <- a[collinears]
            a <- a[-c(active,ignores, collinears)]
          }
          if (checkunignore){
            checkunignore = FALSE
            toberemoved = NULL
### check which ones we need to consider - the ones we are leaving the faces of
            for (i in which(abs(abs(acollinears) - abs(A)) > eps)){
              if(trace)
                cat("Variable", collinears[i], 
                  "\tunder consideration for re-adding...\n")
              if (abs(acollinears[i]) >  abs(A)) {
### adding this one will break sign, so we're hitting a drop situation for this 
                if (trace)
                  cat("Variable", collinears[i], 
                  "\tremoved from collinear variables\n")
                toberemoved = c(toberemoved, i)
              } else {
                if(use.Gram) {
                  newR <- updateR(Gram[collinears[i], collinears[i]], R, drop(Gram[
                                                        collinears[i], active]), Gram = TRUE,eps=eps)
                } else {
                  newR <- updateR(extractx(collinears[i]) -> inewx, R, activex, Gram
                          = FALSE,eps=eps)
                }
                if(attr(newR, "rank") == length(active)) {
## THIS SHOULD NEVER HAPPEN!!!!!! (Unless I guess we are now collinear with something else that got added back, in which case, leave it as it is.)
                  if(trace)
                    cat("Something silly happened with variable", collinears[i], "\n")	#
                }
                else {
                  R <- newR
                  #if(first.in[collinears[i]] == 0)
                  #  first.in[collinears[i]] <- k
                  active <- c(active, collinears[i])
                  activex <- cbind(activex, inewx)
                  Sign <- c(Sign, sign(Cvec[collinears[i]]))
                  action <- c(action, collinears[i])
                  if(trace)
                    cat("Variable", collinears[i], 
                          "\tadded\n")	
                  toberemoved = c(toberemoved, i)
                  checkunignore = TRUE
                }
              }
            }
            if (length(toberemoved) != 0)
              collinears <- collinears[-toberemoved]
          }
          if (checkunignore == FALSE){
            if ((exists("dropid")) && (!is.null(dropid)) && (abs(afull[dropid] - A) < eps)){
              if (trace)
                cat("Dropped variable stays colinear \n")
              collinears = c(collinears, dropid)
            }
            if (monotone) {
              gam <- c((Cmax - C)/(A - a))#, (Cmax + C)/(A + a))	
            } else {
              gam <- c((Cmax - C)/(A - a), (Cmax + C)/(A + a))
            }
### Any dropouts will have gam=0, which are ignored here <- can't rely on this: omit ignores!
            gamhat <- min(gam[gam > eps], Cmax/A, na.rm=TRUE)	
#	if (is.nan(gamhat)){gamhat = eps}
          } else {
            C <- Cvec[-c(active,ignores,collinears)]
          }
        }
        if (checkunignore == FALSE){
          break
        }
      }
      beta[k+1, 1:n] <- c(active, rep(0,n-length(active)))
      if(type == "lasso") {
        dropid <- NULL
        b1 <- beta[k, (n+1):(2*n)]	# beta starts at 0
	b1 <- b1[beta[k+1,1:n] != 0]
        z1 <-  - b1/w
        zmin <- min(z1[z1 > eps], gamhat)
        if(zmin < gamhat) {
          gamhat <- zmin
          drops <- z1 == zmin
        }
        else drops <- FALSE
      }
#      beta[k + 1,  ] <- beta[k,  ]
      beta[k + 1, (n+1):(2*n)] <- beta[k, (n+1):(2*n)] + c(gamhat * w, rep(0, n-length(w)))
      if(use.Gram) {
        Cvec <- Cvec - gamhat * Gram[, active, drop = FALSE] %*% w
      }
      else {
        residuals <- residuals - gamhat * u
	if (trace){
	  cat("RSS:\t", sum(residuals^2), "\tCMAX:\t", Cmax, "\n")
	}
        Cvec <- as.vector(monocor(residuals, sortcode))#drop(t(residuals) %*% x)
      }
      Gamrat <- c(Gamrat, gamhat/(Cmax/A))
      arc.length <- c(arc.length, gamhat)	
### Check if we have to drop any guys
      if(type == "lasso" && any(drops)) {
        dropid <- seq(drops)[drops]	
                                        #turns the TRUE, FALSE vector into numbers
        for(id in rev(dropid)) {
          if(trace)
            cat("Lasso Step", k+1, ":\t Variable", active[
                                                        id], "\tdropped\n")
          R <- downdateR(R, id)
        }
        dropid <- active[drops]	# indices from 1:m
	pos = which(beta[k+1,1:n] == dropid)
	beta[k+1,] = c(append(beta[k+1,-c(pos, pos+n)],0,after=n-1),0)
        #beta[k+1,dropid]<-0  # added to make sure dropped coef is zero
        active <- active[!drops]
        activex <- activex[,!drops]
        Sign <- Sign[!drops]
      }
      if(!is.null(vn))
        names(action) <- vn[abs(action)]
      actions[[k]] <- action
      collinearities[[k]] <- collinears
      inactive <- im[ - c(active, ignores, collinears)]
      if(type=="stepwise")Sign=Sign*0
    }
  beta <- beta[seq(k + 1), ,drop=FALSE ]	#
  lambda=lambda[seq(k)]
#  dimnames(beta) <- list(paste(0:k), vn)	### Now compute RSS and R2
  if(trace)
    cat("Computing residuals, RSS etc .....\n")
  residuals <- y - cbind(rep(0,n),apply(beta[-1,],1, function(x) extractx(x[1:n]) %*% (x[(n+1):(2*n)])))
#print(residuals)
#  beta <- scale(beta, FALSE, normx)
  RSS <- apply(residuals^2, 2, sum)
  R2 <- 1 - RSS/RSS[1]
  actions=actions[seq(k)]
  collinearities = collinearities[seq(k)]
  netdf=sapply(actions,function(x)sum(sign(c(x,0))))
  df=cumsum(netdf)### This takes into account drops
  if(intercept)df=c(Intercept=1,df+1)
  else df=c(Null=0,df)
  rss.big=rev(RSS)[1]
  df.big=n-rev(df)[1]
  if(rss.big<eps|df.big<eps)sigma2=NaN
  else
    sigma2=rss.big/df.big
  Cp <- RSS/sigma2 - n + 2 * df
  attr(Cp,"sigma2")=sigma2
  attr(Cp,"n")=n
  object <- list(call = call, type = TYPE, df=df, lambda=lambda,R2 = R2, RSS = RSS, Cp = Cp, 
                 actions = actions[seq(k)], Gamrat = Gamrat, 
                 arc.length = arc.length, Gram = if(use.Gram) Gram else NULL, collinearities = collinearities, 
                 beta = beta, mu = mu, dims = dim(x), ranges = cbind(drop(apply(x, 2, min)), drop(apply(x,2,max))), x = x, vn=vn)
  class(object) <- "liso"
  invisible(object)
}

snaptoanswer <- function(X,Y,lambda=0, beta, tol = 1e-06 * (max(Y)-min(Y))/length(Y), liso=FALSE, diagnose = TRUE, eps = 1e-08, monotone=FALSE, ignores = integer(0)){
  #assuming we've found a superset of the true params...
  truesuper = which(abs(beta) > tol)
  result = rep(0,length(beta))
#  signs = sign(beta[truesuper])
  if (length(truesuper)== 0){
    if (diagnose){
      if (liso){
    sortcode = getSort(X)
        corr  = as.vector(liso.monocorr((Y) -> res, sortcode))
      } else {
        corr = ((Y) -> res) %*% X
      }
      if (monotone) corr <- pmax(corr, 0)
      if (max(abs(corr[-ignores])) <= lambda){
        if ((length(ignores) == 0)&& max(abs(corr)) > lambda){
          attr(result,"valid") = FALSE
        } else {
          attr(result,"valid") = TRUE
        }
    } else {
      attr(result,"valid") = FALSE
    }
#    browser()
    attr(result,"correlations") = corr
    attr(result,"residuals") = res
    attr(result,"original") = beta
    }
    return(result)
  
  }
  if (liso){
    sortcode = getSort(X)
    trueX = liso.extractx(truesuper, sortcode)
  } else {
    trueX = X[,truesuper]
  }
  resultimp = predict(lars(trueX, Y, intercept = FALSE, normalize = FALSE, monotone = monotone, min.lambda = lambda, trace=F, use.Gram=FALSE), s=lambda, type="coef", mode = "lambda")$coef
#  qrd <- qr(trueX)
#  if (qrd$rank < length(truesuper)){
#    if (!liso) warning("Rank deficit! Resolution can fail for small X! Hopefully we have zapped small covariates.")
#    basis = sort(qrd$pivot[seq_len(qrd$rank)])
#    truesuper = truesuper[basis]
#    trueX = trueX[,basis]
#    qrd <- qr(trueX)
#    signs = signs[basis]
#  } 
#  temp = qr.qty(qrd, Y) - c(backsolve(qr.R(qrd), lambda * signs, transpose = TRUE), rep(0, length(Y) - length(truesuper)))
#  result = replace(result, truesuper, backsolve(qr.R(qrd), temp) -> resultimp)
  result = replace(result, truesuper, resultimp)
  if (diagnose){
    if (liso){
      corr  = as.vector(liso.monocorr((Y - trueX %*% resultimp) -> res, sortcode))
    } else {
      corr = ((Y - trueX %*% resultimp) -> res) %*% X
    }
    if (monotone) corr <- pmax(corr, 0)
    if ((min(sign(corr[truesuper]) * sign(result[truesuper])) >= 0 ) && (max(abs(corr[-c(which(abs(result) > eps), ignores)])) < min(abs(corr[which(abs(result) > eps)])) + eps)){
      attr(result,"valid") = TRUE
    } else {
      attr(result,"valid") = FALSE
    }
#    browser()
    attr(result,"correlations") = corr
    attr(result,"residuals") = res
    attr(result,"original") = beta
  }
  result
}

#ave for sorted factors by Charles Berry
#plus weights
ave.sorted <- function(y, x, weights = NULL){
  reps <- rle(x)$lengths
  lens <- rep(reps,reps)
  uniqLens <- unique(lens)
  for (i in uniqLens[ uniqLens != 1]){
    if (is.null(weights)){
      y[ lens == i] <- rep( colMeans(matrix(y[ lens == i], nr=i)), each=i)
    } else {
      w = matrix(weights[lens == i], nr = i)
      y[ lens == i] <- rep( colSums(matrix(y[ lens == i], nr=i) * w)/colSums(w), each=i)
    }
  }
  y
}

#changes step function to a new set of steps
#rejig <- (stepobj, newX, newY=NULL){
removeZero <- function(obj){
  obj[which(obj!=0)]
}

plotCVLars <- function(cv.lars.object,se=TRUE, ...){
#  attach(cv.lars.object)
      plot(cv.lars.object$fraction, cv.lars.object$cv, type = "b", ylim = range(cv.lars.object$cv, cv.lars.object$cv + cv.lars.object$cv.error, cv.lars.object$cv - cv.lars.object$cv.error), ...)
    if(se)
      error.bars(cv.lars.object$fraction, cv.lars.object$cv + cv.lars.object$cv.error, cv.lars.object$cv - cv.lars.object$cv.error, 
                 width = 1/length(cv.lars.object$fraction))
#  detach(cv.lars.object)
  
invisible()
}

allowcheck <- function(){
  if (file.exists("~/STOP")){
    browser()
  }
}

bagging.liso <- function(X, Y, K = 10, fraction=(10:1)/2, lambdabest=NULL, with.replacement = TRUE, meanpred = FALSE, bagsize = length(Y) *(1 - (!with.replacement) * 0.5), tracing = FALSE,...){
#create a bunch of bags
  nn = nrow(X)
  pp = ncol(X)
  if (with.replacement) {
    bootstrap = matrix(sample(1:nn, K*bagsize, replace=T), K)
  } else {
#    all.folds <- cv.folds(length(Y), K)
    all.folds = NULL
    for (i in 1:K) all.folds[[i]]= sample(1:nn, bagsize, replace=F)
  }
  seedarray = NULL
#do crossvalid, making lambda
#if we need to, remove whole samples
  if (is.null(lambdabest)){
    if (meanpred){
      residmat <- matrix(0, length(fraction), K)
    } else {
      residmat <- matrix(0, length(fraction), nn)
      appearances <- rep(0,nn)
    }
    for (i in 1:K){
      if (!with.replacement){
        keep <- (1:nn)[ -all.folds[[i]] ]
        omit <- all.folds[[i]]
      } else {
        keep <- bootstrap[i,]
        omit <- (1:nn)[-keep]
        while (length(omit) == 0){
          bootstrap[i,] = sample(1:nn, bagsize, replace=T)
          keep <- bootstrap[i,]
          omit <- (1:nn)[-keep]
        }
      }
      if (meanpred){
      fit <- holdout.generic(X[keep,], Y[keep], X[omit,], Y[omit], fraction, lisofit, ...)
      seedarray[[i]] = fit$extra
      residmat[, i] <- fit$rss
      } else {
      appearances[omit] = appearances[omit]+1
      temp = lisofit(X[keep,], Y[keep], fraction , newx = X[omit,],...)
      seedarray[[i]] = attr(temp, "extra")
      residmat[,omit] = residmat[,omit] + t(temp)
      }
      if (tracing) 
            cat("\n CV Fold", i, "\n")    
    }
    if (meanpred){
      cv <- rowMeans(residmat)
      lambdabest = fraction[which.min(cv)]
    } else {
      cv <- colMeans((Y[which(appearances != 0)] - t(residmat[,which(appearances != 0)] / removeZero(appearances)))^2)
      lambdabest = fraction[which.min(cv)]
    }
  }
  if (!meanpred){
    residvec <- rep(0, nn)
    appearances <- rep(0,nn)
  }
  coefchain = rep(0, pp * (nn-1))
#  bigY = NULL
#  bigX = NULL
  bigfit = matrix(0, nn, pp)
  ooberr = 0
  for (i in 1:K){
    if (!with.replacement){
      keep <- (1:nn)[ -all.folds[[i]] ]
      omit <- all.folds[[i]]
    } else {
      keep <- bootstrap[i,]
      omit <- (1:nn)[-keep]
    }
#let's reach our target incrementally
    if (is.null(seedarray)){
    bigY = as.array(liso.backfit(X[keep,], Y[keep], lambdabest, ...) -> temp)#[[3]])
    } else {
    bigY = as.array(liso.backfit(X[keep,], Y[keep], lambdabest, feed = seedarray[[i]],...) -> temp)#[[3]])
    }
    #bigY = as.array(liso.backfit(X[keep,], Y[keep], c( 2*(lambdabest+1),lambdabest +1, lambdabest), ...))[[3]])

#get the out of bag error
    if (meanpred){
      ooberr = ooberr + mean((Y[-keep] - temp * X[-keep,])^2)
    } else {
      appearances[omit] = appearances[omit] + 1
      residvec[omit] = residvec[omit] + temp * X[omit,]
    }
    bigX = apply(X[keep,],2,sort)
    for (j in 1:pp){
      bigfit[,j] = bigfit[,j] + scale(approx(bigX[,j], bigY[,j], sort(X[,j]), rule=2)$y, scale=F)
    }
  }
  for (j in 1:pp){
    coefchain[((j-1)*(nn-1) + 1):(j*(nn-1))] = diff(bigfit[,j]/K )
  }
  result = multistep(coefchain, X, intercept = mean(Y), names = colnames(X), sorter=apply(X, 2, order), lambda = lambdabest)
  result$residual = Y - result * X
  if (meanpred){
    result$oob = ooberr/K
  } else {
    result$oob = mean((Y[which(appearances != 0)] - residvec[which(appearances != 0)]/removeZero(appearances))^2)
  }
  class(result) = c("lisofit", "multistep")
  result
}

#create log scale grids
seq.log <- function(from = 1, to = 1, length.out = 50, add.zero = FALSE, shifting = 0){
  res = exp(seq(from = log(from + shifting), to = log(to + shifting), length=length.out - add.zero)) - shifting
  if (add.zero) {
    if (from > to) {
      res = c(res,0)
    } else {
      res = c(0,res)
    }
  }
  res
}

objsort <- function(){
  sort(sapply(ls(1), function(x) object.size(get(x))))
}

#plots a matrix as a set of lines or points with legend
repeatplot <- function(x, y, margin = 1, col = 1, type = "p", form = 1, new = TRUE, lims = TRUE, ...){
  if(missing(y)) {
    y <- x
    x <- 1:dim(x)[3 - margin]
  }
  if (margin != 1){
    x = t(x)
    y = t(y)
  }
  dd = nrow(y)
  nn = ncol(y)
  if (length(col) < dd) col = rep(col,dd)
  if (length(type) < dd) type = rep(type,dd)
  if (length(form) < dd) form = rep(form,dd)
  if (!is.matrix(drop(x))){
    x <- matrix(rep(x, dd), dd, byrow=T)
  }
  if (new){
    if (lims){
      plot(x[1,], y[1,], col = col[1], type= type[1], xlim = range(x), ylim = range(y), ...)
    } else {
      plot(x[1,], y[1,], col = col[1], type= type[1], ...)
    }
  }
  for (i in (new + 1):dd){
    if (type[i] == "l") lines(x[i,], y[i,], col= col[i], lty = form[i])
    if (type[i] == "p") points(x[i,], y[i,], col= col[i], pch = form[i])
  }
}

#sort, then plot a line plot
sortplot = function(x, y, sort =F, new=T, col = 1,...){
if (missing(y)){
y = x
x = 1:length(x)
}
if (sort){
  ord = order(x)
  sortplot(x[ord], y[ord], sort=F,...)
}

 if (new)  plot(x, y, type="l", col=0,...)

if (length(unique(col)) == 1){
    lines(x, y, col=col,...)
} else {

colchanges = c(which( diff( c(Inf,col[-length(col)], Inf)) != 0  ))
for (i in 1:  (length(colchanges ) - 1)){ 
lines(x[colchanges[i]:colchanges[i+1]], y[colchanges[i]:colchanges[i+1]], col = col[colchanges[i]],...)
}
}

}

#plots a x-y pair line plot with directionality
crosslines <- function(x, right = T, new = TRUE, type="l",...){
  if (ncol(x) != 2){ x = t(x)}
  if (right){
    crosspts = c(which(diff(x[,1]) < 0), nrow(x))
  } else {
    crosspts = c(which(diff(x[,1]) > 0), nrow(x))
  }
  if (new) {
    plot(x[1,1],x[1,2], xlim = range(x[,1]), ylim = range(x[,2]), type=type, ...)
  }
  start = 1
  for (end in crosspts){
    lines(x[start:end,1], x[start:end,2], type, ...)
    start = end + 1
  }
}


constrain <- function(x, minimum, maximum=-minimum){
  if (minimum > maximum) {
    temp <- minimum
    minimum <- maximum
    maximum <- temp
  }
  x = replace(x, which(x > maximum), maximum)
  x = replace(x, which(x < minimum), minimum)
  x
}

importance.liso = function(obj, weights = rep(1/obj$n, obj$n)){
  sqrt(colSums(as.array.multistep(obj)^2 * apply(obj$sorter, 2, function(x) weights[x])))
}

# USAGE: 
# squelch((maxnumofcols), cols, F)[oldindex] will give the new index with cols
# removed
# squelch((oldnumofcols), cols, T)[oldindex] will give the new index with cols
# added (cols are original positions)
squelch <- function(n, cols=NULL, add = F){
  if (add){
    setdiff(1:n, cols)
  } else {
    cumsum(c(1,replace(rep(1, n), cols, 0)))
  }
}

#which min amongst x > eps
which.min.pos <- function(x, eps = 1e-16){
  x[x <= eps] = NA
  which.min(x)
}

#remove all but one zero column or row, for plotting
cleanzero <- function(X, mar=1, eps=1e-15){
  if (mar == 2) return(t(cleanzero(t(X))))

  zerorows = which(rowSums(abs(X)) < eps)
  if (length(zerorows) > 1){
    newX <- X[-zerorows[-1], ]
  }
  attr(newX, "indices") = (1:nrow(X))[-zerorows[-1]]
  newX
}

asciiplot <- function(x, y=NULL, size = c(35, 70), thresh = 1, symbol="O",xlim=NULL, ylim = NULL, old=NULL){
  if (is.null(y)){
    y = x
    x = 1:length(y)
  }
  if (class(size) == "ASCIIplot") old = size
  if (is.null(old)){
    if (is.null(xlim)) xlim = range(x)
    if (is.null(ylim)) ylim = range(y)

   #form matrix of info
    output = matrix(" ", size[1], size[2])
  } else {
    output = old$output
    xlim= old$xlim
    ylim= old$ylim
    size = old$size
  }
  generics = c(".",",","o", "O", "@", "#")
  cat("Xlim: ",xlim, " Ylim: ", ylim)
  print(" ##Starting##")

  xbox = c(seq(xlim[1], xlim[2], length.out=size[2]), Inf)
  ybox = c(seq(ylim[1], ylim[2], length.out=size[1]), Inf)

  if (length(symbol) < length(thresh)) symbol = generics[1:length(thresh)]

  for (k in 1:length(thresh)){
    threshval = thresh[k]
  for (i in 1:size[2]){
    ind = (x >= xbox[i]) & (x < xbox[i+1])
    tmp = y[ind]
    for (j in 1:size[1]){
      if (sum((tmp >= ybox[j]) & (tmp < ybox[j+1])) >= threshval) output[size[1] - j+1,i] = symbol[k]
    }
  }
  }
  outtext= apply(output, 1, function(t) paste(t, collapse=""))
  print(outtext)
  res = list(output=output, outtext = outtext, xlim=xlim, ylim=ylim, size=size)
  class(res) <- "ASCIIplot"
  invisible(res)
}

matchsign <- function(x, y=1, eps=.Machine$double.eps){
  if (length(y) < length(x)) y = rep(y, length(x))
  xp = x*y+eps > 0
  xn = -x*y + eps > 0

  if (sum(xp) == length(x)){
    return(1)
  } else if (sum(xn) == length(x)){
    return(-1)
  } else {
    return(0)
  }
}


normalise <- function(x, mar = 1, mode = c("both", "center", "scale")){
  mode = match.arg(mode)
  if (length(dim(x)) != 2){
    return(drop(normalise(matrix(x,1), mode = mode)))
  }
  if (mar == 2) {
    return(t(normalise( t(x), mar=1, mode=mode)))
  }
  means = norms = NULL
if ((mode == "both") || (mode == "center")) {
  means = rowMeans(x)
  x = x-means
}
if ((mode == "both") || (mode == "scale")) {
  norms = sqrt(rowSums(x^2))
  x = x * 1/norms
}

attr(x, "means") = means
attr(x, "norms") = norms
x
}

image.mat <- function(x, ...){
  image(z=t(x)[ncol(x):1, ], ...)
}

#extract the xyz etc dimensions 
string2pos <- function(string, dims=c(20,20)){
  proddims = c(1,cumprod(dims)[1:(length(dims)-1)])
  string = string - 1
  pos = matrix(0, length(string), length(dims))
  for (column in (length(dims): 1)){
    pos[,column] = floor((string)/proddims[column]) + 1
    string = string - (pos[,column] - 1) * proddims[column]
  }
  pos
}
  

pos2string <- function(pos, dims=c(20,20)){
  pos = matrix(pos, ncol= length(dims))
  proddims = c(1,cumprod(dims)[1:(length(dims)-1)])
  pos[,-1] = pos[,-1] - 1
  string = rowSums(pos %*% proddims)
  string
}

#get digits
digits <- function(x, len = max(floor(log10(x))) + 1){
  res = matrix(0, length(x), len)
  for (i in 1:len){
    res[,i] = floor(x/10^(len-i))
    x = x - res[,i] * 10^(len - i)
  }
  res
}


library(chron)
hms <- function(x, contains= "HMSm" ){
contains = strsplit(contains, split="")[[1]]

if (! inherits(x, "chron")){
xsec = x
 x = chron(x /(60*60*24))
} else {
xsec = as.numeric(x) *60*60*24
}
out = NULL
if ("M" %in% contains) out = sprintf("%02d",minutes(x))
if ("H" %in% contains) out = paste(sprintf("%02d",hours(x)), out, sep = ":")
if ("S" %in% contains) out = paste( out,sprintf("%02d",seconds(x)),  sep = ":")
if ("m" %in% contains) out = paste(out, sprintf("%02d",round(100*( xsec -floor(xsec) ))), sep= ".")

out
}

#moves a vector or matrix by an offset while preserving the same size, filling missing data
shift = function( x, offset, expand = 0, fill = c("zero", "edge", "loop", "mean")){
fill = match.arg(fill)

if (length(dim(x)) == 2){
if (length(expand) == 1) expand = rep(expand,2)
n = nrow(x)
p = ncol(x)
xlim = nrow(x) + expand[1]
ylim = ncol(x) + expand[2]
x2 = matrix(0, xlim, ylim)
if (fill =="mean") x2 = matrix(mean(x), xlim, ylim)
TL = c(1,1) - pmin(0, offset)
#BR = pmin(c(nrow(x) , ncol(x)) - TL + offset + c(1,1), c(xlim, ylim)) - offset
BR = pmin(offset + c(n,p) , c(xlim, ylim)) - offset

x2[0:((BR-TL)[1]) +max(offset[1],0)+1,0:((BR-TL)[2])+max(offset[2],0)+1] =  x[TL[1]:BR[1], TL[2]:BR[2]]


if ((fill != "zero") && (fill != "mean") ){

#top left corner
#if (min(offset) > 0){
#
#if (fill == "loop"){
#	x2[1: offset[1], 1:offset[2]] = x[ 1:offset[1] + n - offset[1], 1:offset[2] + p - offset[2]]
#} else {
#x2[1: offset[1], 1:offset[2]] = x[1,1]
#}
#}

#top edge
if (offset[1] > 0) x2[1:offset[1],] = matrix(shift(x[1,], offset[2], expand = ylim - p, fill="edge"), nrow  =  offset[1], byrow = T, ncol = ylim)
#left edge

if (offset[2] > 0) x2[,1:offset[2]] = matrix(shift(x[,1], offset[1], expand = xlim - n, fill="edge"), ncol = 1)

#bottom edge
if (n + offset[1] < xlim) x2[(n + offset[1]+1) :xlim,] = matrix(shift(x[n,], offset[2], expand = ylim - p, fill="edge"), nrow = xlim - offset[1] - n,  ncol= ylim, byrow = T)

#right edge

if (p + offset[2] < ylim) x2[,(p + offset[2]+1) :ylim] = matrix(shift(x[,p], offset[1], expand = xlim - n, fill="edge"), ncol = 1)


if (fill == "loop") print("Currently umimplemented loop fill, edging instead")
#if (sum(expand) != 0)	print("Warning, loop results may not make sense if matrix size changes")

}


} else {
#return(drop(shift(x=matrix(x, ncol=1), offset = c(offset,0), expand =c(expand, 0), fill = fill)))
offset = offset[1]
expand  = expand[1]
x2 = rep(0, length(x) + expand)
beg = 1 - pmin(offset, 0)
end =  min (offset + length(x), length(x2)) - offset # min(  length(x) - beg + offset + 1, length(x) + expand) - offset

x2[( max(offset,0) +1) : min(length(x2), length(x)+ offset)] = x[beg:end]

if (fill == "loop"){
if (offset > 0){
x2[1:offset] = tail(x, offset)
}else{
x2[((length(x) + offset + 1): length(x2))] = x[1:(-offset)]
}
} else if (fill == "edge"){
if (offset > 0){
x2[1:offset] = x[1]
}
if ((length(x) + offset) <  length(x2)){
x2[ (length(x) + offset +1) : length(x2)] = tail(x,1)
}

}

}
x2
}


#puts a vector into the range 0-1
conv01 <- function(x){
(x - min(x))/ (max(x)- min(x))
}

#fill up the current layout.
fillplots <- function(){
while ( !identical(par("mfg")[1:2],par("mfg")[3:4])){
plot.new()
}
}

#faster apply among consecutive blocks, trim off incomplete blocks
#bapply <- function(X, size = 1, FUN, Y = NULL){
#
##get dimensionality of output
#n = floor(length(X)/size)
#dimout = length(FUN(X[ 1:size]))
#if (dimout > 1) {
#res = matrix(0, dimout,n )
#if (length(Y) > n){
#for (i in 1:n) res[,i] = FUN(X[ (i-1)*k + 1:k], Y[i])
#}else {
#for (i in 1:n) res[,i] = FUN(X[ (i-1)*k + 1:k])
#}
#}else{
#res = rep(0, n)
#if (length(Y) > n){
#for (i in 1:n) res[i] = FUN(X[ (i-1)*k + 1:k], Y[i])
#}else {
#for (i in 1:n) res[i] = FUN(X[ (i-1)*k + 1:k])
#}
#}
#
#res
#}

#simplified bapply for vectors
bapply.basic <- function(X, k, FUN) { res = rep(0, floor(length(X) / k)); for (i in 1:floor(length(X)/k)) res[i] = FUN(X[ (i-1)*k + 1:k]); return(res)}

bapply <- function(X, k, FUN) { dimout = length(FUN(X[1:k])); res = matrix(0, dimout, floor(length(X) / k)); for (i in 1:floor(length(X)/k)) res[(i-1)* dimout + 1:dimout] = FUN(X[ (i-1)*k + 1:k]); return(res)}


expand <- function(X, length = (length(X)*100)){
c(rep(X, each = floor(length / length(X))), rep(tail(X,1), length - length(X) * floor(length/length(X))))
}


require(compiler)
bapplyc.basic <- cmpfun(bapply.basic)
bapplyc <- cmpfun(bapply)
expandc <- cmpfun(expand)


