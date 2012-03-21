#does a gain correction for the old firmware bug, using rlm
correctpages <- function(data, offset = 0, nobs = 300){
require(MASS)
ind = 1: floor((nrow(data$dat) - offset)/nobs) * nobs + offset
gainchange = NULL
tmp = data$data.out[ind, 2:4]
for (k in 2:4){
gainchange = c(gainchange,coef(rlm(as.vector(data$data.out[ind - 1, k]), tmp[,k-1] )))
}
data$data.out[,2:4] = scale(data$data.out[,2:4], center= F, scale = 1/gainchange)
data$data.out[ind, 2:4] = tmp
data
}
