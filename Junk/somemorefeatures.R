#autocorrelation of diffs
difftmp2 = apply(tmp2, 2, diff); plot(runmed(bapply.basic( 1: nrow(difftmp2), 100, function(t) sum(diag(cov(difftmp2[t[1:98], 2:4], difftmp2[t[3:100], 2:4]))^2)/sum(sd(difftmp2[t[1:98], 2:4])^2 * sd(difftmp2[t[3:100], 2:4])^2)), 11), type="l", ylab="", ylim = c(0, 0.5))
#quantiles of diffs
repeatplot( bapply(rowSums(apply(tmp2[,-1], 2, diff)^2), 100, function(t) quantile(t, c(0.1, 0.25, 0.5, 0.75, 0.9))), mar = 1, col=1:5, type="l", log = "y")
#autocorr
plot(  runmed(bapplyc.basic(1: nrow(tmp2), 100 , function(t) sum(diag(cov(tmp2[t[1:99], 2:4], tmp2[t[2:100], 2:4]))^2) / sum(sd(tmp2[t, 2:4])^2)^2), 11), type = "l", ylab  ="")
#rescaled diffs
repeatplot( apply(bapply(rowSums(apply( apply(tmp2[,-1],2, function(t) bapply(t, 100 , function(s) drop(scale(s)) )), 2, diff)^2), 100, function(t) quantile(t, c(0.1, 0.25, 0.5, 0.75, 0.9))),1, function(t) runmed(t, 11) ), mar = 2, col=1:5, type="l", log = "y")

repeatplot( apply(bapply(rowSums(apply( apply(tmp2[,-1],2, function(t) bapply(t, 100 , function(s) drop(scale(s)) )), 2, function(r) diff(diff(r)) )^2), 100, function(t) quantile(t, c(0.1, 0.25, 0.5, 0.75, 0.9))),1, function(t) runmed(t, 11) ), mar = 2, col=1:5, type="l", log = "y")

 repeatplot( apply(bapply(rowSums(apply(     apply(tmp2[,-1],2, function(t) bapply(t, 100 , function(s) drop(scale(s, scale=mad(s))) ))    , 2, function(r) (diff(r)) )^2), 100, function(t) quantile(t, c(0.1, 0.25, 0.5, 0.75, 0.9))),1, function(t) runmed(t, 1) ), mar = 2, col=1:5, type="l", log = "y")

svm = rowSums(tmp2[,-1]^2)


 repeatplot( apply(bapply( abs(diff(as.vector(bapply(svm, 100,   function(r) (( scale(r , scale = mad(r)))) ))))  , 100, function(t) quantile(t, c(0.1, 0.25, 0.5, 0.75, 0.9))),1, function(t) runmed(t, 1) ), mar = 2, col=1:5, type="l", log = "y")

