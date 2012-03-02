#
plot(acos(tmp[,2] / sqrt(rowSums(tmp^2)) ) *180/pi -90, col = hcl(180 *acos(tmp[,1] / sqrt(rowSums(tmp[,-2]^2)))/pi + 180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp^2)) - 1), 0,1) ), pch=".")


halfday2 = read.bin("C:/Users/zhou/Data/__011661_2012-02-29 14-05-12.bin", cali=T)



tmp2 = get.intervals(halfday2, "10:00", "11:00" , incl.date=T)

dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))


plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l"); abline(h=1:2, col=2)




tmp2 = get.intervals(halfday2, "09:30", "10:00" , incl.date=T)

dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))


plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l", ylim=c(0, 3)); abline(h=1:2, col=2)
########




tmp2 = get.intervals(halfday2, "9:30", "11:30" , incl.date=T)
tmp2 = get.intervals(halfday2, "7:10", "9:10" , incl.date=T)
tmp2 = get.intervals(halfday2, "8:10", "9:10" , incl.date=T)
tmp2 = get.intervals(halfday2, "12:00", "14:00" , incl.date=T)
tmp2 = get.intervals(halfday2, "16:50", "18:50" , incl.date=T)
tmp2 = get.intervals(halfday2, "19:20", "21:20" , incl.date=T)
tmp2 = get.intervals(halfday2, "23:00", "01:00" , incl.date=T)

dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))


plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)


svm = sqrt(rowSums(tmp2[,-1]^2))
plot(times2(tmp2[,1]),svm, pch = ".", col=2)
lines(times2(tmp2[,1]),(cumsum(svm) - shift(cumsum(svm), 6000, fill="edge")) / 6000, type="l")


#plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l", ylim=c(0, 3)); abline(h=1:2, col=2)

#################################################
####################################


tmp2 = get.intervals(halfday2, "11:03", 60, incl.date=T)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(tmp2[,2] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
tmp2 = get.intervals(halfday2, "11:03", 120, incl.date=T)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(tmp2[,2] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
180 *acos(tmp2[,2] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180
subs(180 *acos(tmp2[,2] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180, 0.25, 0.5)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
subs(180 *acos(tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180, 0.25, 0.5)
subs(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180, 0.25, 0.5)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
subs(tmp2[,3])
subs(tmp2[,4])
subs(tmp2[,4, 0.25, 0.4])
subs(tmp2[,4], 0.25, 0.4)
subs(tmp2[,4], 0.1, 0.2)
tmp2 = get.intervals(halfday2, "11:03", 2400, incl.date=T)
tmp2 = get.intervals(halfday2, "11:03", 360, incl.date=T)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
tmp2 = get.intervals(halfday2, "11:12", 60, incl.date=T)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
tmp2 = get.intervals(halfday2, "17:00", "20:00" , incl.date=T)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
tmp2 = get.intervals(halfday2, "19:00", "21:00" , incl.date=T)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
dev.new()
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
dev.new()
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 100*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
times2(halfday2$dat[which(halfday2$dat[,6] != 0),1])
tmp2 = get.intervals(halfday2, "10:00", "11:00" , incl.date=T)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
par(mfrow = c(2,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(v = c(-2:2) * 45)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(h = c(-2:2) * 45)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(h = c(-2:2) * 45)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90)); abline(h = c(-2:2) * 45, xlab = "", ylab = "")
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45)
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l")
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l"); abline(h=1, col=2)
history()
history(500)
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l"); abline(h=1, col=2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l"); abline(h=1:2, col=2)
tmp2 = get.intervals(halfday2, "10:00", "11:00" , incl.date=T)
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l"); abline(h=1:2, col=2)
tmp2 = get.intervals(halfday2, "19:30", "21:00" , incl.date=T)
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l"); abline(h=1:2, col=2)
tmp2 = get.intervals(halfday2, "19:30", "21:00" , incl.date=T)
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l", ylim=c(0, 3); abline(h=1:2, col=2)
tmp2 = get.intervals(halfday2, "19:30", "21:00" , incl.date=T)
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l", ylim=c(0, 3)); abline(h=1:2, col=2)
tmp2 = get.intervals(halfday2, "09:30", "10:00" , incl.date=T)
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l", ylim=c(0, 3)); abline(h=1:2, col=2)
tmp2 = get.intervals(halfday2, "06:30", "9:30" , incl.date=T)
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l", ylim=c(0, 3)); abline(h=1:2, col=2)
pnorm(5)
1 - pnorm(5)
1 - pnorm(3)
??apply
?gapply
?nlme::gapply
library(help = nlme)
runmed
?runmed
tmp2
svm = sqrt(rowSums(tmp2[,-1]^2))
svm
cumsum(svm) - shift(cumsum(svm), -10)
(cumsum(svm) - shift(cumsum(svm), -10)) / 10
dev.new() ; plot(cumsum(svm) - shift(cumsum(svm), -10)) / 10
 plot((cumsum(svm) - shift(cumsum(svm), -10)) / 10)
cumsum(svm)
 plot((cumsum(svm) - shift(cumsum(svm), -10, fill="edge")) / 10)
 plot((cumsum(svm) - shift(cumsum(svm), 10, fill="edge")) / 10)
 plot((cumsum(svm) - shift(cumsum(svm), 10, fill="edge")) / 10, type="l")
points((1: length(svm))/10 ,svm, pch = ".", col=2)
 plot((cumsum(svm) - shift(cumsum(svm), 10, fill="edge")) / 10, type="l")
points(svm, pch = ".", col=2)
plot(svm, pch = ".", col=2, type="l")
lines((cumsum(svm) - shift(cumsum(svm), 10, fill="edge")) / 10, type="l")
lines((cumsum(svm) - shift(cumsum(svm), 6000, fill="edge")) / 10, type="l")
plot(svm, pch = ".", col=2, type="l")
plot(svm, pch = ".", col=2)
lines((cumsum(svm) - shift(cumsum(svm), 6000, fill="edge")) / 6000, type="l")
plot(times2(tmp2[,1]),svm, pch = ".", col=2)
lines(times2(tmp2[,1]),(cumsum(svm) - shift(cumsum(svm), 6000, fill="edge")) / 6000, type="l")
tmp2 = get.intervals(halfday2, "08:00", "9:30" , incl.date=T)
svm = sqrt(rowSums(tmp2[,-1]^2))
plot(times2(tmp2[,1]),svm, pch = ".", col=2)
lines(times2(tmp2[,1]),(cumsum(svm) - shift(cumsum(svm), 6000, fill="edge")) / 6000, type="l")
dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
svm = sqrt(rowSums(tmp2[,-1]^2))
plot(times2(tmp2[,1]),svm, pch = ".", col=2)
lines(times2(tmp2[,1]),(cumsum(svm) - shift(cumsum(svm), 6000, fill="edge")) / 6000, type="l")
source("C:/Users/zhou/PortableGit/ReadGenea/Junk/helper.R")
qsvm = bapplyc(svm, 6000, function(t) quantile(t, c(0.1, 0.25, 0.5, 0.75, 0.9)))
dim(qsvm)
dev.new()
repeatplot(qsvm, 1, type="l")
repeatplot(qsvm, mar = 1, type="l")
repeatplot(bapplyc(tmp2[,1] , 6000, function(t) times2(t[3000])), qsvm, mar = 1, type="l", add = T, col=3)
repeatplot(bapplyc(tmp2[,1] , 6000, function(t) times2(t[3000])), qsvm, mar = 1, type="l", new = F, col=3)
dev.select(9)
dev.set(9)
repeatplot(bapplyc(tmp2[,1] , 6000, function(t) times2(t[3000])), qsvm, mar = 1, type="l", new = F, col=3)
abline(h = 1)
abline(h = 1000)
abline(h = 10000)
par("usr")
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]),svm, pch = ".", col=2)
repeatplot(bapplyc(tmp2[,1] , 6000, function(t) times2(t[3000])), qsvm, mar = 1, type="l", new = F, col=3)
plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]),svm, pch = ".", col=2, ylim = c(0, 3))
repeatplot(bapplyc(tmp2[,1] , 6000, function(t) times2(t[3000])), qsvm, mar = 1, type="l", new = F, col=3)
history(500)

