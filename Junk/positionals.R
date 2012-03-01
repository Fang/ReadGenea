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


tmp2 = get.intervals(halfday2, "9:30", "11:30" , incl.date=T)
tmp2 = get.intervals(halfday2, "7:10", "9:10" , incl.date=T)
tmp2 = get.intervals(halfday2, "8:10", "9:10" , incl.date=T)
tmp2 = get.intervals(halfday2, "12:00", "14:00" , incl.date=T)
tmp2 = get.intervals(halfday2, "16:50", "18:50" , incl.date=T)
tmp2 = get.intervals(halfday2, "19:20", "21:20" , incl.date=T)
tmp2 = get.intervals(halfday2, "23:00", "01:00" , incl.date=T)

dev.new();par(mfrow = c(2,1), mar = c(1,1,1,1))


plot(times2(tmp2[,1]), -acos(tmp2[,3] / sqrt(rowSums(tmp2[,-1]^2)) ) *180/pi +90, col = hcl(180 *acos(-tmp2[,4] / sqrt(rowSums(tmp2[,-c(1,3)]^2)))/pi +180 , alpha = constrain(1 - 0*abs( sqrt(rowSums(tmp2[,-1]^2)) - 1), 0,1) ), pch=".", ylim = c(-90, 90), xlab = "", ylab=""); abline(h = c(-2:2) * 45, lty= 2)
plot(times2(tmp2[,1]), sqrt(rowSums(tmp2[,-1]^2)), type="l", ylim=c(0, 3)); abline(h=1:2, col=2)

