library(ReadGenea)


#binfile = "10037H_012073_2012-02-22 10-35-29.bin"
#binfile.orig = "30570E_012404_2012-03-21 10-54-23.bin"
##30637D_012065_2012-04-13 08-49-23.bin
#binfile = paste("C:/Users/zhou/Data/whitehall/", binfile.orig, sep = "")
#
#obj = read.bin(binfile, cali = T, start = "1 20:00", end = "10:00")
# positionals(obj, filter= 0)
#
#
###############
#
#library(ReadGenea)
#binfile = "10037H_012073_2012-02-22 10-35-29.bin"
#binfile = "30570E_012404_2012-03-21 10-54-23.bin"
##30637D_012065_2012-04-13 08-49-23.bin
#binfile = paste("C:/Users/zhou/Data/whitehall/", binfile, sep = "")
#obj = read.bin(binfile, cali = T, start = "1 20:00", end = "10:00", mmap =T )
# positionals(obj, filter= 0)
# positionals(obj, "20:00", "10:00")
#plot(stft(obj, "20:00", "10:00", win = 20), mode = "pval")
#plot(stft.mv(obj, "20:00", "10:00", win = 20), mode = "pval")
#plot(stft(obj, start= "20:00", end = "10:00", win = 20), mode = "pval")
#stft
#stft.AccData
#plot(stft.AccData(obj, start= "20:00", end = "10:00", win = 20), mode = "pval")
#plot(stft.AccData(obj, start= "20:00", end = "10:00", win = 60), mode = "pval")
#plot.stft(stft.AccData(obj, start= "20:00", end = "10:00", win = 60), mode = "pval", showmax = F, topthresh = 10)
#warnings()
#plot.stft(stft.AccData(obj, win = 60), mode = "pval", showmax = F, topthresh = 10)
#history(4000)
#
#
########


#filelist = c("30570E_012404_2012-03-21 10-54-23.bin", "10083C_012529_2012-04-02 10-22-02.bin", "20071L_012363_2012-04-02 10-23-04.bin")

filelist = c("30842E_012498_2012-04-03 09-45-02.bin", "30982I_012284_2012-03-27 10-48-59.bin", "10037H_012073_2012-02-22 10-35-29.bin", "30576K_012465_2012-03-27 09-09-54.bin","30637D_012065_2012-04-13 08-49-23.bin", "30662L_012362_2012-04-03 10-23-02.bin","10264N_012428_2012-04-16 11-42-25.bin")

for (binfile.orig in filelist){
#30637D_012065_2012-04-13 08-49-23.bin
binfile = paste("C:/Users/zhou/Data/whitehall/", binfile.orig, sep = "")


obj = read.bin(binfile, cali = T, start = "20:00", end = "10:00", mmap=T)
par(bg = "white")
par(mfrow = c(1,1))
 positionals(obj, "20:00", "10:00")
 axis(1,pretty(par("usr")[1:2] /3600, n = 14) * 3600, label = NA)
 dev.print(png, paste(binfile.orig, "0", "pos", substring(as.character(obj$page.t[1]), 6, 10),".png", sep = "_") , width = 11, units = "in", res = 300, antialias = "none")

plot.stft(stft.AccData(obj, start= "20:00", end = "10:00", win = 60), mode = "pval", showmax = F, topthresh = 10)
 axis(1,pretty(par("usr")[1:2] /3600, n = 14) * 3600, label = NA)
 dev.print(png, paste(binfile.orig, "0", "stft",substring(as.character(obj$page.t[1]), 6, 10), ".png", sep = "_") , width = 11, units = "in", res = 300, antialias = "none")



obj = read.bin(binfile, cali = T, start = "1 20:00", end = "10:00", mmap=T)
if (!is.na(tail(obj[,7],1))){
par(bg = "white")
par(mfrow = c(1,1))
 positionals(obj, "20:00", "10:00")
 axis(1,pretty(par("usr")[1:2] /3600, n = 14) * 3600, label = NA)
 dev.print(png, paste(binfile.orig, "1", "pos", substring(as.character(obj$page.t[1]), 6, 10),".png", sep = "_") , width = 11, units = "in", res = 300, antialias = "none")

plot.stft(stft.AccData(obj, start= "20:00", end = "10:00", win = 60), mode = "pval", showmax = F, topthresh = 10)
 axis(1,pretty(par("usr")[1:2] /3600, n = 14) * 3600, label = NA)
 dev.print(png, paste(binfile.orig, "1", "stft",substring(as.character(obj$page.t[1]), 6, 10), ".png", sep = "_") , width = 11, units = "in", res = 300, antialias = "none")

}
#obj = read.bin(binfile, cali = T, start = "2 20:00", end = "10:00", mmap = T)
#par(bg = "white")
#par(mfrow = c(1,1))
# positionals(obj, "20:00", "10:00")
# axis(1,pretty(par("usr")[1:2] /3600, n = 14) * 3600, label = NA)
# dev.print(png, paste(binfile.orig, "2", "pos",substring(as.character(obj$page.t[1]), 6, 10), ".png", sep = "_") , width = 11, units = "in", res = 300, antialias = "none")
#
#plot.stft(stft.AccData(obj, start= "20:00", end = "10:00", win = 60), mode = "pval", showmax = F, topthresh = 10)
# axis(1,pretty(par("usr")[1:2] /3600, n = 14) * 3600, label = NA)
# dev.print(png, paste(binfile.orig, "2", "stft",substring(as.character(obj$page.t[1]), 6, 10), ".png", sep = "_") , width = 11, units = "in", res = 300, antialias = "none")
#
#
}
