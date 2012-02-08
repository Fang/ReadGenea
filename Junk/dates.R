plot(stft( princomp( jogandstuff$dat[1:20000,2:4])$scores[,1], coef = 512, start.time=jogandstuff$dat[1,1], plot= F)-> stfobj, showmax = 2, log="y", xaxt="n"); axis(1, at = stfobj$time[which(1:75 %% 3 == 1)], label = times (stfobj$time[which(1:75 %% 3 == 1)] / (60*60*24) ))

