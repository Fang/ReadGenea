bits = 1:1000000
alpha = 4
beta = 5

indalpha = which(bits %% alpha == 0)
indbeta = which(bits %% beta == 0)

res = NULL

for (freq in ((1:50)/100 + 0.002)){

#freq = 
signal = sin(bits * freq * 2*pi)

pr1 = (which.max(Mod(fft(signal[indalpha]))[1:(length(indalpha)/2)]) -1) / (length(indalpha)/2) *  ((1/4)/2)
pr2 = (which.max(Mod(fft(signal[indbeta]))[1:(length(indbeta)/2)])-1) / (length(indbeta)/2) * ((1/5)/2)

res = rbind(res, c(freq, pr1, pr2))
}

 plot(res[,2:3], type="b")


pr1
pr2
freq

