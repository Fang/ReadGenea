convert.hexstream <-
function (stream) 
{

maxint <- 2^(12 - 1)

#packet <- as.integer(paste("0x",stream,sep = "")) #strtoi is faster
packet <-bitShiftL(strtoi(stream, 16),4*(2:0))
packet<-rowSums(matrix(packet,ncol=3,byrow=TRUE))

packet[packet>=maxint] <- -(maxint - (packet[packet>=maxint] - maxint))

packet<-matrix(packet,nrow=4)

light <- bitShiftR(packet[4,],2)
button <-bitShiftR(bitAnd(packet[4,],2),1)

packet<-rbind(packet[1:3,],light,button)

packet
}

