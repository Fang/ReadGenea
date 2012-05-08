
#reformat.time <-
#function (t, format = "julian") 

#turns a time vector of form "2012-04-27 08:45:18:500" or "27/4/2010 08:45:18" or "08:45:22" into either the raw second, second classed as posixct, or days since epoch
#only posix deals with the time zone. The other methods work with the same time zone as the start of recording.
#now for easier dependencies we use this as a wrapper for strptime
parse.time <-
function (t="", format = c("seconds", "days", "POSIX"), tzone = 0) 
{
format = match.arg(format)
millisec = rep(0, length(t))
offset = 0
t1= t[1]
informat = ""

#do we have time in here?
switch(length(strsplit(t1, split = ":")[[1]]), "1" = {
informat = ""
}, "2" = {
informat = "%H:%M"
}, "3" = {
informat = "%H:%M:%S"
if (length(strsplit(t1, split = "\\.")[[1]])>1){
millisec = sapply( strsplit(t, split="\\."), function(t) as.numeric( paste("0.", t[2], sep = "")))
t = sapply( strsplit(t, split="\\."), function(t) t[1])
}
}, "4" = {
informat = "%H:%M:%S"
millisec = sapply( strsplit(t, split=":"), function(t) as.numeric( paste("0.", t[4], sep = "")))
t = sapply( strsplit(t, split=":"), function(t) paste(t[1:3], collapse = ":"))
})

#do we have date?
#strip whitespace/time
t1 = strsplit(t1, split=" ")[[1]]
t1 = t1[min(which(t1 != ""))]
#mode yyyy-mm-dd
if (length(strsplit(t1, split = "-")[[1]]) > 1){
 informat = paste("%Y-%m-%d", informat, sep = " ")
} else if (length(strsplit(t1, split = "/")[[1]]) > 1){
#mode d/m/y
if (nchar(strsplit(t1, split = "/")[[1]][3])<4){
 informat = paste("%d/%m/%y", informat, sep = " ")
} else {
 informat = paste("%d/%m/%Y", informat, sep = " ")
}
} else if (length(strsplit(t1, split = ":")[[1]]) > 1) {
#send us back to 1970
offset = as.numeric(strptime("00:00", format = "%H:%M", tz = "UTC"))
} else {
#add some days?
t = lapply(strsplit(t, split=" "), function(t) (t[t != ""]))
t1 = sapply(t, function(t) as.numeric(t[ 1]))
t = sapply(t, function(t) t[ 2])
offset = as.numeric(strptime("00:00", format = "%H:%M", tz = "UTC")) - t1 * 24*60*60 
}

if (informat != ""){
t = as.POSIXct(strptime(t, format = informat), tz = "UTC") 
} else {
t = as.POSIXct(strptime("00:00", format = "%H:%M", tz = "UTC"))
}

t= t+ millisec - offset

if (format == "seconds"){
t = as.numeric(t)
} else if (format == "POSIX"){
t = c(t - tzone*60*60)
} else {
t = as.numeric(t) / (60*60*24)
}

return(t)
}
