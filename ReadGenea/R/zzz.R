.First.lib <-function(lib,pkg)
{
ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
     ver <- as.character(ver)	

options(digits=12)
cat("ReadGenea", ver, "loaded\n")
}
