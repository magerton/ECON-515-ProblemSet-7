rm    (list=ls())

# mydir = "C:/Users/magerton/OneDrive/"
mydir = "C:/mja3/SkyDrive/"
# mydir = "/Users/magerton/OneDrive/"

setwd (paste(mydir, "Rice/Class/econ 515 - Labor/PS7/code", sep=""))

library(ggplot2)
library(reshape2)
# library(plm)

data <- read.table("../data_ps7_spring2015.raw", 
                   header=TRUE, 
                   col.names=c("id","s","y","ma","mb","x","z","xm"),
                   colClasses=c("integer","integer",rep("numeric",6))
                   )

origdata <- data
data <- melt(data, id.vars=c("id","s"))

plots <- list()
 
for (var in c("x","z","xm","ma","mb","y") ) {
plots[[var]] <- 
    ggplot(subset(data, variable==var), aes(x=value)) + 
    geom_density(fill="black") + 
    facet_grid(s ~ ., scales="free") + xlab("") + ylab("") + 
    ggtitle(paste("Kernel Density estimates of",var))
}

# for (var in c("w","y") ) {
# plots[[var]] <- 
#   ggplot(subset(data, variable==var), aes(x=log(value))) + 
#   geom_density(fill="black") + 
#   facet_grid(p ~ ., scales="free") + xlab("") + ylab("") + 
#   ggtitle(paste("Kernel Density estimates of log",var))
# }
# 
# # ggplot(subset(data, variable=="y"), aes(x=log(value))) + 
# #   geom_histogram(fill="black") + 
# #   facet_grid(p ~ ., scales="free") + xlab("") + ylab("") + 
# #   ggtitle(paste("Kernel Density estimates of log",var))
# 
# plots[["XZ"]] <- 
# ggplot(origdata, aes(x=x, y=z)) + 
#   stat_density2d(aes(fill = ..level..), geom="polygon") +
#   facet_grid(p ~ .)
# 
# # ggplot(origdata, aes(x=x, y=z)) + 
# #   stat_density2d(aes(fill = ..density..), geom="tile", contour=FALSE) +
# #   facet_grid(p ~ .) + scale_fill_gradient2(high="blue")

for (fmt in c("pdf","wmf","png") ){
  for (i in names(plots)){
     
    filename <- paste("../plots/", fmt, "/", i, ".", fmt, sep="")
    cat(paste("\n",filename))    
    ggsave(file=filename, dpi=600, width=6.5, height=9.5, plot=plots[[i]])    
  
  }    
}



