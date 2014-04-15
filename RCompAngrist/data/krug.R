# user <- Sys.info()["user"]
# 
# ## I need this trick to switch from laptop to desktop
# if(user=="mat"){
#   pathDrop <- "/home/mat/Dropbox/"    
# } else if(user=="stigler"){
#   pathDrop <- "C:/Users/stigler/Dropbox/"
# } 
# 
# krug_dat <- read.csv(paste(pathDrop, "HEI/Bootstrap/abuelita.csv", sep=""))
# save(krug_dat, file=paste(pathDrop, "Documents/stats/R/RcompAngrist/pkg/data/krug_dat.rda", sep=""))