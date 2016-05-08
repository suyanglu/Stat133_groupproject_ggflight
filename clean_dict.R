##function: remove all lines with empty dict index in dict_CAMERANAME.csv 
##input: input/CAMERANAME/dict_CAMERANAME.csv 
##output: input/CAMERANAME/dict_CAMERANAME.csv (OVERRIDE the previous one)

args <- commandArgs(trailingOnly = TRUE)

camera_name = args[1]
wd= args[2]

#To test this code by itself, run from here
setwd(wd)
setwd("..")
under_s = "_"

library(dplyr)

filename_dict=paste("dict",under_s,camera_name,".csv",sep='') 

setwd(paste("./input/",camera_name,"/",sep=''))
Price<-read.csv(filename_dict,fileEncoding = "UTF-8")
Price$index_final<-as.character(Price$index_final)
Price=Price%>%filter(index_final!="")

write.csv(Price,file=filename_dict)
