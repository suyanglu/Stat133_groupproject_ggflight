##function: clean lst_CAMERANAME.csv:
####1. Delete all the empty lines (based on if the column URL is empty);
####2. Scrape item id from URL to get rid of scientific notation problem;
####3. Sort by (item_id, bundle_index);
####4. Delete redundant lines (when two lines have the same item_id,bundle_index,dict_yi_index)
##input: input/CAMERANAME/lst_CAMERANAME.csv 
##output: input/CAMERANAME/lst_CAMERANAME.csv (OVERRIDE the previous one)

args <- commandArgs(trailingOnly = TRUE)

camera_name = args[1]
wd= args[2]

#To test this code by itself, run from here
setwd(wd)
setwd("..")
under_s = "_"

library(dplyr)

filename_lst=paste("lst",under_s,camera_name,".csv",sep='') 

setwd(paste("./input/",camera_name,"/",sep=''))
Camera<-read.csv(filename_lst,stringsAsFactors = FALSE)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"

Camera=Camera%>%
  filter(URL!="")%>%
  mutate(item_id=gsub("(^(.+)/item/)|(\\.htm(.*))","",URL))%>%
  arrange(item_id,bundle_index)

Redundant=Camera[duplicated(Camera[,c("item_id","bundle_index","dict_yi_index")]),]

Camera<-Camera[!duplicated(Camera[,c("item_id","bundle_index","dict_yi_index")]),]

colnames(Camera)[colnames(Camera)=="item_id"] <- "ItemID(LIsting_id)"

write.csv(Camera,file=filename_lst)

setwd(paste("../../tmp/",camera_name,"/",sep=''))
write.csv(Redundant,file="lines_deleted.csv")







