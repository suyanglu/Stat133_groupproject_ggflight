
args <- commandArgs(trailingOnly = TRUE)

camera_name = args[1]
wd= args[2]

setwd(wd)
setwd("..")

under_s = "_"

library(dplyr)
library(ggplot2)
library(tidyr)

file_name1= paste("dictIdx_table_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
file_name2= paste("compared_dict_unfiltered_",format(Sys.time(),"%Y%m%d"),".csv",sep='')

file_name3= paste(paste(camera_name,under_s,"all",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')



setwd(paste("./tmp/",camera_name,"/",sep=''))

diff <- read.csv(file_name1,encoding="UTF-8",stringsAsFactors = FALSE)
disp <- read.csv(file_name2,encoding="UTF-8",stringsAsFactors = FALSE)


setwd("../..")

setwd(paste("./output/",camera_name,"/",sep=''))

all <- read.csv(file_name3,encoding="UTF-8",stringsAsFactors = FALSE)


alltop2 <- unique(diff$dictIdx_top2)

disp <- disp %>% select(item_id, dict_index, disp_acc_price_sd, disp_acc_price_variance, disp_acc_price_range,	
               disp_acc_price_gap_2lowest,	disp_acc_price_gap_avg_min,	
               disp_frac_of_matching)

join_trable <- diff %>% left_join(disp, by = c("ItemID"="item_id", "dic_index"="dict_index"))
join_trable$one = 1

a <- join_trable %>% spread(dictIdx_top2, one)


for (i in alltop2) {
  colnames(a)[colnames(a) == i] = paste("ind_", i,sep="")
}


alltop2 = as.character(alltop2)

for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_1_disp_acc_price_sd",sep="")
}

for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_2_disp_acc_price_sd",sep="")
}



for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_1_disp_acc_price_variance",sep="")
}

for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_2_disp_acc_price_variance",sep="")
}



for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_1_disp_acc_price_range",sep="")
}

for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_2_disp_acc_price_range",sep="")
}





for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_1_disp_acc_price_gap_2lowest",sep="")
}

for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_2_disp_acc_price_gap_2lowest",sep="")
}



for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_1_disp_acc_price_gap_avg_min",sep="")
}

for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_2_disp_acc_price_gap_avg_min",sep="")
}



for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_1_disp_frac_of_matching",sep="")
}

for (i in alltop2) {
  a$b = NA
  colnames(a)[colnames(a) == "b"] = paste(i, "_2_disp_frac_of_matching",sep="")
}



for (i in 1:length(a[,1])) {
  top2 = substr(a[i,"dic_index"], 1, 2)
  first = a[i, "first"]
  value = a[i, "disp_acc_price_sd"]
  a[i, paste(top2, first, "disp_acc_price_sd", sep = "_")] = value
}

for (i in 1:length(a[,1])) {
  top2 = substr(a[i,"dic_index"], 1, 2)
  first = a[i, "first"]
  value = a[i, "disp_acc_price_variance"]
  a[i, paste(top2, first, "disp_acc_price_variance", sep = "_")] = value
}

for (i in 1:length(a[,1])) {
  top2 = substr(a[i,"dic_index"], 1, 2)
  first = a[i, "first"]
  value = a[i, "disp_acc_price_range"]
  a[i, paste(top2, first, "disp_acc_price_range", sep = "_")] = value
}


for (i in 1:length(a[,1])) {
  top2 = substr(a[i,"dic_index"], 1, 2)
  first = a[i, "first"]
  value = a[i, "disp_acc_price_gap_2lowest"]
  a[i, paste(top2, first, "disp_acc_price_gap_2lowest", sep = "_")] = value
}


for (i in 1:length(a[,1])) {
  top2 = substr(a[i,"dic_index"], 1, 2)
  first = a[i, "first"]
  value = a[i, "disp_acc_price_gap_avg_min"]
  a[i, paste(top2, first, "disp_acc_price_gap_avg_min", sep = "_")] = value
}


for (i in 1:length(a[,1])) {
  top2 = substr(a[i,"dic_index"], 1, 2)
  first = a[i, "first"]
  value = a[i, "disp_frac_of_matching"]
  a[i, paste(top2, first, "disp_frac_of_matching", sep = "_")] = value
}

temp <- !is.na(a[grepl("^(ind)", colnames(a))])
temp <- temp*1
a[,13:52] = temp


a$two_bundle_index=substr(a$two_bundle_index,start=2,stop=nchar(a$two_bundle_index)-1)
a=a%>%separate(two_bundle_index,into=c("bundle_index_1","bundle_index_2"),sep=",")
a$bundle_index_1=as.numeric(a$bundle_index_1)
a$bundle_index_2=as.numeric(a$bundle_index_2)


a=a%>%left_join(all,by=c("ItemID"="item_id","bundle_index_1"="bundle_index1","bundle_index_2"="bundle_index2"))



write.csv(a,file = paste(camera_name,"_all_with_disp_",format(Sys.time(),"%Y%m%d"),".csv",sep=''))

