##function: combine many other street prices with automated process of computing prem
##input: lst, price_originalorder_DATE.csv,compared_dict_unfiltered_DATE.csv, camera_w_price_DATE.csv, prem_relB0_DATE.csv, prem_rel_all_DATE.csv

#fixme: import filtered as well

##output: prem_relB0_w_many_prices_DATE.csv, prem_rel_all_w_many_prices_DATE.csv

args <- commandArgs(trailingOnly = TRUE)

camera_name = args[1]
wd= args[2]

setwd(wd)
setwd("..")

under_s = "_"

library(dplyr)
library(ggplot2)
library(tidyr)

filename_list=paste("lst",under_s,camera_name,".csv",sep='') 

file_name_camera_w_price=paste("camera_w_price_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
file_name_prem_relB0=paste("prem_relB0_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
file_name_prem_rel_all=paste("prem_rel_all_",format(Sys.time(),"%Y%m%d"),".csv",sep='')

#these street prices are from geyao
file_name_compare_price=paste("compared_dict_unfiltered_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
#these filtered street prices are from geyao
file_name_compare_filtered_price=paste("compared_dict_filtered_tmall15_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
#these street prices are from suyang
file_name_originalorder_price=paste("price_originalorder_", format(Sys.time(), "%Y%m%d"), ".csv", sep="")


setwd(paste("./input/",camera_name,"/",sep=''))
Camera<-read.csv(filename_list,stringsAsFactors = FALSE)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"
Camera=Camera%>%filter(item_id!=45410114111)	
setwd("..")

Benchmark=Camera%>%
  group_by(item_id)%>%
  summarise(benchmark=first(bundle_price))

setwd(paste("../tmp/",camera_name,"/",sep=''))
#CameraWPrice has the same key as lst w additional acc street prices 
CameraWPrice=read.csv(file_name_camera_w_price,encoding="UTF-8",stringsAsFactors = FALSE)
result=read.csv(file_name_prem_relB0,encoding="UTF-8",stringsAsFactors = FALSE)
result3=read.csv(file_name_prem_rel_all,encoding="UTF-8",stringsAsFactors = FALSE)
df.compare_price=read.csv(file_name_compare_price,encoding="UTF-8",stringsAsFactors = FALSE)
df.compare_filtered_price = read.csv(file_name_compare_filtered_price, encoding="UTF-8",stringsAsFactors = FALSE)
df.compare_price_suyang=read.csv(file_name_originalorder_price,encoding="UTF-8",stringsAsFactors = FALSE)
#suyang


df.compare_price1=df.compare_price[,grepl("^((item_id)|(dict_index)|(acc_price))",colnames(df.compare_price))]%>%
  unique()
df.compare_price_suyang1=df.compare_price_suyang[,grepl("^((index)|(acc_price))",colnames(df.compare_price_suyang))]%>%
  unique()
for (i in 3:(length(colnames(df.compare_price1))))
  df.compare_price1[,i]=as.character(df.compare_price1[,i])

df.compare_price1$item_id=as.character(df.compare_price1$item_id)
df.compare_filtered_price$item_id=as.character(df.compare_filtered_price$item_id)

CameraWComparePrice=CameraWPrice%>%
  select(item_id,Accessory_title,bundle_index,bundle_price,dict_yi_index,Avg..Price.Esitimate)%>%
  left_join(df.compare_price1,by=c("item_id"="item_id","dict_yi_index"="dict_index"))

CameraWComparePrice=CameraWComparePrice%>%
  left_join(df.compare_filtered_price,by=c("item_id"="item_id","dict_yi_index"="dict_index"))

CameraWComparePrice=CameraWComparePrice%>%
  left_join(df.compare_price_suyang1,by=c("dict_yi_index"="index"))
CameraWComparePrice=CameraWComparePrice%>%filter(!grepl("item", item_id))
CameraW
#set all "NA" in the street prices to the original price(Avg..Price.Esitimate)
for (i in which(colnames(CameraWComparePrice)%in%colnames(CameraWComparePrice[,grepl("^(acc_price)",colnames(CameraWComparePrice))])) )
{
  for (j in 1:length(CameraWComparePrice[,i]))
    if(is.na(CameraWComparePrice[j,i]))
      CameraWComparePrice[j,i]=CameraWComparePrice$Avg..Price.Esitimate[j]
}


#Spilt CameraWComparePrice into different dataframes in a list in order to auto
l=list()
v=vector()
#v keeps track of the column name of different acc_street_price
for (i in 1:(length(names(CameraWComparePrice))-6))
{
  l[[i]]=CameraWComparePrice[,c(1,2,3,4,5,(i+6))]
  v[i]=colnames(l[[i]])[6]
  colnames(l[[i]])[6]="acc_price"
}


#store the part xxx from "acc_price_xxx" in v
v=gsub("acc_price","",v)


#gen premium based on diff acc_street_price
for (k in 1:(length(names(CameraWComparePrice))-6))
{
  #Generate premium rel B0
  result_tmp<-l[[k]]%>%
    group_by(item_id,bundle_index)%>%
    summarise(total=sum(Accessory_title!=""),
              bundle_price=mean(bundle_price),
              price_ref=sum(acc_price))
  result_tmp[is.na(result_tmp)]<-0
  result_tmp<-result_tmp%>%
    mutate(bundle_add_on=bundle_price-first(bundle_price),
           price_ref_rel=price_ref-first(price_ref),
           num_of_acc_rel=total-first(total),
           premium=bundle_add_on-price_ref_rel,
           premium_percent=(bundle_add_on-price_ref_rel)/bundle_price)%>%
    filter(bundle_index!=0)%>%
    filter(num_of_acc_rel!=0)%>%
    select(item_id,bundle_index,bundle_price,num_of_acc_rel,premium,premium_percent)
  
  
  result_tmp$avg_premium=mean(result_tmp$premium)
  result_tmp$avg_premium_percent=mean(result_tmp$premium_percent)
  result_tmp$positive_prem=nrow(result_tmp%>%filter(premium>0))/nrow(result_tmp)
  
  colnames(result_tmp)[colnames(result_tmp)=="premium_percent"]=paste("perc_premium",v[k],"_relB0",sep='')
  colnames(result_tmp)[colnames(result_tmp)=="avg_premium"]=paste("avg_premium",v[k],"_relB0",sep='')
  colnames(result_tmp)[colnames(result_tmp)=="avg_premium_percent"]=paste("avg_perc_premium",v[k],"_relB0",sep='')
  colnames(result_tmp)[colnames(result_tmp)=="positive_prem"]=paste("frac_positive_premium",v[k],"_relB0",sep='')
  #Generate premium rel B0 ends here
  
  #Genarate premium any two bundle (rel1); a data frame "all" 
  result_tmp2=data.frame()
  i=1
  while(i<=length(result_tmp$item_id))
  {
    j=i+1
    while(j<=length(result_tmp$item_id)&result_tmp$item_id[j]==result_tmp$item_id[i])
    {
      result_tmp2=rbind(result_tmp2,
                        c(result_tmp$item_id[j],
                          result_tmp$bundle_index[i],
                          result_tmp$bundle_index[j],
                          result_tmp$premium[j]-result_tmp$premium[i]))
      j=j+1
    }
    i=i+1
  }  
  
  colnames(result_tmp2)=c("item_id","bundle_index1","bundle_index2","premrel")
  
  result_tmp2=result_tmp2%>%
    left_join(Benchmark,by=c("item_id"="item_id"))%>%
    mutate(premrel_perc=premrel/benchmark)%>%
    select(item_id,bundle_index1,bundle_index2,premrel,premrel_perc)
  
  
  result_tmp2$avg_premium=mean((result_tmp2%>%filter(bundle_index1==1))[,"premrel"])
  result_tmp2$avg_premium_percent=mean((result_tmp2%>%filter(bundle_index1==1))[,"premrel_perc"])
  result_tmp2$positive_prem_rel_1=nrow(result_tmp2%>%filter(bundle_index1==1&premrel>0))/nrow(result3%>%filter(bundle_index1==1))
  
  colnames(result_tmp2)[colnames(result_tmp2)=="premrel"]=paste("premium",v[k],"_relB1",sep='')
  colnames(result_tmp2)[colnames(result_tmp2)=="premrel_perc"]=paste("perc_premium",v[k],"_relB1",sep='')
  colnames(result_tmp2)[colnames(result_tmp2)=="avg_premium"]=paste("avg_premium",v[k],"_relB1",sep='')
  colnames(result_tmp2)[colnames(result_tmp2)=="avg_premium_percent"]=paste("avg_perc_premium",v[k],"_relB1",sep='')
  colnames(result_tmp2)[colnames(result_tmp2)=="positive_prem_rel_1"]=paste("frac_positive_premium",v[k],"_relB1",sep='')
  
  result3=result3%>%
    left_join(result_tmp2,by=c("item_id"="item_id","bundle_index1"="bundle_index1","bundle_index2"="bundle_index2"))
  
  colnames(result_tmp)[colnames(result_tmp)=="premium"]=paste("premium",v[k],"_relB0",sep='')
  
  result=result%>%
    left_join(result_tmp,by=c("item_id"="item_id","bundle_index"="bundle_index","bundle_price"="bundle_price","num_of_acc_rel"="num_of_acc_rel"))
 
}
#gen premium computation ends here



#Note: 
#In result3, there are pairwise premiums/perc_premiums using dif street prices,
#they are always correct. However, other columns with a "rel B1" in their names are only correct
#when the benchmark for premium is Bundle 1

outputfilename = paste("prem_relB0_w_many_prices_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result,file=outputfilename)

outputfilename = paste("prem_rel_all_w_many_prices_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result3,file=outputfilename)



