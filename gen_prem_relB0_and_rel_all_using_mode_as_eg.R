##function: generate premium rel B0 as well as rel all (pairwise) using mode as an example
##input: dict, lst,good_format_dictIdx_table_pr_fk_DATE.csv
##output: camera_w_price_DATE.csv, prem_relB0_DATE.csv, prem_rel_all_DATE.csv

args <- commandArgs(trailingOnly = TRUE)

camera_name = args[1]
wd= args[2]

#To test this code by itself, run from here
setwd(wd)
setwd("..")
under_s = "_"

library(dplyr)
library(ggplot2)
library(tidyr)

filename_list=paste("lst",under_s,camera_name,".csv",sep='') 
filename_dict=paste("dict",under_s,camera_name,".csv",sep='') 

file_name_category=paste("good_format_dictIdx_table_pr_fk_",format(Sys.time(),"%Y%m%d"),".csv",sep='')


setwd(paste("./input/",camera_name,"/",sep=''))
Camera<-read.csv(filename_list,stringsAsFactors = FALSE,fileEncoding = "UTF-8")
Price<-read.csv(filename_dict,fileEncoding = "UTF-8")%>%select(index_final,Avg..Price.Esitimate)
Price$Avg..Price.Esitimate<-as.character(Price$Avg..Price.Esitimate)
colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"
Camera=Camera%>%filter(item_id!=45410114111)

setwd("..")

setwd(paste("../tmp/",camera_name,"/",sep=''))
df.category=read.csv(file_name_category,encoding="UTF-8",stringsAsFactors = FALSE)


#Genarate delta n
df=Camera%>%
  select(item_id,bundle_index,dict_yi_index)%>%
  filter(bundle_index!=0)
##i,j, k index the row numbers for columns bundle_index1, bundle_index2, dict_yi_index resp.
df.deltan=data.frame()
i=1
while(i<=length(df$item_id))
{
  count_n=df%>%
    filter(item_id==item_id[i]&bundle_index==bundle_index[i])%>%
    tally()
  n=count_n[1,1]
  j=i+n
  a=as.numeric(c(df$item_id[i],df$bundle_index[i],df$bundle_index[j],n))
  while(df$item_id[j]==df$item_id[i]&j<=length(df$item_id))
  {
    t=0
    for(k in i:(i+n-1))
      if(df$dict_yi_index[j]==df$dict_yi_index[k]) t=t+1
      if(t==0)
        a=a+c(0,0,0,1)
      else a=a+c(0,0,0,-1)
      if(df$bundle_index[j]!=df$bundle_index[j+1]|(j+1)>length(df$item_id))
      {
        df.deltan=rbind(df.deltan,a)
        a=as.numeric(c(df$item_id[i],df$bundle_index[i],df$bundle_index[j+1],n))
      }
      j=j+1
  }
  i=i+n
}
colnames(df.deltan)=c("item_id","bundle_index1","bundle_index2","diff")


#Set a benchmark as denominator for "prem_perc"
Benchmark=Camera%>%
  group_by(item_id)%>%
  summarise(benchmark=first(bundle_price))

# Set all "unknown" or lost Price = 0
for(i in 1:length(Price$Avg..Price.Esitimate))
  if(Price$Avg..Price.Esitimate[i]==""|
     Price$Avg..Price.Esitimate[i]=="unknown"|
     Price$Avg..Price.Esitimate[i]=="Unknown"|
     is.na(Price$Avg..Price.Esitimate[i]))
    Price$Avg..Price.Esitimate[i]=0

Camera$dict_yi_index<-as.character(Camera$dict_yi_index)
Price$index_final<-as.character(Price$index_final)
Price$Avg..Price.Esitimate<-as.numeric(Price$Avg..Price.Esitimate)
CameraWPrice<-Camera%>%left_join(Price,by=c("dict_yi_index"="index_final"))

#Generate premium
result<-CameraWPrice%>%
  group_by(item_id,bundle_index)%>%
  summarise(total=sum(Accessory_title!=""),
            bundle_price=mean(bundle_price),
            price_ref=sum(Avg..Price.Esitimate))
result[is.na(result)]<-0
result<-result%>%
  mutate(bundle_add_on=bundle_price-first(bundle_price),
         price_ref_rel=price_ref-first(price_ref),
         num_of_acc_rel=total-first(total),
         premium=bundle_add_on-price_ref_rel,
         premium_percent=(bundle_add_on-price_ref_rel)/bundle_price)%>%
  filter(bundle_index!=0)%>%
  filter(num_of_acc_rel!=0)%>%
  select(item_id,bundle_index,bundle_price,num_of_acc_rel,premium,premium_percent)
result$positive_prem=nrow(result%>%filter(premium>0))/nrow(result)



#Genarate a data frame "all"
result2=c()
i=1
while(i<=length(result$item_id))
{
  j=i+1
  while(j<=length(result$item_id)&result$item_id[j]==result$item_id[i])
  {
    result2=rbind(result2,
                  c(result$item_id[j],
                    result$bundle_index[i],
                    result$bundle_index[j],
                    result$premium[j]-result$premium[i]))
    j=j+1
  }
  i=i+1
}  

result2=as.data.frame(result2)
colnames(result2)=c("item_id","bundle_index1","bundle_index2","premrel")
result2$premrel=as.numeric(result2$premrel)
result2$bundle_index1=as.numeric(result2$bundle_index1)
result2$bundle_index2=as.numeric(result2$bundle_index2)
result2=result2%>%filter(!grepl("item", item_id))
result2=result2%>%
  left_join(Benchmark,by=c("item_id"="item_id"))%>%
  mutate(premrel_perc=premrel/benchmark)

result2$positive_prem_rel_1=nrow(result2%>%filter(bundle_index1==1&premrel>0))/nrow(result2%>%filter(bundle_index1==1))
df.deltan$item_id=as.character(df.deltan$item_id)
result3<-result2%>%left_join(df.deltan,by=c("item_id"="item_id","bundle_index1"="bundle_index1","bundle_index2"="bundle_index2"))
result3$diff[is.na(result3$diff)]=0

df.category$item_id=as.character(df.category$item_id)
result3=result3%>%
  left_join(df.category,by=c("item_id"="item_id","bundle_index1"="bundle_index_1","bundle_index2"="bundle_index_2"))

df.bundleprice=result%>%
  select(item_id,bundle_index,bundle_price)
colnames(df.bundleprice)[colnames(df.bundleprice)=="bundle_price"]="bundle_price1"

df.bundleprice$item_id=as.character(df.bundleprice$item_id)
result3=result3%>%
  left_join(df.bundleprice,by=c("item_id"="item_id","bundle_index1"="bundle_index"))
colnames(df.bundleprice)[colnames(df.bundleprice)=="bundle_price1"]="bundle_price2"

result3=result3%>%
  left_join(df.bundleprice,by=c("item_id"="item_id","bundle_index2"="bundle_index"))%>%
  mutate(rel_cost_deln=bundle_price2-bundle_price1-premrel)



# fixme mover me to another code file
result$avg_premium_mode_relB0=mean(result$premium)
result$avg_perc_premium_mode_relB0=mean(result$premium_percent)
result$frac_positive_prem_mode_relB0=result$positive_prem
result$premium_mode_relB0=result$premium
result$perc_premium_mode_relB0=result$premium_percent


result3$avg_premium_mode_relB1=mean(result3$premrel)
result3$avg_perc_premium_mode_relB1=mean(result3$premrel_perc)
result3$frac_positive_premium_mode_relB1=result3$positive_prem_rel_1
result3$premium_mode_relB1=result3$premrel
result3$perc_premium_mode_relB1=result3$premrel_perc



result$camera_name=camera_name
result3$camera_name=camera_name

result$deltan=result$num_of_acc_rel

outputfilename = paste("prem_relB0_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result,file=outputfilename)

outputfilename = paste("prem_rel_all_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result3,file=outputfilename)

outputfilename = paste("camera_w_price_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(CameraWPrice,file=outputfilename)


