##function: add share (suyang's part) and write final output csv
##input: prem_relB0_w_many_prices_DATE.csv, prem_rel_all_w_many_prices_DATE.csv, transec
##output: 
#[in OUTPUT FOLDER] 700d_all_DATE.csv, 700d_relB0, 700d_relB1
#AND 2 share graphs relB0


args <- commandArgs(trailingOnly = TRUE)

camera_name = args[1]
wd= args[2]

setwd(wd)
setwd("..")

under_s = "_"


library(dplyr)
library(ggplot2)
library(tidyr)

file_name_prem_relB0_w_many_prices= paste("prem_relB0_w_many_prices_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
file_name_prem_rel_all_w_many_prices= paste("prem_rel_all_w_many_prices_",format(Sys.time(),"%Y%m%d"),".csv",sep='')
df_input=paste("transec",under_s,camera_name,".csv",sep='') 
Camera=paste("lst", under_s,camera_name,".csv",sep="")

setwd(paste("./input/",camera_name,"/",sep=''))
df_input = read.csv(df_input,head=TRUE)
Camera=read.csv(Camera, head=TRUE)
setwd("..")

setwd(paste("../tmp/",camera_name,"/",sep=''))
result=read.csv(file_name_prem_relB0_w_many_prices,encoding="UTF-8",stringsAsFactors = FALSE)
result3=read.csv(file_name_prem_rel_all_w_many_prices,encoding="UTF-8",stringsAsFactors = FALSE)

### add share

df_input<-df_input %>% select(item_id, amount, bundle_index)

#group the cells
grouped_sales <- df_input %>% group_by(item_id) %>% summarize(item_sales=sum(amount))

grouped_bundle <- df_input %>% group_by(item_id, bundle_index) %>% summarize(bundle_sales=sum(amount))
#grouped_f_sales <- df_input%>%group_by(item_id, color_category_ind_18_55) %>% filter(color_category_ind_18_55==1)%>%summarize(filterted_item_sales=sum(amount))

#grouped_f_bundle = df_input %>%group_by(item_id, bundle_index, color_category_ind_18_55)%>%filter(color_category_ind_18_55==1)%>%summarize(filtered_bundle_sales=sum(amount))
#grouped_f_bundle =grouped_f_bundle%>%select(item_id, bundle_index,filtered_bundle_sales)

#merge
df1 = data.frame(grouped_sales)
df2 = data.frame(grouped_bundle)
s1 <- merge(df1,df2,by="item_id")
#df3 <- data.frame(grouped_f_sales%>%select(item_id,filterted_item_sales ))
#df4 <- data.frame(grouped_f_bundle%>%select(item_id, bundle_index,filtered_bundle_sales))
#s1 = left_join(s1, df3, by='item_id')
#s1 = left_join(s1, df4, by=c("item_id", "bundle_index"))
#share function

count<-function(x){
  return(x['bundle_sales'] / x['item_sales'])
}
#f_count<-function(x){
#return(x['filtered_bundle_sales'] / x['filterted_item_sales'])
#}
#counting share
s1['share'] <- count(s1)

#s1['filtered_item_share'] <- f_count(s1)
s11<-s1%>%select(item_id, item_sales)
s11<-unique(s11)
s12<-s1%>%select(-item_sales)
s12<-unique(s12)
# s1

n0<-s1 %>%filter(bundle_index==0)%>%summarise(Bundle0=sum(bundle_sales))
n1<-s1 %>% filter(bundle_index==1)%>%summarise(B1=sum(bundle_sales))
n2<-s1 %>% filter(bundle_index==2)%>%summarise(B2=sum(bundle_sales))
n3<-s1 %>% filter(bundle_index==3)%>%summarise(B3=sum(bundle_sales))
n4<-s1 %>%filter(bundle_index== 4)%>%summarise(B4=sum(bundle_sales))
n5<-s1 %>% filter(bundle_index==5)%>%summarise(B5=sum(bundle_sales))
n6<-s1 %>% filter(bundle_index==6)%>%summarise(B6=sum(bundle_sales))
n7<-s1 %>% filter(bundle_index==7)%>%summarise(B7=sum(bundle_sales))
n8<-s1 %>% filter(bundle_index==8)%>%summarise(B8=sum(bundle_sales))
sum<-sum(n0,n1,n2,n3,n4,n5,n6,n7,n8)
Bundle0<-n0/sum
B1<-n1/sum
B2<-n2/sum
B3<-n3/sum
B4<-n4/sum
B5<-n5/sum
B6<-n6/sum
B7<-n7/sum
B8<-n8/sum

other0_1_8<-data.frame(Bundle0, Bundle1to8=sum(B1,B2,B3,B4,B5,B6,B7,B8))
other0_1_8<-other0_1_8%>%gather(bundle, value)
other0_1_2<-data.frame(B1, B2, B3,B4,B5,B6,B7,B8)
other0_1_2<-other0_1_2%>%gather(bundle, value)
other0_1_8<-data.frame(Bundle0, Bundle1to8=sum(B1,B2,B3,B4,B5,B6,B7,B8))
other0_1_8<-other0_1_8%>%gather(bundle, value)
other0_1_2<-data.frame(B1, B2, B3,B4,B5,B6,B7,B8)
other0_1_2<-other0_1_2%>%gather(bundle, value)


setwd(paste("../../output/",camera_name,"/",sep=''))
setwd("./0_relative_to_bundle_0")
setwd("./graph_share_rel0")


png(file="Avg_Bundle_Share1.png", height=500, width=560)
other0_1_8$value <- 100*other0_1_8$value
p<-barplot(other0_1_8$value, main="Avg Bundle Share vs. Bundle Index", col= "white", ylim=c(0,100), xlab="Bundle Index", ylab="Bundle Share", cex.lab=1.5, cex.axis=1, cex.main=2, cex.sub=1)
labs <- paste(round(other0_1_8$value, 2), "%", sep="")
text(x = p, y = other0_1_8$value, label =labs, pos = 3, cex = 1)
axis(1, at=p, labels=other0_1_8$bundle, tick=FALSE, cex.axis = 1)
dev.off()

png(file="Avg_Bundle_Share2.png", height=500, width=560)                        
other0_1_2$value<-round(other0_1_2$value, 4)
other0_1_2$value <- 100*other0_1_2$value
q<-barplot(other0_1_2$value, main="Avg Bundle Share vs. Bundle Index", col= "white", ylim=c(0,40), xlab="Bundle Index", ylab="Average Share", cex.lab=1.5, cex.axis=1, cex.main=2, cex.sub=1)
labs <- paste(round(other0_1_2$value, 2), "%", sep="")
text(x = q, y = other0_1_2$value, label =labs, pos = 3, cex = 1)
axis(1, at=q, labels=other0_1_2$bundle, tick=FALSE)
dev.off()


s1$bundle_index=as.integer(s1$bundle_index)

result3<-result3%>%left_join(s1, by=c("item_id"="item_id", "bundle_index2"="bundle_index"), copy=FALSE)
result3=unique(result3)
result3$item_sales[is.na(result3$item_sales)]<-"."



for(i in 1:nrow(result3)){
  if(result3[i,"item_sales"]=="."){
    result3[i,"bundle_sales"]<-"."
  }
}


for(i in 1:nrow(result3)){
  if(result3[i,"item_sales"]=="." & result3[i,"bundle_sales"]=="."){
    result3[i,"share"]<-"."
  }
}

result3$bundle_sales[result3$bundle_sales=="NA"] <- 0
result3$share[result3$share=="NA"] <- 0
result3$camera_name=camera_name
result3=unique(result3)

#s1$bundle_index=as.numeric(s1$bundle_index)
#result$bundle_index=as.factor(result$bundle_index)
result=result%>%left_join(s1, by=c("item_id", "bundle_index"))

colnames(Camera)[colnames(Camera)=="ItemID.LIsting_id."] <- "item_id"
OnlyPrice=Camera%>%select(item_id, bundle_index, bundle_price)
OnlyPrice=unique(OnlyPrice)
#OnlyPrice$bundle_index=as.factor(OnlyPrice$bundle_index)

####revenue
revenueready=s1%>%left_join(OnlyPrice, by=c("item_id"="item_id", "bundle_index"="bundle_index"))
revenueready=revenueready%>%filter(bundle_index != "NA")
revenueready[is.na(revenueready)]<-0
forevenue=data.frame(revenueready%>%group_by(bundle_index)%>%summarise(rsum=sum(bundle_price)))
forevenue=forevenue%>%mutate(per=rsum/sum(rsum))

###pprem share rel 0

pprem0=result%>%group_by(gr=cut(premium, breaks=c((min(premium)+1),0,50,100,150,200,(max(premium)+1))))
pprem0[is.na(pprem0)]<-0
pprem0=pprem0%>%filter(gr != "NA")
for(i in 1:nrow(pprem0)){
  if(pprem0[i,"bundle_sales"]=="."){
    pprem0[i,"bundle_sales"]<-0  }
}
pprem0$bundle_sales=as.numeric(pprem0$bundle_sales)
rprem0=pprem0%>%group_by(gr)%>%summarise(pp=sum(bundle_sales))
rprem0=rprem0%>%mutate(per=pp/sum(pp))
rprem0=rprem0[-1,]
rprem0$gr=gsub("([(])", "", rprem0$gr)
rprem0$gr=gsub("([]])", "", rprem0$gr)
rprem0$gr=gsub(",", "-", rprem0$gr)
rprem0$gr=gsub("200(-).*", ">200", rprem0$gr)

###pprem share rel 1

pprem1=result3%>%filter(bundle_index1==1)%>%group_by(gr=cut(premrel, breaks=c(min(premrel),0,50,100,150,200,max(premrel))))
pprem1[is.na(pprem1)]<-0
#pprem1=pprem1%>%filter(gr != "NA")
for(i in 1:nrow(pprem1)){
  if(pprem1[i,"bundle_sales"]=="."){
    pprem1[i,"bundle_sales"]<-0  }
}
pprem1$bundle_sales=as.numeric(pprem1$bundle_sales)
rprem1=pprem1%>%group_by(gr)%>%summarise(pp=sum(bundle_sales))
rprem1=rprem1%>%mutate(per=pp/sum(pp))
rprem1=rprem1[-1,]
rprem1$gr=gsub("([(])", "", rprem1$gr)
rprem1$gr=gsub("([]])", "", rprem1$gr)
rprem1$gr=gsub(",", "-", rprem1$gr)
rprem1$gr=gsub("200(-).*", ">200", rprem1$gr)



setwd("../../")

outputfilename = paste(paste(camera_name,under_s,"all",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result3,file = outputfilename)

outputfilename = paste(paste(camera_name,under_s,"relative_to_bundle_0",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result,file=outputfilename)

result4=result3%>%
  filter(bundle_index1==1)
outputfilename = paste(paste(camera_name,under_s,"relative_to_bundle_1",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(result4,file=outputfilename)


outputfilename = paste(paste(camera_name,under_s,"onlyshare_filtered",under_s,sep='') ,format(Sys.time(),"%Y%m%d"),".csv",sep='')
write.csv(s1,file=outputfilename)


setwd("./4_share")
png(file="share_bundle_index.png")
boxplot(s1$share~s1$bundle_index,xlab="Bundle Index", ylab="Share", main="Bundle Share vs. Bundle Index",cex.lab=1.5, cex.axis=1, cex.main=2, cex.sub=1)+
  theme(axis.title=element_text(size=30), axis.text=element_text(size=20), title=element_text(size=30))
dev.off()
png(file="share_bundle_index_highlighted.png")
boxplot(s1$share~s1$bundle_index,xlab="Bundle Index", ylab="Share", main="Bundle Share vs. Bundle Index", col=ifelse(s1$item_id==16831203027, "red", "white"),cex.lab=1.45, cex.axis=1, cex.main=2, cex.sub=1)+
  theme(axis.title=element_text(size=25), axis.text=element_text(size=15), title=element_text(size=25))
dev.off()

png(file="bundle_index_vs_bundle_sale.png", bg="transparent")
boxplot(s1$bundle_sales~s1$bundle_index, xlab="Bundle Index",ylab="Bundle Sales", main="Bundle Sales vs. Bundle Index", cex.lab=1.45, cex.axis=1.1, cex.main=2, cex.sub=1)+
  theme(axis.title=element_text(size=25), axis.text=element_text(size=15), title=element_text(size=25))
dev.off()


png(file="revenue bundle index.png")
forevenue%>%ggplot(aes(x=bundle_index, y=rsum))+geom_bar(stat="identity")+labs(x="Bundle Index", y="Sum of Revenue", title="Revenue vs. Bundle Index")+geom_text(aes(label=gsub("000", "k", round(rsum, -3))),vjust=-0.5)+
  theme(axis.title=element_text(size=25), axis.text=element_text(size=15), title=element_text(size=25))
dev.off()

png(file="revenue bundle index percentage.png")
forevenue%>%ggplot(aes(x=bundle_index, y=per))+geom_bar(stat="identity")+geom_text(aes(label=paste(round(per*100, 2),"%", sep="")),vjust=-0.5)+labs(x="Bundle Index", y="Percentage of Revenue", title="Revenue vs. Bundle Index")+
  theme(axis.title=element_text(size=25), axis.text=element_text(size=15), title=element_text(size=25))
dev.off()

setwd("../0_relative_to_bundle_0/graph_share_rel0")
png(file="Market_Share_vs_Premium_relB0.png", height=560, width=560)
rprem0%>%ggplot(aes(x=gr, y=per))+geom_bar(stat="identity")+
  geom_text(aes(label=paste(gr,"RMB",sep=""), vjust=-0.5), size=5)+
  geom_text(aes(label=paste(round(per*100, 2),"%", sep=""),vjust=-2), size=7)+
  labs(x="Premium Range", y="Share", title="Market Share vs. Premium relB0")+
  ylim(0,0.6)+
  scale_x_discrete(limits=c("0-50", "50-100","100-150","150-200",">200"))+
  theme(axis.title=element_text(size=25), axis.text=element_text(size=15), title=element_text(size=25))+
  geom_text(data=NULL,x=3,y=0.5,label=paste("The total market share for \n bundles w + premia is ", round(sum(rprem0$per)*100,2),"%", sep=""), size=10)
dev.off()

png(file="Sales_vs_Premium_relB0.png", width=700, height=650)
rprem0%>%ggplot(aes(x=gr, y=pp))+geom_bar(stat="identity")+geom_text(aes(label=pp), vjust=-3,size=8)+
  labs(x="Premium Range", y="Sales", title="Sales vs. Premium relB0")+geom_text(aes(label=gr), vjust=-0.5,size=10)+
  theme(axis.text.x=element_blank())+ylim(0,5000)+
  scale_x_discrete(limits=c("0-50", "50-100","100-150","150-200",">200"))+
  theme(axis.title=element_text(size=25), axis.text=element_text(size=15), title=element_text(size=25))+
  geom_text(data=NULL,x=3,y=4000,label=paste("The total sales for \n bundles w + premia is ", sum(rprem0$pp), sep=""), size=10)
dev.off()

setwd("../../1_relative_to_bundle_1/graph_share_rel1")
png(file="Market_Share_vs_Premium_relB1.png", height=560, width=560)
rprem1%>%ggplot(aes(x=gr, y=per))+geom_bar(stat="identity")+
  geom_text(aes(label=paste("RMB",gr,sep=""), vjust=-0.5), size=6)+
  geom_text(aes(label=paste(round(per*100, 2),"%", sep=""),vjust=-2), size=7)+
  labs(x="Premium Range", y="Share", title="Market Share vs. Premium relB1")+
  ylim(0,0.6)+
  scale_x_discrete(limits=c("0-50", "50-100","100-150","150-200",">200"))+
  theme(axis.title=element_text(size=25), axis.text=element_text(size=15), title=element_text(size=25))+
  geom_text(data=NULL,x=3,y=0.5,label=paste("The total market share for \n bundles w + premia is ", round(sum(rprem1$per)*100,2),"%", sep=""), size=10)
dev.off()

png(file="Sales_vs_Premium_relB1.png", width=700, height=650)
rprem1%>%ggplot(aes(x=gr, y=pp))+geom_bar(stat="identity")+geom_text(aes(label=pp), vjust=-3,size=8)+
  labs(x="Premium Range", y="Sales", title="Sales vs. Premium RelB1")+geom_text(aes(label=gr), vjust=-0.5,size=10)+
  theme(axis.text.x=element_blank())+ylim(0,5000)+
  scale_x_discrete(limits=c("0-50", "50-100","100-150","150-200",">200"))+
  theme(axis.title=element_text(size=25), axis.text=element_text(size=15), title=element_text(size=25))+
  geom_text(data=NULL,x=3,y=4000,label=paste("The total sales for \n bundles w + premia is ", sum(rprem1$pp), sep=""), size=10)
dev.off()

