pat_brul_2011<-read.csv2("F:/Invs/Brulés/patients_rm_fm_dp_2011.csv")
brules_2011<-read.csv2("F:/Invs/Brulés/sejours_fm_dp_2011.csv")
library(ggplot2) ## pour faire beau graphiques !!!
library(dplyr)

sort(names(brules_2011))
sort(names(pat_brul_2011))

#ggplot(data=brules_2011,aes(x=brules_2011$ageg14,y=f))+geom_point()

table(brules_2011$age2,brules_2011$ageg14)
str(pat_brul_2011$ageg14)
pat_brul_2011$ageg14<-as.factor(pat_brul_2011$ageg14)
levels(pat_brul_2011$ageg14)<-c( "0-2","2-14","15-49" ,"50-79" ,">80")
table(  pat_brul_2011$ageg14)

str(brules_2011$ageg14)
brules_2011$ageg14<-as.factor(brules_2011$ageg14)
levels(brules_2011$ageg14)<-c( "0-2","2-14","15-49" ,"50-79" ,">80")                                                                      
table(brules_2011$ageg14)

graph<-pat_brul_2011 %>% 
  group_by(ageg14,fm_residence) %>%
  summarise(total = length(numAno))
  
graph$name<-"patients"

graph2<-brules_2011 %>% 
  group_by(ageg14,fm_residence) %>%
  summarise(total = length(numAno))

graph2$name<-"séjours"

#d <- rbind(graph, graph2)
#d<-d[d$fm_residence==1 & !is.na(d$fm_residence),]
#d$name<-as.factor(d$name)
#p <- ggplot(d, aes(x=ageg14, y=total, fill = name)) + geom_bar(stat="identity")
#
#p



d<-d[d$fm_residence==1 & !is.na(d$fm_residence),]
d$name<-as.factor(d$name)
p <- ggplot(d ,aes(x=ageg14, y=total, fill = name)) + geom_bar(position="dodge",stat="identity")+xlab("Age")+ guides(fill=guide_legend(title=NULL))
p



# CTB
pat_brul_2011$gb<-ifelse (pat_brul_2011$finess %in% c('130783236' ,'690781810', '330781196' ,'590780193', '690805361', '130786049' ,'570005165', '340780477' ,'540002078' ,'440000289' ,'310781406' ,'370000481' ,'970100228' ,'970211207', '970408589',

 '980500003' ,'920120011' ,'830100574' ,'750100109' ,'750100166',
 '750100083'),1,0)



# 
#graph3<-pat_brul_2011 %>% 
#  group_by(ageg14,fm_residence,fm_hopital,gb,gravite2) %>%
#  summarise(total = length(numAno))
#
#graph3<-graph3[graph3$fm_residence==1 & graph3$fm_hopital==1 & graph3$gb==1 & !is.na(graph3$gravite2),]
#graph3$gravite2<-as.factor(graph3$gravite2)


graph3B<-pat_brul_2011[pat_brul_2011$fm_residence==1 & pat_brul_2011$fm_hopital==1 & pat_brul_2011$gb==1 & !is.na(pat_brul_2011$gravite2),]

graph3B$gravite2<-as.factor(graph3B$gravite2)
levels(graph3B$gravite2)<-c("non grave","grave")

 graph4<-graph3B %>% 
  group_by(ageg14,gravite2) %>%
  summarise(totali = n()) %>%
  mutate(pour=round(totali*100/sum(totali), 2) )

ggplot(data=graph4,aes(x=ageg14,y=pour,fill=gravite2))+geom_bar(position="dodge",stat="identity") +xlab("Age")+ylab("Pourcentage")+ 
guides(fill=guide_legend(title=NULL))
  
  
#  ggplot(data=graph2[graph2$fm_residence==1,],aes(x=graph2$ageg14[graph2$fm_residence==1],y=graph2$total[graph2$fm_residence==1]))+
#  geom_bar(position="dodge",stat="identity")

