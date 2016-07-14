pat_brul_2011<-read.csv2("F:/Invs/Brulés/patients_rm_fm_dp_2011.csv")
brules_2011<-read.csv2("F:/Invs/Brulés/sejours_fm_dp_2011.csv")
library(ggplot2) ## pour faire beau graphiques !!!
library(dplyr)
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







ggplot(data=graph[graph$fm_residence==1,],aes(x=graph$ageg14[graph$fm_residence==1],y=graph$total[graph$fm_residence==1]))+
  geom_bar(stat="identity")+
  ggplot(data=graph2[graph2$fm_residence==1,],aes(x=graph2$ageg14[graph2$fm_residence==1],y=graph2$total[graph2$fm_residence==1]))+
  geom_bar(stat="identity")

