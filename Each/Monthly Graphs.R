library(ggplot2)
library(gridExtra)
library(reshape)
library(lubridate)
library(plyr)
library(dplyr)
library(stringr)

m46 = read.csv("m46labelled.csv")
m60 = read.csv("m60labelled.csv")


m46$type = factor(m46$type)

m46$StartTime <- as.POSIXct(m46$StartTime, format = "%d/%m/%Y %H:%M")

m60$type = factor(m60$type)
m60$StartTime <- as.POSIXct(m60$StartTime, format = "%Y-%m-%d %H:%M:%S")

monthname = "Apr"

m46 = m46[which(month.abb[month(m46$StartTime)]==monthname),]
m60 = m60[which(month.abb[month(m60$StartTime)]==monthname),]


m46typecount = data.frame(t(summary(m46$type)))
m60typecount = data.frame(t(summary(m60$type)))
names(m46typecount)=c(0,1,100,101,111,1001,1100,1101,1111)
names(m60typecount)=c(0,1,100,101,111,1001,1100,1101,1111)

m46summary=tapply(m46$THD1,m46$type,mean)
m46summary=rbind(m46summary,tapply(m46$THD2,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$THD3,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H1_1,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H2_1,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H3_1,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H5_1,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H7_1,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H1_2,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H2_2,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H3_2,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H5_2,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H7_2,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H1_3,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H2_3,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H3_3,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H5_3,m46$type,mean))
m46summary=rbind(m46summary,tapply(m46$H7_3,m46$type,mean))

m60summary=tapply(m60$THD1,m60$type,mean)
m60summary=rbind(m60summary,tapply(m60$THD2,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$THD3,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H1_1,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H2_1,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H3_1,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H5_1,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H7_1,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H1_2,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H2_2,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H3_2,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H5_2,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H7_2,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H1_3,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H2_3,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H3_3,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H5_3,m60$type,mean))
m60summary=rbind(m60summary,tapply(m60$H7_3,m60$type,mean))

tablenames = c('THD1','THD2','THD3','H1_1','H2_1','H3_1','H5_1','H7_1','H1_2','H2_2','H3_2','H5_2','H7_2',
               'H1_3','H2_3','H3_3','H5_3','H7_3')
rownames(m46summary) = tablenames
rownames(m60summary) = tablenames

m46rawmean = as.matrix(m46summary) %*% t(as.matrix(m46typecount))
m60rawmean = as.matrix(m60summary) %*% t(as.matrix(m60typecount))


#df3 <- m46[,c('THD1','THD2','THD3','H1_1','H2_1','H3_1','H5_1','H7_1','H1_2','H2_2','H3_2','H5_2','H7_2',
#              'H1_3','H2_3','H3_3','H5_3','H7_3', 'type')]
#df3 <- melt(df3, id=c("type"))

df3=m46[,c('THD1','type')]
df3$type=str_pad(df3$type, 4, pad = "0")

df3count = data.frame(t(m46typecount)/sum(m46typecount))
colnames(df3count) = c("count")
df3count$x=c(0,1,100,101,111,1001,1100,1101,1111)
df3count$x=str_pad(df3count$x, 4, pad = "0")

means <- aggregate(THD1 ~  type, df3, mean)
means$THD1 = round(means$THD1,3)

jpeg(paste0(monthname,' (m46).jpg'))

ggplot() + 
  geom_boxplot(data = df3,aes(x=type, y = THD1, fill = type)) + 
  stat_summary(data = df3,aes(x=type, y = THD1),fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=4,show_guide = FALSE) + 
  xlab("Filter Combination") + ylab("THD") + 
  geom_line(group=1,data=df3count, aes(y=count,x=factor(x)),colour="red") + 
  geom_text(data = means, aes(x = factor(type),label = THD1, y = THD1 + 0.12)) +
  ggtitle(monthname) + theme_bw() + 
#  ylim(0,1.5) + 
  scale_y_continuous(limits = c(0,1.5), expand = c(0, 0)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.ticks.x=element_blank(), 
        axis.ticks.y=element_blank(), plot.title = element_text(hjust = 0.5), 
        axis.line = element_line(colour = "black"))

dev.off()


df4=m60[,c('THD1','type')]
df4$type=str_pad(df4$type, 4, pad = "0")

df4count = data.frame(t(m60typecount)/sum(m60typecount))
colnames(df4count) = c("count")
df4count$x=c(0,1,100,101,111,1001,1100,1101,1111)
df4count$x=str_pad(df4count$x, 4, pad = "0")

means1 <- aggregate(THD1 ~  type, df4, mean)
means1$THD1 = round(means1$THD1,3)

jpeg(paste0(monthname,' (m60).jpg'))

ggplot() + 
  geom_boxplot(data = df4,aes(x=type, y = THD1, fill = type)) + 
  stat_summary(data = df4,aes(x=type, y = THD1),fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=4,show_guide = FALSE) + 
  xlab("Filter Combination") + ylab("THD") + 
  geom_line(group=1,data=df4count, aes(y=count,x=factor(x)),colour="red") + 
  geom_text(data = means1, aes(x = factor(type),label = THD1, y = THD1 + 0.12)) +
  ggtitle(monthname) + theme_bw() + 
  #  ylim(0,1.5) + 
  scale_y_continuous(limits = c(0,1.5), expand = c(0, 0)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.ticks.x=element_blank(), 
        axis.ticks.y=element_blank(), plot.title = element_text(hjust = 0.5), 
        axis.line = element_line(colour = "black"))

dev.off()

#2 sample t test
combi0101 = df3[which(df3$type=='0101'),'THD1']
combi0111 = df3[which(df3$type=='0111'),'THD1']
t.test(combi0101,combi0111,var.equal = FALSE, paired=FALSE,alternative = 'greater')
