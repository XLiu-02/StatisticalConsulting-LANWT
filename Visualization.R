library(ggplot2)
library(scales)
library(plotly)

###### Q1 ######

#1.barplot for if.talk10
ggplot(call.log.callback2, aes(x= IF.TALK10)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent of Callbacks", x="Conversation") +
  scale_y_continuous(labels=percent) +
  scale_fill_discrete(name = "Conversation", labels = c("No", "Yes")) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.text.x=element_blank(),axis.ticks.x=element_blank())


#2.distribution of talk time
library(ggpubr)
library(cowplot)

mean(call.log.callback2$TALK.TIME)
gghistogram(
  call.log.callback2, x = "TALK.TIME", 
  add = "mean", rug = TRUE, 
  fill = "CALL.TYPE",
  palette = "#00AFBB",
  title = "Mean Talk Time is 14.4 mins")


##### Q2 ######
Answered <- c("Yes", "No")
Percentage <- c(32.6,67.4)
df<-data.frame(Answered,Percentage)
df

ggplot(df, aes(x=Answered, y=Percentage, fill=Answered)) + 
  geom_bar(stat = "identity")+
  ggtitle("67.4% of calls are unanswered on average per day")+
  geom_text(aes(label=Percentage),vjust = -.4)+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))
            
##### Q3 ######
library(lubridate)
table.q3$MONTH <- month(as.POSIXlt(table.q3$DATE, format = "%Y/%m/%d"))
table.q3$MONTH <- ifelse(table.q3$MONTH==9, "Sep", 
                         ifelse(table.q3$MONTH==10, "Oct", "Nov"))

table.q3$DAY <- day(as.POSIXlt(table.q3$DATE, format = "%Y/%m/%d"))

#1.barplot with number of completed calls per day (with a line of average)
table.q3$IF.COMPLETE <- as.factor(table.q3$IF.COMPLETE)
x_axis_labels <- min(table.q3$DAY):max(table.q3$DAY)
yintercept = mean(subset(table.q3,table.q3$IF.COMPLETE==1)$Freq)
ggplot(table.q3, aes(DAY, Freq)) +
  geom_bar(stat = "identity", aes(fill = IF.COMPLETE)) +
  facet_grid(MONTH~.) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  labs(x = "Date", y = "Number of Calls") +
  scale_fill_discrete(name = "Calls", labels = c("Incompleted", "Completed")) +
  geom_hline(yintercept = 157, color="blue")+
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
  scale_fill_brewer(palette="Paired")


###### Q4 a) ######
#Pie chart of wait time 
call.log.hold2$QUEUE.WAIT.TIME.GROUP <- cut(call.log.hold2$QUEUE.WAIT.TIME,
                                            breaks = c(-Inf, 0, 1, 5, 10, 15, Inf),
                                            labels = c("0 min", "Less than 1 min", "1~5 mins",
                                                       "5~10 mins", "10~15 mins", "More than 15 mins"))

table.q4a <- call.log.hold2 %>%
  group_by(QUEUE.WAIT.TIME.GROUP) %>% summarise(Freq=n()) 
table.q4a$pct <- round((table.q4a$Freq)/sum(table.q4a$Freq),2)

ggplot(table.q4a, aes(x = "", y = Freq, fill = QUEUE.WAIT.TIME.GROUP)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = pct*100), position = position_stack(vjust=0.5),size=5) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Blues")+
  ggtitle("Percentage of Wait Time - hold on the hotline")

###### Q4.b) ######
#Pie chart for QUEUE.CALLBACK.WAIT.TIME
call.log.cb2$QUEUE.CALLBACK.WAIT.TIME.GROUP <- cut(call.log.cb2$QUEUE.CALLBACK.WAIT.TIME,
                                                   breaks = c(-Inf, 0, 1440, 2160, 2880, Inf),
                                                   labels = c("0 hr", "Less than 24 hrs", "24~36 hrs",
                                                              "36~48 hrs", "More than 48 hrs"))


table.q4b <- call.log.cb2 %>%
  group_by(QUEUE.CALLBACK.WAIT.TIME.GROUP) %>% summarise(Freq=n()) 

table.q4b$pct <- round((table.q4b$Freq)/sum(table.q4b$Freq),2)

ggplot(table.q4b, aes(x = "", y = Freq, fill = QUEUE.CALLBACK.WAIT.TIME.GROUP)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = pct*100), position = position_stack(vjust=0.5),size =5) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Blues")+
  ggtitle("Percentage of Wait Time - choose a callback")


##### tidy data (in ppt) ######

calltype <- call.log5 %>%
  group_by(CALL.TYPE) %>% summarise(Freq=n()) %>% mutate(PERCENT = round(100*Freq/sum(Freq),1))

ggplot(calltype, aes(x = "", y = PERCENT, fill = CALL.TYPE)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(label = PERCENT), position = position_stack(vjust=0.5),size =5) +
  theme_void()
  
table(call.log5$IF.TALK)
ggplot(call.log5, aes(IF.TALK, fill=as.factor(IF.TALK)))+
  geom_bar(aes(y=(..count..)/sum((..count..))))+
  ylab('Percentage of Calls')+theme_classic()+
  scale_fill_discrete(name = "If Talk", labels = c("No", "Yes"))+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


ggplot(call.log5, aes(IF.TALK10, fill=as.factor(IF.TALK10)))+
  geom_bar(aes(y=(..count..)/sum((..count..))))+
  ylab('Percentage of Calls')+theme_classic()+
  scale_fill_discrete(name = "If Talk (≥ 10 mins)", labels = c("No", "Yes"))+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

ggplot(call.log5, aes(DAY.OF.WEEK, fill=DAY.OF.WEEK))+
  geom_bar(aes(y=(..count..)/sum((..count..))))+
  ylab('Percentage of Calls')+xlab("Day of Week")+theme_classic()
