library(tidyverse)

# read data
call.log <- read.csv("/Users/xiaoyingziliu/Desktop/LANWT Project/Call Log 0912-1110.csv")  # 28711,13

str(call.log)

###############################
##### Data preprocessing ######
###############################

## convert factors into char
call.log$DATE <- as.character.Date(call.log$DATE)
call.log$AGENT.NAME <- as.character(call.log$AGENT.NAME)
call.log$TIME <- as.character(call.log$TIME)
call.log$DAY.OF.WEEK <- as.character(call.log$DAY.OF.WEEK)

## convert all time variables into minutes
#talk time
call.log$TALK.TIME <- as.character(call.log$TALK.TIME)      
call.log$TALK.TIME <- sapply(strsplit(call.log$TALK.TIME, ":"), function(x) {
  x <- as.numeric(x)
  x[1]*60 + x[2] + x[3]/60
})
call.log <- call.log %>% 
  mutate_at(vars(TALK.TIME), list(~ round(., 2)))

#call time
call.log$CALL.TIME <- as.character(call.log$CALL.TIME)      
call.log$CALL.TIME <- sapply(strsplit(call.log$CALL.TIME, ":"), function(x) {
  x <- as.numeric(x)
  x[1]*60 + x[2] + x[3]/60
})
call.log <- call.log %>% 
  mutate_at(vars(CALL.TIME), list(~ round(., 2)))

#queue wait time
call.log$QUEUE.WAIT.TIME <- as.character(call.log$QUEUE.WAIT.TIME)      
call.log$QUEUE.WAIT.TIME <- sapply(strsplit(call.log$QUEUE.WAIT.TIME, ":"), function(x) {
  x <- as.numeric(x)
  x[1]*60 + x[2] + x[3]/60
})
call.log <- call.log %>% 
  mutate_at(vars(QUEUE.WAIT.TIME), list(~ round(., 2)))

#queue callback wait time
call.log$QUEUE.CALLBACK.WAIT.TIME <- as.character(call.log$QUEUE.CALLBACK.WAIT.TIME)      
call.log$QUEUE.CALLBACK.WAIT.TIME <- sapply(strsplit(call.log$QUEUE.CALLBACK.WAIT.TIME, ":"), function(x) {
  x <- as.numeric(x)
  x[1]*60 + x[2] + x[3]/60
})
call.log <- call.log %>% 
  mutate_at(vars(QUEUE.CALLBACK.WAIT.TIME), list(~ round(., 2)))

#### data cleaning ####
## Campaign: LAL
call.log2 <- subset(call.log, call.log$CAMPAIGN == 'LAL')

## Mon-Fri, 9am-6:30pm
call.log3 <- call.log2 %>%
  filter(DAY.OF.WEEK!="Saturday" & DAY.OF.WEEK!="Sunday")
call.log3 <- call.log3 %>%
  filter(TIME <= "18:30:00" & TIME >= "09:00:00")

call.log3$DAY.OF.WEEK <- factor(call.log3$DAY.OF.WEEK,levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday"))

## Remove 3rd party conference, 3rd party transfer in call.type
call.log4 <- subset(call.log3, call.log3$CALL.TYPE!="3rd party conference" & call.log3$CALL.TYPE!="3rd party transfer")

## Remove Duplicated Callback Request
call.log5 <- subset(call.log4, call.log4$DISPOSITION != "Duplicated Callback Request")
  
length(unique(call.log5$DATE))  

#### feature engineering ####
# New Variable: if talk time is 0 or not 
call.log5$IF.TALK <- ifelse(call.log5$TALK.TIME==0 , 0, 1)

# New variable: IF.TALK (at least 10 mins)
call.log5$IF.TALK10 <- ifelse(call.log5$TALK.TIME >= 10, 1,0)

# New variable:IF.ANSWERED
call.log5$IF.ANSWERED <- ifelse(call.log5$DISPOSITION %in% c("Call Not Answered","Queue Callback Timeout","Timeout","Do Not Call","Abandon","Caller Disconnected"),
                                0,1)
# New variable: IF.COMPLETE (or talk time is greater than 10)
call.log5$IF.COMPLETE <- ifelse(call.log5$DISPOSITION !="Call Complete"  & call.log5$IF.TALK10==0,0,1)

########### Question 1 ###########

# subset data if call.type is callback
call.log.callback <- subset(call.log5, call.log5$CALL.TYPE == "Queue Callback") 

# exclude receptionists
call.log.callback2 <- call.log.callback %>%
  filter(AGENT.NAME!="" & AGENT.NAME!="Christina Talton" & AGENT.NAME!="Anthony Holley" & AGENT.NAME!="Jessica Castro")

# percent of callbacks result in a conversation with an attorney
table(call.log.callback2$IF.TALK10)
percent.callbacks <- sum(table(call.log.callback2$IF.TALK10)[2])/nrow(call.log.callback2)
percent.callbacks    

summary(call.log.callback2$TALK.TIME)

######### Question 2 ###########

# subset data if call.type is inbound
call.log.inbound <- subset(call.log5, call.log5$CALL.TYPE == "Inbound")  # 14203

table(call.log.inbound$IF.ANSWERED)

table.q2 <- call.log.inbound %>%
  group_by(DATE, IF.ANSWERED) %>% summarise(Freq=n()) %>% mutate(PERCENT = 100*Freq/sum(Freq))

# NUMBER OF CALLS ARE UNANSWERED ON AVERAGE PER DAY
mean.unanswered <- mean(subset(table.q2, IF.ANSWERED== 0)$Freq)

mean.unanswered # 226 unanswered calls on average per day

# PERCENTAGE OF CALLS ARE UNANSWERED ON AVERAGE PER DAY
perc.unanswer <- mean(subset(table.q2, IF.ANSWERED== 0)$PERCENT)

perc.unanswer 

########## Question 3 ############
# no matter if it is a callback or taken by someone directly from the queue.

table(call.log5$IF.COMPLETE)
 
# NUMBER OF CALLS ON AVERAGE ARE COMPLETED PER DAY
table.q3 <- call.log5 %>% 
  group_by(DATE, IF.COMPLETE) %>% summarise(Freq=n()) %>% mutate(PERCENT = 100*Freq/sum(Freq))

mean.complete <- mean(subset(table.q3, IF.COMPLETE==1)$Freq)

mean.complete #157 calls were completed on average per day

# PERCENTAGE OF CALLS ON AVERAGE ARE COMPLETED PER DAY
perc.complete <- mean(subset(table.q3, IF.COMPLETE==1)$PERCENT)

perc.complete 


############ Question 4 (a) #############
# choose not to callback 
call.log.hold <- call.log.inbound %>%
  filter(QUEUE.CALLBACK.REGISTERED ==0)

# talked to the attorney, exclude receptionists
call.log.hold2 <- call.log.hold %>%
  filter(AGENT.NAME!="" & AGENT.NAME!="Christina Talton" & AGENT.NAME!="Anthony Holley" & AGENT.NAME!="Jessica Castro")


summary(call.log.hold2$QUEUE.WAIT.TIME)  


######### Question 4 (b) #########
# choose callback                       
call.log.cb <- call.log.inbound %>%
  filter(QUEUE.CALLBACK.REGISTERED == 1 )

# total callback number is 4612, which is queue callback in call.type

# 2173 callback numbers has talk time with attorney
call.log.callback3 <- call.log.callback2 %>%
  filter(IF.TALK == 1)

# check those qualified 2173 in call.log.cb to see the queue callback wait time
call.log.cb2 <- subset(call.log.cb, call.log.cb$CALL.ID %in% call.log.callback3$CALL.ID)

summary(call.log.cb2$QUEUE.CALLBACK.WAIT.TIME) 

