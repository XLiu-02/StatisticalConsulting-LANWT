library(shiny)
library(tidyverse)
library(shinythemes)

######UI#######
###############
ui<- fluidPage(theme = shinytheme("sandstone"),
               titlePanel(title = h2("Information Inquiry System--Legal Aid of NorthWest Texas",align="center"),
                          windowTitle = "Inquiry System"),
               sidebarLayout(
                   sidebarPanel(
                       tags$head(tags$style("#tot{color: black;
                             font-size: 20px;
                         }"
                       )),
                       tags$head(tags$style("#q1{color: black;
                             font-size: 20px;
                         }"
                       )),
                       tags$head(tags$style("#q2_1{color: black;
                             font-size: 20px;
                         }"
                       )),
                       tags$head(tags$style("#q2_2{color: black;
                             font-size: 20px;
                         }"
                       )),
                       tags$head(tags$style("#q3_1{color: black;
                             font-size: 20px;
                         }"
                       )),
                       tags$head(tags$style("#q3_2{color: black;
                             font-size: 20px;
                         }"
                       )),
                       tags$head(tags$style("#q4_1{color: black;
                             font-size: 20px;
                         }"
                       )),
                       tags$head(tags$style("#q4_2{color: black;
                             font-size: 20px;
                         }"
                       )),
                    
                       fileInput("file", label = "Upload the file"),
                       dateInput("date",
                                 label = "Date Input",
                                 value = Sys.Date(),
                                 width = "1000px",
                                 format = "yyyy/mm/dd"),
                       width=3
                   ),
                   
                   mainPanel(
                       tabsetPanel(
                           tabPanel("Table", DT::dataTableOutput("input_file")),
                           tabPanel("Summary", 
                                     textOutput("tot"),
                                     br(),
                                     textOutput("q1"),
                                     br(),
                                     textOutput("q2_1"),
                                     br(),
                                     textOutput("q2_2"),
                                     br(),
                                     textOutput("q3_1"),
                                     br(),
                                     textOutput("q3_2"),
                                     br(),
                                     textOutput("q4_1"),
                                     br(),
                                     textOutput("q4_2")
                           )
                       )
                   )
               ))

#####Server#####
################
server <- function(input, output, session){
    
    
    ####Read data####
    
    newdata <- reactive({
        file <- input$file
        if(is.null(file)) return(NULL)
        data <-  read.csv(file$datapath, header = TRUE)
        ##convert factors into char
        data$DATE <- as.character.Date(data$DATE)
        data$AGENT.NAME <- as.character(data$AGENT.NAME)
        data$TIME <- as.character(data$TIME)
        data$DAY.OF.WEEK <- as.character(data$DAY.OF.WEEK)
        ###convert all time variables into mintues
        #talk time
        data$TALK.TIME <- as.character(data$TALK.TIME)      
        data$TALK.TIME <- sapply(strsplit(data$TALK.TIME, ":"), function(x) {
            x <- as.numeric(x)
            x[1]*60 + x[2] + x[3]/60
        })
        data <- data %>% 
            mutate_at(vars(TALK.TIME), list(~ round(., 2)))
        #call time
        data$CALL.TIME <- as.character(data$CALL.TIME)      
        data$CALL.TIME <- sapply(strsplit(data$CALL.TIME, ":"), function(x) {
            x <- as.numeric(x)
            x[1]*60 + x[2] + x[3]/60
        })
        data <- data %>% 
            mutate_at(vars(CALL.TIME), list(~ round(., 2)))
        #queue wait time
        data$QUEUE.WAIT.TIME <- as.character(data$QUEUE.WAIT.TIME)      
        data$QUEUE.WAIT.TIME <- sapply(strsplit(data$QUEUE.WAIT.TIME, ":"), function(x) {
            x <- as.numeric(x)
            x[1]*60 + x[2] + x[3]/60
        })
        data <- data %>% 
            mutate_at(vars(QUEUE.WAIT.TIME), list(~ round(., 2)))
        #queue callback wait time
        data$QUEUE.CALLBACK.WAIT.TIME <- as.character(data$QUEUE.CALLBACK.WAIT.TIME)      
        data$QUEUE.CALLBACK.WAIT.TIME <- sapply(strsplit(data$QUEUE.CALLBACK.WAIT.TIME, ":"), function(x) {
            x <- as.numeric(x)
            x[1]*60 + x[2] + x[3]/60
        })
        data <- data %>% 
            mutate_at(vars(QUEUE.CALLBACK.WAIT.TIME), list(~ round(., 2)))
        #subset LAL campaign
        data <- subset(data, data$CAMPAIGN == 'LAL')
        # Mon-Fri, 9am-6:30pm
        data <- data %>%
            filter(DAY.OF.WEEK!="Saturday" & DAY.OF.WEEK!="Sunday")
        data <- data %>%
            filter(TIME <= "18:30:00" & TIME >= "09:00:00")
        data$DAY.OF.WEEK <- factor(data$DAY.OF.WEEK,levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday"))
        # Remove 3rd party conference, 3rd party transfer in call.type
        data <- subset(data, data$CALL.TYPE!="3rd party conference" & data$CALL.TYPE!="3rd party transfer")
        # Remove Duplicated Callback Request
        data <- subset(data, data$DISPOSITION != "Duplicated Callback Request")
        # New Variable: IF.TALK time is 0 or not 
        data$IF.TALK <- ifelse(data$TALK.TIME==0 , 0, 1)
        # New variable: IF.TALK10 (at least 10 mins)
        data$IF.TALK10 <- ifelse(data$TALK.TIME >= 10, 1,0)
        # new variable:IF.ANSWERED
        data$IF.ANSWERED <- ifelse(data$DISPOSITION %in% c("Call Not Answered","Queue Callback Timeout","Timeout","Do Not Call","Abandon","Caller Disconnected"),
                                   0,1)
        data.inbound <- subset(data, data$CALL.TYPE == "Inbound") 
        #New variable: IF.COMPLETE (or talk time is greater than 10)
        data$IF.COMPLETE <- ifelse(data$DISPOSITION !="Call Complete"  & data$IF.TALK10==0,0,1)
        data
    })
    
    ####Tidy data Table####
    output$input_file <- DT::renderDataTable({
        DT::datatable(newdata())
    })
    
    ####Questions####  
    output$tot <- renderText({
        table.tot = newdata() %>% 
            group_by(DATE) %>% summarise(Freq=n()) %>% mutate(PERCENT = 100*Freq/sum(Freq))
        tot = table.tot %>% filter(DATE==input$date)
        paste("Total Calls Today:", tot$Freq)
    })
    #Q1. Average percentage of callbacks per day
    output$q1 <- renderText({
        # exclude receptionists
        callback = newdata() %>% filter(CALL.TYPE == "Queue Callback") %>% 
            filter(AGENT.NAME!="" & AGENT.NAME!="Christina Talton" & AGENT.NAME!="Anthony Holley" & AGENT.NAME!="Jessica Castro")
        table.q1 <- callback %>% dplyr::group_by(DATE, IF.TALK10) %>% dplyr::summarise(Freq=n()) %>% dplyr::mutate(PERCENT = 100*Freq/sum(Freq))
        q1 = table.q1 %>% filter(IF.TALK10==1) %>% filter(DATE==input$date)

        paste("Percentage of Callbacks that have at least 10 mins conversation with attorney:", round(q1$PERCENT, 2), "%")
    })
    
    #Q2.CALLS ARE UNANSWERED ON AVERAGE PER DAY
    output$q2_1 <- renderText({  
        table.q2 = newdata() %>%
            group_by(DATE, IF.ANSWERED) %>% summarise(Freq=n()) %>% mutate(PERCENT = 100*Freq/sum(Freq))
        q2 = table.q2 %>% filter(IF.ANSWERED==0) %>% filter(DATE==input$date)
        paste("Number of Calls are unanswered:", round(q2$Freq, 2))
    }) 
    
    output$q2_2 <- renderText({
        table.q2 = newdata() %>%
            group_by(DATE, IF.ANSWERED) %>% summarise(Freq=n()) %>% mutate(PERCENT = 100*Freq/sum(Freq))
        q2 = table.q2 %>% filter(IF.ANSWERED==0) %>% filter(DATE==input$date)
        paste("Percentage of Calls are unanswered:", round(q2$PERCENT, 2), "%")
    })  
    
    #Q3.CALLS ON AVERAGE ARE COMPLETED PER DAY
    output$q3_1 <- renderText({  
        table.q3 = newdata() %>% 
            group_by(DATE, IF.COMPLETE) %>% summarise(Freq=n()) %>% mutate(PERCENT = 100*Freq/sum(Freq))
        q3 = table.q3 %>% filter(IF.COMPLETE==1) %>% filter(DATE==input$date)
        paste("Number of Calls are completed:", round(q3$Freq, 2))
    })
    
    output$q3_2 <- renderText({ 
        table.q3 = newdata() %>% 
            group_by(DATE, IF.COMPLETE) %>% summarise(Freq=n()) %>% mutate(PERCENT = 100*Freq/sum(Freq))
        q3 = table.q3 %>% filter(IF.COMPLETE==1) %>% filter(DATE==input$date)
        paste("Percentage of Calls are completed:", round(q3$PERCENT, 2), "%")
    })
    
    #Q4. HOW LONG IS THE WAIT TO SPEAK TO AN ATTORNEY PER DAY
    output$q4_1 <- renderText({   
        q4_1 = newdata() %>% filter(CALL.TYPE == "Inbound")
        q4_1 = q4_1 %>% filter(QUEUE.CALLBACK.REGISTERED ==0)
        q4_1 = q4_1 %>% filter(AGENT.NAME!="" & AGENT.NAME!="Christina Talton" & AGENT.NAME!="Anthony Holley" & AGENT.NAME!="Jessica Castro")
        q4_1 = q4_1 %>% filter(IF.TALK ==1) 
        q4_1 = mean(q4_1$QUEUE.WAIT.TIME)
        paste("Average Wait Time for those hold on the hotline:", round(q4_1, 2), "Minutes")
    })
    
    output$q4_2 <- renderText({  
        q4_2 = newdata() %>% filter(CALL.TYPE == "Inbound")
        q4_2 = q4_2 %>% filter(QUEUE.CALLBACK.REGISTERED ==1)
        callback = newdata() %>% filter(CALL.TYPE == "Queue Callback") 
        callback = callback %>% filter(AGENT.NAME!="" & AGENT.NAME!="Christina Talton" & AGENT.NAME!="Anthony Holley" & AGENT.NAME!="Jessica Castro")
        callback = callback %>% filter(IF.TALK == 1)
        q4_2 = subset(q4_2, q4_2$CALL.ID %in% callback$CALL.ID)
        q4_2 = mean(q4_2$QUEUE.CALLBACK.WAIT.TIME)
        paste("Average Wait Time for those choose a callback:", round(q4_2/60, 2), "Hours")
    })
}

#####Result######
#################
shinyApp(ui = ui, server = server)
