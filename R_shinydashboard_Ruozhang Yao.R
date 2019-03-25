library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shiny)
library(scales)
#load the data 

load("members.rda")
load("pos_renew.rda")

#build the dashboard

members$RENEW_STATUS = as.factor(members$RENEW_STATUS)
levels(members$RENEW_STATUS) = c("Non-Renew", "Renew")
levels(members$autorenew_ind) = c("No", "Yes")


#data modification
member1 = members%>%
  group_by(ethnic_desc, RENEW_STATUS)%>%
  summarise(count=n_distinct(MEMBERSHIP_ID))%>%
  filter(count>= 2000)
member1_1 = members%>%
  group_by(ethnic_desc)%>%
  summarise(sum = n_distinct(MEMBERSHIP_ID))
member1_2=left_join(member1, member11, by ='ethnic_desc')%>%
  mutate(percent = paste0(round(count/sum* 100,1), '%'))

member2 = members%>%
  group_by(autorenew_ind, RENEW_STATUS)%>%
  summarise(count=n_distinct(MEMBERSHIP_ID))

member2_1 = members%>%
  group_by(autorenew_ind)%>%
  summarise(sum = n_distinct(MEMBERSHIP_ID))

member2_2=left_join(member2, member21, by ='autorenew_ind')%>%
  mutate(percent = paste0(round(count/sum* 100,1), '%'))


  
#build the dashboard 
  sidebar <- dashboardSidebar(width = 180,
  sidebarMenu(
    menuItem("Demographic", tabName = "demo", icon = icon("dashboard")),
    menuItem("Behavior",tabName = "bh", icon = icon("th")),
    menuItem("Visit Us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.samsclub.com/sams/")
   
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "demo",
            fluidRow(height = "100px",
              valueBoxOutput("value1" ,width = 6)
              ,valueBoxOutput("value2", width = 6)
            ),
            fluidRow(
              box(
                title = "Demographic Factors: Renew VS Non-renew"
                ,selectInput(inputId="selection",label = "Choose a demographic variable",
                              choices = c("Ethnicity",
                                          "Member's Age",
                                          'Marital Status',
                                          'Household Income Level',
                                          'Household Size'))),
              box(
                title = "Membership Type: Renew VS Non-renew"
                ,selectInput(inputId="selection2",label = "Choose a membership type",
                            choices = c('Auto Renew or Not',
                                        'Business Or Savings', 
                                        'Base or Plus'))
              )
            ),
            fluidRow(
              box(
                title = "Demographic"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("demographic")
              ),
              box(
                title = "Membership Type"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("membership")
              )
            )
            
    ),

    tabItem(tabName = "bh",
            fluidRow(height = "100px",
                     valueBoxOutput("value3" ,width = 6)
                     ,valueBoxOutput("value4", width = 6)
            ),
            
            fluidRow(
              box(
                title = "Renewed Customer Shopping Behavior",
                selectInput(inputId="selection3",label = "Choose a variable",
                            choices = c('Visit Date',
                                        'Visit Time'))),
              box(
                title = "Renewed Customer Shopping Categories",
                selectInput(inputId="selection4",label = "Choose a variable",
                            choices = c('Purchased Category',
                                        'Purchased Sub-Category')))
            ),
            
            fluidRow(
              box(
                title = "Shopping Behavior"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("plot1")
              ),
              box(
                title = "Shopping Categories"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("plot2")
              )
            )
              
              
    )
  )
)

# Put them together into a dashboardPage
ui = dashboardPage(skin = "blue",
  dashboardHeader(title = "Sam's Club Dashboard",  titleWidth = 180),
  sidebar,
  body
)





server <- function(input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)
 
  # output part   
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC("56%", format="d")
      ,paste('Renewal Rate:',' All Members')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC("64%", format="d")
      ,paste('Renewal Rate:',' Members with a Purchase')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC("56%", format="d")
      ,paste('Renewal Rate:',' All Members')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$value4 <- renderValueBox({ 
    valueBox(
      formatC("64%", format="d")
      ,paste('Renewal Rate:',' Members with a Purchase')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  
  # creating the plot
  #Olivia's part1
  output$demographic = renderPlot(
    if(input$selection=='Ethnicity'){
      ggplot(member1_2, aes(x=reorder(ethnic_desc,count), y=count/1000,fill= factor(RENEW_STATUS)))+
        geom_col(position="dodge")+
        geom_text(aes(label = percent), position = position_dodge(0.9), hjust =-0.1)+
        coord_flip()+
        ggtitle("Ethnicity")+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        labs(fill='')+
        scale_fill_manual(values = c("steelblue3","palegreen3"))+
        theme(legend.position=c(0.8, 0.2), legend.text = element_text(size=12))+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5),
              axis.text.y = element_text(colour="black",size=12,angle=0,hjust=1,vjust=0),  
              axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0),
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
    }
    else if(input$selection=="Member's Age"){
      member1_3 = members%>%
        mutate(age_group = ifelse(Age<20,"0-20",
                                  ifelse(Age<30,"20-30",
                                         ifelse(Age<40, "30-40",
                                                ifelse(Age<50,"40-50",
                                                       ifelse(Age<60,"50-60","60+"))))))%>%
        group_by(RENEW_STATUS,age_group)%>%
        summarise(count=n_distinct(MEMBERSHIP_ID))
      
      member1_4 = member1_3%>%
        group_by(age_group)%>%
        summarise(sum = sum(count))
      
      member1_5 = left_join(member1_3, member1_4, by ='age_group')%>%
        mutate(percent = paste0(round(count/sum* 100,1), '%'))
      ggplot(member1_5, aes(x=age_group,y=count/1000, fill = factor(RENEW_STATUS)))+
        geom_col(position="dodge")+
        geom_text(aes(label = percent), position = position_dodge(0.9), vjust =-0.5)+
        ggtitle("Member's Age")+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        labs(fill='')+
        scale_fill_manual(values = c("steelblue3","palegreen3"))+
        theme(legend.position=c(0.8, 0.2), legend.text = element_text(size=12))+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5),
              axis.text.x = element_text(colour="black",size=12,angle=0,hjust=1,vjust=0),  
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=0),
              axis.title.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
    }
    else if(input$selection=="Marital Status"){
      member1_6 = members%>%
        group_by(RENEW_STATUS,marital_status_desc)%>%
        summarise(count=n_distinct(MEMBERSHIP_ID))
      
      member1_7 = member1_6%>%
        group_by(marital_status_desc)%>%
        summarise(sum = sum(count))
      
      member1_8 = left_join(member1_6, member1_7, by ='marital_status_desc')%>%
        mutate(percent = paste0(round(count/sum* 100,1), '%'))
      
      ggplot(member1_8, aes(x=marital_status_desc,y=count/1000, fill = factor(RENEW_STATUS)))+
        geom_col(position="dodge")+
        geom_text(aes(label = percent), position = position_dodge(0.9), vjust =-0.5)+
        ggtitle(input$selection)+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        labs(fill='')+
        scale_fill_manual(values = c("steelblue3","palegreen3"))+
        theme(legend.position=c(0.8, 0.7), legend.text = element_text(size=12))+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5),
              axis.text.x = element_text(colour="black",size=12,angle=0,hjust=1,vjust=0),  
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=0),
              axis.title.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
    }
    else if(input$selection=="Household Income Level"){
      members$income_desc = factor(members$income_desc, levels = c("< $15K", "$15K - $24K","$25K - $34K","$35K - $49K","$50K - $74K",
                                                                   "$75K - $99K","$100K-$124K","$125K-$149K","$150K +"))
      
      member1_9 = members%>%
        group_by(RENEW_STATUS,income_desc)%>%
        summarise(count=n_distinct(MEMBERSHIP_ID))
      
      member1_10 = member1_9%>%
        group_by(income_desc)%>%
        summarise(sum = sum(count))
      
      member1_11 = left_join(member1_9, member1_10, by ='income_desc')%>%
        mutate(percent = paste0(round(count/sum* 100,1), '%'))
      
      ggplot(member1_11, aes(x=income_desc,y=count/1000, fill = factor(RENEW_STATUS)))+
        geom_col(position="dodge")+
        geom_text(aes(label = percent), position = position_dodge(0.9), vjust =-0.5)+
        ggtitle(input$selection)+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        labs(fill='')+
        scale_fill_manual(values = c("steelblue3","palegreen3"))+
        theme(legend.position=c(0.8, 0.8), legend.text = element_text(size=12))+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5),
              axis.text.x = element_text(colour="black",size=12,angle=90,hjust=1,vjust=0),  
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=0),
              axis.title.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
    }
    else if(input$selection=="Household Size"){
      member1_12 = members%>%
        group_by(RENEW_STATUS,hh_size_desc)%>%
        summarise(count=n_distinct(MEMBERSHIP_ID))
      
      member1_13 = member1_12%>%
        group_by(hh_size_desc)%>%
        summarise(sum = sum(count))
      
      member1_14 = left_join(member1_12, member1_13, by ='hh_size_desc')%>%
        mutate(percent = paste0(round(count/sum* 100,1), '%'))
    
      ggplot(member1_14, aes(x=hh_size_desc,y=count/1000, fill = factor(RENEW_STATUS)))+
        geom_col(position="dodge")+
        geom_text(aes(label = percent), position = position_dodge(0.9), vjust =-0.5)+
        ggtitle(input$selection)+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        labs(fill='')+
        scale_fill_manual(values = c("steelblue3","palegreen3"))+
        theme(legend.position=c(0.8, 0.7), legend.text = element_text(size=12))+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5),
              axis.text.x = element_text(colour="black",size=12,angle=90,hjust=1,vjust=0),  
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=0),
              axis.title.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
      
    })
  
  
  output$membership = renderPlot(
    if(input$selection2=='Auto Renew or Not'){
      ggplot(member2_2, aes(x= autorenew_ind, y=count/1000,fill= factor(RENEW_STATUS)))+
        geom_col(position="dodge")+
        geom_text(aes(label = percent), position = position_dodge(0.9), hjust =-0.1)+
        coord_flip()+
        ggtitle("Auto-Renew")+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        labs(fill='')+
        scale_fill_manual(values = c("steelblue3","palegreen3"))+
        theme(legend.position=c(0.8, 0.7), legend.text = element_text(size=12))+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0),
              axis.text.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5),
              axis.text.y = element_text(colour="black",size=12,angle=0,hjust=1,vjust=0),  
              axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0),
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
    }
    else if(input$selection2=='Business Or Savings'){
      member2_3 = members%>%
        group_by(RENEW_STATUS,MEMBERSHIP_TYPE_DESC)%>%
        summarise(count=n_distinct(MEMBERSHIP_ID))
      
      member2_4 = member2_3%>%
        group_by(MEMBERSHIP_TYPE_DESC)%>%
        summarise(sum = sum(count))
      
      member2_5 = left_join(member2_3, member2_4, by ='MEMBERSHIP_TYPE_DESC')%>%
        mutate(percent = paste0(round(count/sum* 100,1), '%'))
      
      ggplot(member2_5, aes(x=MEMBERSHIP_TYPE_DESC,y=count/1000, fill = factor(RENEW_STATUS)))+
        geom_col(position="dodge")+
        geom_text(aes(label = percent), position = position_dodge(0.9), vjust =-0.5)+
        ggtitle(input$selection2)+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        labs(fill='')+
        scale_fill_manual(values = c("steelblue3","palegreen3"))+
        theme(legend.position=c(0.8, 0.2), legend.text = element_text(size=12))+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5),
              axis.text.x = element_text(colour="black",size=12,angle=0,hjust=1,vjust=0),  
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=0),
              axis.title.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
      
    }
    else if(input$selection2=='Base or Plus'){
      member2_6 = members%>%
        group_by(RENEW_STATUS,PLUS_STATUS_BEFORE_REN)%>%
        summarise(count=n_distinct(MEMBERSHIP_ID))
      
      member2_7 = member2_6%>%
        group_by(PLUS_STATUS_BEFORE_REN)%>%
        summarise(sum = sum(count))
      
      member2_8 = left_join(member2_6, member2_7, by ='PLUS_STATUS_BEFORE_REN')%>%
        mutate(percent = paste0(round(count/sum* 100,1), '%'))
      
      ggplot(member2_8, aes(x=PLUS_STATUS_BEFORE_REN,y=count/1000, fill = factor(RENEW_STATUS)))+
        geom_col(position="dodge")+
        geom_text(aes(label = percent), position = position_dodge(0.9), vjust =-0.5)+
        ggtitle(input$selection2)+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        labs(fill='')+
        scale_fill_manual(values = c("steelblue3","palegreen3"))+
        theme(legend.position=c(0.8, 0.2), legend.text = element_text(size=12))+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5),
              axis.text.x = element_text(colour="black",size=12,angle=0,hjust=1,vjust=0),  
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=0),
              axis.title.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
    })
  

  
  

  output$plot2 = renderPlot(
    if(input$selection4 =='Purchased Category'){
      pos_renew%>%
        group_by(`Purchased Category`)%>%
        summarise(count =n())%>%
        arrange(desc(count))%>%
        top_n(10)%>%
        ggplot(aes(x=reorder(`Purchased Category`,count),y=count/1000))+
        geom_col(fill = "palegreen3")+
        coord_flip()+
        geom_text(aes(label = count/1000), hjust =-0.1)+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        ggtitle('Top 10 Purchased Categories')+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5),
              axis.text.y = element_text(colour="black",size=10,angle=0,hjust=1,vjust=0),  
              axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0),
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
        
    }
    else if(input$selection4 =='Purchased Sub-Category'){
      pos_renew%>%
        group_by(`Purchased Sub-Category`)%>%
        summarise(count=n())%>%
        arrange(desc(count))%>%
        top_n(10)%>%
        ggplot(aes(x=reorder(`Purchased Sub-Category`,count),y=count/1000))+
        geom_col(fill = "palegreen3")+
        coord_flip()+
        geom_text(aes(label = count/1000), hjust =-0.1)+
        ylab('Count of Members(in thousands)')+
        xlab('')+
        ggtitle('Top 10 Purchased Sub-Categories')+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5),
              axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0),  
              axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0),
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))
    })
  
  output$plot1 = renderPlot(
    if(input$selection3 =='Visit Time'){
      ggplot(pos_renew,aes(x=`Visit Time`))+
        geom_histogram(fill = "steelblue3") +
        geom_line(stat = "density")+
        ylab('')+
        xlab('Visit Hours')+
        ggtitle('Distribution Among Visit Hours')+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4))
    }
    else if(input$selection3 =='Visit Date'){
      pos_renew%>%
        group_by(`Visit Date`)%>%
        summarise(count=n())%>%
        arrange(-count)%>%
        ggplot(aes(x=`Visit Date`,y=count/1000))+
        geom_col(fill = "steelblue3")+
        geom_text(aes(label = round(count/1000,0)), vjust =-0.8)+
        xlab('')+
        ylab('Count of Members(in thousands)')+
        ggtitle('Distribution of Visit Dates')+
        theme(plot.title = element_text(colour = "black", size=18, hjust = 0.4),
              axis.text.y = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5),
              axis.text.x = element_text(colour="black",size=10,angle=0,hjust=1,vjust=0),  
              axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=0),
              axis.title.x = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5))

    })
}

shinyApp(ui, server)



