library(shiny)
library(tidyverse)
library(plotly)

#load data set ##################### original data #############################
my_data <- read_csv("~/OPER682/Deliverables/Selection_DB_OPER682.csv", 
                    col_types = cols(ADULTERY_AFFAIR = col_factor(levels = c("0","1")), 
                                     ALCOHOL_PX = col_factor(levels = c("0", "1")), 
                                     ARRESTED = col_factor(levels = c("0", "1")), 
                                     CODE = col_skip(), 
                                     Column1 = col_skip(), 
                                     DOB = col_skip(),
                                     #DOB = col_date(format = "%m/%d/%Y"), 
                                     DRUGS = col_factor(levels = c("0", "1", "2")), 
                                     Date = col_skip(),
                                     #Date = col_date(format = "%m/%d/%Y"), 
                                     FAMADVCY = col_factor(levels = c("0", "1")), 
                                     FINANCES = col_factor(levels = c("0", "1")), 
                                     MARRIED = col_factor(levels = c("0","1","2")), 
                                     MH_CONTACT = col_factor(levels = c("0","1")), 
                                     PARENT_STATUS = col_factor(levels=c("1","2","3")), 
                                     PORNPOST = col_factor(levels = c("0","1")), 
                                     PSYCH_DOC = col_skip(), 
                                     PSYCH_REC = col_skip(),
                                     SCHOOLPX = col_factor(levels = c("0","1")), 
                                     UCMJVIOL = col_factor(levels = c("0","1","2"))),
                    na = "NA")

my_data <- my_data %>% 
  separate(RANK, c("Grade", "Rank"), sep=" ")

my_data$FINAL_SELECTION <- as.factor(my_data$FINAL_SELECTION)

my_data <- my_data %>% 
  mutate(Grade2 = Grade)

my_data$Grade2 <- my_data$Grade2 %>% 
  recode(E4 = "ENLISTED",
         E5 = "ENLISTED",
         E6 = "ENLISTED",
         E7 = "ENLISTED",
         E8 = "ENLISTED",
         O1 = "OFFICER",
         O2 = "OFFICER",
         O3 = "OFFICER", O4 = "OFFICER",
         O5 = "OFFICER")

my_plot <- my_data[,-c(3:4)] %>% 
  gather(variable, value, -c(Grade2, FINAL_SELECTION)) %>%
  group_by(Grade2, variable, FINAL_SELECTION) %>% 
  summarise(average = mean(as.numeric(value), na.rm=TRUE)) %>% 
  filter(FINAL_SELECTION != 3, Grade2 != "") %>% 
  group_by(Grade2, variable) %>% 
  #SELECT - NON SELECT - positive numbers means that the value is higher for SELECT
  summarise(mu = average[FINAL_SELECTION ==1] - average[FINAL_SELECTION==2]) 

########################### IMPUTED DATA #######################################
## change location of file ##
my_data2 <- read_csv("~/OPER682/Deliverables/my_impute.csv",
                     col_types = cols(ADULTERY_AFFAIR = col_factor(levels = c("0","1")),
                                      ALCOHOL_PX = col_factor(levels = c("0", "1")),
                                      ARRESTED = col_factor(levels = c("0", "1")),
                                      CODE = col_skip(),
                                      Column1 = col_skip(),
                                      DOB = col_skip(),
                                      DRUGS = col_factor(levels = c("0", "1", "2")),
                                      Date = col_skip(),
                                      FAMADVCY = col_factor(levels = c("0", "1")),
                                      FINANCES = col_factor(levels = c("0", "1")),
                                      MARRIED = col_factor(levels = c("0","1","2")),
                                      MH_CONTACT = col_factor(levels = c("0","1")),
                                      PARENT_STATUS = col_factor(levels=c("1","2","3")),
                                      PORNPOST = col_factor(levels = c("0","1")),
                                      PSYCH_DOC = col_skip(),
                                      PSYCH_REC = col_skip(),
                                      SCHOOLPX = col_factor(levels = c("0","1")),
                                      UCMJVIOL = col_factor(levels = c("0","1","2"))),
                     na = "NA") 

my_data2$FINAL_SELECTION <- as.factor(my_data2$FINAL_SELECTION) 

my_data2 <- my_data2 %>%
  separate(RANK, c("Grade", "Rank"), sep=" ")

my_data2 <- my_data2 %>%
  mutate(Grade2 = Grade)

my_data2$Grade2 <- my_data2$Grade2 %>%
  recode(E4 = "ENLISTED",
         E5 = "ENLISTED",
         E6 = "ENLISTED",
         E7 = "ENLISTED",
         E8 = "ENLISTED",
         O1 = "OFFICER",
         O2 = "OFFICER",
         O3 = "OFFICER", O4 = "OFFICER",
         O5 = "OFFICER")

my_plot2 <- my_data2[,-c(3:4)] %>%
  gather(variable, value, -c(Grade2, FINAL_SELECTION)) %>%
  group_by(Grade2, variable, FINAL_SELECTION) %>%
  summarise(average = mean(as.numeric(value), na.rm=TRUE)) %>%
  filter(FINAL_SELECTION != 3, Grade2 != "") %>%
  group_by(Grade2, variable) %>%
  summarise(mu = average[FINAL_SELECTION ==1] - average[FINAL_SELECTION==2])


############################ MAIN MAGIC ########################################
#user interface
ui <- shinyUI(fluidPage(
  #make the title
  titlePanel("Difference Between Group Means by Variable"),
  #how the body is formatted
  sidebarLayout(
    sidebarPanel(
      sliderInput("MeanDiffSlider", #this is called in filter to use slider bar
                  "Mean Difference Filter: ",
                  min=0,
                  max=10,
                  value = 0,
                  step = 1),
      helpText("Variables that fall above the center (0) line correspond to SELECT,
               those under to NON-SELECT")
    ),
    mainPanel(
      plotOutput("MeanDiffPlot"),
      plotOutput("ImputeMeanDiffPlot")
      
    )
    
  )
))
#curly brackets make things reactive within the app
#server to host
server <- shinyServer(function(input, output) { 
  output$MeanDiffPlot <- renderPlot({
    #ggplot object - my_plot
  
      filter(my_plot, abs(mu) >= input$MeanDiffSlider) %>% 
      ggplot() +
      geom_point(aes(x=variable, y=mu,label=variable)) +
      facet_wrap(~Grade2, nrow=3) +
      geom_hline(yintercept=0) +
      theme(axis.text.x = element_text(angle=90, hjust=1)) +
      labs("Difference between group means by variable - Original data")

  })
  
  output$ImputeMeanDiffPlot <- renderPlot({
    #ggplot object - my_plot
    
    filter(my_plot2, abs(mu) >= input$MeanDiffSlider) %>% 
      ggplot() +
      geom_point(aes(x=variable, y=mu,label=variable)) +
      facet_wrap(~Grade2, nrow=3) +
      geom_hline(yintercept=0) +
      theme(axis.text.x = element_text(angle=90, hjust=1))
  })
})

shinyApp(ui = ui, server = server)