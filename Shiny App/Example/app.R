#Sharif Amlani
#R 4.0.2
#Summer 2020

######################## Code Summary ##################
#https://www.youtube.com/watch?v=Gyrfsrd4zK0


#********************* V1 *************************
#Generated from Redistinring and Ideology - v7 - R Code
#Only inlcues AMount
#Scales Donor Disconitnity 
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


######################### Library #####################
library(ggplot2)
library(lmtest)
library(effects)
library(sandwich)
library(margins)
library(shiny)
library(shinysky)

######################## Upload Data ##################
#Set Working Directory
setwd("C:/Users/Shari/OneDrive/University of California, Davis/Fourth Year/Dissertation Prospectus/Out of State Donors/Shiny App/Out of District Member App")

#Upload Data
load(file = "Out of District Donor Contibutions -- Shiny App -- w1.rda"); Bonica.1 <- Bonica.Pure



# #Set Working Directory
# setwd("C:/Users/Shari/OneDrive/University of California, Davis/Fourth Year/Dissertation Prospectus/Out of State Donors/Shiny App/Out of District Member App")
# 
# #Upload Data
# load(file = "Candidate Census - Long - With Controls - Master -- w14.rda"); Bonica.1 <- Bonica.Pure
# 

# 
# #Set Working Directory
# setwd("C:/Users/Shari/OneDrive/University of California, Davis/Fourth Year/Dissertation Prospectus/Out of State Donors/Shiny App/Out of District Member App")
# 
# #Upload Data
# load(file = "Master - Candidate Census - Candidate Level Data - Long - All and Type -- w10.rda"); Bonica.1 <- Bonica.Pure
# 


######################## Data Management ###################
#Fix Name
Bonica.1$flname <- paste(Bonica.1$fname, Bonica.1$lname, sep = " ")



ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Out of District Donors"),
    
    sidebarPanel(
      textInput("Name", "Please Enter First and Last Name of a U.S. House Member", "Nancy Pelosi"),
      
      # textInput.typeahead(id="Name",
      #                     placeholder="Type your name please",
      #                     local=data.frame(name=unique(Bonica.1$flname)),
      #                     valueKey = "name",
      #                     tokens=c(1:length(unique(Bonica.1$flname))),
      #                     template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
      # ),
      # 
      selectInput("Donor_Type", "Please Select Donors' Type",
                  choices = c("All", "Corporation"="corp", "PAC" = "committee","Individual" = "individual")
      )
      
      
    ),
    
    
    mainPanel(plotOutput("MyPlot"))
  )
)


######################## Server ####################
server <-  function(input, output, session){
  
  output$MyPlot <- renderPlot({
    input_name <- input$Name
    input_type <- input$Donor_Type
    
    
    
    Bonica.2 <- unique(subset(Bonica.1, cycle < 2018 & flname == tolower(input_name) & Overall_Type == input_type & Donor_Location == "Out of District"))
    
    
    library(ggplot2)
    ggplot(Bonica.2, aes(x = as.factor(cycle), y = Amount_Percentage*100, fill = as.factor(cycle))) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label=round(Amount_Percentage*100, 1)), position=position_dodge(width=0.9), vjust=-0.25) +
      labs(title = "Out-of-District Contributions",
           subtitle = simpleCap(unique(Bonica.2$flname)),
           y = "Percent of Total Contributions") +
      theme_minimal() +
      theme(axis.title.x=element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none")
    
    
  })
  
}




shinyApp(ui, server)
