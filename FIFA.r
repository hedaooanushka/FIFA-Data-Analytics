library(shiny) #to build interactive web apps straight from R
library(shinydashboard) #used to build dashboards with shiny
library(ggplot2) #used to plot graphs

m = read.csv("Countrywise_data_csv.csv", header=T)
q = read.csv("Playerwise_data2_csv.csv", header=T)
p = read.csv("Playerwise_data_csv.csv", header=T)
r = read.csv("Clubwise_data_csv.csv", header=T)

ui <- shinyUI(
  dashboardPage( title="FIFA", skin="red",
                 dashboardHeader(title = "FIFA Analytics"),
                 dashboardSidebar(
                   sidebarMenu( 
                     menuItem("Yearwise Analysis", tabName = "YearwiseAnalysis"),
                     menuItem("Clubs and Players Analysis", tabName = "dash3"),
                     menuItem("General", tabName = "dash2", badgeColor = "green", badgeLabel = "New")
                   )),
                 dashboardBody(
                   tabItems(
                     tabItem(tabName = "YearwiseAnalysis",
                             h1(class="big-heading", "FIFA & Football League Analytics"),
                             h1(class="big-heading1","An analysis of the most widely viewed sports event in the world: The FIFA World Cup. 
                                Here are the Data-driven Historical Stats from the FIFA World Cup between 1930 and 2014. Considered dataset contains 21 observations as the
                                FIFA World Cup is played after 4 years."),
                             tags$style(HTML(".big-heading1{font-family: 'Open Sans', sans-serif; padding-bottom: 20px; font-size: 25px; letter-spacing: 3;}")),
                             tags$style(HTML(".big-heading{font-family: 'Open Sans', sans-serif; font-size: 40px;}")),
                             fluidRow(
                               column(width=12,
                                      infoBox("Years", "1930-2014", icon=icon("calendar"), color="teal"),
                                      infoBox("Total matches played", 836, icon=icon("handshake-o"), color="teal"),
                                      infoBox("Total Goals Scored", "2379", icon=icon("soccer-ball-o"), color="teal")
                               )
                             ),
                             fluidRow(
                               
                               box(title="FIFA Cup Winning Country", status="primary", solidHeader = T, plotOutput("winner"), h4("The boxplot shows that Urugway 
                               won the very first FIFA Cup in 1930 and Germany won the FIFA Cup in 2014. England, France, Germany and Spain are one 
                               time FIFA Cup Winners.")),
                               box(title="FIFA Cup Runner-Up Country", status="primary", solidHeader = T,plotOutput("runnerup"), h4("This boxplot shows that Argentina
                               was FIFA runner-up in 1930 as well as 2014. Brazil, Hungary, Italy and CZrepublic have a normal distribution curve of their Runner Up Wins."))
                               
                             ),
                             fluidRow(
                               column(width=12,
                                      box(title="FIFA Cup Host Country", status="primary", solidHeader = T, plotOutput("Host")),
                                      box(title="Matches played and Goals scored per Year", status="primary", solidHeader = T, plotOutput("MvsG"))
                                      
                               ),
                             )),
                     tabItem(tabName = "dash2",
                             h1(class="big-heading", "Analysis of General Attributes"),
                             h1(class="big-heading1","The visualisation of these graphs is based on the general attributes of the FIFA players.The dataset considered
                                for this analysis has 18,208 total observations."),
                             tags$style(HTML(".big-heading1{font-family: 'Open Sans', sans-serif; padding-bottom: 20px; font-size: 25px;}")),
                             tags$style(HTML(".big-heading{font-family: 'Open Sans', sans-serif; font-size: 40px;}")),
                             
                     fluidRow(
                       
                       box(title="Preferred Foot count", status="primary", solidHeader = T, plotOutput("foot")),
                       box(title="Onfield position vs Dribbling speed", status="primary", solidHeader = T,plotOutput("PvsD")),
                       box(title="Height Analysis", status="primary", solidHeader = T,plotOutput("Height")),
                       box(title="Age vs Potential", status="primary", solidHeader = T,plotOutput("agevspotential")),
                     )),
                     
                     tabItem(tabName = "dash3",
                             h1(class="big-heading", "Club-wise Analysis"),
                             h1(class="big-heading1","Along with overall data analysis on the complete FIFA dataset of 84 years, club-wise analysis is all done
                             to get a better insight of how the clubs perform in various football leagues such as UEFA Champions League, Italian League, Premier League, etc.
                                The considered dataset has 11 observations of different clubs and the attributes are Club Name, Highest salary and name of the player with highest salary,
                                Market value and revenue."),
                     fluidRow(
                       box(title="Players vs Yearly Wage", status="primary", solidHeader = T,plotOutput("playerwage")),
                       box(title="Clubs vs Revenue and Market Value", status="primary", solidHeader = T,plotOutput("richclub")),
                       box(title="Clubs vs Highest paid players", status="primary", solidHeader = T,plotOutput("highpay"))
                     ))
                   )
                 )
  )
)


server <- (function(input, output){
  output$winner <- renderPlot({
    ggplot(m) +
      aes(x = Winner, y = Year) +
      geom_boxplot(fill = "#2171b5") +
      labs(x = "Winning Country", y = "Year", title = "FIFA Cup Winning Country") +
      theme_gray()
  })
  output$runnerup <- renderPlot({
    ggplot(m) +
      aes(x = Runners.Up, y = Year) +
      geom_boxplot(fill = "#6baed6") +
      labs(x = "Runner-Up Country", y = "Year", title = "FIFA Cup Runner-Up Country") +
      theme_gray()
  })
  #output$valuebox <- renderValueBox({
   # valueBox(836, "Total Matches Played", icon=icon("handshake-o"), color="green")
  #})
  output$Host <- renderPlot(
    ggplot(m) +
      aes(x = Country, y = Year) +
      geom_boxplot(fill = "#fa9e3b") +
      labs(x = "Host Country", y = "Year", title = "FIFA Cup Host Country") +
      theme_gray()
  )
  output$MvsG <- renderPlot(
    ggplot(m) +
      aes(x = Year, y = MatchesPlayed, colour = GoalsScored) +
      geom_line(size = 1.50) +
      scale_color_viridis_c(option = "viridis") +
      labs(x = "Year", y = "No. of Matches played", title = "Matches played and Goals scored per Year ", color = "Goals Scored") +
      theme_gray()
  )
  output$foot <- renderPlot(
    ggplot(q) +
      aes(x = Preferred.Foot, fill = Preferred.Foot) +
      geom_bar() +
      #scale_fill_hue() +
      labs(x = "Preferred Foot", y = "Preference level", title = "Preferred Foot count") +
      coord_flip() +
      theme_gray()
  )
  output$PvsD <- renderPlot(
    ggplot(q) +
      aes(x = Position, y = Dribbling) +
      geom_boxplot(fill = "#fa9e3b") +
      labs(x = "Position", y = "Dribbling", title = "Onfield position vs Dribbling speed") +
      theme_gray()
  )
  output$Height <- renderPlot(
    ggplot(q) +
      aes(x = Height) +
      geom_bar(fill = "#525252") +
      labs(x = "Height", y = "Frequency", title = "Height Histrogram") +
      coord_flip() +
      theme_gray()
  )
  output$agevspotential <- renderPlot(
    ggplot(p) +
      aes(x = Age, y = Potential, colour = Overall) +
      geom_point(size = 2.18) +
      #scale_color_gradient() +
      labs(x = "Age", y = "Potential", title = "Age vs Potential and Overall Skills", color = "Overall Skills") +
      theme_gray()
  )
  output$playerwage <- renderPlot(
    ggplot(r) +
      aes(x = Player, weight = Highest.Salary) +
      geom_bar(fill = "#31688e") +
      labs(x = "Player Name", y = "Yearly Wage in Euro millions", title = "Players vs Yearly Wage") +
      theme_gray()
  )
  output$richclub <- renderPlot(
    ggplot(r) +
      aes(x = Clubs, fill = Market.Value, weight = Revenue) +
      geom_bar() +
      scale_fill_distiller(palette = "BuPu") +
      labs(x = "Club Name", y = "Revenue in Euro millions", title = "Clubs vs Revenue and Market Value", fill = "Market Value in Euro Millions") +
      theme_gray()
  )
#  output$highpay <- renderPlot(
 #   ggplot(r) +
  #    aes(x = Player, y = Clubs) +
  #    geom_tile(size = 1L) +
   #   labs(x = "Club Name", y = "Highest Paid Players", title = "Clubs vs Highest paid players") +
    #  theme_gray()
#  )
  output$highpay <- renderPlot(
    ggplot(r) +
      aes(x = Clubs, y = Player) +
      geom_tile(size = 1L) +
      labs(x = "Club Name", y = "Highest Paid Players", title = "Clubs vs Highest paid players") +
      theme_gray()
  )
})
shinyApp(ui, server)

