
########################
#1) Libraries
library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(networkD3)
########################


########################
# 2) Reading the csv files
goals_wins_df <- read.csv("final_goals_wins.csv")
wc <- read.csv("final_wins.csv")
wc_2018_players <- read.csv("final_wc_2018_players.csv",stringsAsFactors=FALSE)
wc_stats_nw <- read.csv("final_cleaned_world_cup_matches_stats.csv")
wc_stats<- read.csv("final_cleaned_world_cup_stats.csv")
########################



########################
# 3) Begin of UI section
ui <- (
  navbarPage(
    title = 'Soccer Mania',
    fluid = TRUE,
    collapsible = FALSE,
    
    ########################################################
    #1st tab -  Welcome
    #Information: This is the landing page of the app.
                # Contains the introduction lines.
    ########################################################
    
    tabPanel('Welcome', 
             tags$head(includeCSS('www/football.css')),#including the css file
             
             ####################################################################################
             #Introduction lines
             div(class = '.style_1', 
                 div(class = 'first_line', strong('Soccer Mania')),
                 br(),
                 div(class = 'second_line', 
                     'One stop for all soccer mania fans. The application has 2 divisions'),
                 br(),
                 div(class = 'third_line', 
                     '1) FIFA worldcup visualization (1930 - 2018)'),
                 div(class = 'fifth_line', 
                     '2) Team manager portal,to assist him to select the players based on their skills'),
                 br(),
                 br(),
                 br(),
                 div(class = 'sixth_line', 
                     'Choose the tabs at the top to begin')
             ),
             
             img(class = 'background', 
                 src = 'welcome.jpg') #setting the background image
             ####################################################################################
            
    ),# end of welcome tab
    
    
    ##################################################################################################
    
    ################################################################
    #2nd tab -  FIFA_Visualization
    #Information: Contains 2 tabs:
                  # 1st tab: 1930-2018 world cup visulization.
                            #Graphs: (choropleth map and bar plot)
    
                  # 2nd tab: Year wise world cup information.
                            #Graphs: network plot
    ################################################################
    
    tabPanel("FIFA_Visualization",
             tabsetPanel(
               tabPanel("Welcome to FIFA_Visualization tab", "World cup explorer for football/soccer fans. View number of goals/wins of 
                        each country and other key statistics",
                        br(),
                        br(),
                        #Adding nested tabs[1]
                        tabsetPanel(
                          #################################
                          # 1st tab under FIFA_Visualization
                          #################################
                          tabPanel( 
                            '1930-2018', 
                            sidebarPanel(
                              tags$style(".well {background-color: #e1e6bf;}"),
                              tags$head(tags$script(src="floating_sidebar.js")), #making the panel auto scroll[3]
                              img(src="fifa.PNG",width="50%"),
                              
                              #Adding the year range selector
                              sliderInput(inputId = "year",
                                          label="Choose between range of years",
                                          min=1930,max=2018,sep = "",step=4,width="50%", value = c(1950, 2018)),
                              
                              #Adding the points selector
                              radioButtons("radio_points", "Points type:",
                                           c("Total Goals scored" = "goals",
                                             "Total wins" = "wins")),
                              width = 4
                            ),
                            mainPanel(
                              
                              #Adding the choropleth map 
                              fixedRow(
                                h2('FIFA World Cup (1930 - 2018)'),
                                tags$em('Hover(for information)  on country or click(for graph)'),
                                plotlyOutput("map")
                              ),
                              
                              #Adding the bar plot, which is dynamic 
                              fixedRow(
                                column(5, offset = 1,
                                       plotlyOutput("wins_goals_bar_plot"))
                                
                              )
                              
                            )
                            
                            
                          ),
                          #################################
                          #2nd tab under FIFA_Visualization
                          #################################
                          tabPanel("Year wise Information",
                                   sidebarLayout(
                                     #Adding the year selector
                                     sidebarPanel( width = 3,
                                                   tags$style(".well {background-color: #e1e6bf;}"),
                                                   helpText(h5("This application gives info and stats about FIFA worldcups")),
                                                   selectInput("year2","select year:",choices = (wc_stats$Year))
                                     ),
                                     mainPanel(
                                       fluidRow(
                                         
                                         # Displaying all stats with network plot
                                         column(3,h4("Winner:","\n",textOutput(outputId="winner"))),
                                         column(5,h4("Runner-up:","\n",textOutput(outputId="runner"))),
                                         h4("Third:","\n",textOutput(outputId="third")),
                                         h4("Teams Qualified:","\n",textOutput(outputId="teams_qualified")),
                                         h4("Total Matches played:","\n",textOutput(outputId="total_matches")),
                                         h4("Total Goals Scored:","\n",textOutput(outputId="total_goals")),
                                         h4("Attendance:","\n",textOutput(outputId="attend")),
                                         br(),
                                         h3("Below is the network graph"),
                                         h4("The network displays how each team interacted with each other"),
                                         simpleNetworkOutput("simple")
                                         
                                       )
                                     )#end of main panel
                                     
                                   )# end of sidebarLayout
                          )#end of tab2 
                        )#end of inner tabsetPanel
               )
             ) #end of outer tabsetPanel
    ), #end of FIFA_Visualization tab
    
    ##################################################################################################
    
    ##################################################################################
    #3rd Tab - Team_Manager tab
    #Information: Contains 2 tabs:
                    # 1st tab: Selected players information in a table format
                             #Graphs: each player's face is shown
    
                    # 2nd tab: Skills page, selected players skills such as 
                              #technical and physical/mental and other key attributes.
    
                             #Graphs: scatterpolar charts for skills and bar 
                                                  #plot for other attributes
    ##################################################################################
    
    #start of 3rd Tab - Team_Manager tab
    tabPanel(
      'Team_Manager',
      fluidPage(
        theme = shinytheme("flatly"), #choosing theme for shiny
        tags$style(type="text/css", # hiding any error messages on UI[2]
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        #sidebar starts here
        sidebarPanel(
          tags$head(tags$script(src="floating_sidebar.js")), #[3]
          img(src="manager.PNG",width="100%"),
          br(),
          br(),
          br(),
          #Inputs starts here
          selectInput('country',"Select Country",choices = unique(wc_2018_players$Nationality),
                      selected="France"),
          radioButtons("choice", "Select player division",
                       c("striker" = "striker",
                         "midfielder" = "midfielder",
                         "defender"="defender","goal_keeper"="goal_keeper")),
          tags$em("scroll down for goalkeeper"),
          
          width = 2
        ),
        
        #main panel starts here
        mainPanel(
          h4("Football can look a lot like chaos but top managers are able to see the bigger picture at 
             all times, Soccer Mania offers managers to select best players based on skills."),
          img(src="player_banner.png",width="100%"),
          tabsetPanel(
            ######################################
            #3.1 Tab starts here, selected players
            ######################################
            
            tabPanel('Selected_players',
                     br(),
                     fixedRow(
                       column(5, offset = 1,
                              div(DT::dataTableOutput("contents"), style = "font-size: 75%; width: 50%")),
                       
                       column(5,offset = 1,
                              div(DT::dataTableOutput("gk"), style = "font-size: 75%; width: 50%"))
                     )),
            
            ##############################################################
            #3.2 Tab starts here, skills
            #Key poinsts: Striker/Midfielder and Defender have same graphs,
                         #whereas, Goal keeper a second graph is added.
            ##############################################################
            tabPanel('Skills',
                     fixedRow(
                       h3("Striker/Midfielder/Defender attributes comparison"),
                       tags$em("Click on player's name in legend to select/de-select player"),
                       br(),
                       br(),
                       column(5,
                              h4("Technical skills"),
                              plotlyOutput('technical')
                       ),
                       column(5,offset = 2,
                              h4("Physical/Mental skills"),
                              plotlyOutput('physical')
                       )
                     ),
                     br(),
                     br(),
                     br(),
                     br(),
                     fixedRow( br(), br(),h4("Other Attributes")),
                     tags$em("Click on player's name in legend to select/de-select player"),
                     fixedRow(
                       column(5,
                              plotlyOutput('subplot1')
                       )
                     ),
                     br(),
                     br(),
                     br(),
                     br(),
                     tags$hr(style="border-color: black;"),
                     br(),
                     h3("GoalKeeper attributes comparison"),
                     tags$em("Click on player's name in legend to select/de-select player"),
                     fixedRow(
                       column(5,
                              h4("Technical skills"),
                              plotlyOutput('gkplot_technical')
                       ),
                       column(5,offset = 2,
                              h4("Physical/Mental skills"),
                              plotlyOutput('gkplot_physical')
                       )
                     ),
                     br(),
                     column(5,offset = 2,
                            h4("Other Attributes"),
                            tags$em("Click on player's name in legend to select/de-select player"),
                            plotlyOutput('subplot2')
                     )))))
      
      
      ) #end of tab-team_manager
  
)#end of navbarPage
)#end of shinyui
########################


#########################################################################################################
#4) Begin of server section


server <- function(input, output, session) {
 
  #######################################
  #Tab 1: FIFA Tab
  # Tab1.1: Under FIFA tab, 1930-2018 tab
  #######################################
  
  ###########################################################################################
  # Choroplet map
  ###############
  
  # On mouse hover on country, below is the text information.[4]
  wc$hover <- with(wc, paste(country,"<br>","gold:", gold,"<br>", "silver:", silver, "<br>",
                             "bronze:", bronze,"<br>","total_wins:",total_wins))
  
  l <- list(color = toRGB("white"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    showframe = TRUE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
  
  # plotting the choropleth map [4]
  output$map <- renderPlotly({
    
    p <- plot_geo(data=wc) %>% 
      add_trace(
        z = ~code,text = ~hover, locations = ~code, marker = list(line = l),showscale=FALSE,
        colors = 'Purples'
      ) %>%
    
      layout(
        geo = g,width=800
      )
    p
  })
  ###########################################################################################
  # Bar plot of Goals/Wins based on mouse click on country
  ########################################################
  
  # Below part handles the events, on mouse click on country > respective plots are displayed.
  #One more condition is also handled here, based on radio buttons selected,
                                      # respective bar plot is displayed(either wins or goals)
  
  output$wins_goals_bar_plot <- renderPlotly({
    mouse_event <- event_data("plotly_click") #capture the mouse click event[5]
    selected_code <- (mouse_event[3])
    selected_df <- goals_wins_df %>% 
      filter(code == selected_code$z)%>% 
      filter(Year >=input$year[1] & Year <= input$year[2])
    country_name=as.vector(selected_df$country[1]) # used for displaying the title name
    if (input$radio_points == "wins"){
      plot_ly(selected_df, x = ~Year, y=~wins,color = ~wins,type="bar",width = 0.4,text =~wins, textposition = 'auto')%>%
        layout(xaxis=list(type='category'),title=country_name)
    }else if(input$radio_points == "goals"){
      plot_ly(selected_df, x = ~Year, y=~goals,color = ~goals,type="bar",width = 0.4,text =~goals, textposition = 'auto')%>%
        layout(xaxis=list(type='category'),title=country_name)
    }
    
  })
  ###########################################################################################
  
  ########################################
  ## Tab1.2: Under FIFA tab, Year wise tab
  ########################################
  
  #Based on country year selected, making the dataframe reactive
  selected_wc_stats_nw <- reactive(wc_stats_nw[wc_stats_nw['Year'] == input$year2,])
  
  # Displaying of wins, runner, third and other information is handled here
  output$winner <- renderText({
    as.character(wc_stats$Winner[wc_stats$Year == input$year2])
  })
  
  output$runner <-renderText({
    as.character(wc_stats$Runners[wc_stats$Year == input$year2])
  })
  output$third <-renderText({
    as.character(wc_stats$Third[wc_stats$Year == input$year2])
  })
  output$teams_qualified <-renderText({
    as.character(wc_stats$QualifiedTeams[wc_stats$Year == input$year2])
  })
  output$total_matches <-renderText({
    as.character(wc_stats$MatchesPlayed[wc_stats$Year == input$year2])
  })
  output$total_goals <-renderText({
    as.character(wc_stats$GoalsScored[wc_stats$Year == input$year2])
  })
  output$attend <-renderText({
    as.character(wc_stats$Attendance[wc_stats$Year == input$year2])
  })
  
  # Below part handles the network graph[6]
  output$simple <- renderSimpleNetwork({
    selected_wc_stats_nw <- selected_wc_stats_nw()
    selected_wc_stats_nw = dplyr::select(selected_wc_stats_nw,home_team,away_team)
    simpleNetwork(selected_wc_stats_nw, 
                  # Size of the plot (horizontal)
                  fontSize = 20,opacity = 0.85, # opacity
                  zoom = TRUE, # ability to zoom when click on the node
                  linkDistance = 300, height = 500, width = 500
                  
    )
  })
  
  # end of FIFA tab
  
  ###########################################################################################
  ###################
  # Team Manager Tab
  ###################
  
  # Based on Country selected, making the dataframe reactive
  selected_country <- reactive(wc_2018_players %>% filter(Nationality == input$country))
  
  
  # Selecting respective dataframes fore each player division
  player_stats <- reactive({
    if(input$choice =='striker'){
      player_stats<- selected_country() %>% dplyr::filter(grepl('ST|LS|RS|RF|LF|LW|RW',Preferred.Positions)) %>%
        head(arrange(desc(ST),desc(Overall)),n=6)
    }
    
    else if(input$choice =='midfielder'){
      player_stats<- selected_country() %>% dplyr::filter(grepl('CAM|CDM|CM|LAM|LCM|LDM|LM|RAM|RCM|RDM|RM',
                                                                Preferred.Positions)) %>%
        head(arrange(desc(ST),desc(Overall)),n=6)
    }
    
    else if(input$choice =='defender'){
      player_stats<- selected_country() %>% dplyr::filter(grepl('CB|LB|LCB|LWB|RB|RCB|RWB',
                                                                Preferred.Positions)) %>%
        head(arrange(desc(ST),desc(Overall)),n=6)
    }
    
  })
  
  gk_stats <- reactive({
    if(input$choice =='goal_keeper'){
      gk_stats<- selected_country() %>% dplyr::filter(grepl('GK',Preferred.Positions)) %>%
        head(arrange(desc(ST),desc(Overall)),n=6)
    }
  })
  
  # end of reactive
  
  ################################################################################
  
  ##################################################################################################
  #Note: Striker,midfielder and defender have comon skills, whereas Goalkepper has different skills
          #hence two dataframes are accessed and displayed on screen accordingly    
  ##################################################################################################
  
  #Displaying the selected player dataframe(striker,midfielder and defender)
  output$contents <- DT:: renderDataTable({
    test_df<- player_stats() 
    test_df <- select(test_df,"Name","Age","Photo","Overall","Wage")
    #Below part handles the display of each player face
    test_df$photo <- apply(test_df['Photo'], 1, function(x){gsub("(.*)",'<img src="\\1"></img>',x)})
    test_df <- select(test_df,"Name","Age","photo","Overall","Wage")
    DT::datatable(test_df, escape = FALSE)
  })
  
  #Displaying the selected player dataframe (goalkeeper)
  output$gk <- DT:: renderDataTable({
    test_df<- gk_stats() 
    test_df <- select(test_df,"Name","Age","Photo","Overall","Wage")
    test_df$photo <- apply(test_df['Photo'], 1, function(x){gsub("(.*)",'<img src="\\1"></img>',x)})
    test_df <- select(test_df,"Name","Age","photo","Overall","Wage")
    DT::datatable(test_df, escape = FALSE)
  })
  ################# end of table
  
  ######################################################################################################
  
  #######################################
  # Technical skills and other attributes
  #######################################
  
  # Technical plot for striker, midfielder and defender
  #Note: Scatterpolar chart is used, and using 'for' loop all player's skills are overlapped on chart
  
  output$technical <- renderPlotly({
    player_stats <- player_stats()
    technical <- plot_ly(type = 'scatterpolar',mode = 'lines')#width = 500, height = 500)
    
    for(i in 1:nrow(player_stats)){ 
      technical<-add_trace(technical,
                           r = c(player_stats$Dribbling[i],player_stats$Crossing[i],player_stats$Volleys[i],
                                 player_stats$Long.passing[i],player_stats$Short.passing[i],
                                 player_stats$Sliding.tackle[i],player_stats$Standing.tackle[i],
                                 player_stats$Curve[i],player_stats$Shot.power[i],
                                 player_stats$Long.shots[i],player_stats$Penalties[i],
                                 player_stats$Ball.control[i],player_stats$Heading.accuracy[i],
                                 player_stats$Free.kick.accuracy[i],player_stats$Interceptions[i]),
                           theta = c("Dribbling","Crossing","Volleys","Long.passing","Short passing","Sliding tackle",
                                     "Standing tackle","Curve","Shot power","Long shots","Penalties",
                                     "Ball control","Heading accuracy","Free kick accuracy","Interception"),
                           name = player_stats$Name[i],
                           fill = 'toself')%>%
        layout(legend = list(orientation = 'h'))
    }
    technical
    
  })
  #############
  # Physical plot for striker, midfielder and defender
  #Note: Scatterpolar chart is used, and using 'for' loop all player's skills are overlapped on chart
  
  output$physical <- renderPlotly({
    player_stats <- player_stats()
    physical <- plot_ly(type = 'scatterpolar',mode = 'lines')
    
    for(i in 1:nrow(player_stats)){ 
      physical<-add_trace(physical,
                          r = c(player_stats$Acceleration[i],player_stats$Aggression[i],
                                player_stats$Agility[i],player_stats$Balance[i],
                                player_stats$Composure[i],player_stats$Reactions[i],
                                player_stats$Sprint.speed[i],player_stats$Stamina[i],
                                player_stats$Strength[i],player_stats$Jumping[i],
                                player_stats$Vision[i],player_stats$Finishing[i],
                                player_stats$Positioning[i],player_stats$Marking[i]),
                          theta = c("Acceleration","Aggression","Agility","Balance","Composure","Reactions",
                                    "Sprint speed","Stamina","Strength","Jumping","Vision","Finishing",
                                    "Positioning","Marking"),
                          name = player_stats$Name[i],
                          fill = 'toself')%>%
        layout(legend = list(orientation = 'h'))
    }
    physical
    
  })
  
  ############
  
  #################################
  #Other attributes plot, bar plot
  #################################
  
  output$subplot1 <- renderPlotly({
    player_stats <- player_stats()
    age <- select(player_stats,Name,Age)
    wage <- select(player_stats,Name,Wage)
    overall <- select(player_stats,Name,Overall)
    potential <- select(player_stats,Name,Potential)
    value <- select(player_stats,Name,Value)
    
    
    q <- plot_ly(age, x = ~Name, y=~Age,color = ~Name,type="bar",legendgroup=~Name)
    
    
    r <- plot_ly(wage, x = ~Name, y=~Wage,color = ~Name,type="bar",legendgroup=~Name,showlegend = F)
    
    s <- plot_ly(overall, x = ~Name, y=~Overall,color = ~Name,type="bar",legendgroup=~Name,showlegend = F)
    
    t <- plot_ly(potential, x = ~Name, y=~Potential,color = ~Name,type="bar",legendgroup=~Name,showlegend = F)
    
    u <- plot_ly(value, x = ~Name, y=~Value,color = ~Name,type="bar",legendgroup=~Name,showlegend = F)
    
    subplot(q,r,s,t,u,nrows=1,titleY = TRUE, shareX = TRUE,margin=c(0.085,0.01,0.1,0.1))%>%
      layout(autosize = F, width = 1000, height = 500)
    
    
  })
  
  ############
  # Technical plot for goal keeper
  #Note: Scatterpolar chart is used, and using 'for' loop all player's skills are overlapped on chart
  
  output$gkplot_technical <- renderPlotly({
    gk_stats <- gk_stats()
    gkplot_technical <- plot_ly(type = 'scatterpolar',mode = 'lines')
    
    for(i in 1:nrow(gk_stats)){ 
      gkplot_technical<-add_trace(gkplot_technical,
                                  r = c(gk_stats$GK.diving[i],gk_stats$GK.handling[i],
                                        gk_stats$GK.kicking[i],gk_stats$GK.positioning[i],
                                        gk_stats$GK.reflexes[i]),
                                  theta = c("GK.diving","GK.handling","GK.kicking","GK.positioning","GK.reflexes"),
                                  name = gk_stats$Name[i],
                                  fill = 'toself')%>%
        layout(legend = list(orientation = 'h'))
    }
    gkplot_technical
  })
  #####################
  # Physical plot for goal keeper
  #Note: Scatterpolar chart is used, and using 'for' loop all player's skills are overlapped on chart
  
  output$gkplot_physical <- renderPlotly({
    gk_stats <- gk_stats()
    gkplot_physical <- plot_ly(type = 'scatterpolar',mode = 'lines')
    
    for(i in 1:nrow(gk_stats)){ 
      gkplot_physical<-add_trace(gkplot_physical,
                                 r = c(gk_stats$Acceleration[i],gk_stats$Aggression[i],
                                       gk_stats$Agility[i],gk_stats$Balance[i],
                                       gk_stats$Composure[i],gk_stats$Reactions[i],
                                       gk_stats$Sprint.speed[i],gk_stats$Stamina[i],
                                       gk_stats$Strength[i],gk_stats$Jumping[i],
                                       gk_stats$Vision[i],gk_stats$Finishing[i],
                                       gk_stats$Positioning[i],gk_stats$Marking[i]),
                                 theta = c("Acceleration","Aggression","Agility","Balance","Composure","Reactions",
                                           "Sprint speed","Stamina","Strength","Jumping","Vision","Finishing",
                                           "Positioning","Marking"),
                                 name = gk_stats$Name[i],
                                 fill = 'toself')%>%
        layout(legend = list(orientation = 'h'))
    }
    gkplot_physical
    
  })
  #####################
  
  ###############################################
  #Other attributes plot for goalkeeper, bar plot
  ###############################################
  
  output$subplot2 <- renderPlotly({
    gk_stats <- gk_stats()
    age <- select(gk_stats,Name,Age)
    wage <- select(gk_stats,Name,Wage)
    overall <- select(gk_stats,Name,Overall)
    potential <- select(gk_stats,Name,Potential)
    value <- select(gk_stats,Name,Value)
    
    
    q <- plot_ly(age, x = ~Name, y=~Age,color = ~Name,type="bar",legendgroup=~Name)
    
    
    r <- plot_ly(wage, x = ~Name, y=~Wage,color = ~Name,type="bar",legendgroup=~Name,showlegend = F)
    
    s <- plot_ly(overall, x = ~Name, y=~Overall,color = ~Name,type="bar",legendgroup=~Name,showlegend = F)
    
    t <- plot_ly(potential, x = ~Name, y=~Potential,color = ~Name,type="bar",legendgroup=~Name,showlegend = F)
    
    u <- plot_ly(value, x = ~Name, y=~Value,color = ~Name,type="bar",legendgroup=~Name,showlegend = F)
    
    subplot(q,r,s,t,u,nrows=1,titleY = TRUE, shareX = TRUE,margin=c(0.085,0.01,0.1,0.1))%>%
      layout(autosize = F, width = 900, height = 500)
    
    
  })
  
  
  #####################
  
  
} #end of server


#calling the shiny app
shinyApp(ui, server)






# References:
#[1] https://community.rstudio.com/t/can-i-have-tab-panels-inside-tab-panel/25770/3
#[2] https://stackoverflow.com/questions/30887205/shiny-showing-one-message-for-all-errors 
#[3] https://github.com/LyzandeR/FootballeR/tree/master/app
#[4] https://plot.ly/r/choropleth-maps/ 
#[5] https://plot.ly/r/shinyapp-plotly-events/
#[6] https://christophergandrud.github.io/networkD3/
  