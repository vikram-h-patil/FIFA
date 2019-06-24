# FIFA
FIFA WorldCup visualization using  R, Rshiny. 


### 1) Introduction           
Soccer Mania is a one stop destination to view the 1930 -2018 FIFA Worldcup story. This project displays the matches, teams and players' statistics. The whole project is implemented in Rshiny.
And the intended audiences are:        
• FIFA fan: A FIFA enthusiastic can view how many goals/ wins each country scored.               
• Team Manager: Soccer mania assists managers to view how each player vary with skills under each player division (striker, midfielder, defender and goalkeeper). For example, manager can view top 6 strikers and soccer mania assist to decide whom to select among 6 strikers.


-------------------------
#### The URL link can be found below.             
[FIFA_shinyapp](https://vpatil.shinyapps.io/soccer_mania/)

-------------------------         

### 2) Libraries used in this project are:
• library(shiny) : This library helps to create a web application.            
* library(plotly): This library creates a graph with interactions, which isn’t possible using ggplot. The graphs created are choropleth map, bar plots, scatterpolar.          
  • Choropleth map is useful in two ways, on hover, displays key information and on click displays the bar plot.          
  • Scatterpolar is powerful for comparison, using loop all skills are overlapped in single plot.                

• library(DT): This library is useful to show dataframes in shiny UI. Each player division (striker, midfielder, defender and goalkeeper) data is shown in a dataframe in front end.                 
• library(shinythemes): this library is useful to implement a theme with pre-loaded designs, I have used “flatly”.           
• library(networkD3): This is useful to create network diagrams. Each year’s team interactions are captured through network diagram.     
• Additionally a javascript file is present to make the sidepanel to auto scroll as user traverse the webpage.



-------------------------

### 3) Tab-1: Landing page
![](images/1.PNG)


-------------------------

### 4) Tab-2: FIFA visualization tab
![](images/2.PNG)

![](images/3.PNG)

![](images/4.PNG)

![](images/5.PNG)

-------------------------

### 5) Tab-3: Team Manager tab

![](images/6.PNG)

![](images/7.PNG)

![](images/8.PNG)
