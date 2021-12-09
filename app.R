# This is a Shiny web application for our Final ADS Project

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(shiny)
library(tidyverse)
library(plotly)
library(readr)
library(readxl)
library(leaflet)
library(maps)
library(maptools)
library(readxl)
library(shinythemes)
library(ggplot2)
library(reshape2)

plusTIMSS2<-read_csv("~/Desktop/Fall21/Advanced Data Science/KA_CT_project/plus_timss.csv")

map_data<- read_csv("~/Desktop/Fall21/Advanced Data Science/KA_CT_project/Only_IV_NA")
country_coordinates <- read_csv("average-latitude-longitude-countries.csv")

#tidy the map data
map_data<-map_data %>% 
  select(-m8_avg_scale_score, -m4_avg_scale_score, -s8_avg_scale_score, -s4_avg_scale_score, -m8_scaled, -m4_scaled, -s8_scaled, -s4_scaled)


#Full map data (like the actual one)
full_map_data<- map_data %>% 
  inner_join(country_coordinates, by=c("country"))

#Categorizing data for map 
CompData<-full_map_data %>% 
  
  mutate(TalentCompetitiveness_Categorical =ifelse(talent_competitiveness_ranking_imd <=21, "low", 
                                                   ifelse(talent_competitiveness_ranking_imd >21 & talent_competitiveness_ranking_imd <= 42, "intermediate",
                                                          ifelse(talent_competitiveness_ranking_imd > 64, "high", "other"))), 
         
         Position2020_Categorical =ifelse(position_2020 <=21, "low", 
                                          ifelse(position_2020 >21 & position_2020 <= 42, "intermediate",
                                                 ifelse(position_2020 > 64, "high", "other"))),
         
         USNews2020_Categorical =ifelse(best_countries_for_education_ranking_us_news_2020<=25,"low",
                                        ifelse(best_countries_for_education_ranking_us_news_2020 >25 & best_countries_for_education_ranking_us_news_2020 <=50, "intermediate",
                                               ifelse(best_countries_for_education_ranking_us_news_2020 >77, "high", "other"))),
         
         USNews2021_Categorical = ifelse(best_countries_for_education_ranking_us_news_2021<=25, "low",
                                         ifelse(best_countries_for_education_ranking_us_news_2021>25 & best_countries_for_education_ranking_us_news_2021<=50,"intermediate",
                                                ifelse(best_countries_for_education_ranking_us_news_2021 >77, "high", "other"))),  
         
         CEOWORLD_Categorical =ifelse(ceoworld_index<=46, "low",
                                      ifelse(ceoworld_index>46 & ceoworld_index<=135,"intermediate",
                                             ifelse(ceoworld_index>135, "high", "other"))), 
         
         TIMSS_Categorical =ifelse(TIMSS_score<=42, "low",
                                   ifelse(TIMSS_score>42 & TIMSS_score<=111,"intermediate",
                                          ifelse(TIMSS_score >111, "high", "other"))), 
         
         PISAIndex_Categorical =ifelse(pisa_index<=61, "low",
                                       ifelse(pisa_index>61 & pisa_index<=175,"intermediate",
                                              ifelse(pisa_index >175, "high", "other"))),
         
         
  )



#Building pallettes for map
pal1 <- colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$TalentCompetitiveness_Categorical)

pal2<-colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$Position2020_Categorical)

pal3<-colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$USNews2020_Categorical)

pal4<-colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$USNews2021_Categorical)

pal5<-colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$CEOWORLD_Categorical)

pal6<-colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$TIMSS_Categorical)

pal7<-colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$PISAIndex_Categorical)


#data work
plusTIMSS2<-plusTIMSS2 %>% 
    mutate(schooling_rank=cut(schooling_index, breaks=c(-Inf, 284, 496, Inf), labels=c("low","middle","high")))
plusTIMSS2<-plusTIMSS2 %>% 
    mutate(stringency_cat=cut(stringency_index, breaks=c(-Inf, 48, 69, Inf), labels=c("low","middle","high")))
plusTIMSS2<-plusTIMSS2 %>% 
    mutate(government_cat=cut(government_response_index, breaks=c(-Inf, 44, 65, Inf), labels=c("low","middle","high")))
plusTIMSS2<-plusTIMSS2 %>% 
    mutate(economic_cat=cut(economic_support_index, breaks=c(-Inf, 25, 75, Inf), labels=c("low","middle","high")))


# Define UI for application 
ui <- fluidPage(
    theme = bs_theme(bg = "#0b3d91", 
                     fg = "white",
                     primary = "#FCC780", 
                     secondary = "#D44420", 
                     base_font = font_google("Space Mono"), 
                     bootswatch = "minty"),
    
    # Application title
    h1("Education and the Pandemic"),
    h3("Analysing the relationship between international rankings on education and schooling during the Pandemic"),
    p(" "),
    h5(tags$a(href = "https://candetorresjimenez.netlify.app", "Cande Torres Jimenez")),
    h5(tags$a(href = "https://www.linkedin.com/in/kashviajitsaria", "Kashvi Ajitsaria")),
    #Tab Panels 
    
    tabsetPanel(type="tabs",

                tabPanel("Report",
                         p(""),
                         h4("Introduction"),
                         p("According to the former Russian president Dmitry Medvedev, “Human capital is becoming the main area of global competition, and it is the most important and, at the same time, the most dynamic facet of modern industry. This form of competition will become severe due to a clear realization that has appeared in the world: leading positions are going to be occupied by countries that are attractive to educated and energetic people” (Medvedev, 2015). Since the beginning of the new millennium, we can identify a global trend of states focusing investments of resources and attention on the improvement of their educational systems as a way to acquire international recognition. The World Economic Forum defines competitiveness as “the set of institutions, policies and factors that determine the level of productivity of a country” and is closely tied to education because human capital increases productivity. Competitiveness of a country directly correlates with the level of the life standards reached (Yildiri Keser, H. 2015). At the same time, productivity has been linked with economic development and higher welfare."),
                         p("Then, there are major incentives for countries to pursue “world-class” education and recognition for high quality human capital. The development of an economic system that praises the consumption and production of intellectual capital has become known as the “knowledge economy”. In the framework of  the knowledge economy it is vital for nations to position themselves in a competitive place because it may have benefits to industry promotion and economic standards."),
                         p("The international order is highly dependent on appearance. In such a system, the success or failure of diplomacy and negotiations are based upon the state’s image in the international scene. Thus, achieving remarkable results in strategic areas, such as the knowledge economy, represents a priority for states. Since 2004, rankings have not only intensified cross-country comparison, but also they attract “the attention of policymakers and the academy, challenging perceived wisdom about the status and reputation, as well as quality and performance (...)” of educational institutions and systems around the world (Hazelkorn, 2014). In other words, rankings and international standards are crucial to understand the pursuit of status internationally within the knowledge economy."),
                         p("One of the principal ways in which states seek to outperform their competitors is through international assessments that produce rankings. For some authors, “today’s [rankings] signify national reputation and status in the global market” which enhances students' choices (Hazelkorn, 2014) and promotes the identification of best practices (OCDE, 2006, p.8)."),
                         p("The disruption caused by COVID-19 changed many things. With the need for isolation and quarantine, the rise in fear and anxiety, and the decline in economic activity, one of the areas that the pandemic impacted the most is certainly education. When the possibility of a quick recovery and an easy solution to the crisis was discarded, the pandemic challenged governments around the world to quickly provide an efficient and flexible response to the thousands of students and teachers affected. Today, more than a year since the first case was detected, understanding countries’ efforts on educational public policy is extremely important to assess not only the present impact the pandemic had on the society, but also the future one."),
                         p("For that reason, we decided to gather data that would allow us to test the influence of international rankings on education decisions at the national level during the pandemic. From the UNESCO Monitoring of School Closures Caused by COVID-19 we obtained data on the amount of days schools were fully or partially closed and open and the number of instruction and academic break days. With this information, we coded the proportions of the year for each variable, and then ranked the countries accordingly. Then we created an index that we denominated “schooling_index” which became our main dependent variable. "),
                         p("For the independent variable, we decided to code multiple variables, utilizing international rankings on education. Because different countries participate on different rankings, we coded models with the same dependent variable and the same controls but using different main explanatory variables. Thus, each model fitted measures the relationship for different amounts of observations (countries). This decision mirrors the reality of the world because not all the countries participate from all the international rankings. Then, changing the main independent variable for the same model will include different amounts of observations. Initially I thought combining the 5 indexes would give a better rounded idea of the global perspective on countries on Education, however, this proved futile considering that not all countries participated or were integrated in the same rankings."),
                         p("The independent variables that we modeled are the following: "),
                         p("- pisa_index -> A ranking combining the countries results on Reading, math and science from PISA test 2018. Recorded for 76 countries. Available for 60 observations in the data set. Arguably the most famous assessment is the Programme for International Student Assessment (PISA) tests. As an initiative from the Organization for Economic Cooperation and Development (OECD), the PISA-based tests were first launched in 2000 as a voluntary assessment that countries could choose to administer. It provides comparative data on 15-year-olds’ performance in reading, mathematics and science plus additional modules undertaken accordingly which is not directly linked to the national curriculum"),
                         p("- ceoworld -> In order to determine the international rankings for education systems, researchers at the CEOWORLD magazine compiled, analyzed and compared 93 countries across two key categories: 1) Quality and 2) Opportunity. The index combines the results from both rankings for each country. Available for 73 observations."),
                         p("- talent_competitiveness_ranking_imd -> the World Talent Ranking (International Institute for Management Development) for 64 countries. The IMD World Talent Ranking (WTR) assesses the status and the development of competencies necessary for enterprises and the economy to achieve long term value creation. It does so by using a set of indicators which measure the development, retention and attraction of a domestic and international highly-skilled workforce. Available for 53 observations in the data set. "),
                         p("- TIMSS_score -> The Trends in International Mathematics and Science Study (TIMSS) is an international comparative study that has measured trends in mathematics and science achievement at 4th and 8th grade every 4 years since 1995. In 2019, TIMSS mathematics and science data were collected in 4th grade and 46 education systems in 8th grade. The data was released in December 2019. Available for 28 observations in the data set. "),
                         p("- best_countries_for_education_ranking_us_news_2020 -> The 2020 Best Countries for Education are ranked by the U.S. News and World Report based on a perception-based global survey, which used a compilation of scores from three equally weighted country attributes: having a well-developed public education system, whether people would consider attending university there and if that country provides a top quality education. The Best Countries rankings are based largely on perception, and countries are assessed on the same set of 76 country attributes each year. Available for 66 observations in the data set. "),
                         p(strong("PLOT")),
                         mainPanel(plotOutput(outputId = "covidplot", width = "140%")),
                         p(" "),
                         p("For Control Variables we utilized data from the Oxford COVID-19 response tracker. They have recorded data on confirmed cases, confirmed number of deaths and several other categorical variables on policy response across 2020. Because working with the time-series was not central to our investigation, we filtered the data to correspond to the countries’ situation on December 15th 2020. As you can see in the following plot, the number of cases and number of deaths were the following for the countries with the top 10 highest numbers"),
                         p(strong("PLOT")),
                         mainPanel(plotOutput(outputId = "exploraplot1", width = "110%")),
                         p(" "),
                         p("In the modeling, we included variables such as Stringency Index, Government Response Index and Economic Support Index which are variables coded based on the other variables that the data has. In other words, these 3 indicators summarized the others. We categorized them into 3 levels to make it easier for the audience to visualize the distribution of countries. "),
                         mainPanel(plotOutput(outputId = "exploraplot2", width = "110%")),
                         p(""),
                         p(strong("Data Sources")),
                         p(" "),
                         p("Our data sources include the Oxford COVID-19 Government Response Tracker, the UNESCO Monitoring of School Closures Caused by COVID-19, the Rapid situation tracking for COVID-19 socioeconomic impacts (UNICEF), the World Talent Ranking (International Institute for Management Development), the Best Education Ranking (U.S News and World Report), the CEOWORLD ranking for Opportunities in Education and Quality of Education Systems 2020, and the results on the latest PISA tests 2018 (OECD) and TIMSS 2019"),
                         ),
                         
                
                tabPanel("Search", helpText(strong("Input your parameters to learn about country's situation during the pandemic. NOTE: Data is for December 2020")),
                         p(" "),
                         p("Because the data comes from different sources, some of the countries have no value for some columns"),
                         fluidRow(
                             numericInput("days_academic_break", "Max. number of days on academic break", 146,min = 0, max=146),
                             numericInput("instruction_days", "Min. number of instruction days", 89, 89, 235),
                             numericInput("days_fully_closed", "Max. number of days schools remained closed ", 211, 0, 211),
                             numericInput("days_fully_open", "Min. number of days schools remained open ", 0, 0, 225),
                             selectInput("schooling_rank", "Rank obtained in schooling_index",
                                         choices= c("low","middle","high", "Any"), selected = "Any"),
                             selectInput("economic_cat", "Oxford Economic support index",
                                         choices = c("low","middle","high", "Any"), selected = "Any"),
                             selectInput("stringency_cat", "Oxford Stringency index",
                                         choices = c("low","middle","high", "Any"), selected = "Any"),
                             selectInput("government_cat", "Oxford Government Response index",
                                         choices = c("low","middle","high", "Any"), selected = "Any"),
                                    ),
                         mainPanel(dataTableOutput(outputId = "searchlist"))
                         ),
                
                
                
                
                tabPanel("Map", leafletOutput(outputId="mymap"),
                         helpText("The map shows the countries classified by color in the categories shown in the panel with the most recent data available"),
                         helpText("To have an accurate perspective, please choose only ONE CATEGORY to explore at the time"),
                         fluidRow(sidebarPanel(#top = 250, left = 20, 
                                       checkboxInput("talent", "IMD Talent Competitiveness Ranking", FALSE),
                                       checkboxInput("position", "Position 2020", FALSE),
                                       checkboxInput("best20", "US News Education Ranking 2020", FALSE),
                                       checkboxInput("best21", "US News Education Ranking 2021", FALSE),
                                       checkboxInput("ceoworld", "CEOWORLD index Quality of Education System and Opportunity", FALSE),
                                       checkboxInput("timss", "TIMSS score 2019", FALSE),
                                       checkboxInput("pisa_index", "PISA Rank 2018", FALSE))
                         )
                ),
                
              tabPanel("Testing the correlation",
                       helpText("Choose among the international rankings and test its correlation with the schooling_index"),
                       p(" "),  
                       fluidRow(
                                sidebarPanel(
            
                                        selectInput(inputId = "variable",
                                         label = "Choose a variable:",
                                         choices = c("pisa_index",
                                                     "ceoworld_index",
                                                     "talent_competitiveness_ranking_imd",
                                                     "TIMSS_score",
                                                     "best_countries_for_education_ranking_us_news_2020")),
                                             ),
                                  mainPanel(plotOutput(outputId = "ivplot")),
                                  p(" "),
                                  p(strong("Model Coefficient estimates")),
                                  p(" "),
                                  mainPanel(verbatimTextOutput("summary"),
                                          br())#deleted a comma here
 
                        ))
            ) #tabset 
)#fluidPage

# Define server logic 
server <- function(input, output) {

# Now use that function, with no arguments.
output$ivplot <- renderPlot({
    plusTIMSS2 %>% #eventually replace data for carefully
        ggplot(aes( x = .data[[input$variable]], y= schooling_index)) +
        geom_point() +
        geom_smooth(method = lm) +
        labs(title = paste(input$variable, "during the pandemic"),
             x = "",
             y = "") +
        theme_minimal()
})


output$summary <- renderPrint({
  fit <- lm(schooling_index~ .
              ,data= plusTIMSS2 %>% select(h4_emergency_investment_in_healthcare,
                                             confirmed_cases,
                                             confirmed_deaths,
                                             government_response_index,
                                             stringency_index,
                                             economic_support_index, schooling_index,.data[[input$variable]] ))
  summary(fit)
})

output$mymap <-renderLeaflet({
  leaflet(data) %>% 
    setView(lng = -99, lat = 45, zoom = 2) %>% 
    addTiles() %>% 
    addCircles(data = CompData, 
               lat = ~ Latitude, lng = ~ Longitude, label = ~as.character(paste0("Country: ", sep = " ", country)), 
               weight = 1, popup = ~ country, 
               radius=70000, color = ~gray,
               fillOpacity = 0.5)
})


observe({
  proxy1 <- leafletProxy("mymap", data=CompData)
  proxy1 %>% clearMarkers()
  if (input$talent) {
    proxy1 %>% addCircleMarkers(lat = ~ Latitude, lng = ~ Longitude, stroke = FALSE, color = ~pal1(TalentCompetitiveness_Categorical), fillOpacity = 0.2,
                                label = ~as.character(paste0(country, sep = " ", talent_competitiveness_ranking_imd))) %>%
      addLegend("bottomright", pal = pal1, values = CompData$TalentCompetitiveness_Categorical,
                title = "IMD Talent Competitiveness Ranking",
                opacity = 3)}
  else {
    proxy1 %>% clearMarkers() %>% clearControls()
  }
})

observe({
  proxy2 <- leafletProxy("mymap", data = CompData)
  proxy2 %>% clearMarkers()
  if (input$position) {
    proxy2 %>%  addCircleMarkers(lat = ~ Latitude, lng = ~ Longitude, stroke = FALSE, color = ~pal2(Position2020_Categorical), fillOpacity = 0.2,
                                 label = ~as.character(paste0(country, sep = " ", position_2020))) %>%
      addLegend("bottomright", pal = pal2, values = CompData$Position2020_Categorical,
                title = "Position 2020",
                opacity = 3)}
  else{
    proxy2 %>% clearMarkers() %>% clearControls()
  }
})

observe({
  proxy3 <- leafletProxy("mymap", data= CompData)
  proxy3 %>% clearMarkers()
  if (input$best20) {
    proxy3 %>% addCircleMarkers(lat = ~ Latitude, lng = ~ Longitude, stroke = FALSE, color = ~pal3(USNews2020_Categorical), fillOpacity = 0.2,
                                label = ~as.character(paste0(country, sep = " ", best_countries_for_education_ranking_us_news_2020))) %>%
      addLegend("bottomright", pal = pal3, values = CompData$USNews2020_Categorical,
                title = "US News Education Ranking 2020",
                opacity = 3)}
  else {
    proxy3 %>% clearMarkers() %>% clearControls()
  }
})

observe({
  proxy4 <- leafletProxy("mymap", data= CompData)
  proxy4 %>% clearMarkers()
  if (input$best21) {
    proxy4 %>% addCircleMarkers(lat = ~ Latitude, lng = ~ Longitude, stroke = FALSE,
                                color = ~pal4(USNews2021_Categorical), fillOpacity = 0.2,
                                label = ~as.character(paste0(country, sep = " ", best_countries_for_education_ranking_us_news_2021))) %>%
      addLegend("bottomright", pal = pal4, values = CompData$USNews2021_Categorical,
                title = "US News Education Ranking 2021",
                opacity = 3)}
  else {
    proxy4 %>% clearMarkers() %>% clearControls()
  }
})

observe({
  proxy5 <- leafletProxy("mymap", data= CompData)
  proxy5 %>% clearMarkers()
  if (input$ceoworld) {
    proxy5 %>% addCircleMarkers(lat = ~ Latitude, lng = ~ Longitude, stroke = FALSE, color = ~pal5(CEOWORLD_Categorical), fillOpacity = 0.2,
                                label = ~as.character(paste0(country, sep = " ", ceoworld_index))) %>%
      addLegend("bottomright", pal = pal5, values = CompData$CEOWORLD_Categorical,
                title = "CEOWORLD Index Quality of Education System and Opportunity",
                opacity = 3)}
  else {
    proxy5 %>% clearMarkers() %>% clearControls()
  }
})

observe({
  proxy6 <- leafletProxy("mymap", data= CompData)
  proxy6 %>% clearMarkers()
  if (input$timss) {
    proxy6 %>% addCircleMarkers(lat = ~ Latitude, lng = ~ Longitude, stroke = FALSE, color = ~pal6(TIMSS_Categorical), fillOpacity = 0.2,
                                label = ~as.character(paste0(country, sep = " ", TIMSS_score))) %>%
      addLegend("bottomright", pal = pal6, values = CompData$TIMSS_Categorical,
                title = "TIMSS score 2019",
                opacity = 3)}
  else {
    proxy6 %>% clearMarkers() %>% clearControls()
  }
})

observe({
  proxy7 <- leafletProxy("mymap", data= CompData)
  proxy7 %>% clearMarkers()
  if (input$pisa_index) {
    proxy7 %>% addCircleMarkers(lat = ~ Latitude, lng = ~ Longitude, stroke = FALSE, color = ~pal7(PISAIndex_Categorical), fillOpacity = 0.2,
                                label = ~as.character(paste0(country, sep = " ", pisa_index))) %>%
      addLegend("bottomright", pal = pal7, values = CompData$PISAIndex_Categorical,
                title = "PISA Rank 2018",
                opacity = 3)}
  else {
    proxy7 %>% clearMarkers() %>% clearControls()
  }
})


df2 <- plusTIMSS2 %>%
  select(unicef_country, confirmed_cases,confirmed_deaths) %>% 
  group_by(unicef_country) %>% 
  arrange(desc(confirmed_cases)) %>% 
  head(10) %>% 
  melt(id.vars='unicef_country')
df2<-df2 %>% mutate(label=value)

output$covidplot <- renderPlot({
  df2 %>% 
    ggplot(aes(x=fct_reorder(unicef_country, value, max), y=value, fill=variable))+
    geom_bar(stat='identity', position = 'dodge')+
    #facet_wrap(~ variable)+
    theme_minimal()+
    labs( x="Countries", tittle="Top-10 Countires with higher confirmed COVID cases (December 2020)")+
    scale_y_continuous(name="COVID-19 impact", breaks=seq(0,17000000,1000000))+
    geom_text(aes(label = value), vjust = -0.8)
})

output$exploraplot1 <- renderPlot({
  plusTIMSS2 %>% 
    select(days_academic_break,days_fully_closed,days_fully_open,days_partially_closed, instruction_days, confirmed_cases,confirmed_deaths, stringency_index,government_response_index,economic_support_index, talent_competitiveness_ranking_imd, best_countries_for_education_ranking_us_news_2020, ceoworld_index,pisa_index, TIMSS_score) %>% 
    pivot_longer(cols = everything(),
                 names_to = "variable", 
                 values_to = "value") %>% 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill="red") +
    facet_wrap(vars(variable), 
               scales = "free")+
    theme_minimal()
})

output$exploraplot2 <- renderPlot({
  plusTIMSS2 %>% 
    select(schooling_rank,economic_cat,stringency_cat, government_cat) %>% 
    pivot_longer(cols = everything(),
                 names_to = "variable", 
                 values_to = "value") %>% 
    ggplot(aes(x = value)) +
    geom_bar(fill="red") +
    facet_wrap(vars(variable), 
               scales = "free", 
               nrow = 2)+
    theme_minimal()
})

output$searchlist <- DT::renderDataTable(DT::datatable({ #plusTIMSS2
     data <- plusTIMSS2 %>% 
         select(-...1, -date, -m8_avg_scale_score, -m4_avg_scale_score, -s8_avg_scale_score, -s4_avg_scale_score, -m8_scaled, -m4_scaled, -s8_scaled, -s4_scaled, -iv_index_no_timss, -prop_closed,-prop_break,-prop_open, -prop_instruction,
                -tracking_or_monitoring_of_student_learning,-content_need_sensitive,-Support_vulnerable, -usage_of_remote_learning_platforms, -parental_caregiver_support,-curriculum_coverage,-access_to_remote_learning,-teacher_support, -quality_remote_platform_content,
                -position_2020,-best_countries_for_education_ranking_us_news_2021,-pisa_reading_2018,-pisa_math_2018,-pisa_science_2018,
                -closed_rank, -break_rank, -instruction_rank, -open_rank) %>% 
         filter(days_academic_break<=input$days_academic_break) %>% 
         filter(instruction_days >= input$instruction_days) %>%
         filter(days_fully_open >= input$days_fully_open) %>%
         filter(days_fully_closed <= input$days_fully_closed)
    if(input$schooling_rank != "Any"){
        data <- data[data$schooling_rank == input$schooling_rank,]
    }
    if(input$economic_cat != "Any"){
        data <- data[data$economic_cat == input$economic_cat,]
    }
    if(input$government_cat != "Any"){
        data <- data[data$government_cat == input$government_cat,]
    }
    if(input$stringency_cat != "Any"){
        data <- data[data$stringency_cat == input$stringency_cat,]
    }
     data
    }
))

}

# Run the application 
shinyApp(ui = ui, server = server)

