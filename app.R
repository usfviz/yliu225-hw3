setwd("~/MSAN/SpringModule2/622Visual/yliu225-hw3")

library(dplyr)
library(ggvis)
library(shiny)
library(ggplot2)
library(plotly)
library(GGally)

# fb <- read.table("dataset_Facebook.csv", sep = ";", header = TRUE)
# diabetic <- read.csv("diabetic_data.csv")

# unique(diabetic$race)
# unique(diabetic$gender)
# unique(diabetic$age)[1]
# typeof(unique(diabetic$age)[1])
# length(unique(diabetic$patient_nbr))
# unique(diabetic$admission_type_id)




# df_bubble <- aggregate(diabetic[c("time_in_hospital", "num_medications")], by = list(diabetic$race, diabetic$age), mean)
# colnames(df_bubble) <- c("race", "age", "avg_time_in_hospital", "avg_num_medications")
# df_bubble <- subset(df_bubble, race != "?")

#write.table(df_bubble, file = "df_bubble.csv", sep = ",", row.names = FALSE)


ui <- fluidPage(
  headerPanel('Bubble Plot'),
  sidebarPanel(
    
    radioButtons("race", "Race",
                 choices = list("AfricanAmerican" = "AfricanAmerican",
                                "Caucasian" = "Caucasian",
                                "Asian" = "Asian",
                                "Hispanic" = "Hispanic",
                                "Other" = "Other",
                                "All" = "all"
                                ),
                 selected = "all"
    )
  ),
  
  mainPanel(
    
    tabsetPanel(
                tabPanel("Bubble Plot", uiOutput("ggvis_ui"),
                ggvisOutput("ggvis")),
                tabPanel("Parallel Coordinate Plot", plotlyOutput("parallel_plot")),
                tabPanel("Scatterplot Matrix", plotlyOutput("scatter_plot"))
                )
  
    )
)



server <- function(input, output) {
  
  selected_race <- reactive(input$race)
  df <- read.csv("df_bubble.csv")
  
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[(df$race == x$race & df$avg_num_medications == x$avg_num_medications), ]
    paste0("Race: ", row$race, "</br>", 
           "Age: ", row$age, "</br>"
           )
  }
  
  
  df$radius <- (df$avg_num_medications)^3 / 5
  
  # add_opacity <- function(x) {
  #   if (selected_race() == "all") {0.5} else {
  #     if (x == selected_race()) {0.5} else {0.2}
  #   }
  # }
  
  add_opacity <- function (x) {
    ifelse(x==selected_race() | selected_race()=="all", 0.6, 0.1)
  }
  
  # df$opacity <- lapply(df$race, add_opacity)
  
  # df_sub <- reactive({df %>%
  #     # filter(race==selected_race()) %>%
  #     arrange(desc(avg_num_medications)) %>%
  #     
  #     mutate(opacity = ifelse(race==selected_race() | selected_race()=="all", 0.6, 0.1))
  #   })
  
  
  df_sub <- reactive({df %>%
      # filter(race==selected_race()) %>%
      arrange(desc(avg_num_medications)) %>%
      
      mutate(opacity = ifelse(race==selected_race() | selected_race()=="all", 0.5, 0.1))
  })
  
  
  
  df_sub %>% ggvis(~avg_num_medications, ~avg_time_in_hospital, fill = ~race) %>% 
         add_tooltip(all_values, "hover") %>%
         layer_points(size := ~200 , fillOpacity := ~opacity, strokeOpacity := ~0.5,
                      size.hover := ~radius+190,
                      strokeWidth.hover := 1.5, 
                      fillOpacity.hover := 1.0, strokeOpacity.hover := 1.9) %>% 
         add_axis("x", title="Average number of medications") %>%
         add_axis("y", title="Average time in hospital") %>%
         scale_numeric("y", domain = c(2, 6), nice = FALSE) %>%
         add_legend(c("fill")) %>%
         bind_shiny("ggvis", "ggvis_ui")
  
  
  diabetic <- read.csv("df_bubble.csv")
  
  output$parallel_plot <- renderPlotly({
         ggplotly(ggparcoord(diabetic, columns = 2:4, groupColumn = 1))
    
  
    
  })
  
  
  fb <- read.csv("dataset_Facebook.csv", sep = ";", header = TRUE)
  
  fb <- fb[, c("Lifetime.Engaged.Users", "comment", "like", "share")]
  
  output$scatter_plot <- renderPlotly({
                  ggplotly(ggpairs(fb, colour='cut'))
    
  })
  
}

shinyApp(ui = ui, server = server)




