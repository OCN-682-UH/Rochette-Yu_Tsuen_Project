# Title : Final Project - Oceanographic Data
# Created by: Keanu Rochette
# Date Creation: 2024-11-29
############################

# Load Libraries
library(shiny)
library(tidyverse)
library(here)
library(shinythemes)


#Load the Data
fcm <- read_csv(here("data", "fcm.csv"))
CTD <- read_csv(here("data", "CTD.csv"))
CTD_FCM <- read_csv(here("data", "CTD_FCM.csv"))
fcm_long <- read_csv(here("data", "fcm_long.csv"))
# Define UI 
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  titlePanel("🌊 Oceanographic Data: a Story of the Ocean 🌊"),
  
  h5("Created by: Keanu Rochette-Yu Tsuen"),
  h5("Date: 2024-11-29"),
  
  h3("🚢 Sampling Map to the West of Oahu 🚢"),
  
  # Add an image to the shiny app
  img(src ="cast_oahu.png", height="50%", width="50%", align = "center"),
  
  h5("In September 2023, a cruise onboard the R/V Kilo Moana set sails to the 
     West of Oahu to investigate the magnitude of the island Mass Effect."),
  
  # First row displays the CTD data, physical parameters 
  h3("🧪 Physical Parameter Profile 🧪"),
  
  fluidRow(
    column(3,
           
           #adding drop down menu        
           selectInput(inputId = "station", 
                       label = h5("Select a Station"), 
                       choices = unique(sort(CTD$station)), 
                       selected = "A"),
           
           #adding checkboxes  
           checkboxGroupInput( 
             inputId = "parameter", 
             label = h5("Oceanographic Parameters"), 
             c( 
               "Temperature (°C)" = "temp", 
               "PAR" = "PAR", 
               "Pressure (dbar)" = "pressure",
               "Oxygen (umol/kg)" = "oxygen_conc",
               "Oxygen (% saturation)"= "perc_ox_sat",
               "Fluorescence" = "fluorescence"))),
    
    column(9,
           # Profile plot space 
           plotOutput('profile'))),
  
  # 2nd row displays the FCM data, microbial profiles
  
  h3("🔬 Microbial Profile 🦠"),
  
  fluidRow(
    column(3,
           #adding drop down menu  
           selectInput(inputId = "site", 
                       label = h5("Select a Station"), 
                       choices = unique(sort(fcm$site)), 
                       selected = "A"),
           #adding checkboxes 
           checkboxGroupInput( 
             inputId = "microbe", 
             label = h5("Select a Microbe"), 
             c( 
               "Prochlorococcus" = "pro_rel_abun",
               "Synechococcus" = "syn_rel_abun",
               "Pico Eukaryote" = "pico_euk_rel_abun",
               "Heterotrophic Bacteria" = "hbac_rel_abun"
             ))),
    
    column(9, 
           #microbe profile plot space
           plotOutput("microbe_plot"))),
  
  # 3rd row displays the CTD and FCM data, correlations btwn both
  h3("📈 Correlation between Physics and Biology 📉"),
  
  fluidRow(
    column(3,
           #adding drop down menu  
           selectInput(inputId = "parameter_corr", 
                       label = h5("Select a Parameter"), 
                       choices = unique(sort(CTD_FCM$parameter)), 
                       selected = "pressure"),
           #adding drop down menu  
           selectInput(inputId = "microbe_corr", 
                       label = h5("Select a Microbe"), 
                       choices = unique(sort(CTD_FCM$microbe)), 
                       selected = "Prochlorococcus")),
    
    column(9, 
           #correlation plot space
           plotOutput("correlation_plot"))),
  
  # 4th row displays violin plots of the relative abundance of microbes by site
  
  h3("📊 Relative Abundance Distribution 📊"),
  
  fluidRow(
    column(3,
           #adding drop down menu  
           selectInput(inputId = "rel_abun", 
                       label = h5("Select a Microbe"), 
                       choices = unique(sort(fcm_long$microbes)), 
           selected = "Prochlorococcus")),
    
    column(9, 
           #violin plot space
           plotOutput("violin_plot")))
)



# Define server 
server <- function(input, output) {
  
  ## CTD Graph
  ### Indicates reactive elements in the code
  CTD_data <- reactive({
    CTD %>% 
      mutate(station = as.factor(station)) %>% 
      filter(station == input$station)
    
  })
  ### coding the profile plot to render 
  output$profile <- renderPlot({
    
    profile <- CTD_data() %>%
      ggplot(aes(x = depth_cat* -1)) 
    
    #### If no check boxes are selected, plot a blank plot
    if (is.null(input$parameter)) {
      profile <- ggplot(data = data.frame(x = 1, y = 1)) +
        geom_blank() + coord_flip()
      
    } else {
      #### Define color mapping for each parameter
      color_mapping <- c(
        "temp" = "#FF9800",
        "PAR" = "#FFE082",
        "pressure" = "#607D8B",
        "oxygen_conc" = "#4DB6AC",
        "perc_ox_sat" = "#4DD0E1",
        "fluorescence" = "#4CAF50"
      )
      
      #### Loop through selected parameters and add layers dynamically
      for (param in input$parameter) {
        profile <- profile +
          geom_point(aes_string(y = param), color = color_mapping[[param]]) +
          geom_line(aes_string(y = param), color = color_mapping[[param]])
      }
    }
    
    ### Return the plot
    profile <- profile + 
      labs( title= "Oceanographic Profile",
            x = "Depth (m)",
            y = "Variables") +
      coord_flip()+
      theme_bw() + 
      theme(plot.title = element_text(size=14, face = "bold"), 
            plot.subtitle = element_text(size=12),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            strip.text.x = element_text(size = 12, face = "bold"),
            legend.title=element_text(size=12, face = "bold"),
            legend.text=element_text(size=12),
            panel.background = element_rect(fill = "azure1"))
    
    profile
  })
  
  ## FCM profile graph
  ### Indicates reactive elements in the code
  FCM_data <- reactive ({
    fcm %>% 
      mutate(depth = as.numeric(depth)) %>% 
      mutate(site = as.factor(site)) %>% 
      filter(site == input$site)
    
  })
  
  ### coding the microbe profile plot to render   
  output$microbe_plot <- renderPlot({
    
    microbe_plot <- FCM_data() %>%
      ggplot(aes(x = depth * -1)) 
    
    #### If no check boxes are selected, plot a blank plot
    if (is.null(input$microbe)) {
      microbe_plot <- ggplot(data = data.frame(x = 1, y = 1)) +
        geom_blank() + coord_flip()
      
    } else {
      #### Define color mapping for each parameter
      color_mapping <- c(
        "pro_rel_abun" = "#66BB6A",
        "syn_rel_abun" = "#2E7D32",
        "pico_euk_rel_abun" = "#0288D1",
        "hbac_rel_abun" = "#673AB7"
      )
      
      #### Loop through selected parameters and add layers dynamically
      for (param in input$microbe) {
        microbe_plot <- microbe_plot +
          geom_point(aes_string(y = param), color = color_mapping[[param]]) +
          geom_line(aes_string(y = param), color = color_mapping[[param]])
      }
    }
    
    ### Return the plot
    microbe_plot <- microbe_plot + 
      labs( title = "Microbial Profile",
            x = "Depth (m)",
            y = "Relative Abundance (%)") +
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      theme_bw() + 
      theme(plot.title = element_text(size=14, face = "bold"), 
            plot.subtitle = element_text(size=12),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            strip.text.x = element_text(size = 12, face = "bold"),
            legend.title=element_text(size=12, face = "bold"),
            legend.text=element_text(size=12),
            panel.background = element_rect(fill = "azure1"))
    
    microbe_plot
  })
  
  ## Correlation plot
  ### Indicates reactive elements in the code  
  CTD_FCM_data <- reactive({
    CTD_FCM %>% 
      filter(microbe == input$microbe_corr) %>% 
      filter(parameter == input$parameter_corr)
  })
  
  ### coding the correlation plot to render  
  output$correlation_plot <- renderPlot({
    
    correlation <- CTD_FCM_data()  %>% 
      ggplot(aes(x= values, y = rel_abun)) +
      geom_point()+
      geom_smooth(method = "lm")+
      labs(title = paste0("Correlation between ", input$parameter_corr,
                          " and ", input$microbe_corr),
           x = paste0(input$parameter_corr),
           y= paste0(input$microbe_corr, " (% abundance)"))+
      scale_y_continuous(labels = scales::percent)+
      theme_bw() + 
      theme(plot.title = element_text(size=16, face = "bold"), 
            plot.subtitle = element_text(size=15), 
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 14),
            legend.position = "none",
            panel.background = element_rect(fill = "azure1"))
    
    correlation
  })
  
  ## Violin plot
  ### Indicates reactive elements in the code  
  fcm_violin <- reactive({
    fcm_long %>% 
      filter(microbes == input$rel_abun) 
  })
  
  ### coding the violin plot to render   
  output$violin_plot <- renderPlot({
    
    violin_plot <- fcm_violin() %>% 
      ggplot(aes(x= site, y = rel_abun, fill = site))+
      geom_violin()+
      #I wanted to add a boxplot within the violin plot but need to make it smaller and discreet
      geom_boxplot(width=0.1, color="white", alpha=0.2)+
      labs(title = paste("Distribution of the Relative Abundance of",
                         input$rel_abun, "by Station"),
           x = "Station",
           y = "Relative Abundance (%)")+
      scale_y_continuous(labels = scales::percent)+
      theme_bw() +
      theme(plot.title = element_text(size=14, face = "bold"), 
            plot.subtitle = element_text(size=12),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(),
            panel.background = element_rect(fill = "azure1"),
            legend.position = "none")+
      scale_fill_manual(values = c("#67B8D6","#E9D097","#1C77A3","#C5A388"))
    
    violin_plot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
