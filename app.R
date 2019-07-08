# Rshiny app for examining maps of species richness and various environmental variables,
# and conducting simple linear regressions between richness and environmental predictors.

library(shiny)
library(shinyBS)

df = read.table('data/SAM_Western_Hemisphere_1dg_edited.txt', sep = '\t', header = TRUE)

source("helpers.R")

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}




# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Species richness patterns"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      helpText("Data are from Rangel et al. 2010's Spatial Analysis in Macroecology."),
      helpText("Choose the taxonomic group whose diversity you'd like to map, and an environmental predictor."),
      
      radioButtons(inputId = "taxon",
                   label = "Taxonomic group",
                   choices = c("Birds", "Mammals", "Amphibians")),
      
      radioButtons(inputId = "env",
                  label = "Environmental variable",
                  choices = c("Annual Mean Temperature", 
                              "Mean Temperature of Warmest Quarter",
                              "Mean Temperature of Coldest Quarter",
                              "Mean Temperature Diurnal Range",
                              "Isothermality",
                              "Mean Relative Humidity",
                              "Mean Annual Precipitation",
                              "Precipitation Seasonality",
                              "Precipitation in Wettest Quarter",
                              "Precipitation in Driest Quarter",
                              "PET",
                              "AET", 
                              "NDVI", 
                              "Topographical Average",
                              "Topographical Range",
                              "Ecoregion Count")),

    radioTooltip(id = "env", choice = "Annual Mean Temperature", title = "Button 1 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Mean Temperature of Warmest Quarter", title = "Button 2 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Mean Temperature of Coldest Quarter", title = "Button 3 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Mean Temperature Diurnal Range", title = "Button 1 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Isothermality", title = "Button 2 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Mean Relative Humidity", title = "Button 3 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Mean Annual Precipitation", title = "Button 1 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Precipitation Seasonality", title = "Button 2 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Precipitation in Wettest Quarter", title = "Button 3 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Precipitation in Driest Quarter", title = "Button 1 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "PET", title = "Button 2 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "AET", title = "Button 3 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "NDVI", title = "Button 3 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Topographical Average", title = "Button 1 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Topographical Range", title = "Button 2 Explanation", placement = "right", trigger = "hover"),
    radioTooltip(id = "env", choice = "Ecoregion Count", title = "Button 3 Explanation", placement = "right", trigger = "hover")
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      fluidRow( 
        verticalLayout( 
          splitLayout(cellWidths = c("50%", "50%"), plotOutput("envMap"), plotOutput("taxaMap")), 
          plotOutput("scatterPlot"))
      ) 
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
    output$scatterPlot <- renderPlot({
    
      taxon <- switch(input$taxon, 
                      "Birds" = df$Bird_Richness,
                      "Mammals" = df$Mammal_Richness,
                      "Amphibians" = df$Amphibian_Richness)
      
      env <- switch(input$env, 
                    "Annual Mean Temperature" = df$Annual_Mean_Temperature,
                    "AET" = df$AET,
                    "NDVI" = df$NDVI,
                    "Mean Annual Precipitation" = df$Mean_Annual_Precipitation,
                    "Topographical Average" = df$Topographical_Average,
                    "Ecoregion Count" = df$EcoRegions_Count,
                    "Mean Temperature of Warmest Quarter" = df$Mean_Temperature_of_Warmest_Quarter,
                    "Mean Temperature of Coldest Quarter" = df$Mean_Temperature_of_Coldest_Quarter,
                    "Mean Temperature Diurnal Range" = df$Mean_Temperature_Diurnal_Range,
                    "Isothermality" = df$Isothermality,
                    "Mean Relative Humidity" = df$Mean_Relative_Humidity,
                    "Precipitation Seasonality" = df$Precipitation_Seasonality,
                    "Precipitation in Wettest Quarter" = df$Precipitation_in_the_Wettest_Quarter,
                    "Precipitation in Driest Quarter" = df$Precipitation_of_Driest_Quarter,
                    "PET" = df$PET,
                    "Topographical Range" = df$Topographical_Range)
      
      plot(env, taxon, ylab = "Species richness", xlab = input$env, pch = 16)
    
      linmod = lm(taxon~env)
    
      legend("topleft", legend = paste("y =", round(linmod$coefficients[2], 2), "x +", round(linmod$coefficients[1], 2), 
                                     "\nR^2 =", round(summary(linmod)$r.squared, 2)), bty = 'n')
      
      abline(linmod, col = 'red', lwd = 2)
    
  })
  

  output$taxaMap <- renderPlot({
    
    env <- switch(input$env, 
                  "Annual Mean Temperature" = df$Annual_Mean_Temperature,
                  "AET" = df$AET,
                  "NDVI" = df$NDVI,
                  "Mean Annual Precipitation" = df$Mean_Annual_Precipitation,
                  "Topographical Average" = df$Topographical_Average,
                  "Ecoregion Count" = df$EcoRegions_Count,
                  "Mean Temperature of Warmest Quarter" = df$Mean_Temperature_of_Warmest_Quarter,
                  "Mean Temperature of Coldest Quarter" = df$Mean_Temperature_of_Coldest_Quarter,
                  "Mean Temperature Diurnal Range" = df$Mean_Temperature_Diurnal_Range,
                  "Isothermality" = df$Isothermality,
                  "Mean Relative Humidity" = df$Mean_Relative_Humidity,
                  "Precipitation Seasonality" = df$Precipitation_Seasonality,
                  "Precipitation in Wettest Quarter" = df$Precipitation_in_the_Wettest_Quarter,
                  "Precipitation in Driest Quarter" = df$Precipitation_of_Driest_Quarter,
                  "PET" = df$PET,
                  "Topographical Range" = df$Topographical_Range)
    
    varMap(env)
    mtext(input$env, 3)
    
  })

  output$envMap <- renderPlot({
    
    taxon <- switch(input$taxon, 
                    "Birds" = df$Bird_Richness,
                    "Mammals" = df$Mammal_Richness,
                    "Amphibians" = df$Amphibian_Richness)
    
    varMap(taxon)
    mtext("Species richness", 3)
    
    
  })
  
  
}


shinyApp(ui = ui, server = server)