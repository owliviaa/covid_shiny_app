library(dplyr)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(plotly)
library(ggplot2)
library(rjson)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sp)
library(raster)
library(rjson)
library(stringr)

### Lineage map settings
load('data/world_spdf3.RData')
### Map settings
load('data/geodata.RData')
### Loading landing page plot
load('data/landing.RData')
### load main page supplement data
load("data/main_sum_supp.RData")


choice1 = c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", 
            "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bahrein", "Baja California", "Bangladesh", 
            "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bonaire", "Bosnia and Herzegovina", 
            "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", 
            "Canary Islands", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Costa Rica", "Croatia", "Cuba", 
            "Curacao", "Cyprus", "Czech Republic", "Côte d'Ivoire", "D.R.C., Denmark", "Djibouti", "Dominican Republic", 
            "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", 
            "French Guiana", "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Guadeloupe", 
            "Guam", "Guatemala", "Guinea", "Guinea Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong", "Hungary", "Iceland", "India", 
            "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kosovo", 
            "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", 
            "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", 
            "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands", 
            "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", 
            "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Puerto Rico", "Qatar", "Republic of the Congo", 
            "Romania", "Russia", "Rwanda", "Saint Barthélemy", "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines", 
            "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Sint Maarten", "Slovakia", 
            "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", 
            "Sweden", "Switzerland", "Syria", "Taiwan", "Thailand", "The Netherlands", "Timor-Leste", "Togo", "Trinidad and Tobago", "Tunisia", 
            "Turkey", "USA", "Uganda", "Ukraine", "Union of the Comoros", "United Arab Emirates", "United Kingdom", "Uruguay", "Uzbekistan", 
            "Vanuatu", "Venezuela", "Vietnam", "Zambia", "Zimbabwe")

row_sum <- samples_month_country[, -1]
# Transpose the dataframe
df_transposed <- t(row_sum)

# Calculate column-wise sum
column_sum <- rowSums(df_transposed)

# Convert column-wise sum to a dataframe
column_sum_df <- data.frame(Sum = column_sum)
# Create a dataframe for plotting
plot_data <- data.frame(Month = rownames(column_sum_df), Sum = column_sum)
#Import Data





button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI
ui <- fluidPage(

#Navbar structure for UI
  navbarPage("Olivia COVID Situations",
             tabPanel("Main", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      fluidRow(
                        
                        h1(p("Interpreting the COVID-19 data from GISAID")),
                        hr(),
                        h3(p("Introduction:")),
                        h4(p("The emergence of the novel coronavirus, SARS-CoV-2, and the 
                             global pandemic of Coronavirus Disease 2019 (COVID-19) 
                             has caused a global crisis which imacting hundreds of thousand of families all over
                             the world. Lots of families lost their relatives and close friends. Since its identification in December 2019 in Wuhan, China, 
                             the virus has spread rapidly across the globe, leading to millions of 
                             infections. The economic, and public health repercussions of COVID-19 underscore 
                             the urgent need for comprehensive analysis, researches and interventions to mitigate its 
                             spread and impact. So it is very important to understand it.")),
                        
                        h3(p("Data:")),
                        h4(p("The data were download from GISAID consist with sequencing samples collected all over the world.
                             The data contains sample ID, where it was collected and when it was collected. ")),
                        h3(p("Findings:")),
                        h4(p("Let's take a look at the data first. First is the samples distributions:")),
                        leafletOutput("map_main") %>% withSpinner(color="#4169e1", type = 4),
                        h4(p("We can find out the fact that, The crimson part representing the most samples. So we can
                        see that the north america and europe facilities, companies or institutes uploaded the 
                             most of the sequencing data.")),
                        leafletOutput('map_main_us') %>% withSpinner(color="#4169e1", type = 4),
                        h4(p("As of the situation in the US I showed above, seems the state of California, Texas, and New York uploaded
                             the most of the data. followed with the state of Washington, Massachusetts, Colorado and Florida.")),
                        h4(p("On the other hand, these sample distributions can partially show the number of patients distribution around the
                             globe except for some countries.")),
                        plotOutput("line") %>% withSpinner(color="#4169e1", type = 4),
                        h4(p("Let us take another look at the monthly reported data above. Through the dot plot, we can found that there are
                             couple of peaks near 2020-03, 2021-03, and 2021-11. Why is that? After I did some research online, the first peak
                             at 2020-03 is because of the first outbreak starting from China. The second is because of the migration of delta and
                             the third one is because of the omicron strand. This indicates the data can refelct the real-world situation.")),
                        h4(p("And below is the data table for the samples uploaded regarding each country:")),
                        dataTableOutput("main_sum_table") %>% withSpinner(color="#4169e1", type = 4),
                        h4(p("The box at the up left corner is the setting for showing how many rows you want to fit at the table.")),
                        # Select input for filtering data
                        h4(p("The Select input was added so that we can easily choose the country we interested so that we can take a deeper look
                             at this country for better understanding.")),
                        selectInput("filter", "Select Interested Country:", choices = choice1, selected = "Australia"),
                        DT::dataTableOutput("main_sum_table2") %>% withSpinner(color="#4169e1", type = 4),
                        h4(p("For instance, if we search Australia then we can find out that from the table there is a peak at 
                             2020-03 and 2020-07, which fit the overall situation at the global level.")),
                        plotOutput("dot_plot") %>% withSpinner(color="#4169e1", type = 4),
                        h3(p("Conclusion:")),
                        h4(p("In conclusion, this app provides a comprehensive overview of the global distribution and analysis of 
                             COVID-19 samples. Through interactive visualizations and data exploration tools, users gain insights into 
                             the spatial and temporal trends of sample collection, allowing for a deeper understanding of the pandemic's 
                             spread and impact worldwide. The analysis reveals notable patterns, such as variations in sampling intensity 
                             across regions and over time, highlighting areas of heightened surveillance and potential hotspots for 
                             targeted intervention efforts. Furthermore, the identification of demographic and epidemiological factors 
                             associated with sample collection aids in informing public health strategies and resource allocation for 
                             effective pandemic response. As the global community continues to navigate the complexities of the COVID-19 
                             pandemic, this can serves as a valuable resource for researchers, policymakers, and the general public in 
                             monitoring and addressing the ongoing challenges posed by the virus.")),
                        
                        
                        
                        )
             ),

  navbarMenu("More", icon = icon("info-circle"),

               tabPanel("About (GitHub link here)", fluid = TRUE,
               fluidRow(
               column(6,
                      #br(),
                      h4(p("Acknowledgement:")),
                      h5(p("Khare, S., et al (2021) GISAID’s Role in Pandemic Response. China CDC Weekly, 3(49): 1049-1051. doi: 10.46234/ccdcw2021.255 PMCID: 8668406")),
                         br(),
                         h5(p("Elbe, S. and Buckland-Merrett, G. (2017) Data, disease and diplomacy: GISAID’s innovative contribution to global health. Global Challenges, 1:33-46. doi: 10.1002/gch2.1018 PMCID: 31565258")),
                         br(),
                         h5(p("Shu, Y. and McCauley, J. (2017) GISAID: from vision to reality. EuroSurveillance, 22(13) doi: 10.2807/1560-7917.ES.2017.22.13.30494 PMCID: PMC5388101"))

                      #hr(),

               ),
               column(6,
                      #br(),
                      h4(p("About the Author")),
                      h5(p("Olivia Zhang")),
                      HTML("Click <a href='https://github.com/owliviaa/covid_shiny_app/blob/main/app.R'>here</a> to visit the GitHub Code Page."),
          br()
                          )
          ),
          br()
                        )
  )
)
)

# Define server
server <- function(input, output, session) {

  
  bins <- c(1, 5000, 10000, 50000, Inf)
  pal <- colorBin("YlOrRd", domain = glob$postal, na.color = "transparent", bins = bins)
  labelsss <- c("1 - 5,000", "5,000 - 10,000", "10,000 - 50,000", "50,000+")
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%s SNV(s)",
    glob$name, glob$postal
  ) %>% lapply(htmltools::HTML)
  
  ## Landing Page Map
  output$map_main <- renderLeaflet({
    leaflet(world_main) %>%
      addProviderTiles(providers$OpenStreetMap, options = tileOptions(minZoom=1, maxZoom=8)) %>%
      addPolygons(
        fillColor = ~pal_main(POP2005),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.9,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = customLabel_main_glob,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        layerId = ~name) %>%
      addLegend(colors = c("#fafcc7","#f8ac74","#ef7260","#cb4964"), opacity=1, title = "Number of samples collected", position = "bottomright", labels = labelsss)
    
  })
  
  ## Map US
  output$map_main_us <- renderLeaflet({
    leaflet(states_main) %>%
      setView(-96, 37.8, 3) %>%
      addProviderTiles(providers$OpenStreetMap, options = tileOptions(minZoom=2, maxZoom=8)) %>%
      addPolygons(
        fillColor = ~pal_states_main(density),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.9,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE),
        label = labels_states_main,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(colors = c("#fafcc7","#f8ac74","#ef7260","#cb4964"), opacity=1, title = "Number of samples collected", position = "bottomright", labels = labelsss)
  })
  
  output$line <- renderPlot({
    ggplot(plot_data, aes(x = Month, y = Sum)) +
      geom_line() +
      geom_point() +
      labs(title = "Sum of Each Column by Month",
           x = "Month",
           y = "Sum") +
      theme_minimal()
  })
  
  ## Main info
  output$main_sum_table <- renderDataTable({
    samples_month_country
  }, options = list(scrollX = TRUE,searching = FALSE))
  
  ## Main info
  output$main_sum_table2 <- renderDataTable({
    dat <- samples_month_country %>% filter(Country == input$filter)
    dat
  }, options = list(scrollX = TRUE,searching = FALSE))
  
  ## Main info
  output$dot_plot <- renderPlot({
    dat <- samples_month_country %>% filter(Country == input$filter)
    
    # Convert the data from wide to long format
    dat_long <- pivot_longer(dat, cols = -Country, names_to = "Month", values_to = "Count")
    
    # Create a dot plot using ggplot2
    ggplot(dat_long, aes(x = Month, y = Count, color = "red")) +
      geom_point() +
      labs(title = paste("Dot Plot of Sample Counts for", input$filter),
           x = "Month",
           y = "Sample Count")
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)

