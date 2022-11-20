library(shiny)
library(tidyverse)
library(tmap)
library(sf)
library(RColorBrewer)
library(plotly)
library(knitr)
library(kableExtra)
library(shinythemes)
library(rsconnect)


# Data --------------------------------------------------------------------

mortality <- read_csv(here::here("data/share-of-deaths-by-cause.csv"))
glimpse(mortality)

world_map <- read_sf(here::here("data/world-administrative-boundaries/world-administrative-boundaries.shp")) %>%
  st_zm(drop = TRUE)

new <- world_map %>%
  mutate(iso3 = tolower(as.character(iso3)))

countries <- unique(new$iso3)

mortality_sf <- mortality %>%
  filter(tolower(Code) %in% countries) %>%
  group_by(Code) %>%
  right_join(world_map, by = c("Code" = "iso3")) %>%
  ungroup()

NCD <- c("Cardiovascular diseases", "Neoplasms", "Chronic respiratory diseases", "Diabetes mellitus")


# Clean -------------------------------------------------------------------

clean_mortality <- mortality_sf %>%
  select(Entity,
         Code,
         Year,
         `Deaths - Cardiovascular diseases - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Neoplasms - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Maternal disorders - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Chronic respiratory diseases - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Digestive diseases - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Diabetes mellitus - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Lower respiratory infections - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Neonatal disorders - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Diarrheal diseases - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Cirrhosis and other chronic liver diseases - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Tuberculosis - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Chronic kidney disease - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Alzheimer's disease and other dementias - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Parkinson's disease - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - HIV/AIDS - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Acute hepatitis - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Malaria - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Nutritional deficiencies - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Meningitis - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Protein-energy malnutrition - Sex: Both - Age: All Ages (Percent)`,
         `Deaths - Enteric infections - Sex: Both - Age: All Ages (Percent)`,
         continent,
         geometry) %>%
  na.omit() %>%
  pivot_longer(-c(Entity, Code, Year, continent, geometry),
               values_to = "Mortality_rate_(%)") %>%
  mutate(Disease = str_replace_all(name, c("Deaths - " = "", " - Sex: Both - Age: All Ages \\(Percent\\)" = "")),
         `Mortality_rate_(%)` = round(`Mortality_rate_(%)`, 4)) %>%
  select(-name) %>%
  relocate(geometry, .after = `Mortality_rate_(%)`)

clean_mortality_highest <- clean_mortality %>%
  group_by(Entity, Year) %>%
  mutate(Highest = max(`Mortality_rate_(%)`),
         Cause = ifelse(Highest == `Mortality_rate_(%)`, "Yes", "No")) %>%
  filter(Cause == "Yes") %>%
  select(-c(Cause, Highest))

clean_mortality_highest <- st_as_sf(clean_mortality_highest) %>%
  filter(Year == 2019)

disease_name <- unique(clean_mortality$Disease)
country_name <- unique(clean_mortality$Entity)
continent_name <- unique(clean_mortality$continent)

my_palette <- c("#ff6961", "#7fc97f", "#beaed4", "#ffff99", "#386cb0", "#bf5b17", "#fdc086")


# Shiny app ---------------------------------------------------------------

# UI ----------------------------------------------------------------------

ui <- fillPage(

  navbarPage("Disease Library", id = "top",

             theme = shinytheme("cerulean"),

             # Map tab
             tabPanel("World Map", icon = icon("earth-americas"),

                      div(class = "outer",

                        tmapOutput("world", width = "100%", height = "100%"),

                        absolutePanel(id = "fact", fixed = TRUE, draggable = TRUE, top = "auto",
                                      left = 30, right = "auto", bottom = 300, width = 330,
                                      height = "auto",

                                      h4("Did you know?"),

                                      p("In 2019, before communicable diseases like COVID-19 struck, the disease that causes the highest number of deaths in the world is Cardiovascular disease, also known as conditions affecting the heart or blood vessels")

                                      ),

                        tags$div(id = "disclaimer",
                                  "Disclaimer: Some countries do not include in this data."
                                )

                      )
                      ),

             # Disease Information tab
             tabPanel("Disease Information", icon = icon("user-doctor"),

                      fluidRow(

                        column(4,


                               tags$div(id = "diseaseinput",
                                        selectInput(
                                          inputId = "disease",
                                          label = "Which disease do you want to learn more about?",
                                          choices = disease_name
                                          )
                                        )

                               ),

                        column(5,

                               selectInput(
                                 inputId = "country",
                                 label = "Which country?",
                                 choices = country_name
                               )

                               ),

                        column(3,

                               sliderInput(
                                 inputId = "year",
                                 label = "Which year?",
                                 min = min(clean_mortality$Year),
                                 max = max(clean_mortality$Year),
                                 value = 2019,
                                 sep = ""
                               )

                               ),

                      ),

                      fluidRow(
                        column(4,

                                 htmlOutput("information")

                        ),
                        column(5,

                               mainPanel(
                                 htmlOutput("title_time")
                               ),

                               plotlyOutput("time_series", height = 500)

                        ),
                        column(3,

                               mainPanel(
                                 htmlOutput("title_rank")
                               ),

                               htmlOutput("ranking", height = 500)

                        )

                      )
             ),

             # Information tab
             tabPanel("Key Takeaway", icon = icon("leanpub"),

                      fluidRow(

                        column(6, offset = 1,

                               selectInput(
                                 inputId = "continent",
                                 label = "Which continent do you want to see?",
                                 choices = continent_name
                               )

                               )

                      ),

                      fluidRow(

                        column(4,

                               tmapOutput("continentmap", height = 500)

                        ),

                        column(5,

                               plotOutput("barplot", height = 500)

                        ),

                        column(3,

                               tags$div(id = "information3",

                                   htmlOutput("takeaway"),

                                   htmlOutput("continentkey")

                                   )


                        )

                      )

                      ),

             # About tab
             tabPanel("About", icon = icon("circle-info"),

                      uiOutput("about")

                      )

             ),

  includeCSS("styles.css")

)


# SERVER ------------------------------------------------------------------

server <- function(input, output) {

  # Title
  output$title_time <- renderText(
    "<h4><b>Mortality Rate Over Time</b></h4>"
  )

  output$title_rank <- renderText(
    "<h4><b>Disease Ranking (mortality rate)</b></h4>"
  )


  # Map output
  output$world <- renderTmap({

    tm_shape(clean_mortality_highest) +
      tm_polygons(col = "Disease", palette = my_palette) +
      tm_basemap(leaflet::providers$CartoDB.VoyagerNoLabels)

  })


  # Disease Information output
  observe({
    req(input$disease)

    if(input$disease == "Cardiovascular diseases") {
      output$information <- renderText(read_file("disease/cardiovascular.txt"))
    }

    if(input$disease == "Neoplasms") {
      output$information <- renderText(read_file("disease/neoplasms.txt"))
    }

    if(input$disease == "Maternal disorders") {
      output$information <- renderText(read_file("disease/maternal.txt"))
    }

    if(input$disease == "Chronic respiratory diseases") {
      output$information <- renderText(read_file("disease/chronic_respiratory_disease.txt"))
    }

    if(input$disease == "Digestive diseases") {
      output$information <- renderText(read_file("disease/digestive.txt"))
    }

    if(input$disease == "Diabetes mellitus") {
      output$information <- renderText(read_file("disease/diabetes.txt"))
    }

    if(input$disease == "Lower respiratory infections") {
      output$information <- renderText(read_file("disease/lower_respiratory.txt"))
    }

    if(input$disease == "Neonatal disorders") {
      output$information <- renderText(read_file("disease/neonatal.txt"))
    }

    if(input$disease == "Diarrheal diseases") {
      output$information <- renderText(read_file("disease/diarrheal.txt"))
    }

    if(input$disease == "Cirrhosis and other chronic liver diseases") {
      output$information <- renderText(read_file("disease/cirrhosis.txt"))
    }

    if(input$disease == "Tuberculosis") {
      output$information <- renderText(read_file("disease/tuberculosis.txt"))
    }

    if(input$disease == "Chronic kidney disease") {
      output$information <- renderText(read_file("disease/chronic_kidney.txt"))
    }

    if(input$disease == "Alzheimer's disease and other dementias") {
      output$information <- renderText(read_file("disease/alzheimer.txt"))
    }

    if(input$disease == "Parkinson's disease") {
      output$information <- renderText(read_file("disease/parkinson.txt"))
    }

    if(input$disease == "HIV/AIDS") {
      output$information <- renderText(read_file("disease/hiv_aids.txt"))
    }

    if(input$disease == "Acute hepatitis") {
      output$information <- renderText(read_file("disease/acute_hepatitis.txt"))
    }

    if(input$disease == "Malaria") {
      output$information <- renderText(read_file("disease/malaria.txt"))
    }

    if(input$disease == "Nutritional deficiencies") {
      output$information <- renderText(read_file("disease/nutritional_deficiencies.txt"))
    }

    if(input$disease == "Meningitis") {
      output$information <- renderText(read_file("disease/meningitis.txt"))
    }

    if(input$disease == "Protein-energy malnutrition") {
      output$information <- renderText(read_file("disease/protein_energy.txt"))
    }

    if(input$disease == "Enteric infections") {
      output$information <- renderText(read_file("disease/enteric.txt"))
    }

  })


  # Time Series Plot
  observe({
    req(input$disease, input$country)

    output$time_series <- renderPlotly({

      ggplotly(clean_mortality %>%
                 filter(Entity == input$country,
                        Disease == input$disease) %>%
                 ggplot(aes(x = Year, y = `Mortality_rate_(%)`)) +
                 geom_line(color = "red") +
                 geom_point(color = "red") +
                 theme_bw() +
                 labs(x = "Year", y = "Mortality Rate (Percentage)") +
                 theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))
               )

    })

  })


  # Ranking Table
  observe({
    req(input$country, input$year)

    output$ranking <- renderText({

      clean_mortality %>%
        filter(Entity == input$country,
               Year == input$year) %>%
        group_by(Disease) %>%
        summarise(Mortality_rate = mean(`Mortality_rate_(%)`), .groups = "drop") %>%
        arrange(-Mortality_rate) %>%
        select(-Mortality_rate) %>%
        mutate(Rank = paste0(1:21,".")) %>%
        relocate(Disease, .after = Rank) %>%
        head(10) %>%
        kable() %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

      })

  })


  # Continent Map
  observe({
    req(input$continent)

    if(input$continent == "Asia" || input$continent == "Africa"){
      output$continentmap <- renderTmap({

        tm_shape(clean_mortality_highest %>%
                   filter(continent == input$continent)) +
          tm_polygons(col = "#ABBAD7") +
          tm_basemap(leaflet::providers$CartoDB.VoyagerNoLabels)
        })

    }


    if(input$continent == "Europe"){
      output$continentmap <- renderTmap({

        tm_shape(clean_mortality_highest %>%
                   filter(continent == input$continent)) +
          tm_polygons(col = "#ABBAD7") +
          tm_basemap(leaflet::providers$CartoDB.VoyagerNoLabels) +
          tm_view(set.view = c(87,61,2))
      })

    }

    if(input$continent == "Oceania"){
      output$continentmap <- renderTmap({

        tm_shape(clean_mortality_highest %>%
                   filter(continent == input$continent)) +
          tm_polygons(col = "#ABBAD7") +
          tm_basemap(leaflet::providers$CartoDB.VoyagerNoLabels) +
          tm_view(set.view = c(155,-29,3))
      })

    }

    if(input$continent == "Americas"){
      output$continentmap <- renderTmap({

        tm_shape(clean_mortality_highest %>%
                   filter(continent == input$continent)) +
          tm_polygons(col = "#ABBAD7") +
          tm_basemap(leaflet::providers$CartoDB.VoyagerNoLabels) +
          tm_view(set.view = c(-70,0,1))
      })

    }

  })


  # Bar Plot
  observe({
    req(input$continent)

    output$barplot <- renderPlot({

      clean_mortality %>%
        filter(continent == input$continent,
               Year == 2019) %>%
        group_by(Disease) %>%
        summarise(Mortality_rate = mean(`Mortality_rate_(%)`), .groups = "drop") %>%
        arrange(-Mortality_rate) %>%
        ggplot(aes(x = reorder(Disease, Mortality_rate), y = `Mortality_rate`)) +
        geom_bar(stat = "identity", fill = "red") +
        theme_bw() +
        labs(x = "", y = "Mortality Rate (Percentage)") +
        theme(axis.text = element_text(size = 15)) +
        coord_flip()

      })

  })


  # Key Takeaway
  output$takeaway <- renderText({read_file("key/takeaway.txt")})

  observe({
    req(input$continent)

    if(input$continent == "Asia") {
      output$continentkey <- renderText({read_file("key/asia.txt")})
    }

    if(input$continent == "Europe"){
      output$continentkey <- renderText({read_file("key/europe.txt")})
    }

    if(input$continent == "Africa"){
      output$continentkey <- renderText({read_file("key/africa.txt")})
    }

    if(input$continent == "Americas" || input$continent == "Oceania"){
      output$continentkey <- renderText({read_file("key/america_oceania.txt")})
    }

  })


  # About
  output$about <- renderUI({

    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()

  })



}



shinyApp(ui = ui, server = server)

