library(shiny)
library(tidyverse)
library(tmap)
library(sf)
library(gapminder)


# Data --------------------------------------------------------------------

mortality <- read_csv(here::here("data/share-of-deaths-by-cause.csv"))
glimpse(mortality)

world_map <- read_sf(here::here("data/world-administrative-boundaries/world-administrative-boundaries.shp")) %>%
  st_zm(drop = TRUE)

new <- gapminder %>%
  mutate(country = tolower(as.character(country)))

countries <- unique(new$country)

mortality_sf <- mortality %>%
  filter(tolower(Entity) %in% countries) %>%
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
  pivot_longer(-c(Entity, Code, Year, continent, geometry),
               values_to = "Mortality_rate_(%)") %>%
  mutate(Disease = str_replace_all(name, c("Deaths - " = "", " - Sex: Both - Age: All Ages \\(Percent\\)" = "")),
         NCD = ifelse(Disease %in% NCD, "Yes", "No"),
         `Mortality_rate_(%)` = round(`Mortality_rate_(%)`, 4)) %>%
  select(-name) %>%
  relocate(`Mortality_rate_(%)`, .after = NCD) %>%
  relocate(geometry, .after = `Mortality_rate_(%)`)

clean_mortality_highest <- clean_mortality %>%
  group_by(Entity, Year) %>%
  mutate(Highest = max(`Mortality_rate_(%)`),
         Cause = ifelse(Highest == `Mortality_rate_(%)`, "Yes", "No")) %>%
  filter(Cause == "Yes") %>%
  select(-c(Cause, Highest))

clean_mortality_highest <- st_as_sf(clean_mortality_highest) %>%
  filter(Year == 2019)

disease_ordered <- clean_mortality_highest %>%
  group_by(Disease) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  pull(Disease)

clean_mortality_highest$Disease <- ordered(clean_mortality_highest$Disease, levels = disease_ordered)


# Shiny app ---------------------------------------------------------------

ui <- fillPage(

  navbarPage("Test", id = "top",

             # Map tab
             tabPanel("World Map",

                      tmapOutput("world", width = "100%", height = 400)

                      ),

             # Exploring tab
             tabPanel("Exploring Disease",



                      ),

             # Information tab
             tabPanel("Information"),

             # About tab
             tabPanel("About")
             ),

)

server <- function(input, output) {

  output$world <- renderTmap({
    tm_shape(clean_mortality_highest) +
      tm_polygons(col = "Disease") +
      tm_basemap(leaflet::providers$CartoDB.VoyagerNoLabels)
  })

}

shinyApp(ui = ui, server = server)
