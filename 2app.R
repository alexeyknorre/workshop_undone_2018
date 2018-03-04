library(shiny)
library(ggvis)

# Load data
df <- read.csv("data/murder_crimes_by_regions.csv", stringsAsFactors = F)

# Define UI
ui <- fluidPage(
  titlePanel("Убийства в России"),
  div(
    conditionalPanel(
      condition = "input.col == 'Обычный график'", ggvisOutput("plot_1")),
    conditionalPanel(
      condition = "input.col == 'График с нормированием'", ggvisOutput("plot_2")),
    align = "center"),
  hr(),
  div(
    radioButtons("col","Переключалка",
                 choices = c("Обычный график", "График с нормированием"),
                 selected = "Обычный график"),
    align = "center"))

# Define server
server <- function(input, output) {
  region_popup <- function(x) {
    region <- df[df$region == x$region, ]
    
    paste0("<b>", as.character(region$region), "</b><br>",
           "Зарегистрировано убийств и покушений: ",region$crimes, "<br>",
           "Население региона: ", format(region$population,
                                         big.mark = " ",
                                         scientific = FALSE)," человек<br>",
           "Площадь региона: ", format(region$area,
                                       big.mark = " ",
                                       scientific = FALSE), " кв.км."
    )
  }
  
  
  df %>% 
    ggvis(~crimes, ~population, key := ~region) %>%
    layer_points() %>%
    add_tooltip(region_popup, "hover") %>%
    add_axis("x", title = "Количество убийств и покушений") %>%
    add_axis("y", title = "Население",title_offset = 80) %>% 
    bind_shiny("plot_1")
  
  df %>% 
    ggvis(~crimes / population * 100000, ~area, key := ~region) %>%
    layer_points() %>%
    add_tooltip(region_popup, "hover") %>%
    add_axis("x", title = "Количество убийств и покушений на 100 000 человек") %>%
    add_axis("y", title = "Площадь региона, в кв. км.",title_offset = 80) %>% 
    bind_shiny("plot_2")
}

# Create Shiny object
shinyApp(ui = ui, server = server)