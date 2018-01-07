library(shiny)
library(ggvis)

df <- read.csv("murder_crimes_by_regions.csv", stringsAsFactors = F)

shinyServer(function(input, output) {
  
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
  
  
})