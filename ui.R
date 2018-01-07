library(shiny)
library(ggvis)


shinyUI(fluidPage(
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
    align = "center")
  )
)

