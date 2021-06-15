library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

data <- read.csv("compensation.csv", header = TRUE)
data$average.compensation <- round(data$average.compensation, 2)

years <- unique(data$year)
job_families <- unique(data$job.family)

# Bar plot function
bar <- function(df) {
  df <- df[c("job.family", "average.compensation")]
  new_df <- df[order(df$average.compensation, decreasing = TRUE), ]
  new_df <- df[1:5, ]

  new_df$label <- paste0(
                    new_df$job.family,
                    "\n Compensation: $",
                    new_df$average.compensation
                  )
  new_df$label_position <- new_df$average.compensation / 2

  ggplot(new_df,
    aes(
      x = job.family,
      y = average.compensation,
      fill = job.family
    )
  ) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_position, label = label)) +
  coord_flip() +
  theme_void() +
  theme(legend.position = "none")
}

# Line plot function
line <- function(df) {
  ggplot(df,
    aes(
      x = year,
      y = average.compensation
    )
  ) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ylab("average compensation") +
  theme_minimal()
}

# UI
ui <- navbarPage(
        theme = shinytheme("united"),
        title = "San Francisco City Employee Salaries",

        tabPanel(
          title = "Dashboard",
          sidebarLayout(
            position = "left",
            sidebarPanel(
              width = 5,
              h4("Average Compensation 2013 — 2020"),
              selectInput(
                inputId = "job",
                label = "Job Family",
                choices = job_families
              ),
              plotOutput("line_chart")
            ),
            mainPanel(
              width = 7,
              h2("Top 5 Job Families by Average Compensation"),
              h5("Compensation is calculated as salary + benefits."),
              sliderInput(
                inputId = "year",
                label = "Year",
                min = 2013,
                max = 2020,
                value = 2013,
                sep = "",
                ticks = FALSE,
                animate = TRUE
              ),
              plotOutput("bar_chart", height = "350px")
            )
          )
        ),

        tabPanel(
          title = "About",

          h2("About"),
          "The original data I used comes from the San Francisco Controller's
          Office and can be found
          ",
          tags$a(href = "https://www.kaggle.com/siddheshera/san-francisco-employee-salary-compensation", "here"),
          "—",
          "this data contained more than 650,000 individual data points, where
          each entry corresponded to a single employee. In order to cut down
          the size of the dataset, I ended up converting the csv into a sqlite
          database and querying prior to use with R.
          ",

          tags$br(),
          tags$br(),

          "The dashboard was made with R Shiny. The code used to produce it can
          be found",
          tags$a(href = "https://github.com/JacobDer/sf-salaries-dashboard", "here"),
          ".",

          h2("Author"),
          "Jacob Der"
        )

      )

# Server
server <- function(input, output) {
  reactive_year <- reactive({
    input$year
  })
  reactive_job <- reactive({
    input$job
  })

  reactive_bar_data <- reactive({
    filter(data, year == reactive_year())
  })
  reactive_line_data <- reactive({
    filter(data, job.family == reactive_job())
  })

  output$bar_chart <- renderPlot({
    bar(reactive_bar_data())
  })
  output$line_chart <- renderPlot({
    line(reactive_line_data())
  })

}

shinyApp(ui, server)
