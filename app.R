#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(googleCloudStorageR) 
library(kippcolors)
library(dplyr)
library(DT)

gcs_global_bucket("idea_map")
gcs_load(file = "map.rda")
# map_records <- reactivePoll(60000, 
#                             session, 
#                             checkFunc = function() {
#                               x <- gcs_list_objects()
#                               x$updated
#                             }, 
#                             valueFunc = function() {
#                               gcs_load(file = "map.rda")
#                             }
# )

# Define UI for application that draws a histogram
ui <- function(request) {
  library(shinydashboard)
  library(shinycssloaders)
  library(kippcolors)
  library(magrittr)
  
  dashboardPage(
    dashboardHeader(title = "NWEA MAP"),
    
    dashboardSidebar(
      bookmarkButton(),
      sidebarMenu(id = "tabs",
        menuItem("Results over time", 
                 tabName = "long_plot", 
                 icon = icon("university"),
                 selected = TRUE
                 ), # END menuItem
        menuItem("Number Students Tested", tabName = "hr", icon = icon("home")) 
      ),
      
      # School selector
      selectInput(
        inputId = "school",
        label = "Schools:",
        choices = c("Ascend", "Academy", "Bloom", "One"),
        selected = c("Ascend", "Academy", "Bloom", "One"),
        multiple = TRUE
      ),
      
      # subject selector
      radioButtons(
        inputId = "subject",
        label = "Subjects:",
        choices = c("Mathematics", "Reading", "General Science"),
        selected = "Mathematics"
      ),
      
      # growth window selector
      selectInput(
        inputId = "growth_window",
        label = "Growth Window:",
        choices = c("Spring to Spring",
                    "Spring to Winter",
                    "Fall to Spring",
                    "Fall to Winter",
                    "Fall to Fall",
                    "Winter to Winter",
                    "Winter to Spring"),
        selected = "Spring to Spring",
        multiple = FALSE
      ),
      
      # metric selector
      selectInput(
        inputId = "metric",
        label = "Metric",
        choices = c("% M/E Typical Growth" = "pct_typical",
                    "% M/E CR Growth" = "pct_accel_growth",
                    "% Negative RIT Change" = "pct_negative",
                    "% ≥ 50th Pctl" = "end_pct_50th_pctl",
                    "% ≥ 75th Pctl" = "end_pct_75th_pctl",
                    "Mean RIT Score" = "end_mean_testritscore",
                    "Median RIT Score" = "end_median_testritscore",
                    "Mean Student Attainment Percentile"  = "end_median_consistent_percentile",
                    "Mean Student Growth Percentile" = "mean_sgp",
                    "Cohort Growth Percentile" = "cgp"),
        selected = "pct_typical",
        multiple = FALSE
      ),
      
      # Grade selection
      sliderInput(
        inputId = "grades",
        label = "Grades",
        min = 0, max = 8,
        value = c(2,8),
        round = TRUE
      ),
      
      # Grade vs cohort selector
      selectInput(
        inputId = "type",
        label = "View by:",
        choices = c("Grade" = "grade",
                    "Cohort (Class of . . . )" = "cohort"),
        selected = "grade",
        multiple = FALSE
      ) 
    
    ), # END Sidbear
    
    dashboardBody(
      # KIPP Chicogo CSS
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "kippcolors_green.css")),
      
      tabItems(
        tabItem(tabName = "long_plot",
                fluidRow(
                  box(width = 12,
                      title = "Longitudinal Data (click and drag to select assessments to populate data table below)",
                      plotOutput("plot", 
                                 brush = brushOpts(
                                   id = "map_brush",
                                   direction = "x"
                                   ) 
                                 ),
                      status = "primary",
                      solidHeader = TRUE
                      ), # END box
                  box(width  = 12,
                      title = "Details (click and drag to select assessments above)",
                      DT::dataTableOutput("brush_test",
                                          height = "100%", width = "100%")
                   )
                  ) # END fluidRow
          
                ), # END tabItem
        tabItem("hr",
                box(title = "Percent of students tested",
                    width = 12,
                    DT::dataTableOutput("pct_tested")
                    )
                )
        ) # End tabItems
      ) # END dashboardBody
  ) # END dashboardPage

  }

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  library(mapvizieR)
  
  
  map_sum <- reactive({
    map_sum_15 %>%
      mutate(School = stringr::str_extract(
        end_schoolname,
        "Ascend|Academy|Bloom|One"
      )#,
      #nwea_norms = norm_in
      ) %>%
      filter(
        School %in% input$school,
        end_grade %in% c(input$grades[1]:input$grades[2]),
        cohort_year >= 2014,
        end_map_year_academic >= 2011,
        measurementscale %in% input$subject
      )
  })
  
  p <- reactive({
    
    
    p <- mapvizieR::summary_long_plot(
      map_sum(),
      growth_window = input$growth_window,
      metric = input$metric,
      school_col = "School",
      by = input$type,
      n_cutoff = 15
    )
    
    p <- p + theme_kipp_light() +
      theme(legend.position = "right",
            axis.text.x = element_text(hjust=1,
                                       angle = 45)) +
      scale_color_kipp()
    if (input$type == "grade") {
      p + facet_grid(School ~ end_grade)
    } else {
      p + facet_grid(School ~ cohort)
    }
    
  })
  
  output$plot <- renderPlot({p()})
  
  selectedRows <- reactive({brushedPoints(p()$data, input$map_brush)})
  
  output$brush_test <-
    DT::renderDataTable({
      req(input$map_brush)
      
      
      # get data
      x <- selectedRows()
      
      
      x <- x %>%
        group_by(end_schoolname,
                 cohort_year) %>%
        dplyr::mutate(
          cohort = sprintf("%s\n(Current Grade: %s)",
                           cohort_year,
                           max(end_grade)),
          Grade = end_grade,
          SY = sprintf(
            "%s-%s",
            stringr::str_extract(end_map_year_academic, "\\d{2}$"),
            as.integer(stringr::str_extract(end_map_year_academic,
                                            "\\d{2}$")) + 1
          )
        )
      
      #x <- brushedPoints(x, input$map_brush)
      
      x <- x %>% ungroup() %>%
        filter(growth_window == input$growth_window) %>%
        select(
          SY,
          "Window" = growth_window,
          School,
          Grade,
          "Subject" = measurementscale,
          N = n_students,
          starts_with("pct_"),
          contains("median"),
          contains("percentile")
        ) %>%
        select(
          -median_cgi,
          -median_sgp,
          "start_percentile" = start_median_consistent_percentile,
          "end_percentile" = end_median_consistent_percentile
        )
      
      
      
      x <- x %>%
        rename("Typical\nGrowth" = pct_typical,
               "College Ready Growth" = pct_accel_growth,
               "Negative\nGrowth" = pct_negative,
               "Pretest Median RIT" = start_median_testritscore,
               "Posttest Median RIT" = end_median_testritscore,
               "Median RIT Growth" = median_rit_growth,
               "Pretest Cohort Pctl." = start_percentile,
               "Posttest Cohort Pctl." = end_percentile)
      
      DT::datatable(x ,
                    rownames = FALSE,
                    style = "bootstrap",
                    extensions = "FixedColumns",
                    options =
                      list(bPaginate = FALSE,
                           autoWidth = TRUE,
                           autoHeight = TRUE,
                           #columnDefs = list(list(width = '75px', targets = c(1))),
                           scrollX = TRUE,
                           fixedColumns = TRUE,
                           dom = 't')
      ) %>%
        DT::formatPercentage(c(7:9),
                             0)
      
    })
  
  
  #### Students Tested
  
  output$pct_tested <- DT::renderDataTable(
    DT::datatable(student_enrollment_tested %>%
                  arrange(Percent),
                rownames = FALSE,
                filter = "top",
                options = list(bPaginate = FALSE),
                style = "bootstrap"
  ) %>%
    DT::formatPercentage("Percent") %>%
    DT::formatStyle(
      'Percent',
      background = DT::styleColorBar(student_enrollment_tested$Percent, 'steelblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
  )
   
}

# Run the application 
enableBookmarking("server")
shinyApp(ui = ui, server = server)

