library(shiny)
library(shinydashboard)
library(auth0)
library(shinyalert)
library(shinycssloaders)
library(DT)
library(tidyverse)

auth0_ui(
  dashboardPage(title = "Membership Database",
                dashboardHeader(
                  title = tags$div(
                    tags$img(src = "filmco.png", height = 40),
                    style = "color: white; font-family:'Hammersmith One'; font-size:30px"
                  ),
                  titleWidth = 350
                ),
                dashboardSidebar(disable = TRUE),
                dashboardBody(
                  
                  tags$head(tags$style("
    /* Logo */
    .skin-blue .main-header .logo {
        background-color: #113140 !important;
    }

    /* Logo when hovered */
    .skin-blue .main-header .logo:hover {
        background-color: #113140 !important;
    }

    /* Navbar (rest of the header) */
    .skin-blue .main-header .navbar {
        background-color: #113140 !important;
    }

    /* Main sidebar */
    .skin-blue .main-sidebar {
        background-color: #113140 !important;
    }
    
    /* Optional: Change the sidebar menu items' active and hover colors */
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #1c80ba !important;
        color: white !important;
    }
    
    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
        background-color: #1c80ba !important;
        color: white !important;
    }
  ")),
                  withMathJax(),
                  
                  fluidRow(
                    column(width = 1),
                    column(width = 10,
                           tabBox(title = "Community Board and Posting", id = "tabset1", width = 12,
                                  tabPanel("Community Board", DTOutput("mytable")),
                                  tabPanel("Post a Job",
                                           fluidRow(
                                             column(width = 12,
                                                    h2("Post to Community Board"),
                                                    selectInput("post_type", "Type:", choices = c("Job", "Office", "Festival", "Other")),
                                                    
                                                    # Conditional Panels for Different Post Types
                                                    conditionalPanel("input.post_type == 'Job'",
                                                                     fluidRow(
                                                                       column(width = 4, textInput("post_job_title", "Post Title:")),
                                                                       column(width = 4, textInput("post_job_dates", "Date info:"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(width = 4, textInput("post_job_rate", "Rate Info:")),
                                                                       column(width = 4, textInput("post_job_contact", "Contact Info:"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(width = 8, textAreaInput("post_job_desc", "Job Description:", height = "150px", width = "650px"))
                                                                     )
                                                    ),
                                                    
                                                    conditionalPanel("input.post_type == 'Office'",
                                                                     fluidRow(
                                                                       column(width = 4, textInput("post_office_title", "Post Title:")),
                                                                       column(width = 4, textInput("post_office_cost", "Cost:"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(width = 4, textInput("post_office_contact", "Contact Info:"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(width = 8, textAreaInput("post_office_desc", "Description:", height = "150px", width = "650px"))
                                                                     )
                                                    ),
                                                    
                                                    conditionalPanel("input.post_type == 'Festival'",
                                                                     fluidRow(
                                                                       column(width = 4, textInput("post_fest_title", "Post Title:")),
                                                                       column(width = 4, textInput("post_fest_due_dates", "Festival Deadlines:"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(width = 4, textInput("post_fest_link", "Link:")),
                                                                       column(width = 4, textInput("post_fest_contact", "Contact Info:"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(width = 8, textAreaInput("post_fest_desc", "Description:", height = "150px", width = "650px"))
                                                                     )
                                                    ),
                                                    
                                                    conditionalPanel("input.post_type == 'Other'",
                                                                     fluidRow(
                                                                       column(width = 4, textInput("post_other_title", "Post Title:")),
                                                                       column(width = 4, textInput("post_other_contact", "Contact Info:"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(width = 8, textAreaInput("post_other_desc", "Description:", height = "150px", width = "650px"))
                                                                     )
                                                    ),
                                                    
                                                    fluidRow(
                                                      column(width = 8,
                                                             actionButton("btn", "Submit", icon = icon("paper-plane"),
                                                                          style = "color: #fff; background-color: #113140; border-color: #2e6da4", width = "100%")
                                                      )
                                                    )
                                             )
                                           )
                                  )
                           )
                    ),
                    column(width = 1)
                  )
                  
                  # Remove the bsModal since we're using showModal
                  # bsModal("myModal", "Details", "mytable", size = "medium",
                  #         htmlOutput("modalText"))
                )
  )
)
