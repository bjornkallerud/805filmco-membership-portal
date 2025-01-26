# app.R
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(googlesheets4)
library(jsonlite)

# ---- Google Sheets authentication & data reading ----
gs4_auth(cache = ".secrets", email = "bjornkallerud@gmail.com")
sht <- "https://docs.google.com/spreadsheets/d/1KRS6UUh2QBQpnCsDTKORLyssxoCX-dtayVfRFp7LfPo/edit#gid=0"
db <- read_sheet(sht) %>%
  distinct()

make_list <- function(row) {
  fromJSON(gsub("'", "", db$entries[row]))
}
board_entries <- lapply(seq_len(nrow(db)), make_list)

# ---- Define UI ----
ui <- dashboardPage(
  title = "Membership Database",
  
  dashboardHeader(
    title = tags$div(
      tags$img(src = "filmco.png", height = 40),
      style = "color: white; font-family:'Hammersmith One'; font-size:30px"
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    # Use external CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    withMathJax(),
    
    fluidRow(
      column(width = 1),
      column(
        width = 10,
        tabBox(
          title = "Community Board and Posting", id = "tabset1", width = 12,
          
          tabPanel("Community Board", 
                   DTOutput("mytable")
          ),
          
          tabPanel("Post a Job",
                   fluidRow(
                     column(
                       width = 12,
                       h2("Post to Community Board"),
                       selectInput("post_type", "Type:", 
                                   choices = c("Job", "Office", "Festival", "Other")
                       ),
                       
                       # ---- Conditional Panels for Different Post Types ----
                       conditionalPanel(
                         condition = "input.post_type == 'Job'",
                         fluidRow(
                           column(width = 4, textInput("post_job_title", "Post Title:")),
                           column(width = 4, textInput("post_job_dates", "Date info:"))
                         ),
                         fluidRow(
                           column(width = 4, textInput("post_job_rate", "Rate Info:")),
                           column(width = 4, textInput("post_job_contact", "Contact Info:"))
                         ),
                         fluidRow(
                           column(width = 8, 
                                  textAreaInput("post_job_desc", "Job Description:",
                                                height = "150px", width = "650px"))
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "input.post_type == 'Office'",
                         fluidRow(
                           column(width = 4, textInput("post_office_title", "Post Title:")),
                           column(width = 4, textInput("post_office_cost", "Cost:"))
                         ),
                         fluidRow(
                           column(width = 4, textInput("post_office_contact", "Contact Info:"))
                         ),
                         fluidRow(
                           column(width = 8, 
                                  textAreaInput("post_office_desc", "Description:",
                                                height = "150px", width = "650px"))
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "input.post_type == 'Festival'",
                         fluidRow(
                           column(width = 4, textInput("post_fest_title", "Post Title:")),
                           column(width = 4, textInput("post_fest_due_dates", "Festival Deadlines:"))
                         ),
                         fluidRow(
                           column(width = 4, textInput("post_fest_link", "Link:")),
                           column(width = 4, textInput("post_fest_contact", "Contact Info:"))
                         ),
                         fluidRow(
                           column(width = 8, 
                                  textAreaInput("post_fest_desc", "Description:", 
                                                height = "150px", width = "650px"))
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "input.post_type == 'Other'",
                         fluidRow(
                           column(width = 4, textInput("post_other_title", "Post Title:")),
                           column(width = 4, textInput("post_other_contact", "Contact Info:"))
                         ),
                         fluidRow(
                           column(width = 8, 
                                  textAreaInput("post_other_desc", "Description:", 
                                                height = "150px", width = "650px"))
                         )
                       ),
                       
                       fluidRow(
                         column(
                           width = 8,
                           actionButton("btn", "Submit", icon = icon("paper-plane"),
                                        style = "color: #fff; background-color: #113140; 
                                                 border-color: #2e6da4", 
                                        width = "100%")
                         )
                       )
                     )
                   )
          )
        )
      ),
      column(width = 1)
    )
  )
)

# ---- Define Server ----
server <- function(input, output, session) {
  
  # Build the DataFrame for table display
  df <- data.frame(
    type  = unlist(lapply(board_entries, \(x) x$type)),
    title = unlist(lapply(board_entries, \(x) x$title))
  ) %>%
    setNames(c("Type", "Title"))
  
  # Render the DataTable
  output$mytable <- renderDT({
    datatable(
      df, 
      rownames = FALSE,
      selection = 'single',
      options = list(
        dom        = "",
        autoWidth  = TRUE,
        pageLength = 100,
        initComplete = JS(
          "setTimeout(function(){",
          "  var table = this.api();",
          "  $(this.api().table().body()).on('click', 'tr', function() {",
          "    if ($(this).hasClass('selected')) {",
          "      $(this).removeClass('selected');",
          "    } else {",
          "      table.$('tr.selected').removeClass('selected');",
          "      $(this).addClass('selected');",
          "    }",
          "  });",
          "}, 500);"
        )
      )
    )
  })
  
  # Observe row selection and show modal
  observeEvent(input$mytable_rows_selected, {
    req(input$mytable_rows_selected)
    selected_row <- input$mytable_rows_selected
    
    output$modalText <- renderUI({
      entry <- board_entries[[selected_row]]
      if (is.null(entry)) return(NULL)
      
      # Dynamically build HTML output based on `type`
      if (entry$type == "Job") {
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Dates</b>:", entry$dates,
          "<b>Rate</b>:", entry$rate,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          sep = "<br>"
        ))
      } else if (entry$type == "Office") {
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Cost</b>:", entry$cost,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          sep = "<br>"
        ))
      } else if (entry$type == "Festival") {
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Link</b>:", entry$link,
          "<b>Deadline</b>:", entry$due_dates,
          "<b>Fees</b>:", entry$fees,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          sep = "<br>"
        ))
      } else if (entry$type == "Other") {
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Description</b>:", entry$desc,
          sep = "<br>"
        ))
      } else {
        "Unknown Type"
      }
    })
    
    showModal(
      modalDialog(
        title = "Details",
        uiOutput("modalText"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  # Handle "Post a Job" (or other) submission
  observeEvent(input$btn, {
    # Build the list based on type
    if (input$post_type == "Job") {
      master_list <- list(
        type    = "Job",
        title   = input$post_job_title,
        dates   = input$post_job_dates,
        rate    = input$post_job_rate,
        contact = input$post_job_contact,
        desc    = input$post_job_desc
      )
    } else if (input$post_type == "Office") {
      master_list <- list(
        type    = "Office",
        title   = input$post_office_title,
        cost    = input$post_office_cost,
        contact = input$post_office_contact,
        desc    = input$post_office_desc
      )
    } else if (input$post_type == "Festival") {
      master_list <- list(
        type      = "Festival",
        title     = input$post_fest_title,
        due_dates = input$post_fest_due_dates,
        link      = input$post_fest_link,
        contact   = input$post_fest_contact,
        desc      = input$post_fest_desc
      )
    } else if (input$post_type == "Other") {
      master_list <- list(
        type    = "Other",
        title   = input$post_other_title,
        contact = input$post_other_contact,
        desc    = input$post_other_desc
      )
    } else {
      shinyalert("Error", "Unknown post type selected.", type = "error")
      return(NULL)
    }
    
    # Convert to JSON and append to Google Sheet
    df_append <- data.frame(
      entries = paste0("'", toJSON(master_list), "'")
    )
    sheet_append(sht, df_append)
    
    shinyalert("Thank you!", 
               "Your post has been added to the board", 
               type = "success")
    
    # Optionally re-load data here if you want the table to refresh immediately
    # db <<- read_sheet(sht) %>% distinct()
    # board_entries <<- lapply(seq_len(nrow(db)), make_list)
    # output$mytable <- renderDT({ ... })
  })
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
