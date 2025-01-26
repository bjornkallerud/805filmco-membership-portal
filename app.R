# app.R (Version 2) 
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinycssloaders)
library(DT)
library(googlesheets4)
library(jsonlite)
library(auth0)
library(tidyverse)

# ---- Google Sheets authentication & data reading ----
gs4_auth(cache = ".secrets", email = "bjornkallerud@gmail.com")
google_doc <- "https://docs.google.com/spreadsheets/d/1KRS6UUh2QBQpnCsDTKORLyssxoCX-dtayVfRFp7LfPo/edit#gid=0"
db <- read_sheet(google_doc, sheet = "Community Board") %>%
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
  
  # Sidebar with 3 menu items
  dashboardSidebar(
    sidebarMenu(
      menuItem("Community Board", tabName = "community_board", icon = icon("list")),
      menuItem("Local Professionals", tabName = "local_professionals", icon = icon("user-friends")),
      menuItem("Gear Database", tabName = "gear_database", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    # Use external CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    withMathJax(),
    
    # Main tab items
    tabItems(
      
      # --- Community Board ---
      tabItem(
        tabName = "community_board",
        fluidRow(
          column(
            width = 12,
            tabBox(
              title = "Community Board", 
              id    = "tabCommunity", 
              width = 12,
              
              # The board itself
              tabPanel("Community Board",
                       verbatimTextOutput("user_info"),
                       DTOutput("mytable"),
                       br(),
                       # Instead of a big set of inputs, use a button that opens a modal:
                       actionButton("open_modal", "Add a New Post", icon = icon("plus"),
                                    style = "color: #fff; background-color: #113140; border-color: #2e6da4")
              ),
              
              # We can add more sub-tabs here if desired, but the main difference
              # is that we do not fill them with big forms.
              tabPanel("Guidelines", 
                       p("Here you could place additional guidelines or rules for posting."))
            )
          )
        )
      ),
      
      # --- Local Professionals ---
      tabItem(
        tabName = "local_professionals",
        fluidRow(
          column(
            width = 12,
            tabBox(
              title = "Local Professionals", id = "tabLocalPros", width = 12,
              tabPanel("Coming Soon", "This section will be filled out later.")
            )
          )
        )
      ),
      
      # --- Gear Database ---
      tabItem(
        tabName = "gear_database",
        fluidRow(
          column(width = 12, 
                 h2("Gear Database"),
                 p("Coming soon!")
          )
        )
      )
    )
  )
)

# ---- Define Server ----
server <- function(input, output, session) {
  
  # Print user info from Auth0
  output$user_info <- renderPrint({
    session$userData$auth0_info$name
  })
  
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
  
  #-------------------------------------------------------
  # A more elegant approach: Show a modal for new posts
  #-------------------------------------------------------
  # 1) Observe the "Add a New Post" button
  observeEvent(input$open_modal, {
    showModal(
      modalDialog(
        title = "Add a New Community Post",
        # We can nest everything in fluidRows
        fluidRow(
          column(
            width = 6,
            selectInput("modal_post_type", "Type:",
                        choices = c("Job", "Office", "Festival", "Other"))
          )
        ),
        
        # Now show fields conditionally based on the selected type
        conditionalPanel(
          "input.modal_post_type == 'Job'",
          fluidRow(
            column(width = 6, textInput("modal_job_title", "Job Title:")),
            column(width = 6, textInput("modal_job_dates", "Dates:"))
          ),
          fluidRow(
            column(width = 6, textInput("modal_job_rate", "Rate:")),
            column(width = 6, textInput("modal_job_contact", "Contact Info:"))
          ),
          fluidRow(
            column(width = 12, textAreaInput("modal_job_desc", "Job Description:", 
                                             height = "100px"))
          )
        ),
        
        conditionalPanel(
          "input.modal_post_type == 'Office'",
          fluidRow(
            column(width = 6, textInput("modal_office_title", "Office Title:")),
            column(width = 6, textInput("modal_office_cost", "Cost:"))
          ),
          fluidRow(
            column(width = 6, textInput("modal_office_contact", "Contact Info:"))
          ),
          fluidRow(
            column(width = 12, textAreaInput("modal_office_desc", "Description:", 
                                             height = "100px"))
          )
        ),
        
        conditionalPanel(
          "input.modal_post_type == 'Festival'",
          fluidRow(
            column(width = 6, textInput("modal_fest_title", "Festival Title:")),
            column(width = 6, textInput("modal_fest_due", "Deadline:"))
          ),
          fluidRow(
            column(width = 6, textInput("modal_fest_link", "Link:")),
            column(width = 6, textInput("modal_fest_contact", "Contact Info:"))
          ),
          fluidRow(
            column(width = 12, textAreaInput("modal_fest_desc", "Description:", 
                                             height = "100px"))
          )
        ),
        
        conditionalPanel(
          "input.modal_post_type == 'Other'",
          fluidRow(
            column(width = 6, textInput("modal_other_title", "Post Title:")),
            column(width = 6, textInput("modal_other_contact", "Contact Info:"))
          ),
          fluidRow(
            column(width = 12, textAreaInput("modal_other_desc", "Description:", 
                                             height = "100px"))
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_modal", "Submit", icon = icon("paper-plane"),
                       style = "color: #fff; background-color: #113140; 
                                border-color: #2e6da4")
        ),
        size = "m",
        easyClose = FALSE
      )
    )
  })
  
  # 2) Observe the submission button in the modal
  observeEvent(input$submit_modal, {
    
    # Build the list according to the chosen type
    new_post <- switch(input$modal_post_type,
                       "Job" = list(
                         type    = "Job",
                         title   = input$modal_job_title,
                         dates   = input$modal_job_dates,
                         rate    = input$modal_job_rate,
                         contact = input$modal_job_contact,
                         desc    = input$modal_job_desc
                       ),
                       "Office" = list(
                         type    = "Office",
                         title   = input$modal_office_title,
                         cost    = input$modal_office_cost,
                         contact = input$modal_office_contact,
                         desc    = input$modal_office_desc
                       ),
                       "Festival" = list(
                         type      = "Festival",
                         title     = input$modal_fest_title,
                         due_dates = input$modal_fest_due,
                         link      = input$modal_fest_link,
                         contact   = input$modal_fest_contact,
                         desc      = input$modal_fest_desc
                       ),
                       "Other" = list(
                         type    = "Other",
                         title   = input$modal_other_title,
                         contact = input$modal_other_contact,
                         desc    = input$modal_other_desc
                       )
    )
    
    # Simple validation (optional)
    if (is.null(new_post$title) || new_post$title == "") {
      shinyalert("Error", "Please enter a title.", type = "error")
      return()
    }
    
    # Convert to JSON and append to Google Sheet
    df_append <- data.frame(
      entries = paste0("'", toJSON(new_post), "'")
    )
    sheet_append(sht, df_append)
    
    # Hide the modal
    removeModal()
    
    shinyalert("Thank you!", "Your post has been added to the board", type = "success")
    
    # Optionally re-load data and re-render the table here
    # db <<- read_sheet(sht) %>% distinct()
    # board_entries <<- lapply(seq_len(nrow(db)), make_list)
    # output$mytable <- renderDT({ ... })
  })
  
}

# ---- Run the App with Auth0 ----
shinyAppAuth0(ui, server)
