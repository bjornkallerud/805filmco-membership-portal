# app.R
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinycssloaders)
library(DT)
library(googlesheets4)
library(jsonlite)
library(auth0)
library(tidyverse)
readRenviron("www/.Renviron")

# ------------------------------------------------------------------------------
# 1) Google Sheets authentication & data reading
# ------------------------------------------------------------------------------
gs4_auth(cache = ".secrets", email = "bjornkallerud@gmail.com")

google_doc <- "https://docs.google.com/spreadsheets/d/1KRS6UUh2QBQpnCsDTKORLyssxoCX-dtayVfRFp7LfPo/edit#gid=0"

# --- Community Board data (sheet: "Community Board") ---
db <- read_sheet(google_doc, sheet = "Community Board") %>%
  distinct()

make_list <- function(row) {
  fromJSON(gsub("'", "", db$entries[row]))
}
board_entries <- lapply(seq_len(nrow(db)), make_list)

# --- Professionals data (sheet: "Professionals") ---
prof_db <- read_sheet(google_doc, sheet = "Professionals") %>%
  select(1:9) %>%
  set_names(c("name", "role", "company", "location",
              "email", "website", "social", "phone", "notes")) %>%
  # Keep first 8 columns for display
  select(name, role, company, location, email, website, social, phone) %>%
  mutate(across(c(role, company, location, email, website, social),
                ~ ifelse(is.na(.x), "", .x))) %>%
  mutate(phone = ifelse(phone == "NULL", "", phone)) %>%
  mutate(website = gsub("https://", "", website)) %>%
  mutate(location = gsub("Los Angeles", "LA", location)) %>%
  mutate(location = gsub("SB", "Santa Barbara", location)) %>%
  mutate(location = gsub("ventura", "Ventura", location)) %>%
  mutate(location = gsub("oxnard", "Oxnard", location)) %>%
  mutate(location = gsub("Carp$", "Carpinteria", location)) %>%
  mutate(location = gsub("SYV", "Santa Ynez", location)) %>%
  mutate(location = gsub("Los Angeles", "LA", location))

# --- Members data (sheet: "Members") ---
members_df <- read_sheet(google_doc, sheet = "Members") %>%
  distinct()
# We assume columns: "email", "name", "permissions"

# ------------------------------------------------------------------------------
# 2) Define UI
# ------------------------------------------------------------------------------
ui <- dashboardPage(
  title = "Membership Database",
  
  # ---------------- HEADER ----------------
  dashboardHeader(
    title = tags$div(
      tags$img(src = "filmco.png", height = 40),
      style = "color: white; font-family:'Hammersmith One'; font-size:30px"
    ),
    titleWidth = 350,
    
    # Show user's name/role
    tags$li(
      class = "dropdown",
      style = "padding: 0 8px; line-height: 50px; color: white;",
      textOutput("user_greeting")
    ),
    
    # Logout button
    tags$li(
      class = "dropdown",
      style = "padding: 0 8px; line-height: 50px;",
      logoutButton(
        label = "Logout",
        style = "background-color: #880808; 
                 border-color: #ffffff; 
                 color: white; 
                 font-weight: bold; 
                 border-width: 1px;"
      )
    )
  ),
  
  # ---------------- SIDEBAR ----------------
  dashboardSidebar(
    width = 185,
    sidebarMenu(
      menuItem("Community Board", tabName = "community_board", icon = icon("list")),
      menuItem("Local Professionals", tabName = "local_professionals", icon = icon("user-friends")),
      menuItem("Gear Database", tabName = "gear_database", icon = icon("cogs"))
    )
  ),
  
  # ---------------- BODY ----------------
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      
      # COMMUNITY BOARD --------------------------------------------------------
      tabItem(
        tabName = "community_board",
        fluidRow(
          column(
            width = 12,
            tabBox(
              title = "Community Board", id = "tabCommunity", width = 12,
              
              tabPanel("Community Board",
                       uiOutput("post_button_ui"),
                       br(),
                       DTOutput("cb_table"),
                       br()
              ),
              
              tabPanel("Guidelines", 
                       p("Here you could place additional guidelines or rules for posting."))
            )
          )
        )
      ),
      
      # LOCAL PROFESSIONALS ---------------------------------------------------
      tabItem(
        tabName = "local_professionals",
        fluidRow(
          column(
            width = 12,
            tabBox(
              title = "Local Professionals",
              id    = "tabLocalPros",
              width = 12,
              
              tabPanel("Professional Database",
                       uiOutput("prof_explanation"),
                       br(),
                       
                       fluidRow(
                         column(width = 4,
                                selectizeInput("prof_role", "Role(s):",
                                               choices = c("Director", "Producer", "DP", "Editor", "Writer",
                                                           "Camera Assistant", "1st AD", "2nd AD", "Photographer",
                                                           "Sound Engineer", "Sound Designer", "Sound Mixer", "Colorist",
                                                           "Actor", "Gaffer", "G&E", "Production Designer",
                                                           "Entertainment Attorney", "Composer", "Projectionist", 
                                                           "Casting Director", "Creative Director", "Graphic Designer",
                                                           "Animator", "Drone Operator", "Post Production Supervisor",
                                                           "Filmmaker", "Business Owner", "Event Spaces", "Illustrator",
                                                           "Script Supervisor", "DIT", "Camera Operator", "Assistant Editor",
                                                           "Other", "Executive Producer", "Financier", "Distribution", 
                                                           "Grant Writing", "Television", "Theater", "Documentary",
                                                           "Commercial", "Scripted Feature", "Casting Director", 
                                                           "Studio Teacher", "HMU", "Stylist", "Costume Designer", 
                                                           "Web Developer", "Critic", "Moderator", "Social Media Manager",
                                                           "Film Festival", "GFX", "Assistant Producer", "Model", 
                                                           "Events Manager", "Marketing", "PR", "Educator", 
                                                           "Archival Producer", "Screenwriter"),
                                               multiple = TRUE)
                         ),
                         column(width = 4,
                                selectizeInput("prof_loc", "Location(s):",
                                               choices = c("Santa Barbara", "LA", "Ojai", "Ventura", 
                                                           "San Luis Obispo", "Goleta", "Santa Ynez", "Isla Vista",
                                                           "Carpinteria", "Oxnard"),
                                               multiple = TRUE)
                         ),
                         column(width = 4,
                                textInput("prof_name", "Name (Optional):")
                         )
                       ),
                       
                       fluidRow(
                         column(width = 12,
                                h4("Professional Database"),
                                div(style = 'color:red', textOutput("prof_error")),
                                div(style = 'overflow-x: scroll', tableOutput("prof_table"))
                         )
                       )
              )
            )
          )
        )
      ),
      
      # GEAR DATABASE ---------------------------------------------------------
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

# ------------------------------------------------------------------------------
# 3) SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # A) Determine user role (permissions) from Auth0
  user_role <- reactive({
    req(session$userData$auth0_info$name)
    user_email <- session$userData$auth0_info$name
    row_data <- members_df %>% filter(email == user_email)
    if (nrow(row_data) == 0) {
      "User"   # default if not found
    } else {
      row_data$permissions[1]  # e.g. "Admin", "Member", "Student", "User"
    }
  })
  
  # B) Greet user by name
  output$user_greeting <- renderText({
    user_email <- session$userData$auth0_info$name
    row_data <- members_df %>% filter(email == user_email)
    if (nrow(row_data) == 0) {
      paste0("Welcome, ", user_email, " | Member type: User")
    } else {
      paste0("Welcome, ", row_data$name[1],
             " | Member type: ", row_data$permissions[1])
    }
  })
  
  # ============================================================================
  # COMMUNITY BOARD
  # ============================================================================
  # 1) Post button vs. free message
  output$post_button_ui <- renderUI({
    the_role <- tolower(user_role())
    if (the_role %in% c("admin", "member")) {
      # Paid membership => show button
      tagList(
        p("Thank you for support as a paid member! 805 Film Co could not exist without you!",
          style = "font-weight: bold; color: #113140;"),
        actionButton("cb_new_post", "Add a New Post", icon = icon("plus"),
                     style = "color: #fff; background-color: #113140; border-color: #2e6da4")
      )
    } else {
      # Non-paid => upgrade
      p("Upgrade to a paid membership to be able to post to the community board.",
        style = "font-weight: bold; color: #113140;")
    }
  })
  
  # 2) Data for Community Board table
  cb_data <- data.frame(
    type  = unlist(lapply(board_entries, \(x) x$type)),
    title = unlist(lapply(board_entries, \(x) x$title))
  ) %>%
    setNames(c("Type", "Title"))
  
  output$cb_table <- renderDT({
    datatable(cb_data,
              rownames = FALSE,
              selection = "single",
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
  
  observeEvent(input$cb_table_rows_selected, {
    req(input$cb_table_rows_selected)
    sel_row <- input$cb_table_rows_selected
    entry <- board_entries[[sel_row]]
    
    output$cb_modal_text <- renderUI({
      if (is.null(entry)) return(NULL)
      switch(entry$type,
             "Job" = HTML(paste(
               "<b>Title</b>:", entry$title,
               "<b>Dates</b>:", entry$dates,
               "<b>Rate</b>:", entry$rate,
               "<b>Contact</b>:", entry$contact,
               "<b>Description</b>:", entry$desc,
               sep = "<br>"
             )),
             "Office" = HTML(paste(
               "<b>Title</b>:", entry$title,
               "<b>Cost</b>:", entry$cost,
               "<b>Contact</b>:", entry$contact,
               "<b>Description</b>:", entry$desc,
               sep = "<br>"
             )),
             "Festival" = HTML(paste(
               "<b>Title</b>:", entry$title,
               "<b>Link</b>:", entry$link,
               "<b>Deadline</b>:", entry$due_dates,
               "<b>Fees</b>:", entry$fees,
               "<b>Contact</b>:", entry$contact,
               "<b>Description</b>:", entry$desc,
               sep = "<br>"
             )),
             "Other" = HTML(paste(
               "<b>Title</b>:", entry$title,
               "<b>Description</b>:", entry$desc,
               sep = "<br>"
             )),
             "Unknown Type")
    })
    
    showModal(
      modalDialog(
        title = "Details",
        uiOutput("cb_modal_text"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  # 3) The "Add New Post" modal
  observeEvent(input$cb_new_post, {
    showModal(
      modalDialog(
        title = "Add a New Community Post",
        
        fluidRow(
          column(width = 6,
                 selectInput("cb_post_type", "Type:",
                             choices = c("Job", "Office", "Festival", "Other")))
        ),
        
        conditionalPanel(
          "input.cb_post_type == 'Job'",
          fluidRow(
            column(width = 6, textInput("cb_job_title", "Job Title:")),
            column(width = 6, textInput("cb_job_dates", "Dates:"))
          ),
          fluidRow(
            column(width = 6, textInput("cb_job_rate", "Rate:")),
            column(width = 6, textInput("cb_job_contact", "Contact Info:"))
          ),
          fluidRow(
            column(width = 12, textAreaInput("cb_job_desc", "Job Description:", height = "100px"))
          )
        ),
        
        conditionalPanel(
          "input.cb_post_type == 'Office'",
          fluidRow(
            column(width = 6, textInput("cb_office_title", "Office Title:")),
            column(width = 6, textInput("cb_office_cost", "Cost:"))
          ),
          fluidRow(
            column(width = 6, textInput("cb_office_contact", "Contact Info:"))
          ),
          fluidRow(
            column(width = 12, textAreaInput("cb_office_desc", "Description:", height = "100px"))
          )
        ),
        
        conditionalPanel(
          "input.cb_post_type == 'Festival'",
          fluidRow(
            column(width = 6, textInput("cb_fest_title", "Festival Title:")),
            column(width = 6, textInput("cb_fest_deadline", "Deadline:"))
          ),
          fluidRow(
            column(width = 6, textInput("cb_fest_link", "Link:")),
            column(width = 6, textInput("cb_fest_contact", "Contact Info:"))
          ),
          fluidRow(
            column(width = 12, textAreaInput("cb_fest_desc", "Description:", height = "100px"))
          )
        ),
        
        conditionalPanel(
          "input.cb_post_type == 'Other'",
          fluidRow(
            column(width = 6, textInput("cb_other_title", "Post Title:")),
            column(width = 6, textInput("cb_other_contact", "Contact Info:"))
          ),
          fluidRow(
            column(width = 12, textAreaInput("cb_other_desc", "Description:", height = "100px"))
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("cb_submit_post", "Submit", icon = icon("paper-plane"),
                       style = "color: #fff; background-color: #113140; border-color: #2e6da4")
        ),
        size = "m",
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$cb_submit_post, {
    if (is.null(input$cb_post_type)) return(NULL)
    
    new_post <- switch(input$cb_post_type,
                       "Job" = list(
                         type    = "Job",
                         title   = input$cb_job_title,
                         dates   = input$cb_job_dates,
                         rate    = input$cb_job_rate,
                         contact = input$cb_job_contact,
                         desc    = input$cb_job_desc
                       ),
                       "Office" = list(
                         type    = "Office",
                         title   = input$cb_office_title,
                         cost    = input$cb_office_cost,
                         contact = input$cb_office_contact,
                         desc    = input$cb_office_desc
                       ),
                       "Festival" = list(
                         type      = "Festival",
                         title     = input$cb_fest_title,
                         due_dates = input$cb_fest_deadline,
                         link      = input$cb_fest_link,
                         contact   = input$cb_fest_contact,
                         desc      = input$cb_fest_desc
                       ),
                       "Other" = list(
                         type    = "Other",
                         title   = input$cb_other_title,
                         contact = input$cb_other_contact,
                         desc    = input$cb_other_desc
                       )
    )
    
    # Validate
    if (is.null(new_post$title) || new_post$title == "") {
      shinyalert("Error", "Please enter a title.", type = "error")
      return()
    }
    
    # Append
    df_append <- data.frame(
      entries = paste0("'", toJSON(new_post), "'")
    )
    sheet_append(google_doc, df_append, sheet = "Community Board")
    
    removeModal()
    shinyalert("Success", "Your post has been added.", type = "success")
  })
  
  # ============================================================================
  # LOCAL PROFESSIONALS
  # ============================================================================
  
  # Explanation text
  output$prof_explanation <- renderUI({
    the_role <- tolower(user_role())
    if (the_role %in% c("admin", "member")) {
      p("Contact info is displayed for paid members. Thank you for your support!",
        style = "color: #113140; font-weight: bold;")
    } else {
      p("Upgrade to a paid membership to view contact info.",
        style = "color: #113140; font-weight: bold;")
    }
  })
  
  # Filter the professionals
  prof_filtered <- reactive({
    data_in <- prof_db
    if (!is.null(input$prof_role) && length(input$prof_role) > 0) {
      data_in <- data_in %>% filter(grepl(paste(input$prof_role, collapse = "|"), role))
    }
    if (!is.null(input$prof_loc) && length(input$prof_loc) > 0) {
      data_in <- data_in %>% filter(grepl(paste(input$prof_loc, collapse = "|"), location))
    }
    data_in <- data_in %>% filter(grepl(input$prof_name, name, ignore.case = TRUE))
    
    data_in
  })
  
  observe({
    if (nrow(prof_filtered()) == 0) {
      output$prof_error <- renderText("No results found.")
    } else {
      output$prof_error <- renderText("")
    }
  })
  
  # Show or hide contact columns
  output$prof_table <- renderTable({
    the_role <- tolower(user_role())
    data_show <- prof_filtered()
    if (nrow(data_show) == 0) {
      return(NULL)
    }
    
    if (!(the_role %in% c("admin", "
                          member"))) {
      # Hide email, social, phone
      data_show <- data_show %>% select(name, role, company, location, website)
    }
    
    data_show
  })
}

# ------------------------------------------------------------------------------
# 4) Run the App with Auth0
# ------------------------------------------------------------------------------
shinyAppAuth0(ui, server)
