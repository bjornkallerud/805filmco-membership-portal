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

# Build data for table with sorting: include Date and row order
cb_data <- do.call(rbind, lapply(seq_along(board_entries), function(i) {
  entry <- board_entries[[i]]
  data.frame(
    Type = entry$type,
    Title = entry$title,
    Date = if("date" %in% names(entry)) entry$date else NA,
    row_order = i,
    stringsAsFactors = FALSE
  )
}))
# Parse dates using the format "mm-dd-YYYY"
cb_data$Date_parsed <- as.Date(cb_data$Date, format = "%m-%d-%Y")
cb_data_sorted <- cb_data %>% arrange(desc(Date_parsed), desc(row_order))
cb_data_display <- cb_data_sorted %>% select(Type, Title, Date)

# --- Members data (sheet: "Members") ---
members_df <- read_sheet(google_doc, sheet = "Members") %>%
  distinct()

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
  mutate(location = gsub("Los Angeles", "LA", location)) %>%
  # Add membership flag: display the dark blue 805 Film Co logo if the email is in Members data
  mutate(membership = ifelse(email %in% members_df$email,
                             '<img src="805filmco-logomark.png" height="20">',
                             "")) |> 
  mutate(ordr = ifelse(membership != "", 1, 2)) |> 
  arrange(ordr, name) |> 
  select(-ordr)

# ------------------------------------------------------------------------------
# 2) Define UI
# ------------------------------------------------------------------------------
ui <- dashboardPage(
  title = "Membership Database",
  
  # ---------------- HEADER ----------------
  dashboardHeader(
    title = tags$div(
      tags$img(src = "805filmco-logo.png", height = 50),
      style = "color: white; font-family:'Hammersmith One'; font-size:30px"
    ),
    titleWidth = 250,
    
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
    
    # If actual user is Admin, show "view as ..." select
    uiOutput("view_as_select"),
    
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
                       fluidRow(
                         column(4,
                                selectizeInput("cb_type", "Filter by Type:",
                                               choices = c("Job-Offering", "Job-Seeking", "Office-Offering", "Office-Seeking", "Festival", "Other"),
                                               multiple = TRUE, options = list(placeholder = 'Select type(s)'))
                         )
                       ),
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
                                               choices = c("Director", "Producer", "DP", "Editor", "Writer", "Production Assistant (PA)",
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
                                                           "Archival Producer", "Screenwriter", "Steadicam Operator", "Other"),
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
                                textInput("prof_name", "Name:")
                         )
                       ),
                       fluidRow(
                         column(width = 12,
                                h4("Professional Database"),
                                div(style = 'color:red', textOutput("prof_error")),
                                div(style = 'overflow-x: scroll', DTOutput("prof_table"))
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
  
  # --------------------------------
  # A) Actual user role from Auth0
  # --------------------------------
  actual_user_role <- reactive({
    req(session$userData$auth0_info$name)
    user_email <- session$userData$auth0_info$name
    row_data <- members_df %>% filter(email == user_email)
    if (nrow(row_data) == 0) {
      "User"
    } else {
      row_data$permissions[1]
    }
  })
  
  # --------------------------------
  # B) If actual user is Admin, show a "view as ..." select
  # --------------------------------
  output$view_as_select <- renderUI({
    role_now <- tolower(actual_user_role())
    if (role_now == "admin") {
      selectInput("view_as", 
                  label = "View as:",
                  choices = c("User", "Student", "Member"),
                  selected = "Member")
    } else {
      NULL
    }
  })
  
  # --------------------------------
  # C) Effective role
  # --------------------------------
  effective_role <- reactive({
    real_role <- tolower(actual_user_role())
    if (real_role != "admin") {
      real_role
    } else {
      req(input$view_as)
      tolower(input$view_as)
    }
  })
  
  # --------------------------------
  # D) Greet user
  # --------------------------------
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
  
  # --------------------------------
  # E) Professional Explanation
  # --------------------------------
  output$prof_explanation <- renderUI({
    role_now <- effective_role()
    if (role_now %in% c("admin", "member")) {
      tagList(
        p("Welcome to the Professional Database! Thank you for support as a paid member! 805 Film Co could not exist without you.",
          style = "font-weight: bold; color: #113140;"),
        "This is the ultimate rolodex with over 500+ local film professionals (growing weekly!), so you can nurture your local network. Only paying members are able to access the entire database.",
        tags$u("Mass emails to all Members are not allowed."),
        " If you want to share something with the wider Collective, please contact ",
        tags$a("hello@805filmco.com" )
      )
    } else {
      tagList(
        p("Welcome to the Professional Database! Thank you for support as a paid member! 805 Film Co could not exist without you.",
          style = "font-weight: bold; color: #113140;"),
        "This is the ultimate rolodex with over 500+ local film professionals (growing weekly!), so you can nurture your local network. Only paying members are able to access the entire database.",
        tags$u("Mass emails to all Members are not allowed."),
        " If you want to share something with the wider Collective, please contact ",
        tags$a("hello@805filmco.com" ),
        br(),
        p(span("Upgrade to paid membership to have access to entire database.", style = "font-weight: bold; color: #880808;"))
      )
    }
  })
  
  # ============================================================================
  # COMMUNITY BOARD
  # ============================================================================
  output$post_button_ui <- renderUI({
    role_now <- effective_role()
    if (role_now %in% c("admin", "member")) {
      tagList(
        p("Welcome to the Community Board! Thank you for support as a paid member! 805 Film Co could not exist without you!",
          style = "font-weight: bold; color: #113140;"),
        "Think of it as a specialized bulletin board for film professionals. You can see & post things here for the Collective to see, including but not limited to: Job Posts (both seeking and offering), Offices for Rent, Film Festival Calls for Submissions, and other Queries.",
        tags$u("Anyone"),
        " can see the Community Board, but ",
        tags$u("only paying members"),
        " can post. Posts are monitored & archived after 30 days.",
        br(),
        br(),
        actionButton("cb_new_post", "Add a New Post", icon = icon("plus"),
                     style = "color: #fff; background-color: #113140; border-color: #2e6da4")
      )
    } else {
      tagList(
        "Think of it as a specialized bulletin board for film professionals. You can see & post things here for the Collective to see, including but not limited to: Job Posts (both seeking and offering), Offices for Rent, Film Festival Calls for Submissions, and other Queries.",
        tags$u("Anyone"),
        " can see the Community Board, but ",
        tags$u("only paying members"),
        " can post. Posts are monitored & archived after 30 days.",
        br(),
        br(),
        p(span("Upgrade to paid membership to have access to post.", style = "font-weight: bold; color: #880808;"))
      )
    }
  })
  
  # Reactive filtering for Community Board based on Type filter input
  cb_filtered_data <- reactive({
    if (is.null(input$cb_type) || length(input$cb_type) == 0) {
      cb_data_display
    } else {
      cb_data_display %>% filter(Type %in% input$cb_type)
    }
  })
  
  output$cb_table <- renderDT({
    datatable(
      cb_filtered_data(),
      rownames = FALSE,
      selection = "single",
      options = list(
        dom = "",
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
    entry <- board_entries[[ cb_data_sorted$row_order[sel_row] ]]
    
    output$cb_modal_text <- renderUI({
      if (is.null(entry)) return(NULL)
      if (entry$type %in% c("Job-Offering", "Job-Seeking")) {
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Dates</b>:", entry$dates,
          "<b>Rate</b>:", entry$rate,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          if(!is.null(entry$date)) paste("<b>Date</b>:", entry$date) else "",
          sep = "<br>"
        ))
      } else if (entry$type %in% c("Office-Offering", "Office-Seeking")) {
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Cost</b>:", entry$cost,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          if(!is.null(entry$date)) paste("<b>Date</b>:", entry$date) else "",
          sep = "<br>"
        ))
      } else if (entry$type == "Festival") {
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Link</b>:", entry$link,
          "<b>Deadline</b>:", entry$due_dates,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          if(!is.null(entry$date)) paste("<b>Date</b>:", entry$date) else "",
          sep = "<br>"
        ))
      } else if (entry$type == "Other") {
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          if(!is.null(entry$date)) paste("<b>Date</b>:", entry$date) else "",
          sep = "<br>"
        ))
      } else {
        "Unknown Type"
      }
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
  
  # Modal for adding a new post
  observeEvent(input$cb_new_post, {
    showModal(
      modalDialog(
        title = "Add a New Community Post",
        fluidRow(
          column(width = 6,
                 selectInput("cb_post_type", "Type:",
                             choices = c("Job-Offering", "Job-Seeking", "Office-Offering", "Office-Seeking", "Festival", "Other"))
          )
        ),
        conditionalPanel(
          "input.cb_post_type == 'Job-Offering' || input.cb_post_type == 'Job-Seeking'",
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
          "input.cb_post_type == 'Office-Offering' || input.cb_post_type == 'Office-Seeking'",
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
        size = "m", easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$cb_submit_post, {
    if (is.null(input$cb_post_type)) return(NULL)
    
    new_post <- switch(input$cb_post_type,
                       "Job-Offering" = list(
                         type    = "Job-Offering",
                         title   = input$cb_job_title,
                         dates   = input$cb_job_dates,
                         rate    = input$cb_job_rate,
                         contact = input$cb_job_contact,
                         desc    = input$cb_job_desc,
                         date    = format(Sys.Date(), "%m-%d-%Y")
                       ),
                       "Job-Seeking" = list(
                         type    = "Job-Seeking",
                         title   = input$cb_job_title,
                         dates   = input$cb_job_dates,
                         rate    = input$cb_job_rate,
                         contact = input$cb_job_contact,
                         desc    = input$cb_job_desc,
                         date    = format(Sys.Date(), "%m-%d-%Y")
                       ),
                       "Office-Offering" = list(
                         type    = "Office-Offering",
                         title   = input$cb_office_title,
                         cost    = input$cb_office_cost,
                         contact = input$cb_office_contact,
                         desc    = input$cb_office_desc,
                         date    = format(Sys.Date(), "%m-%d-%Y")
                       ),
                       "Office-Seeking" = list(
                         type    = "Office-Seeking",
                         title   = input$cb_office_title,
                         cost    = input$cb_office_cost,
                         contact = input$cb_office_contact,
                         desc    = input$cb_office_desc,
                         date    = format(Sys.Date(), "%m-%d-%Y")
                       ),
                       "Festival" = list(
                         type      = "Festival",
                         title     = input$cb_fest_title,
                         due_dates = input$cb_fest_deadline,
                         link      = input$cb_fest_link,
                         contact   = input$cb_fest_contact,
                         desc      = input$cb_fest_desc,
                         date      = format(Sys.Date(), "%m-%d-%Y")
                       ),
                       "Other" = list(
                         type    = "Other",
                         title   = input$cb_other_title,
                         contact = input$cb_other_contact,
                         desc    = input$cb_other_desc,
                         date    = format(Sys.Date(), "%m-%d-%Y")
                       )
    )
    
    if (is.null(new_post$title) || new_post$title == "") {
      shinyalert("Error", "Please enter a title.", type = "error")
      return()
    }
    
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
  # Reactive filtering for Professionals table based on inputs
  prof_filtered <- reactive({
    data_in <- prof_db
    if (!is.null(input$prof_role) && length(input$prof_role) > 0) {
      data_in <- data_in %>% filter(grepl(paste(input$prof_role, collapse = "|"), role))
    }
    if (!is.null(input$prof_loc) && length(input$prof_loc) > 0) {
      data_in <- data_in %>% filter(grepl(paste(input$prof_loc, collapse = "|"), location))
    }
    data_in <- data_in %>% filter(grepl(input$prof_name, name, ignore.case = TRUE))
    # For "user" privileges, show only rows where membership is not empty
    if (effective_role() == "user") {
      data_in <- data_in %>% filter(membership != "")
    }
    data_in
  })
  
  output$prof_error <- renderText({
    if (nrow(prof_filtered()) == 0) "No results found." else ""
  })
  
  output$prof_table <- renderDT({
    data_show <- prof_filtered()
    if (nrow(data_show) == 0) return(NULL)
    # Select and order columns: membership, name, role, location, website
    data_show <- data_show %>% select(membership, name, role, location, website)
    colnames(data_show) <- c("Membership", "Name", "Role", "Location", "Website")
    datatable(data_show,
              rownames = FALSE,
              selection = "single",
              options = list(
                dom = "t",
                pageLength = 100,
                autoWidth = TRUE
              ),
              escape = FALSE,
              class = "stripe"
    )
  })
  
  observeEvent(input$prof_table_rows_selected, {
    req(input$prof_table_rows_selected)
    sel_row <- input$prof_table_rows_selected
    selected_prof <- prof_filtered()[sel_row, ]
    
    output$prof_modal_text <- renderUI({
      HTML(paste(
        "<b>Name:</b>", selected_prof$name,
        "<b>Role:</b>", selected_prof$role,
        "<b>Company:</b>", selected_prof$company,
        "<b>Location:</b>", selected_prof$location,
        "<b>Email:</b>", selected_prof$email,
        "<b>Website:</b>", selected_prof$website,
        "<b>Social:</b>", selected_prof$social,
        "<b>Phone:</b>", selected_prof$phone,
        sep = "<br>"
      ))
    })
    
    showModal(modalDialog(
      title = "Professional Details",
      uiOutput("prof_modal_text"),
      easyClose = TRUE,
      size = "m"
    ))
  })
}

# ------------------------------------------------------------------------------
# 4) Run the App with Auth0
# ------------------------------------------------------------------------------
shinyAppAuth0(ui, server)
