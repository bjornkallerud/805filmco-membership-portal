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
db <- read_sheet(google_doc, sheet = "Community Board") %>% distinct()
make_list <- function(row) { fromJSON(gsub("'", "", db$entries[row])) }
board_entries <- lapply(seq_len(nrow(db)), make_list)
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
cb_data$Date_parsed <- as.Date(cb_data$Date, format = "%m-%d-%Y")
cb_data_sorted <- cb_data %>% arrange(desc(Date_parsed), desc(row_order))
cb_data_display <- cb_data_sorted %>% select(Type, Title, Date)

# --- Members data (sheet: "Members") ---
members_df <- read_sheet(google_doc, sheet = "Members") %>% distinct()

# --- Professionals data (sheet: "Professionals") ---
prof_db <- read_sheet(google_doc, sheet = "Professionals") %>%
  select(1:9) %>%
  set_names(c("name", "role", "company", "location", "email", "website", "social", "phone", "notes")) %>%
  select(name, role, company, location, email, website, social, phone) %>%
  mutate(across(c(role, company, location, email, website, social), ~ ifelse(is.na(.x), "", .x))) %>%
  mutate(phone = ifelse(phone == "NULL", "", phone)) %>%
  mutate(website = gsub("https://", "", website)) %>%
  mutate(location = gsub("Los Angeles", "LA", location),
         location = gsub("SB", "Santa Barbara", location),
         location = gsub("ventura", "Ventura", location),
         location = gsub("oxnard", "Oxnard", location),
         location = gsub("Carp$", "Carpinteria", location),
         location = gsub("SYV", "Santa Ynez", location),
         location = gsub("Los Angeles", "LA", location)) %>%
  mutate(membership = ifelse(email %in% members_df$email,
                             '<img src="805filmco-logomark.png" height="20">',
                             "")) |> 
  mutate(ordr = ifelse(membership != "", 1, 2)) |> 
  arrange(ordr, name) |> 
  select(-ordr)

# --- Gear Catalog data (sheet: "Gear Catalog") ---
gear_db_data <- reactive({ read_sheet(google_doc, sheet = "Gear Catalog") %>% distinct() })
gear_entries <- reactive({
  if(nrow(gear_db_data()) == 0) return(list())
  lapply(seq_len(nrow(gear_db_data())), function(i) {
    fromJSON(gsub("'", "", gear_db_data()$entries[i]))
  })
})
gear_data <- reactive({
  if(length(gear_entries()) == 0) return(data.frame())
  do.call(rbind, lapply(seq_along(gear_entries()), function(i) {
    entry <- gear_entries()[[i]]
    data.frame(
      GearType = entry$gear_type,
      Terms = entry$terms,
      Intent = entry$intent,
      Description = entry$description,
      Price = entry$price,
      Contact = entry$contact,
      Date = if("date" %in% names(entry)) entry$date else NA,
      row_order = i,
      stringsAsFactors = FALSE
    )
  }))
})
gear_data_display <- reactive({
  df <- gear_data()
  if(nrow(df) == 0) return(df)
  df$Date_parsed <- as.Date(df$Date, format = "%m-%d-%Y")
  df_sorted <- df %>% arrange(desc(Date_parsed), desc(row_order))
  df_sorted %>% select(GearType, Terms, Intent, Price, Date)
})

# --- Hospitality Services data (sheet: "Hospitality Services") ---
hospitality_db_data <- reactive({ read_sheet(google_doc, sheet = "Hospitality Services") %>% distinct() })
hospitality_entries <- reactive({
  if(nrow(hospitality_db_data()) == 0) return(list())
  lapply(seq_len(nrow(hospitality_db_data())), function(i) {
    fromJSON(gsub("'", "", hospitality_db_data()$entries[i]))
  })
})
hospitality_data <- reactive({
  if(length(hospitality_entries()) == 0) return(data.frame())
  do.call(rbind, lapply(seq_along(hospitality_entries()), function(i) {
    entry <- hospitality_entries()[[i]]
    data.frame(
      Type = entry$type,
      LodgingType = if("lodging_type" %in% names(entry)) entry$lodging_type else "",
      Company = entry$company,
      Contact = entry$contact,
      DiscountCode = entry$discount_code,
      Date = if("date" %in% names(entry)) entry$date else NA,
      row_order = i,
      stringsAsFactors = FALSE
    )
  }))
})
hospitality_data_display <- reactive({
  df <- hospitality_data()
  if(nrow(df)==0) return(df)
  df$Date_parsed <- as.Date(df$Date, format = "%m-%d-%Y")
  df_sorted <- df %>% arrange(desc(Date_parsed), desc(row_order))
  df_sorted %>% select(Type, LodgingType, Company, Date)
})

# ------------------------------------------------------------------------------
# 2) Define UI
# ------------------------------------------------------------------------------
ui <- dashboardPage(
  title = "Membership Database",
  dashboardHeader(
    title = tags$div(
      tags$img(src = "805LogoLooped.gif", height = 50),
      style = "color: white; font-family:'Hammersmith One'; font-size:30px"
    ),
    titleWidth = 250,
    tags$li(class = "dropdown", style = "padding: 0 8px; line-height: 50px; color: white;", textOutput("user_greeting")),
    tags$li(class = "dropdown", style = "padding: 0 8px; line-height: 50px;",
            logoutButton(label = "Logout", style = "background-color: #880808; border-color: #ffffff; color: white; font-weight: bold; border-width: 1px;"))
  ),
  dashboardSidebar(
    width = 185,
    uiOutput("view_as_select"),
    sidebarMenu(
      menuItem("Community Board", tabName = "community_board", icon = icon("list")),
      menuItem("Local Professionals", tabName = "local_professionals", icon = icon("user-friends")),
      menuItem("Gear Catalog", tabName = "gear_database", icon = icon("camera")),
      menuItem("Hospitality Services", tabName = "hospitality_services", icon = icon("hotel")),
      menuItem("Actors & Talent Registry", tabName = "actors_registry", icon = icon("star"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tabItems(
      # COMMUNITY BOARD --------------------------------------------------------
      tabItem(
        tabName = "community_board",
        fluidRow(
          column(12,
                 tabBox(title = "Community Board", id = "tabCommunity", width = 12,
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
                        )
                 )
          )
        )
      ),
      # LOCAL PROFESSIONALS ---------------------------------------------------
      tabItem(
        tabName = "local_professionals",
        fluidRow(
          column(12,
                 tabBox(title = "Local Professionals", id = "tabLocalPros", width = 12,
                        tabPanel("Professional Database",
                                 uiOutput("prof_explanation"),
                                 br(),
                                 fluidRow(
                                   column(4,
                                          selectizeInput("prof_role", "Role(s):",
                                                         choices = c("Director", "Producer", "DP", "Editor", "Writer", "Production Assistant (PA)",
                                                                     "Camera Assistant", "1st AD", "2nd AD", "Photographer",
                                                                     "Sound Engineer", "Sound Designer", "Sound Mixer", "Colorist",
                                                                     "Actors & Talent", "Gaffer", "G&E", "Production Designer",
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
                                                                     "Film Festival", "GFX", "Assistant Producer",
                                                                     "Events Manager", "Marketing", "PR", "Educator", 
                                                                     "Archival Producer", "Screenwriter", "Steadicam Operator", "Other"),
                                                         multiple = TRUE)
                                   ),
                                   column(4,
                                          selectizeInput("prof_loc", "Location(s):",
                                                         choices = c("Santa Barbara", "LA", "Ojai", "Ventura",
                                                                     "San Luis Obispo", "Goleta", "Santa Ynez", "Isla Vista",
                                                                     "Carpinteria", "Oxnard"),
                                                         multiple = TRUE)
                                   ),
                                   column(4, textInput("prof_name", "Name (Optional):"))
                                 ),
                                 fluidRow(
                                   column(12,
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
      # GEAR CATALOG -----------------------------------------------------------
      tabItem(
        tabName = "gear_database",
        fluidRow(
          column(12,
                 tabBox(title = "Gear Catalog", id = "tabGear", width = 12,
                        tabPanel("Gear Catalog",
                                 uiOutput("gear_explanation"),
                                 uiOutput("gear_post_button_ui"),
                                 br(),
                                 uiOutput("gear_filters_ui"),
                                 uiOutput("gear_table_ui")
                        )
                 )
          )
        )
      ),
      # HOSPITALITY SERVICES ---------------------------------------------------
      tabItem(
        tabName = "hospitality_services",
        fluidRow(
          column(12,
                 tabBox(title = "Hospitality Services", id = "tabHosp", width = 12,
                        tabPanel("Hospitality Services",
                                 uiOutput("hosp_explanation"),
                                 uiOutput("hosp_post_button_ui"),
                                 br(),
                                 uiOutput("hosp_filters_ui"),
                                 uiOutput("hosp_table_ui")
                        )
                 )
          )
        )
      ),
      # ACTORS & TALENT REGISTRY ----------------------------------------------
      tabItem(
        tabName = "actors_registry",
        fluidRow(
          column(12,
                 tabBox(title = "Actors & Talent Registry", id = "tabActors", width = 12,
                        tabPanel("Actors & Talent Registry",
                                 h2("Coming Soon")
                        )
                 )
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
  # ---- Common Functions ----
  actual_user_role <- reactive({
    req(session$userData$auth0_info$name)
    user_email <- session$userData$auth0_info$name
    row_data <- members_df %>% filter(email == user_email)
    if(nrow(row_data) == 0) "User" else row_data$permissions[1]
  })
  
  output$view_as_select <- renderUI({
    if(tolower(actual_user_role()) == "admin"){
      # Drop "Student" option.
      selectInput("view_as", "View as:", choices = c("Guest", "Member"), selected = "Member")
    } else { NULL }
  })
  
  effective_role <- reactive({
    if(tolower(actual_user_role()) != "admin"){
      role <- tolower(actual_user_role())
      if(role == "user") role <- "guest"
      role
    } else {
      req(input$view_as)
      role <- tolower(input$view_as)
      if(role == "user") role <- "guest"
      role
    }
  })
  
  output$user_greeting <- renderText({
    user_email <- session$userData$auth0_info$name
    row_data <- members_df %>% filter(email == user_email)
    if(nrow(row_data) == 0){
      paste0("Welcome, ", user_email, " | Member type: Guest")
    } else {
      paste0("Welcome, ", row_data$name[1], " | Member type: ", row_data$permissions[1])
    }
  })
  
  # ---- Professional Database Explanation ----
  output$prof_explanation <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      tagList(
        p("Welcome to the Professional Database! Thank you for your support as a paid member; 805 Film Co could not exist without you.",
          style = "font-weight: bold; color: #113140;"),
        "This is the ultimate rolodex with over 500+ local film professionals (growing weekly), to nurture your local network. Only paying members have access to the entire database.",
        tags$u("Mass emails to all Members are not allowed."),
        " If you want to share something with the wider Collective, please contact ", tags$a("hello@805filmco.com")
      )
    } else {
      tagList(
        p("Welcome to the Professional Database! Only paying members can view full contact info.", style = "font-weight: bold; color: #113140;"),
        br(),
        p(span("Upgrade to paid membership to have access to the entire database.", style = "font-weight: bold; color: #880808;"))
      )
    }
  })
  
  # ---- Community Board Section ----
  output$post_button_ui <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      tagList(
        p("Welcome to the Community Board! Thank you for your support as a paid member; 805 Film Co could not exist without you.",
          style = "font-weight: bold; color: #113140;"),
        "Think of it as a specialized bulletin board for film professionals. You can see & post things here for the Collective, including but not limited to: Job Posts (both seeking and offering), Offices for Rent, Film Festival Calls for Submissions, and other Queries.",
        tags$u("Anyone"), " can see the Community Board, but ", tags$u("only paying members"), " can post. Posts are monitored & archived after 30 days.",
        br(), br(),
        actionButton("cb_new_post", "Add a New Post", icon = icon("plus"),
                     style = "color: #fff; background-color: #113140; border-color: #2e6da4")
      )
    } else {
      tagList(
        "Think of it as a specialized bulletin board for film professionals. You can see & post things here for the Collective, including but not limited to: Job Posts (both seeking and offering), Offices for Rent, Film Festival Calls for Submissions, and other Queries.",
        tags$u("Anyone"), " can see the Community Board, but ", tags$u("only paying members"), " can post. Posts are monitored & archived after 30 days.",
        br(), br(),
        p(span("Upgrade to paid membership to have access to post.", style = "font-weight: bold; color: #880808;"))
      )
    }
  })
  
  # *** New: Community Board Post Modal ***
  observeEvent(input$cb_new_post, {
    showModal(modalDialog(
      title = "Add a New Community Post",
      fluidRow(
        column(6, selectInput("cb_post_type", "Type:",
                              choices = c("Job-Offering", "Job-Seeking", "Office-Offering", "Office-Seeking", "Festival", "Other")))
      ),
      conditionalPanel(
        "input.cb_post_type == 'Job-Offering' || input.cb_post_type == 'Job-Seeking'",
        fluidRow(
          column(6, textInput("cb_job_title", "Job Title:")),
          column(6, textInput("cb_job_dates", "Dates:"))
        ),
        fluidRow(
          column(6, textInput("cb_job_rate", "Rate:")),
          column(6, textInput("cb_job_contact", "Contact Info:"))
        ),
        fluidRow(
          column(12, textAreaInput("cb_job_desc", "Job Description:", height = "100px"))
        )
      ),
      conditionalPanel(
        "input.cb_post_type == 'Office-Offering' || input.cb_post_type == 'Office-Seeking'",
        fluidRow(
          column(6, textInput("cb_office_title", "Office Title:")),
          column(6, textInput("cb_office_cost", "Cost:"))
        ),
        fluidRow(
          column(6, textInput("cb_office_contact", "Contact Info:"))
        ),
        fluidRow(
          column(12, textAreaInput("cb_office_desc", "Description:", height = "100px"))
        )
      ),
      conditionalPanel(
        "input.cb_post_type == 'Festival'",
        fluidRow(
          column(6, textInput("cb_fest_title", "Festival Title:")),
          column(6, textInput("cb_fest_deadline", "Deadline:"))
        ),
        fluidRow(
          column(6, textInput("cb_fest_link", "Link:")),
          column(6, textInput("cb_fest_contact", "Contact Info:"))
        ),
        fluidRow(
          column(12, textAreaInput("cb_fest_desc", "Description:", height = "100px"))
        )
      ),
      conditionalPanel(
        "input.cb_post_type == 'Other'",
        fluidRow(
          column(6, textInput("cb_other_title", "Post Title:")),
          column(6, textInput("cb_other_contact", "Contact Info:"))
        ),
        fluidRow(
          column(12, textAreaInput("cb_other_desc", "Description:", height = "100px"))
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("cb_submit_post", "Submit", icon = icon("paper-plane"),
                     style = "color: #fff; background-color: #113140; border-color: #2e6da4")
      ),
      size = "m", easyClose = FALSE
    ))
  })
  
  cb_filtered_data <- reactive({
    if(is.null(input$cb_type) || length(input$cb_type)==0){
      cb_data_display
    } else {
      cb_data_display %>% filter(Type %in% input$cb_type)
    }
  })
  
  output$cb_table <- renderDT({
    datatable(cb_filtered_data(),
              rownames = FALSE,
              selection = "single",
              options = list(dom = "", autoWidth = TRUE, pageLength = 100,
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
                             )),
              escape = FALSE,
              class = "stripe")
  })
  
  observeEvent(input$cb_table_rows_selected, {
    req(input$cb_table_rows_selected)
    sel_row <- input$cb_table_rows_selected
    entry <- board_entries[[ cb_data_sorted$row_order[sel_row] ]]
    output$cb_modal_text <- renderUI({
      if(is.null(entry)) return(NULL)
      if(entry$type %in% c("Job-Offering", "Job-Seeking")){
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Dates</b>:", entry$dates,
          "<b>Rate</b>:", entry$rate,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          if(!is.null(entry$date)) paste("<b>Date</b>:", entry$date) else "",
          sep = "<br>"
        ))
      } else if(entry$type %in% c("Office-Offering", "Office-Seeking")){
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Cost</b>:", entry$cost,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          if(!is.null(entry$date)) paste("<b>Date</b>:", entry$date) else "",
          sep = "<br>"
        ))
      } else if(entry$type == "Festival"){
        HTML(paste(
          "<b>Title</b>:", entry$title,
          "<b>Link</b>:", entry$link,
          "<b>Deadline</b>:", entry$due_dates,
          "<b>Contact</b>:", entry$contact,
          "<b>Description</b>:", entry$desc,
          if(!is.null(entry$date)) paste("<b>Date</b>:", entry$date) else "",
          sep = "<br>"
        ))
      } else if(entry$type == "Other"){
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
    showModal(modalDialog(
      title = "Details",
      uiOutput("cb_modal_text"),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  observeEvent(input$cb_submit_post, {
    if(is.null(input$cb_post_type)) return(NULL)
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
    if(is.null(new_post$title) || new_post$title == ""){
      shinyalert("Error", "Please enter a title.", type = "error")
      return()
    }
    df_append <- data.frame(entries = paste0("'", toJSON(new_post), "'"))
    sheet_append(google_doc, df_append, sheet = "Community Board")
    removeModal()
    shinyalert("Success", "Your post has been added.", type = "success")
  })
  
  # ---- Gear Catalog Section ----
  output$gear_explanation <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      p("Welcome to the Gear Catalog! You can view and post gear listings.",
        style = "color: #113140; font-weight: bold;")
    } else {
      p(span("Gear Catalog is only available for paid members.", style = "font-weight: bold; color: #880808;"))
    }
  })
  
  output$gear_post_button_ui <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      actionButton("gear_new_post", "Add New Gear", icon = icon("plus"),
                   style = "color: #fff; background-color: #113140; border-color: #2e6da4")
    } else {
      NULL
    }
  })
  
  output$gear_filters_ui <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      fluidRow(
        column(4,
               selectizeInput("gear_type_filter", "Filter by Gear Type:",
                              choices = c("Camera", "Grip & Electric", "Post Production Supplies", "Production Supplies", "Cases & Bags", "Costumes", "Props", "Miscellaneous"),
                              multiple = TRUE, options = list(placeholder = 'Select gear type(s)'))
        ),
        column(4,
               selectizeInput("terms_filter", "Filter by Terms:",
                              choices = c("Buy", "Rent"),
                              multiple = TRUE, options = list(placeholder = 'Select term(s)'))
        ),
        column(4,
               selectizeInput("intent_filter", "Filter by Intent:",
                              choices = c("Seeking", "Offering"),
                              multiple = TRUE, options = list(placeholder = 'Select intent(s)'))
        )
      )
    } else { NULL }
  })
  
  output$gear_table_ui <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      DTOutput("gear_table")
    } else { NULL }
  })
  
  gear_data_display <- reactive({
    df <- gear_data()
    if(nrow(df)==0) return(df)
    df$Date_parsed <- as.Date(df$Date, format = "%m-%d-%Y")
    df_sorted <- df %>% arrange(desc(Date_parsed), desc(row_order))
    df_sorted %>% select(GearType, Terms, Intent, Price, Date)
  })
  
  gear_filtered_data <- reactive({
    df <- gear_data_display()
    if(!is.null(input$gear_type_filter) && length(input$gear_type_filter) > 0){
      df <- df %>% filter(GearType %in% input$gear_type_filter)
    }
    if(!is.null(input$terms_filter) && length(input$terms_filter) > 0){
      df <- df %>% filter(Terms %in% input$terms_filter)
    }
    if(!is.null(input$intent_filter) && length(input$intent_filter) > 0){
      df <- df %>% filter(Intent %in% input$intent_filter)
    }
    df
  })
  
  output$gear_table <- renderDT({
    datatable(gear_filtered_data(),
              rownames = FALSE,
              options = list(dom = "t", pageLength = 100, autoWidth = TRUE),
              escape = FALSE,
              class = "stripe")
  })
  
  observeEvent(input$gear_new_post, {
    showModal(modalDialog(
      title = "Add New Gear Listing",
      fluidRow(
        column(6, selectInput("gear_type", "Gear Type:",
                              choices = c("Camera", "Grip & Electric", "Post Production Supplies", "Production Supplies", "Cases & Bags", "Costumes", "Props", "Miscellaneous"))),
        column(6, selectInput("terms", "Terms:", choices = c("Buy", "Rent")))
      ),
      fluidRow(
        column(6, selectInput("intent", "Intent:", choices = c("Seeking", "Offering"))),
        column(6, textInput("price", "Price:"))
      ),
      textAreaInput("gear_description", "Description:", "", height = "100px"),
      textInput("gear_contact", "Contact:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("gear_submit_post", "Submit", icon = icon("paper-plane"),
                     style = "color: #fff; background-color: #113140; border-color: #2e6da4")
      ),
      size = "m", easyClose = FALSE
    ))
  })
  
  observeEvent(input$gear_submit_post, {
    new_gear <- list(
      gear_type = input$gear_type,
      terms = input$terms,
      intent = input$intent,
      description = input$gear_description,
      price = input$price,
      contact = input$gear_contact,
      date = format(Sys.Date(), "%m-%d-%Y")
    )
    if(is.null(new_gear$description) || new_gear$description == ""){
      shinyalert("Error", "Please enter a description.", type = "error")
      return()
    }
    df_append <- data.frame(entries = paste0("'", toJSON(new_gear), "'"))
    sheet_append(google_doc, df_append, sheet = "Gear Catalog")
    removeModal()
    shinyalert("Success", "Your gear listing has been added.", type = "success")
  })
  
  observeEvent(input$gear_table_rows_selected, {
    req(input$gear_table_rows_selected)
    sel_row <- input$gear_table_rows_selected
    selected_gear <- gear_data()[sel_row, ]
    output$gear_modal_text <- renderUI({
      HTML(paste(
        "<b>Type:</b>", selected_gear$GearType,
        "<b>Terms:</b>", selected_gear$Terms,
        "<b>Intent:</b>", selected_gear$Intent,
        "<b>Price:</b>", selected_gear$Price,
        "<b>Date:</b>", selected_gear$Date,
        "<br><br>",
        "<b>Description:</b>", selected_gear$Description,
        "<br>",
        "<b>Contact:</b>", selected_gear$Contact,
        sep = "<br>"
      ))
    })
    showModal(modalDialog(
      title = "Gear Listing Details",
      uiOutput("gear_modal_text"),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  # ---- Hospitality Services Section ----
  output$hosp_explanation <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      p("Welcome to Hospitality Services! You can view and post hospitality listings.",
        style = "color: #113140; font-weight: bold;")
    } else {
      p(span("Hospitality Services is only available for paid members.", style = "font-weight: bold; color: #880808;"))
    }
  })
  
  output$hosp_post_button_ui <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      actionButton("hosp_new_post", "Add New Hospitality Service", icon = icon("plus"),
                   style = "color: #fff; background-color: #113140; border-color: #2e6da4")
    } else {
      NULL
    }
  })
  
  output$hosp_filters_ui <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      fluidRow(
        column(4,
               selectizeInput("hosp_type_filter", "Filter by Type:",
                              choices = c("Catering", "Lodging", "Transport"),
                              multiple = TRUE, options = list(placeholder = 'Select type(s)'))
        ),
        column(4,
               selectizeInput("lodging_filter", "Filter by Lodging Type:",
                              choices = c("Hotel", "Short-term Rental", "Long-term Rental"),
                              multiple = TRUE, options = list(placeholder = 'Select lodging type(s)'))
        )
      )
    } else { NULL }
  })
  
  output$hosp_table_ui <- renderUI({
    if(effective_role() %in% c("admin", "member")){
      DTOutput("hosp_table")
    } else { NULL }
  })
  
  hospitality_data_display <- reactive({
    df <- hospitality_data()
    if(nrow(df)==0) return(df)
    df$Date_parsed <- as.Date(df$Date, format = "%m-%d-%Y")
    df_sorted <- df %>% arrange(desc(Date_parsed), desc(row_order))
    df_sorted %>% select(Type, LodgingType, Company, Date)
  })
  
  hosp_filtered_data <- reactive({
    df <- hospitality_data_display()
    if(!is.null(input$hosp_type_filter) && length(input$hosp_type_filter) > 0){
      df <- df %>% filter(Type %in% input$hosp_type_filter)
    }
    if(!is.null(input$lodging_filter) && length(input$lodging_filter) > 0){
      df <- df %>% filter(LodgingType %in% input$lodging_filter)
    }
    df
  })
  
  output$hosp_table <- renderDT({
    datatable(hosp_filtered_data(),
              rownames = FALSE,
              options = list(dom = "t", pageLength = 100, autoWidth = TRUE),
              escape = FALSE,
              class = "stripe")
  })
  
  observeEvent(input$hosp_new_post, {
    showModal(modalDialog(
      title = "Add New Hospitality Service",
      fluidRow(
        column(6, selectInput("hosp_type", "Type:", choices = c("Catering", "Lodging", "Transport"))),
        column(6, conditionalPanel(
          "input.hosp_type == 'Lodging'",
          selectInput("lodging_type", "Lodging Type:", choices = c("Hotel", "Short-term Rental", "Long-term Rental"))
        ))
      ),
      textInput("hosp_company", "Company:", ""),
      textInput("hosp_contact", "Contact:", ""),
      textInput("hosp_discount", "Member Discount Code:", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("hosp_submit_post", "Submit", icon = icon("paper-plane"),
                     style = "color: #fff; background-color: #113140; border-color: #2e6da4")
      ),
      size = "m", easyClose = FALSE
    ))
  })
  
  observeEvent(input$hosp_submit_post, {
    new_hosp <- list(
      type = input$hosp_type,
      lodging_type = if(input$hosp_type == "Lodging") input$lodging_type else "",
      company = input$hosp_company,
      contact = input$hosp_contact,
      discount_code = input$hosp_discount,
      date = format(Sys.Date(), "%m-%d-%Y")
    )
    if(is.null(new_hosp$company) || new_hosp$company == ""){
      shinyalert("Error", "Please enter a company name.", type = "error")
      return()
    }
    df_append <- data.frame(entries = paste0("'", toJSON(new_hosp), "'"))
    sheet_append(google_doc, df_append, sheet = "Hospitality Services")
    removeModal()
    shinyalert("Success", "Your hospitality listing has been added.", type = "success")
  })
  
  observeEvent(input$hosp_table_rows_selected, {
    req(input$hosp_table_rows_selected)
    sel_row <- input$hosp_table_rows_selected
    selected_hosp <- hospitality_data()[sel_row, ]
    output$hosp_modal_text <- renderUI({
      HTML(paste(
        "<b>Type:</b>", selected_hosp$Type,
        "<b>Lodging Type:</b>", selected_hosp$LodgingType,
        "<b>Company:</b>", selected_hosp$Company,
        "<b>Contact:</b>", selected_hosp$Contact,
        "<b>Discount Code:</b>", selected_hosp$DiscountCode,
        "<b>Date:</b>", selected_hosp$Date,
        sep = "<br>"
      ))
    })
    showModal(modalDialog(
      title = "Hospitality Service Details",
      uiOutput("hosp_modal_text"),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  # ---- Local Professionals Section ----
  prof_filtered <- reactive({
    data_in <- prof_db
    if(!is.null(input$prof_role) && length(input$prof_role) > 0){
      data_in <- data_in %>% filter(grepl(paste(input$prof_role, collapse = "|"), role))
    }
    if(!is.null(input$prof_loc) && length(input$prof_loc) > 0){
      data_in <- data_in %>% filter(grepl(paste(input$prof_loc, collapse = "|"), location))
    }
    data_in <- data_in %>% filter(grepl(input$prof_name, name, ignore.case = TRUE))
    if(effective_role() == "guest"){
      data_in <- data_in %>% filter(membership != "")
    }
    data_in
  })
  
  output$prof_error <- renderText({
    if(nrow(prof_filtered()) == 0) "No results found." else ""
  })
  
  output$prof_table <- renderDT({
    data_show <- prof_filtered()
    if(nrow(data_show) == 0) return(NULL)
    data_show <- data_show %>% select(membership, name, role, location, website)
    colnames(data_show) <- c("Membership", "Name", "Role", "Location", "Website")
    datatable(data_show,
              rownames = FALSE,
              selection = "single",
              options = list(dom = "t", pageLength = 100, autoWidth = TRUE),
              escape = FALSE,
              class = "stripe")
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
