library(shiny)
library(DT)
library(auth0)
library(googlesheets4)
library(jsonlite)
library(shinyalert)
library(tidyverse)

gs4_auth(cache = ".secrets", email = "bjornkallerud@gmail.com")
sht <- "https://docs.google.com/spreadsheets/d/1KRS6UUh2QBQpnCsDTKORLyssxoCX-dtayVfRFp7LfPo/edit#gid=0"
db <- read_sheet(sht) %>%
  distinct()

make_list <- function(row) {
  fromJSON(gsub("'", "", db$entries[row]))
}

board_entries <- lapply(1:nrow(db), make_list)

auth0_server(function(input, output, session) {
  
  df <- data.frame(
    type = unlist(lapply(board_entries, function(x) x$type)),
    title = unlist(lapply(board_entries, function(x) x$title))
  ) %>%
    setNames(c("Type", "Title"))
  
  output$mytable <- renderDT({
    datatable(df, rownames = FALSE, selection = 'single', options = list(
      dom = "", autoWidth = TRUE, pageLength = 100,
      initComplete = JS(
        "setTimeout(function(){",
        "var table = this.api();",
        "$(this.api().table().body()).on('click', 'tr', function() {",
        "if ($(this).hasClass('selected')) {",
        "$(this).removeClass('selected');",
        "} else {",
        "table.$('tr.selected').removeClass('selected');",
        "$(this).addClass('selected');",
        "}",
        "});",
        "}, 500);" # Adjust the delay time if necessary
      )
    ))
  })
  
  observeEvent(input$mytable_rows_selected, {
    req(input$mytable_rows_selected)
    cat("Row selected:", input$mytable_rows_selected, "\n")
    selected_row <- input$mytable_rows_selected
    
    output$modalText <- renderUI({
      entry <- board_entries[[selected_row]]
      
      if (is.null(entry)) {
        return(NULL)
      }
      
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
    
    # Show modal using showModal and modalDialog
    showModal(modalDialog(
      title = "Details",
      uiOutput("modalText"),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  # Post a job -----------------------------------------------------------------------
  observeEvent(input$btn, {
    # Validation can be added here
    
    if (input$post_type == "Job") {
      master_list <- list(
        type = "Job",
        title = input$post_job_title,
        dates = input$post_job_dates,
        rate = input$post_job_rate,
        contact = input$post_job_contact,
        desc = input$post_job_desc
      )
    } else if (input$post_type == "Office") {
      master_list <- list(
        type = "Office",
        title = input$post_office_title,
        cost = input$post_office_cost,
        contact = input$post_office_contact,
        desc = input$post_office_desc
      )
    } else if (input$post_type == "Festival") {
      master_list <- list(
        type = "Festival",
        title = input$post_fest_title,
        due_dates = input$post_fest_due_dates,
        link = input$post_fest_link,
        contact = input$post_fest_contact,
        desc = input$post_fest_desc
      )
    } else if (input$post_type == "Other") {
      master_list <- list(
        type = "Other",
        title = input$post_other_title,
        contact = input$post_other_contact,
        desc = input$post_other_desc
      )
    } else {
      shinyalert("Error", "Unknown post type selected.", type = "error")
      return(NULL)
    }
    
    df_append <- data.frame(entries = c(
      paste0("'", toJSON(master_list), "'")
    ))
    
    sheet_append(sht, df_append)
    
    shinyalert("Thank you!", "Your post has been added to the board", type = "success")
    
    # Optionally, you can re-read the sheet to update the table
    # db <<- read_sheet(sht) %>% distinct()
    # board_entries <<- lapply(1:nrow(db), make_list)
    # output$mytable <- renderDT({...}) # Re-render the table
  })
})

