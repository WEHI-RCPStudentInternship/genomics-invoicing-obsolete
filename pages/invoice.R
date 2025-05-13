
invoicePage <- function(quote_id, project_id, project_title, project_type, platform) {
  fluidPage(
    div(class = "content",
        div(class = "center-container",
            h2("WEHI Advanced Genomics Facility (AGF)"),
            fluidRow(
              column(2, strong("Date:")),
              column(10, p(Sys.Date()))
            ),
            fluidRow(
              column(2, strong("Quote ID:")),
              column(10, textInput("quote_id", NULL, value = quote_id, width = "50%"))
            ),
            br(),
            fluidRow(
              column(2, strong("Project ID:")),
              column(10, textInput("project_id", NULL, value = project_id, width = "50%"))
            ),
            fluidRow(
              column(2, strong("Project Title:")),
              column(10, textInput("project_title", NULL, value = project_title, width = "50%"))
            ),
            br(),
            fluidRow(
              column(2, strong("Project Type:")),
              column(10, textInput("project_id", NULL, value = project_type, width = "50%"))
            ),
            fluidRow(
              column(2, strong("Platform:")),
              column(10, textInput("platform", NULL, value = platform, width = "50%"))
            ),
            br(),
            DT::dataTableOutput("editable_invoice_table"),
            br(),
            actionButton("add_row", "Add New Item"),
            br(),
            actionButton("delete_row", "Delete Selected Item(s)"),
            br(),
            uiOutput("invoice_total"),
            br(),
            tags$small(
              "All prices are in AUD and exclude GST.",
              tags$br(),
              "Cost estimates and quotes are valid for 4 weeks.",
              tags$br(),
              "Purchase orders and additional price enquiries should be directed to Daniela Zalcenstein (zalcenstein.d@wehi.edu.au)."
            ),
            br(),
            downloadButton("download_invoice", "Download PDF"),
            actionButton("back_to_main", "Back to Main", class = "back-button")
        )
    )
  )
}

generateInvoiceTable <- function(invoice_items_data) {
  req(invoice_items_data)
  
  items <- invoice_items_data
  
  items$Quantity <- 1

  
  items$Amount <- as.numeric(items$`%PRJ surcharge`)
  items$Total <- items$Quantity * items$Amount
  items$Description <- paste(items$Brand, items$`Product Category`, sep = " - ")
  
  formatted <- items[, c("Product Name", "Description", "Quantity", "Amount", "Total")]
  colnames(formatted) <- c("Item", "Description", "Quantity", "Amount", "Total")
  
  return(formatted)
}

