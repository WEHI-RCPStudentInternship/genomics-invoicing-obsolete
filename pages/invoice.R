
invoicePage <- function() {
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
              column(10, textInput("quote_id", NULL, placeholder = "Enter Quote ID", width = "50%"))
            ),
            br(),
            fluidRow(
              column(2, strong("Project ID:")),
              column(10, textInput("project_id", NULL, placeholder = "Enter Project ID", width = "50%"))
            ),
            fluidRow(
              column(2, strong("Project Title:")),
              column(10, textInput("project_title", NULL, placeholder = "Enter Project Title", width = "50%"))
            ),
            br(),
            fluidRow(
              column(2, strong("Project Type:")),
              column(10, textInput("project_id", NULL, placeholder = "Enter Project Type", width = "50%"))
            ),
            fluidRow(
              column(2, strong("Platform:")),
              column(10, textInput("platform", NULL, placeholder = "Enter Platform", width = "50%"))
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

generateInvoiceTable <- function(invoice_items) {
  req(invoice_items)
  
  items <- invoice_items
  
  items$Quantity <- 1
  items$Amount <- as.numeric(items$`Base Price`)
  items$Total <- items$Quantity * items$Amount
  items$Description <- paste(items$Brand, items$`Product Category`, sep = " - ")
  
  formatted <- items[, c("Product Name", "Description", "Quantity", "Amount", "Total")]
  colnames(formatted) <- c("Item", "Description", "Quantity", "Amount", "Total")
  
  return(formatted)
}

