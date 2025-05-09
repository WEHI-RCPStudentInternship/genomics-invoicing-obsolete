library(shiny)
library(readxl)
library(tidyr)
library(DT)
library(shinyjs)

source("pages/invoice.R")
# Parse data
parse_data <- function(df) {
  df <- df %>% replace_na(replace = as.list(rep(0, ncol(df))))
  return(df)
}

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  useShinyjs(),
  uiOutput("main_ui")
)

# SERVER
server <- function(input, output, session) {
  # Page state
  current_page <- reactiveVal("main")
  
  # Data containers
  raw_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  file_path <- reactiveVal(NULL)
  invoice_items_data <- reactiveVal(NULL)
  edited_invoice_table <- reactiveVal(NULL)
  
  # Parse function
  parse_data <- function(df) {
    target_col <- "Additional reagent Cost (not incl. in kit)"
    if (target_col %in% names(df)) {
      df[[target_col]][is.na(df[[target_col]])] <- 0
    }
    return(df)
  }
  
  # Upload logic
  observeEvent(input$file, {
    req(input$file)
    file_path(input$file$datapath)
    raw_data(NULL)
    processed_data(NULL)
    invoice_items_data(NULL)
  })
  
  observeEvent(input$upload_button, {
    fp <- file_path()
    if (!is.null(fp)) {
      df <- read_excel(fp)
      raw_data(df)
      df <- parse_data(df)
      processed_data(df)
    } else {
      showNotification("Please upload a file first.", type = "warning")
    }
  })
  
  # Navigate to invoice page
  # observeEvent(input$create_invoice_page, {
  #   current_page("create_invoice")
  #   output$invoice_type_ui <- renderUI({
  #     radioButtons("invoice_type", "Select Invoice Type:",
  #                  choices = c("Internal", "External"),
  #                  selected = "Internal",
  #                  inline = TRUE)
  #   })
  # })
  
  
  
  # Observe event for generating the final invoice 
  observeEvent(input$create_invoice_page, {
    req(invoice_items_data())
    
    # navigating to a "invoice generated" page or triggering a download.
    current_page("invoice_generated")
  })
  
  # Go back
  observeEvent(input$back_to_main, {
    current_page("main")
    # invoice_items_data(NULL)
  })
  
  # Extra info summary table
  extra_info_data <- reactive({
    req(processed_data())
    df <- processed_data()
    if (all(c("Internal Extra", "External Extra") %in% names(df))) {
      return(data.frame(
        `Internal Extra` = unique(na.omit(df[["Internal Extra"]]))[1],
        `External Extra` = unique(na.omit(df[["External Extra"]]))[1]
      ))
    } else {
      return(data.frame(`Internal Extra` = NA, `External Extra` = NA))
    }
  })
  
  # Handle row selection in the data table
  observeEvent(input$data_table_rows_selected, {
    selected_rows <- input$data_table_rows_selected
    if (!is.null(processed_data()) && length(selected_rows) > 0) {
      invoice_items_data(processed_data()[selected_rows, ])
    } else {
      invoice_items_data(NULL)
    }
  })
  
  output$extra_info_table <- renderTable({
    extra_info_data()
  })
  
  # Render UI
  output$main_ui <- renderUI({
    if (current_page() == "main") {
      fluidPage(
        div(class = "navbar",
            div(class = "navbar-right",
                actionButton("home", "Home", class = "nav-button"),
                actionButton("about", "About", class = "nav-button"),
                actionButton("contact", "Contact", class = "nav-button")
            )
        ),
        div(class = "content",
            div(class = "center-container",
                sidebarLayout(
                  sidebarPanel(
                    div(class = "sidebar-box",
                        h2("Create Genomics Invoicing"),
                        fileInput("file", "Upload Master Spreadsheet (.xlsx)", accept = ".xlsx"),
                        actionButton("upload_button", "Upload Master Spreadsheet", class = "upload-button"),
                        uiOutput("create_invoice_page_ui"),
                        br(),
                        h4("Extra info (Internal/External)"),
                        tableOutput("extra_info_table")
                    )
                  ),
                  mainPanel(
                    DT::dataTableOutput("data_table")
                  )
                )
            )
        )
      )
    } else if (current_page() == "create_invoice") {
      # fluidPage(
      #   div(class = "content",
      #       div(class = "center-container",
      #           h2("Select Items for Invoice"),
      #           uiOutput("invoice_type_ui"),
      #           DT::DTOutput("selected_items_table"),
      #           br(),
      #           actionButton("back_to_main", "Back to Main", class = "back-button"),
      #           actionButton("generate_invoice", "Generate Invoice", class = "action-button")
      #       )
      #   )
      # )
    }
    else if (current_page() == "invoice_generated") {
 

                edited_invoice_table(invoice_table())
                invoicePage(quote_id = generateQuoteID(),
                            project_id = "C0000001",
                            project_title = "None",
                            project_type = "Internal",
                            platform = "Xenium")

    }
  })
  
  # Show create invoice only after upload
  output$create_invoice_page_ui <- renderUI({
    req(processed_data())
    actionButton("create_invoice_page", "Create Invoice", class = "invoice-button")
  })
  
  # View the updated master spreadsheet
  output$data_table <- DT::renderDataTable({
    req(processed_data())
    
    # 👉 Select only specific columns to show
    df <- processed_data()[, c("Product Code", "Brand","Product Category", "Product Name", "per reaction cost", "%PRJ surcharge","%EXTERNAL surcharge","Additional reagent Cost (not incl. in kit)")]
    
    datatable(df,
              rownames = FALSE,
              options = list(ordering = FALSE,
                             language = list(search = "Search Item:")),
              selection = "multiple")
  })
  
  observe({
    if (is.null(input$invoice_type)) {
      updateRadioButtons(session, "invoice_type", selected = "Internal")
    }
  })
  
  # # Build invoice reactive
  # observe({
  #   req(processed_data(), input$data_table_rows_selected)
  #   df <- processed_data()
  #   selected_rows <- input$data_table_rows_selected
  #   if (length(selected_rows) == 0) return()
  #   
  #   
  #   selected_df <- df[selected_rows, ]
  #   # Decide internal or external price
  #   price_col <- if (input$invoice_type == "Internal") "%PRJ surcharge" else "%EXTERNAL surcharge"
  #   
  #   if (!(price_col %in% names(selected_df))) {
  #     showNotification("Selected price column not found.", type = "error")
  #     return()
  #   }
  #   
  #   invoice_df <- selected_df[, c("Product Code", "Brand", "Product Category", "Product Name" ,price_col,"Additional reagent Cost (not incl. in kit)")]
  #   names(invoice_df)[names(invoice_df) == price_col] <- "Base Price"
  #   additional_col <- "Additional reagent Cost (not incl. in kit)"
  #   names(invoice_df)[names(invoice_df) == additional_col] <- "Additional Cost"
  #   
  #   # Price = Original price + additional cost
  #   invoice_df$Price <- invoice_df$`Base Price` + invoice_df$`Additional Cost`
  #   
  #   # Discount part
  #   invoice_df$Discount <- 0
  #   invoice_df$`New Price` <- invoice_df$Price
  #   
  #   invoice_items_data(invoice_df)
  # })
  
  # Render selected items table (Discount editable)
  output$selected_items_table <- DT::renderDT({
    req(invoice_items_data())
    datatable(invoice_items_data(),
              rownames = FALSE,
              options = list(ordering = FALSE),
              editable = list(target = "cell", disable = list(columns = c(0,1,2,3,4,6)))) 
  })
  
  # Handle edits to Discount
  observeEvent(input$selected_items_table_cell_edit, {
    info <- input$selected_items_table_cell_edit
    i <- as.numeric(info$row)
    j <- as.numeric(info$col)
    v <- info$value
    
    current_data <- invoice_items_data()
    
    # allow editing Discount column
    if (j == which(names(current_data) == "Discount") - 1) {
      discount_pct <- suppressWarnings(as.numeric(v))
      if (!is.na(discount_pct) && discount_pct >= 0 && discount_pct <= 100) {
        discount_frac <- discount_pct / 100
        current_data$Discount[i] <- discount_pct
        current_data$`New Price`[i] <- round(current_data$Price[i] * (1 - discount_frac))
        invoice_items_data(current_data)
      } else {
        showNotification("Enter a valid discount (0-100%).", type = "error")
      }
    }
  })
  
  # Generate a custom Quote ID
  generateQuoteID <- function() {
    date_part <- format(Sys.Date(), "%Y%m%d")
    random_part <- sprintf("%04d", sample(0:9999, 1))
    paste0("WEHI-AGF-", date_part, "-", random_part)
  }
  
  #Render invoice preview
  output$invoice_preview_table <- renderDataTable({
    datatable(invoice_table(), options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  invoice_table <- reactive({
    req(invoice_items_data())
    generateInvoiceTable(invoice_items_data())
  })
  
  output$editable_invoice_table <- DT::renderDataTable({
    req(edited_invoice_table())
    datatable(
      edited_invoice_table(),
      editable = TRUE, 
      options = list(dom = 't', paging = FALSE),
      rownames = FALSE
    )
  })
  
  
  
  observeEvent(input$editable_invoice_table_cell_edit, {
    info <- input$editable_invoice_table_cell_edit
    print(info)  # Confirm what's received from the browser
    temp <- edited_invoice_table()
    
    if (is.null(temp) || is.null(info$row) || is.null(info$col) ||
        info$row < 1 || info$col < 0) {  # col can be 0 at minimum
      return()
    }
    
    # Shift col from JavaScript (zero-based) to R (one-based)
    col_index <- info$col + 1
    
    if (info$row > nrow(temp) || col_index > ncol(temp)) {
      showNotification("Invalid edit: index out of range.", type = "error")
      return()
    }
    
    temp[info$row, col_index] <- DT::coerceValue(info$value, temp[[col_index]])
    edited_invoice_table(temp)
  })
  
  
  
  
   observeEvent(input$add_row, {
    temp <- edited_invoice_table()
    
    if (is.null(temp)) {
      showNotification("No invoice table loaded. Please select items first.", type = "error")
      return()
    }
    
    # Define dropdown options
    item_options <- c(
      "Chromium Next GEM Single Cell 5’ HT Kit v2",
      "Library Construction Kit",
      "Dual Index kit TT or TN Set A",
      "Chromium Next GEM Chip N Single Cell Kit"
    )
    
    description_options <- c(
      "per capture",
      "per chip",
      "per sample",
      "per HashTag",
      "per test vial"
    )

    # Create new row with dropdown icons
    new_row <- data.frame(
      Item = item_options[1],
      Description = description_options[1],
      Quantity = 1,
      Amount = 100,
      Total = 100,
      stringsAsFactors = FALSE
    )
    
    # Append the new row
    edited_invoice_table(rbind(temp, new_row))
  })
  
  observeEvent(input$delete_row, {
    selected <- input$editable_invoice_table_rows_selected
    temp <- edited_invoice_table()
    if (!is.null(selected) && length(selected) > 0) {
      temp <- temp[-selected, ]
      edited_invoice_table(temp)
    } else {
      showNotification("No row selected for deletion.", type = "warning")
    }
  })
  
  
  #invoice total amount
  output$invoice_total <- renderUI({
    table_data <- if (!is.null(edited_invoice_table())) {
      edited_invoice_table()
    } else {
      invoice_table()
    }
    
    req(table_data)
    
    total <- sum(as.numeric(table_data$Total), na.rm = TRUE)
    
    fluidRow(
      column(6, tags$hr(style = "border-top: 2px solid #000; width: 100%;")),
      column(6, tags$hr(style = "border-top: 2px solid #000; width: 100%;"))
    )
    
    fluidRow(
      column(6, h4("Total Amount:", style = "font-weight: bold;")),
      column(6, h4(paste("$", formatC(total, format = "f", digits = 2)), 
                   style = "text-align: right; font-weight: bold;"))
    )
  })
  
  
  
  
  output$download_invoice <- downloadHandler(
    filename = function() {
      paste0("Invoice_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Save a temporary Rmd file
      tempReport <- file.path(tempdir(), "invoice.Rmd")
      file.copy("invoice.Rmd", tempReport, overwrite = TRUE)
      
      # Parameters to pass into Rmd
      params <- list(
        date = Sys.Date(),
        quote_id = input$quote_id,
        project_id = input$project_id,
        project_title = input$project_title,
        project_type = input$project_type,  # Fix project_type here
        platform = input$platform,
        table_data = generateInvoiceTable(invoice_items_data())
      )
      
      # Use tempdir() to save in the default system temp directory
      output_path <- file.path(tempdir(), paste0("Invoice_", Sys.Date(), ".pdf"))
      
      rmarkdown::render(
        tempReport,
        output_file = output_path,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      # Move the generated file to the 'file' parameter (Shiny will then serve it to the user)
      file.copy(output_path, file)
    }
  )
  
}


# Run the app
shinyApp(ui = ui, server = server)
