# Install required packages if not already installed
# install.packages("shiny")
# install.packages("DT")

# Load the required libraries
library(shiny)
library(DT)

# Sample data
data <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "David", "Eva"),
  Age = c(25, 30, 28, 35, 29),
  Tags = c("Tag1", "Tag2", "Tag3", "Tag1", "Tag2")
)

# Define the UI
ui <- fluidPage(
  titlePanel("Table with Details"),
  sidebarLayout(
    sidebarPanel(
      textInput("selectedID", label = "Selected ID"),
      textInput("selectedName", label = "Selected Name"),
      textInput("selectedAge", label = "Selected Age"),
      selectInput("selectedTags", label = "Selected Tags", choices = c("Tag1", "Tag2", "Tag3"), multiple = TRUE),
      actionButton("addBtn", "Add"),
      actionButton("editBtn", "Edit"),
      actionButton("deleteBtn", "Delete"),
      actionButton("clearBtn", "Clear")  # Added Clear button
    ),
    mainPanel(
      DTOutput("data_table")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  data <- reactiveVal(data)
  
  output$data_table <- renderDT({
    datatable(data(), editable = TRUE, selection = "single")
  })
  
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    str(info)  # Uncomment to view the structure of info in the console
    
    modified_data <- data()
    
    if (!is.null(info)) {
      cell_row <- info$row
      cell_col <- info$col
      cell_value <- info$value
      
      modified_data[cell_row, cell_col] <- cell_value
      data(modified_data)
    }
  })
  
  observeEvent(input$addBtn, {
    new_entry <- isolate(c(
      as.numeric(max(data()$ID)) + 1,
      input$selectedName,
      as.numeric(input$selectedAge),
      paste(input$selectedTags, collapse = ", ")
    ))
    
    data(rbind(data(), new_entry))
    clearInputs()  # Clear inputs after adding a new row
  })
  
  observeEvent(input$editBtn, {
    selected_row <- as.numeric(input$data_table_rows_selected)
    
    if (length(selected_row) == 1) {
      edited_entry <- c(
        as.numeric(input$selectedID),
        input$selectedName,
        as.numeric(input$selectedAge),
        paste(input$selectedTags, collapse = ", ")
      )
      
      modified_data <- data()
      modified_data[selected_row, ] <- edited_entry
      data(modified_data)
      clearInputs()  # Clear inputs after editing a row
    }
  })
  
  observeEvent(input$deleteBtn, {
    selected_rows <- as.numeric(input$data_table_rows_selected)
    
    if (length(selected_rows) > 0) {
      data(data()[-selected_rows, , drop = FALSE])
      clearInputs()  # Clear inputs after deleting a row
    }# Install required packages if not already installed
    # install.packages("shiny")
    # install.packages("DT")
    
    # Load the required libraries
    library(shiny)
    library(DT)
    
    # Sample data
    data <- data.frame(
      ID = 1:5,
      Name = c("Alice", "Bob", "Charlie", "David", "Eva"),
      Age = c(25, 30, 28, 35, 29),
      Tags = c("Tag1", "Tag2", "Tag3", "Tag1", "Tag2")
    )
    
    # Define the UI
    ui <- fluidPage(
      titlePanel("Table with Details"),
      sidebarLayout(
        sidebarPanel(
          textInput("selectedID", label = "Selected ID"),
          textInput("selectedName", label = "Selected Name"),
          textInput("selectedAge", label = "Selected Age"),
          selectInput("selectedTags", label = "Selected Tags", choices = c("Tag1", "Tag2", "Tag3"), multiple = TRUE),
          actionButton("addBtn", "Add"),
          actionButton("editBtn", "Edit"),
          actionButton("deleteBtn", "Delete"),
          actionButton("clearBtn", "Clear")  # Added Clear button
        ),
        mainPanel(
          DTOutput("data_table")
        )
      )
    )
    
    # Define the server
    server <- function(input, output, session) {
      data <- reactiveVal(data)
      
      output$data_table <- renderDT({
        datatable(data(), editable = TRUE, selection = "single")
      })
      
      observeEvent(input$data_table_cell_edit, {
        info <- input$data_table_cell_edit
        str(info)  # Uncomment to view the structure of info in the console
        
        modified_data <- data()
        
        if (!is.null(info)) {
          cell_row <- info$row
          cell_col <- info$col
          cell_value <- info$value
          
          modified_data[cell_row, cell_col] <- cell_value
          data(modified_data)
        }
      })
      
      observeEvent(input$addBtn, {
        new_entry <- isolate(c(
          as.numeric(max(data()$ID)) + 1,
          input$selectedName,
          as.numeric(input$selectedAge),
          paste(input$selectedTags, collapse = ", ")
        ))
        
        data(rbind(data(), new_entry))
        clearInputs()  # Clear inputs after adding a new row
      })
      
      observeEvent(input$editBtn, {
        selected_row <- as.numeric(input$data_table_rows_selected)
        
        if (length(selected_row) == 1) {
          edited_entry <- c(
            as.numeric(input$selectedID),
            input$selectedName,
            as.numeric(input$selectedAge),
            paste(input$selectedTags, collapse = ", ")
          )
          
          modified_data <- data()
          modified_data[selected_row, ] <- edited_entry
          data(modified_data)
          clearInputs()  # Clear inputs after editing a row
        }
      })
      
      observeEvent(input$deleteBtn, {
        selected_rows <- as.numeric(input$data_table_rows_selected)
        
        if (length(selected_rows) > 0) {
          data(data()[-selected_rows, , drop = FALSE])
          clearInputs()  # Clear inputs after deleting a row
        }
      })
      
      observeEvent(input$clearBtn, {
        clearInputs()  # Clear inputs when the "Clear" button is clicked
      })
      
      observeEvent(input$data_table_rows_selected, {
        selected_row <- as.numeric(input$data_table_rows_selected)
        if (length(selected_row) > 0) {
          selected_data <- data()[selected_row, ]
          updateTextInput(session, "selectedID", value = as.character(selected_data$ID))
          updateTextInput(session, "selectedName", value = as.character(selected_data$Name))
          updateTextInput(session, "selectedAge", value = as.character(selected_data$Age))
          updateSelectInput(session, "selectedTags", selected = strsplit(selected_data$Tags, ", ")[[1]])
        }
      })
      
      # Function to clear inputs
      clearInputs <- function() {
        updateTextInput(session, "selectedID", value = "")
        updateTextInput(session, "selectedName", value = "")
        updateTextInput(session, "selectedAge", value = "")
        updateSelectInput(session, "selectedTags", selected = character(0))
      }
    }
    
    # Run the application
    shinyApp(ui, server)
    
    
  })
  
  observeEvent(input$clearBtn, {
    clearInputs()  # Clear inputs when the "Clear" button is clicked
  })
  
  observeEvent(input$data_table_rows_selected, {
    selected_row <- as.numeric(input$data_table_rows_selected)
    if (length(selected_row) > 0) {
      selected_data <- data()[selected_row, ]
      updateTextInput(session, "selectedID", value = as.character(selected_data$ID))
      updateTextInput(session, "selectedName", value = as.character(selected_data$Name))
      updateTextInput(session, "selectedAge", value = as.character(selected_data$Age))
      updateSelectInput(session, "selectedTags", selected = strsplit(selected_data$Tags, ", ")[[1]])
    }
  })
  
  # Function to clear inputs
  clearInputs <- function() {
    updateTextInput(session, "selectedID", value = "")
    updateTextInput(session, "selectedName", value = "")
    updateTextInput(session, "selectedAge", value = "")
    updateSelectInput(session, "selectedTags", selected = character(0))
  }
}


selectNextRow <- function(session) {
  selected_row <- as.numeric(input$data_table_rows_selected)
  
  if (length(selected_row) == 1 && selected_row < nrow(data())) {
    next_row <- selected_row + 1
    updateDTOutput(session, "data_table", row = next_row, resetPaging = FALSE)
  }
}


# Run the application
shinyApp(ui, server)
