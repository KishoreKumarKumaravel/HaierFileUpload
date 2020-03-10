library(shiny)
library(xlsx)
library(odbc)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            width = 350,
            radioButtons(
                "fileType_Input",
                label = h4("Choose File type"),
                choices = list(".csv/txt" = 1, ".xlsx" = 2),
                selected = 1,
                inline = TRUE
            ),
            fileInput(
                'file1',
                h4('Upload Items List'),
                accept = c(
                    'text/csv',
                    'text/comma-separated-values,text/plain',
                    '.csv',
                    '.xlsx'
                )
            ),
            tags$head(
                tags$style(HTML(
                    '#Uploadbutton{background-color:cyan}'
                ))
            ),
            actionButton("Uploadbutton","Upload"),
            p("Upload Members if data looks ok")
            
        ) ,
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tableOutput("contents")
            
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        inFile <- input$file1
        

        if (is.null(inFile)) {
            return(NULL) }
        
        if (input$fileType_Input == 1) {
            df <-read.csv(inFile$datapath,
                     header = TRUE,
                     stringsAsFactors = FALSE)
        } else {
           df <- read.xlsx(inFile$datapath,
                      header = TRUE,sheetIndex = 1,
                      stringsAsFactors = FALSE)
        }
        observeEvent(input$Uploadbutton, {
            conn <- dbConnect(odbc(),
                              Driver = "SQL Server",
                              Server = "10.188.34.5",
                              Database = "testdb",
                              UID = "hsproot",
                              PWD = "Pl@ceholder1")
            dbWriteTable(conn,
                         name="Upload_Test",
                         value=df,
                         overwrite=TRUE)
            dbDisconnect(conn)
        },once = TRUE)
        
        
       return(df)
        
    })
    
}
# Run the app ----
shinyApp(ui, server)