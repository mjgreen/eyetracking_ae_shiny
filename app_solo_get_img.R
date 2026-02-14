library(shiny)
library(magick)
library(base64enc)

ui <- fluidPage(
  fileInput("img", "Choose an image"),
  verbatimTextOutput("dims"),
  uiOutput("imgTag")
)

server <- function(input, output, session) {

  mag_r <- reactive({
    req(input$img)
    image_read(input$img$datapath)
  })

  output$dims <- renderPrint({
    info <- image_info(mag_r())
    list(width = info$width[[1]], height = info$height[[1]], format = info$format[[1]])
  })

  output$imgTag <- renderUI({
    img <- mag_r()
    raw <- image_write(img, format = "png")  # normalise for browser display
    uri <- base64enc::dataURI(raw, mime = "image/png")
    tags$img(src = uri, style = "max-width: 100%; height: auto;")
  })

}

shinyApp(ui, server)