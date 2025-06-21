library(shiny)
library(shinyjs) 
library(bslib) # the best way to customize the layout of a Shiny app is with R’s bslib package. 
library(deldir)
library(jpeg)
library(tidyverse)

ui <- page_fillable(
  useShinyjs(),
  layout_columns(
    card(card_header("Inputs"),
         fileInput("upload_face", "Upload face", accept = "image/jpeg"),
         fileInput("upload_fixrep", "Upload fixation report", accept = "text/csv"),
         actionButton("next_face", "Annotate fixrep for this face, then get next face"),
         actionButton("quit", "quit")
         ),
    card(card_header("Face for edit"),
         plotOutput("face_for_edit", click = "face_for_edit_click", dblclick = "face_for_edit_dblclick")
         ),
    card(card_header("Face for markup"),
         plotOutput("face_for_markup")
         )
    ,col_widths = c(2, 5, 5)
  )
  
)

server <- function(input, output) {
  
  # Globally accessible variables
  glb = reactiveValues(
    aois_all = tibble(),
    aoi = tibble(),
    fixrep = NA,
    vor = NA,
    tiles = NA
  )
  
  # Function for reading face jpegs in
  myjpeg = reactive({
    jpegfile <- input[['upload_face']]
    if(!is.null(jpegfile)){
      readJPEG(jpegfile$datapath, native=TRUE) 
    }
  })

  # Record clicks on the face
  observeEvent(input[['face_for_edit_click']], {
    this_aoi = tibble(x=input$face_for_edit_click$x, y=input$face_for_edit_click$y, id = "A", jpg=input[['upload_face']]$name)
    glb$aoi = glb$aoi %>% bind_rows(this_aoi)
  })
  
  # Respond to upload fixation report button
  observeEvent(input$upload_fixrep, {
    glb$fixrep = read_csv(input$upload_fixrep$datapath) %>% mutate(tile=as.character(NA))
  })
  
  # Respond to quit button
  observeEvent(input$quit, {
    # Add current face aois to aois_all, as when doing 'next face'
    glb$aois_all = glb$aois_all %>% bind_rows(glb$aoi)
    saveRDS(glb$aois_all, "aois_all.rds")
    saveRDS(glb$fixrep, "fixrep.rds")
    stopApp()
    browser()
  })
  
  # Handle the next image button
  observeEvent(input$next_face, {

    # Add current face aois to aois_all
    glb$aois_all = glb$aois_all %>% bind_rows(glb$aoi)
    
    # Empty this aoi tibble
    glb$aoi = tibble()
    
    # Open the file input dialog for the next image
    shinyjs::runjs("document.getElementById('upload_face').click();")
    
  })

  # Prepare the plot for the left-hand-side
  output[['face_for_edit']] <- renderPlot({
    if(!is.null(myjpeg())){
      par(mar=c(0,0,0,0))
      plot(x=0, y=0, type = 'n', xlim = c(0, 600), ylim = c(800, 0), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) 
      if(nrow(glb$aoi) > 0){
        points(x=glb$aoi$x, y=glb$aoi$y, pch=21, col="red", bg="red", cex=5)
      }}}, width=600, height=800)
  
  # Prepare the plot for the RIGHT-hand-side
  output[['face_for_markup']] <- renderPlot({
    if(!is.null(myjpeg())){
      par(mar=c(0,0,0,0))
      plot(x=0, y=0, type = 'n', xlim = c(0, 600), ylim = c(800, 0), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) 
      if(nrow(glb$aoi) >= 2){
        vor <-
          deldir(
            x=glb$aoi$x, 
            y=glb$aoi$y, 
            #id = glb$aoi$id,
            rw=c(xleft = 0, xright=600, ybottom=0, ytop=800)
            )
        plot(vor, add=TRUE, wlines="tess", showpoints=FALSE, showrect=TRUE, labelPts=TRUE, lwd=3, cex=2, lex=3, cmpnt_col=c(tri=1,tess=1,points=1,labels=2,rect=1), cmpnt_lty=c(tri=1,tess=2), axes=TRUE)
        #glb$aoi$id = vor[['ind.orig']] %>% as.character()
        glb$aoi$vor = list(vor)
        glb$aoi$tiles = list(tile.list(vor))
        
        
      }}}, width=600, height=800)

      
}

shinyApp(ui = ui, server = server)
