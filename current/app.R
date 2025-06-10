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
         fileInput("upload_fixrep", "Upload fixation report", accept = "text/csv"),
         fileInput("upload_face", "Upload face (after fixation report)", accept = "image/jpeg"),
         input_switch("toggle_fixations", "Show (or Hide) Fixations on the plot"),
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
  g = reactiveValues(
    aois_all = tibble(),
    aoi = tibble(),
    fixrep = NA,
    vor = NA,
    tiles = NA
  )
  
  flag = FALSE
  
  # Read face jpeg function
  myjpeg = reactive({
    jpegfile <- input[['upload_face']]
    if(!is.null(jpegfile)){
      readJPEG(jpegfile$datapath, native=TRUE) 
    }
  })

  # Respond to upload fixation report button
  observeEvent(input$upload_fixrep, {
    g$fixrep = read_csv(input$upload_fixrep$datapath, show_col_types = F) %>% 
      mutate(tile=as.character(NA)) %>% 
      mutate(
        CURRENT_FIX_X = ((CURRENT_FIX_X/1920) * 600), 
        CURRENT_FIX_Y = ((CURRENT_FIX_Y/1080) * 800),
        tile = as.numeric(NA)
      )
  })
  
  # Observe without event, to assign to reactive values object
  observe({
    if(!is.null(input$upload_face) && !is.null(input$upload_fixrep) && flag==FALSE){
      g$fixrep = g$fixrep %>% filter(face_jpeg == input$upload_face$name)
      flag = TRUE
    }
  })
  
  # Respond to next image button
  observeEvent(input$next_face, {
    #browser()
    for(i in 1:nrow(g$fixrep)){
      x=g$fixrep[i, "CURRENT_FIX_X"] %>% pull()
      y=g$fixrep[i, "CURRENT_FIX_Y"] %>% pull()
      tl=g$aoi[1,'tiles'] %>% unlist(recursive=FALSE)
      g$fixrep[i, "tile"] = which.tile(x,y,tl)
    }
    saveRDS(g$fixrep, "test.rds")
    #browser()
    # Add current face aois to aois_all
    g$aois_all = g$aois_all %>% bind_rows(g$aoi)
    # Empty this aoi tibble
    g$aoi = tibble()
    flag=FALSE
    # Open the file input dialog for the next image
    shinyjs::runjs("document.getElementById('upload_face').click();")
  })

  # Respond to quit button
  observeEvent(input$quit, {
    # Add current face aois to aois_all, as when doing 'next face'
    g$aois_all = g$aois_all %>% bind_rows(g$aoi)
    saveRDS(g$aois_all, "aois_all.rds")
    saveRDS(g$fixrep, "fixrep.rds")
    stopApp()
    browser()
  })
  
  # Respond to clicks on the face
  observeEvent(input[['face_for_edit_click']], {
    this_aoi = tibble(x=input$face_for_edit_click$x, y=input$face_for_edit_click$y, id = "A", jpg=input[['upload_face']]$name)
    g$aoi = g$aoi %>% bind_rows(this_aoi)
  })
  
  # OUTPUT

  # Prepare the plot for the left-hand-side
  output[['face_for_edit']] <- renderPlot({
    if(!is.null(myjpeg())){
      par(mar=c(0,0,0,0))
      plot(x=0, y=0, type = 'n', xlim = c(0, 600), ylim = c(800, 0), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) 
      if(nrow(g$aoi) > 0){
        points(x=g$aoi$x, y=g$aoi$y, pch=21, col="red", bg="red", cex=5)
      }}}, width=600, height=800)
  
  # Prepare the plot for the RIGHT-hand-side
  output[['face_for_markup']] <- renderPlot({
    if(!is.null(myjpeg())){
      par(mar=c(0,0,0,0))
      plot(x=0, y=0, type = 'n', xlim = c(0, 600), ylim = c(800, 0), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) 
      if(input$toggle_fixations==TRUE){
        points(g$fixrep$CURRENT_FIX_X, g$fixrep$CURRENT_FIX_Y, pch=21, bg="yellow", cex=5)
      }
      if(nrow(g$aoi) >= 2){
        vor <-
          deldir(
            x=g$aoi$x, 
            y=g$aoi$y, 
            #id = g$aoi$id,
            rw=c(xleft = 0, xright=600, ybottom=0, ytop=800)
            )
        plot(vor, add=TRUE, wlines="tess", showpoints=FALSE, showrect=TRUE, labelPts=TRUE, lwd=3, cex=2, lex=3, cmpnt_col=c(tri=1,tess=1,points=1,labels=2,rect=1), cmpnt_lty=c(tri=1,tess=2), axes=TRUE)
        #g$aoi$id = vor[['ind.orig']] %>% as.character()
        g$aoi$vor = list(vor)
        g$aoi$tiles = list(tile.list(vor))
        
        
      }}}, width=600, height=800)

      
}

shinyApp(ui = ui, server = server)
