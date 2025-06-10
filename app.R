library(shiny)
library(bslib) # the best way to customize the layout of a Shiny app is with R’s bslib package. 
library(deldir)
#library(ggvoronoi) # remotes::install_github("garretrc/ggvoronoi", dependencies = TRUE, build_opts = c("--no-resave-data"))
#library(ggforce)
library(jpeg)
library(tidyverse)

ui <- page_fillable(
  
  layout_columns(
    card(card_header("Inputs"),
         fileInput("upload_face", "Upload face", accept = "image/jpeg"),
         fileInput("upload_fixrep", "Upload fixation report", accept = "text/csv"),
         actionButton("save_aois", "Save AOIs and exit")
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
  
  myglobals = reactiveValues(
    click_x = c(),
    click_y = c(),
    vor = NA,
    aois = tibble(click_x=as.numeric(NA), click_y=as.numeric(NA)),
    tiles = NA
  )
  
  myjpeg = reactive({
    jpegfile <- input[['upload_face']]
    if(!is.null(jpegfile)){
      readJPEG(jpegfile$datapath, native=TRUE) 
    }
  })

  # Record clicks on the image
  observeEvent(input[['face_for_edit_click']], {
    click_x <- input$face_for_edit_click$x
    click_y <- input$face_for_edit_click$y
    aois <- tibble(click_x, click_y)
    myglobals$click_x = c(myglobals$click_x, click_x)
    myglobals$click_y = c(myglobals$click_y, click_y)
    myglobals$aois = myglobals$aois %>% bind_rows(aois) %>% na.omit()
  })
  
  observeEvent(input$save_aois, {
    if(nrow(myglobals$aois)>=2){
      saveRDS(myglobals$aois, "aois.rds")
      stopApp()
    }
  })

  # Prepare the plot for the left-hand-side
  output[['face_for_edit']] <- renderPlot({
    if(!is.null(myjpeg())){
      par(mar=c(0,0,0,0))
      plot(x=0,
           y=0,
           type = 'n',
           xlim = c(0, 600),
           ylim = c(800, 0), # reversed axis
           xlab = '',
           ylab = '',
           xaxt = 'n',
           yaxt = 'n')
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) # Display the image with correct orientation
      if(length(myglobals$click_x)>0){
        points(x=myglobals$click_x, 
               y=myglobals$click_y,
               pch=21,
               col="red",
               bg="red",
               cex=5)
      }
      }
  }, width=600, height=800)
  
  # Prepare the plot for the RIGHT-hand-side
  output[['face_for_markup']] <- renderPlot({
    if(!is.null(myjpeg())){
      par(mar=c(0,0,0,0))
      plot(x=0,
           y=0,
           type = 'n',
           xlim = c(0, 600),
           ylim = c(800, 0), # reversed axis
           xlab = '',
           ylab = '',
           xaxt = 'n',
           yaxt = 'n')
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) # Display the image with correct orientation
      if(nrow(myglobals$aois)>=2){
        vor <-
          deldir(
            x=myglobals$aois$click_x, 
            y=myglobals$aois$click_y, 
            #id = as.vector(my_reactive_values$clicked_name),
            rw=c(
              xleft=0, 
              xright=600, 
              ybottom=0, 
              ytop=800)
            )
        tiles = tile.list(vor)
        myglobals$tiles = tiles
        plot(
          vor,
          add=TRUE,
          wlines = "tess",
          showpoints=FALSE,
          showrect=TRUE,
          labelPts=TRUE,
          lwd=3,
          cex=2,
          lex=3,
          cmpnt_col=c(tri=1,tess=1,points=1,labels=2,rect=1),
          cmpnt_lty=c(tri=1,tess=2),
          axes=TRUE)
      }
    }
  }, width=600, height=800)

      
}

shinyApp(ui = ui, server = server)
