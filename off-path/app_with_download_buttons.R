library(shiny)
library(shinyjs) # for reset()
library(base64enc)
library(tidyverse)
library(bslib)
library(deldir)
library(jpeg)

options(shiny.maxRequestSize = 30*1024^2)

dummynames=LETTERS



ui <- page_sidebar(
  useShinyjs(),
  title="Eye tracking app",
  sidebar=sidebar(
    width=400,
    title="Inputs",
    fileInput("upload", "Upload image", accept = "image/jpeg"),
    fileInput("upfix", "Upload Fixations", accept = "text/csv"),
    textInput("landmark_name", 
              "First, type a name for an AOI here (or a unique sequential name will be used instead). Next, click in the plot at the centre of the desired AOI", 
              "name"),
    #downloadButton("download_dirsgs", "Get dirsgs"),
    #downloadButton("download_delsgs", "Get delsgs"),
    actionButton("do_which_tile", "Click to assign each fixation to an AOI. You can define more AOIs and repeat this step"),
    input_switch("toggle_fixations", "Show (or Hide) Fixations on the plot"),
    downloadButton("download_tilelist", "Download AOI centres and their names"),
    downloadButton("download_tileassignment", "Download fixation report annotated with an AOI for each fixation"),
    actionButton("next_image", "Next Image"),
    actionButton("compute", "Compute")
  ),
  layout_columns(
    fluidRow(
      column(5, 
             plotOutput("image", click = "image_click", dblclick = "plot_dblclick")
      ),
      column(5, 
             offset =1, 
             plotOutput("image2")
      ),
    )
  )
)

server <- function(input, output, session){
  
  # Set the data 
  data_tilelist <- reactive({
    if(is.data.frame(g$aois)){
      g$aois
    }
  })
  # Do the download
  output$download_tilelist <- downloadHandler(
    filename = function() {
      paste0("AOIS", ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(data_tilelist(), file)
    }
  )
  
  
  # Set the data for the download_tileassignment downloadHandler
  data_tileassignment <- reactive({
    if(!is.null(g$fixrep$tile_id)){
      g$fixrep  
    }
  })
  # Actually download the tileassignment
  output$download_tileassignment <- downloadHandler(
    filename = function() {
      paste0("fixrep_with_tile", ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(data_tileassignment(), file)
    }
  )
  
  # Reactive globals
  
  g = reactiveValues(
    file_name = NULL,
    img_as_jpeg = NA,
    img_x_width = NA,
    img_y_height = NA,
    clicked_x = list(),
    clicked_y = list(),
    clicked_name = list(),
    aois = tibble(x=as.numeric(NA), y=as.numeric(NA), name=NA),
    vor = NA,
    fixrep = NA,
    tile_list = NA
  )
  
  # Observe without event, to assign to reactive values object
  
  observe({
    if(!is.null(myjpeg())){
      g$img_as_jpeg = myjpeg()
      g$img_y_height = dim(myjpeg())[1]
      g$img_x_width = dim(myjpeg())[2]
    }
  })
  
  observe({
    if(!is.null(myfixupload())){
      g$fixrep = myfixupload()
    }
  })
  
  observeEvent(input$do_which_tile, {
    if(length(g$clicked_x > 1)){
      fixrep_for_this_image = g$fixrep %>% filter(face_jpeg==input$upload$name)
      for(i in 1:nrow(fixrep_for_this_image)){
        x=fixrep_for_this_image[i, 'CURRENT_FIX_X'] %>% pull()
        y=fixrep_for_this_image[i, 'CURRENT_FIX_Y'] %>% pull()
        tl=g$tile_list
        fixrep_for_this_image[i, 'tile']=which.tile(x,y,tl)
        #fixrep_for_this_image[i, 'tile_id']=which.tile(x,y,tl)
      }
      saveRDS(fixrep_for_this_image, 'fixrep_for_this_image.rds')
      # for (fixation in 1:nrow(g$fixrep)){
      #   g$fixrep[fixation, "tile"] = 
      #     which.tile(x=as.numeric(g$fixrep[fixation, "CURRENT_FIX_X_adj"]),
      #                y=as.numeric(g$fixrep[fixation, "CURRENT_FIX_Y_adj"]),
      #                tl=g$tile_list)
      #   g$fixrep[fixation, "tile_id"] = 
      #     names(g$tile_list)[as.numeric(g$fixrep[fixation, "tile"])]
      # }
    }
  })
  
  # Record clicks on the image and update the prompt
  
  observeEvent(input$image_click, {
    if(!is.null(input[["upload"]])){
      click_x <- input$image_click$x
      click_y <- input$image_click$y
      
      # Adjust coordinates to be within image bounds
      click_x <- max(0, min(click_x, g$img_x_width))
      click_y <- max(0, min(click_y, g$img_y_height))
      
      # Append the point and its name to the lists
      g$clicked_x = append(g$clicked_x, click_x)
      g$clicked_y = append(g$clicked_y, click_y)
      if(input$landmark_name == "name"){
        dummy_landmark_name = paste0("A", length(g$clicked_x))
        g$clicked_name = append(g$clicked_name, dummy_landmark_name)
        updateTextInput(session, "landmark_name", value = "name")
      } else {
        g$clicked_name = append(g$clicked_name, input[['landmark_name']])
        updateTextInput(session, "landmark_name", value = "name")
      }
    }})
  
  # Handle the next image button
  
  observeEvent(input$next_image, {
    req(input$upload)
    # Open the file input dialog for the next image
    shinyjs::runjs("document.getElementById('upload').click();")
    
    # Append current image data to all_points and all_names
    if (length(g$clicked_x) > 0) {
      g$all_points_x <- append(g$all_points_x, g$clicked_x)
      g$all_points_y <- append(g$all_points_y, g$clicked_y)
      g$all_clicked_names <- c(g$all_clicked_names, g$clicked_name)
      g$all_files <- c(g$all_files, rep(g$file_name, length(g$clicked_x)))
    }
    
    # Clear current points and names for the next image
    g$clicked_x <- list()
    g$clicked_y <- list()
    g$clicked_name <- list()
    g$file_name <- NULL
    updateTextInput(session, "landmark_name", value = "name")
    reset("upload")
  })
  
  # Handle the compute button to analyze fixations
  observeEvent(input$compute, {
    
    # Append current image data to all_points and all_names
    if (length(g$clicked_x) > 0) {
      g$all_points_x <- append(g$all_points_x, g$clicked_x)
      g$all_points_y <- append(g$all_points_y, g$clicked_y)
      g$all_clicked_names <- c(g$all_clicked_names, g$clicked_name)
      g$all_files <- c(g$all_files, rep(g$file_name, length(g$clicked_x)))
    }
    
    # debug
    #browser()
    
    write_csv(x=g$fixrep, file="fixrep_with_tile_id.csv")
    
    # TODO
    
    # Calculate fixdur per IA per trial per subject
    # Alex's app uses sp::over() for this but I don't understand that yet
    
  })
  
  # Reactive functions to extract variables
  
  myjpeg = reactive({
    jpegfile <- input[['upload']]
    if(!is.null(jpegfile)){
      readJPEG(jpegfile$datapath, native=TRUE) # RGB triples like foo[1,300,3] = 0.1843137
    }
  })
  
  myfixupload = reactive({
    fixfile <- input[['upfix']]
    if (!is.null(fixfile)){
      read_csv(fixfile$datapath, show_col_types = F) %>% 
        mutate(
          CURRENT_FIX_X_adj = ((CURRENT_FIX_X/1920) * 600), 
          CURRENT_FIX_Y_adj = ((CURRENT_FIX_Y/1080) * 800),
          tile = as.numeric(NA)
        )
    }
  })
  
  output[['image']] <- renderPlot({
    g$file_name <- input$upload$name
    if (length(g$clicked_x) > 0) {
      aois = tibble()
      for (i in 1:length(g$clicked_x)){
        this_aoi_x =  g$clicked_x[i] %>% as.numeric() 
        this_aoi_y =  g$clicked_y[i] %>% as.numeric()
        this_aoi = tibble(x=this_aoi_x, y=this_aoi_y, name=g$clicked_name[i] %>% as.character())
        aois = aois %>% bind_rows(this_aoi)
        g$aois = aois
      }
      if(length(g$clicked_x) > 1){
        vor = deldir(x=aois$x, y=aois$y, id = as.vector(g$clicked_name),
                     rw=c(xleft=0, xright=g$img_x_width, ybottom=0, ytop=g$img_y_height))
        g$vor = vor
        g$tile_list = tile.list(vor)
      }
    } else {
      aois = tibble(x=as.numeric(NA), y=as.numeric(NA))
    }
    
    if(!is.null(myjpeg())  && length(g$clicked_x) > 0){
      ggplot(aois)+
        scale_y_reverse(limits=c(800, 0), name=NULL)+
        scale_x_continuous(limits=c(0, 600), name=NULL)+
        annotation_raster(raster=myjpeg(), xmin=0, xmax=600, ymin=(-1*800), ymax=0)+
        geom_point(aes(x=x,y=y), size=6, color="red")
    } else if (!is.null(myjpeg())  && length(g$clicked_x) == 0) {
      ggplot()+
        scale_y_reverse(limits=c(800, 0))+
        scale_x_continuous(limits=c(0, 600))+
        annotation_raster(raster=myjpeg(), xmin=0, xmax=600, ymin=(-1*800), ymax=0)
    }
  }, height=800, width=600)
  
  output[['image2']] <- renderPlot({
    if(!is.null(input[['upload']])){
      plot(0, 0, type = 'n', xlim = c(0, g$img_x_width), ylim = c(g$img_y_height, 0), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
      rasterImage(g$img_as_jpeg, xleft=0, ybottom=g$img_y_height, xright=g$img_x_width, ytop=0) # Display the image with correct orientation
      if(length(g$clicked_x) > 1){
        plot(x=g$vor,
             add = TRUE,
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
      if("tile_id" %in% names(g$fixrep) && input$toggle_fixations==TRUE){
        points(g$fixrep$CURRENT_FIX_X_adj, y=g$fixrep$CURRENT_FIX_Y_adj, cex=4, col="red", pch=21, bg="blue")
      }
    }
  }, height=800, width=600)
  
} # close server logic

shinyApp(ui, server)


