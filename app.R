library(shiny)
library(shinyjs) 
library(bslib)
library(DT)
library(deldir)
library(jpeg)
library(dplyr)
library(renv)

ui <- page_fillable(
  useShinyjs(),
  layout_columns(
    col_widths = c(4, 8),
    navset_card_pill(
      nav_panel("Instructions",
                p("Use the Fixation Report tab to upload your fixation report and answer questions about it"),
                p("Use the Faces tab to upload and annotate faces"),
                p("Use the Debug tab to inspect the internal state of the program"),
                p("Use the Save tab to save the processed Fixation report")
      ),
      nav_panel("Fixation Report", 
                fileInput("upload_fixrep", "Upload fixation report", accept = "text/csv"),
                uiOutput("variable_selectors")
                
      ), 
      nav_panel("Faces", 
                fileInput("upload_face", "Upload face", accept = "image/jpeg"),
                input_switch("toggle_fixations", "Toggle fixation visibility on/off"),
                p(paste0(
                  "You can supply a name (like 'nose') for an AOI by writing it in the box below, ",
                  "or let the app name the AOIs for you by not typing anything.")),
                p(paste0("Then use the mouse to single-click on the left-hand-side face to indicate the centroid ",
                  "of an AOI (double-click removes the nearest centroid).")),
                p(paste0("When there are 2 or more AOIs, the right-hand-side face ",
                  "will create and display AOI areas using Voronoi tesselation.")),
                textInput("aoi_name", "Type name for this AOI, then click in AOI", "a"),
                p("You MUST click 'Annotate Current Face' before doing 'Start next face' or any 'Save and Exit' options."),
                actionButton("annotate", "Annotate current face"),
                actionButton("next_face", "Start next face")
      ), 
      nav_panel("Save and exit",
                actionButton("write_annotated", "Save annotated fixation report locally as .rds"),
                downloadButton("download_annotated", "Download annotated fixation report as .csv"),
                actionButton("exit_app", "Exit App")
      ),
      nav_panel("To Do",
                p(paste0(
                  "Handle automatially running 'annotate this face' before either ",
                  "(1) doing another face, or (2) saving the data and exiting.")
                  ),
                
                p(paste0(
                  "If we just call 'annotate this face' naively, then the current face ",
                  "would be duplicated across rows in the event that the user",
                  "did in fact use 'annotate this face' appropriately. ",
                  "However dplyr::distinct() might be good for this as it removes duplicate rows - ",
                  "Need to check whether rows are unique enough for this - I think if fiaxtion id is used, then using distinct should work.")),
                
                p(paste0("FIXED: double-click doesn't do anything")),
                p(paste0("selecting origin doesn't do anything"))
      ),
      nav_panel("Debug",
                actionButton("debug", "Debug (for dev use only)")
      )
    ),
    navset_card_pill(
      nav_panel("Faces",
        card(fluidRow(
                 column(6, plotOutput("face_for_edit", click = "face_for_edit_click", 
                                      dblclick = "face_for_edit_dblclick")),
                 column(6, plotOutput("face_for_markup"))))),
      nav_panel("Annotated fixation report",
                dataTableOutput('table'))
    )
    )
  )





server <- function(input, output, session) {
  
  # Globally accessible variables
  g = reactiveValues(
    aois_all = tibble(),
    aois = tibble(),
    fixrep_raw = NA,
    fixrep = NA,
    fixrep_this_face = NA,
    fixrep_with_annotation = tibble(),
    vor = NA
  )
  
  observe({
    reactiveValuesToList(g)
  })
  
  ### FUNCTIONS ###

  # Read face jpeg function
  myjpeg = reactive({
    jpegfile <- input[['upload_face']]
    if(!is.null(jpegfile)){
      readJPEG(jpegfile$datapath, native=TRUE) 
    }
  })
  
  # Calculate tesselation function
  do_deldir = reactive({
    if(nrow(g$aois) >= 2){
      deldir(
        x = g$aois$x, 
        y = g$aois$y, 
        id = g$aois$aoi_name,
        rw = c(xleft = 0, xright=600, ybottom=0, ytop=800)
      )
    }
  })
  
  ### END FUNCTIONS ###
  
  # Respond to upload fixation report button
  observeEvent(input$upload_fixrep, {
    shinyjs::logjs("just before doing read_csv")
    fix_rep_in = read.csv(input$upload_fixrep$datapath) 
    shinyjs::logjs("just after doing read_csv")
    g$fixrep_raw = fix_rep_in
    output$variable_selectors <- renderUI({
      if (!is.null(fix_rep_in)) {
        tagList(
          radioButtons(inputId="origin_location", label="Fixation Report (0,0) is at:", 
                       choices=c("top-left of screen", "centre of screen", "bottom-left of screen"),
                       selected = character(0)),
          selectInput("participant_id", "Select Participant ID Variable", choices = names(fix_rep_in)),
          selectInput("trial_id", "Select Trial ID Variable", choices = names(fix_rep_in)),
          selectInput("fix_id", "Select Fixation Index Variable", choices = names(fix_rep_in)),
          selectInput("x_var", "Select Eye X Coordinate Variable", choices = names(fix_rep_in)),
          selectInput("y_var", "Select Eye Y Coordinate Variable", choices = names(fix_rep_in)),
          selectInput("face_filename", "Select Face File Name Variable", choices =  names(fix_rep_in)),
          selectInput("fixation_duration", "Select Fixation Duration Variable", choices = names(fix_rep_in)),
          selectInput("condition1", "Condition Variable 1", choices = c("None", names(fix_rep_in))),
          selectInput("condition2", "Condition Variable 2", choices = c("None", names(fix_rep_in)))
        )}})
  })


  # Observe without event
  observe({
    if(!is.null(input$upload_face) && !is.null(input$upload_fixrep)){
      g$fixrep=tibble(
        subject_id = g$fixrep_raw %>% pull(input$participant_id),
        trial_id = g$fixrep_raw %>% pull(input$trial_id),
        condition1 = ifelse(input$condition1 =="None", NA, g$fixrep_raw %>% pull(input$condition1)),
        condition2 = ifelse(input$condition2 =="None", NA, g$fixrep_raw %>% pull(input$condition2)),
        face_jpeg = g$fixrep_raw %>% pull(input$face_filename),
        fix_x = g$fixrep_raw %>% pull(input$x_var),
        fix_y = g$fixrep_raw %>% pull(input$y_var),
        fix_dur = g$fixrep_raw %>% pull(input$fixation_duration),
      )
      g$fixrep_this_face = g$fixrep %>% filter(face_jpeg == input$upload_face$name)
    }})


  # Respond to clicks on the face
  observeEvent(input[['face_for_edit_click']], {
    # Make AOI name
    if(input$aoi_name == "a"){
      aoi_name = paste0(input$aoi_name,nrow(g$aois)+1)
      updateTextInput(session, "aoi_name", value="a")
    } else {
      aoi_name = input$aoi_name
      updateTextInput(session, "aoi_name", value="a")
    }
    this_aoi = tibble(x=input$face_for_edit_click$x, 
                      y=input$face_for_edit_click$y, 
                      jpg=input[['upload_face']]$name,
                      aoi_name=aoi_name)
    g$aois = g$aois %>% bind_rows(this_aoi)
  })
  
  # Respond to double-click in face - Remove aoi on double click
  observeEvent(input$face_for_edit_dblclick, {
    click_x <- input$face_for_edit_dblclick$x
    click_y <- input$face_for_edit_dblclick$y
    g$aois$dbl_x = click_x
    g$aois$dbl_y = click_y
    if(nrow(g$aois) > 0){
      # Go through each aoi and say how far the aoi centre is from the double-click using Pythagoras' Theorem
      for(i in 1:nrow(g$aois)){
        # with origin at top-left
        lower_x = min(g$aois[i, "x"], g$aois[i, "dbl_x"])
        upper_x = max(g$aois[i, "x"], g$aois[i, "dbl_x"])
        lower_y = min(g$aois[i, "y"], g$aois[i, "dbl_y"])
        upper_y = max(g$aois[i, "y"], g$aois[i, "dbl_y"])
        a2 = (upper_x - lower_x)^2
        b2 = (upper_y - lower_y)^2
        c2 = a2+b2
        euclidian_distance = sqrt(c2)
        g$aois[i, "dist"] = euclidian_distance
      }
      # Work out the aoi centre that is closest to the double-click
      row_to_remove_by_rownum = which.min(g$aois$dist)
      # Remove (using slice() with negative indexing) the aoi whose centre is closest to the double-click
      g$aois = g$aois %>% slice(-row_to_remove_by_rownum)
      if(nrow(g$aois) >= 2){
        g$vor = do_deldir()
      }
    }
  })
  
  
  # Respond to annotate
  observeEvent(input[['annotate']], {
    for(i in 1:nrow(g$fixrep_this_face)){
      x = g$fixrep_this_face[i, "fix_x"] %>% pull()
      y = g$fixrep_this_face[i, "fix_y"] %>% pull()
      tl = tile.list(g$vor)
      tile_number = which.tile(x, y, tl)
      g$fixrep_this_face[i, "tile"] = tile_number
      g$fixrep_this_face[i, "tile_name"] = g$aois$aoi_name[tile_number]
    }
    g$fixrep_with_annotation = g$fixrep_with_annotation %>% bind_rows(g$fixrep_this_face)
    g$aois_all = g$aois_all %>% bind_rows(g$aois)
  })
  
  # Respond to start next face
  observeEvent(input[['next_face']], {
    g$aois_all = g$aois_all %>% bind_rows(g$aois)
    g$aois = tibble()
    reset("toggle_fixations")
    reset("upload_face")
    shinyjs::runjs("document.getElementById('upload_face').click();")
  })
  
  # Respond to debug
  observeEvent(input[['debug']], {
    browser()
  })
  
  # Respond to write rds
  observeEvent(input[['write_annotated']], {
    saveRDS(g$fixrep_with_annotation, 
            file.path(
              "outputs", 
              paste0("fixrep_with_annotation-",
                     Sys.time() %>% gsub(pattern=" ", replacement="_") %>% gsub(pattern=".", replacement="_", fixed=TRUE), 
                     ".rds")
            )
    )
  })

  # Respond to write csv and exit
  output$download_annotated <- downloadHandler(
    filename = function() {
      paste("data-", "fixrep_with_annotation-", Sys.time() %>% gsub(pattern=" ", replacement="_") %>% gsub(pattern=".", replacement="_", fixed=TRUE), ".csv", sep="")
    },
    content = function(file) {
      write.csv(g$fixrep_with_annotation, file)
    }
  )

  # Observe exit_app
  observeEvent(input$exit_app, {
    stopApp()
  })
  
  # OUTPUT
  
  # Prepare the plot for the left-hand-side
  output[['face_for_edit']] <- renderPlot({
    if(!is.null(myjpeg())){
      plot(x=0, y=0, type='n', xlim = c(0, 600), ylim = c(800, 0), xaxt='n', xlab=NA, ylab=NA, axes=F, asp=1)
      title("Click to define AOIs", line=2)
      axis(2, at=c(0,800), las=1)
      axis(3, at=c(0,600))
      mtext("Assuming eye-tracker has (0,0) at top-left", side=3, line=1)
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) 
      if(nrow(g$aois) > 0){
        points(x=g$aois$x, y=g$aois$y, pch=21, col="red", bg="red", cex=5)
      }}}, 
    width=600*0.9, height=800*0.9)
  
  # Prepare the plot for the RIGHT-hand-side
  output[['face_for_markup']] <- renderPlot({
    if(!is.null(myjpeg())){
      plot(x=0, y=0, type='n', xlim = c(0, 600), ylim = c(800, 0), xaxt='n', xlab=NA, ylab=NA, axes=F, asp=1)
      axis(2, at=c(0,800), las=1)
      axis(3, at=c(0,600))
      mtext("Assuming eye-tracker has (0,0) at top-left", side=3, line=1)
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) 
      if(input$toggle_fixations==TRUE){
        points(g$fixrep_this_face$fix_x, g$fixrep_this_face$fix_y, pch=21, bg="yellow", cex=5)
        text(g$fixrep_this_face$fix_x, g$fixrep_this_face$fix_y)
        text(g$fixrep_this_face$fix_x, g$fixrep_this_face$fix_y, 
             labels=paste0("(",g$fixrep_this_face$fix_x, ", ", g$fixrep_this_face$fix_y, ")"),
             pos=1, offset=2, col="yellow", cex=2)
      }
      if(nrow(g$aois) >= 2){
        vor <-
          deldir(
            x=g$aois$x, 
            y=g$aois$y, 
            id = g$aois$aoi_name,
            rw=c(xleft = 0, xright=600, ybottom=0, ytop=800)
          )
        g$vor = vor
        plot(vor, add=TRUE, wlines="tess", showpoints=FALSE, showrect=TRUE, labelPts=TRUE, lwd=3, cex=2, lex=3, cmpnt_col=c(tri=1,tess=1,points=1,labels=2,rect=1), cmpnt_lty=c(tri=1,tess=2), axes=TRUE)
      }}}, 
    width=600*0.9, height=800*0.9)
  
  # Table
  output$table <- DT::renderDT(g$fixrep_with_annotation)
  
}

shinyApp(ui = ui, server = server)
