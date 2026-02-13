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
  
  # Format timestamps nicely
  safe_timestamp <- function() {
    format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  }
  
  ### END FUNCTIONS ###
  
  # Respond to upload fixation report button
  observeEvent(input$upload_fixrep, {
    if(tools::file_ext(input$upload_fixrep$datapath)=="xls"){
      message("xls option")
      g$fixrep_raw = read_delim(input$upload_fixrep$datapath, locale = readr::locale(encoding = "UTF-16LE"), show_col_types = FALSE)
    }
    if(tools::file_ext(input$upload_fixrep$datapath)=="csv"){
      message("csv option")
      g$fixrep_raw = read_csv(input$upload_fixrep$datapath, show_col_types = FALSE) 
    }
    if(tools::file_ext(input$upload_fixrep$datapath)=="xlsx"){
      message("xlsx option")
      g$fixrep_raw = read_xlsx(input$upload_fixrep$datapath) 
    }
    output$variable_selectors <- renderUI({
      if (!is.null(g$fixrep_raw)) {
        tagList(
          radioButtons(inputId="origin_location", label="Fixation Report (0,0) is at:", choices=c("top-left of screen", "centre of screen", "bottom-left of screen"),selected = character(0)),
          selectInput("participant_id", "Select Participant ID Variable", choices = names(g$fixrep_raw)),
          selectInput("trial_id", "Select Trial ID Variable", choices = names(g$fixrep_raw)),
          selectInput("fix_id", "Select Fixation Index Variable", choices = names(g$fixrep_raw)),
          selectInput("x_var", "Select Eye X Coordinate Variable", choices = names(g$fixrep_raw)),
          selectInput("y_var", "Select Eye Y Coordinate Variable", choices = names(g$fixrep_raw)),
          selectInput("face_filename", "Select Face File Name Variable", choices =  names(g$fixrep_raw)),
          selectInput("fixation_duration", "Select Fixation Duration Variable", choices = names(g$fixrep_raw)),
          selectInput("condition1", "Condition Variable 1", choices = c("None", names(g$fixrep_raw))),
          selectInput("condition2", "Condition Variable 2", choices = c("None", names(g$fixrep_raw)))
        )}})
  })


  # Make fixrep this face after fixrep and face are uploaded
  observe({
    if(!is.null(input$upload_face) && !is.null(input$upload_fixrep)){
      nrows_of_fixrep = nrow(g$fixrep_raw)
      if(input$condition1 == "None"){condition1_var = rep(as.character(NA), times=nrows_of_fixrep)}
      if(input$condition1 != "None"){condition1_var = g$fixrep_raw %>% pull(input$condition1)}
      g$fixrep=tibble(
        subject_id = g$fixrep_raw %>% pull(input$participant_id),
        trial_id = g$fixrep_raw %>% pull(input$trial_id),
        condition1 = condition1_var,
        #condition2 = ifelse(input$condition2 =="None", NA, g$fixrep_raw %>% pull(input$condition2)),
        face_jpeg = g$fixrep_raw %>% pull(input$face_filename),
        # Here, we do not change the raw values for x and y
        fix_x = g$fixrep_raw %>% pull(input$x_var),
        fix_y = g$fixrep_raw %>% pull(input$y_var),
        # Here, we do change the raw values for x and y
        # fix_x = g$fixrep_raw %>% pull(input$x_var) - ((1920-600)/2),
        # fix_y = g$fixrep_raw %>% pull(input$y_var) - ((1080-600)/2),
        fix_dur = g$fixrep_raw %>% pull(input$fixation_duration),
      )
      g$fixrep_this_face = g$fixrep %>% filter(face_jpeg == input$upload_face$name)
    }})


  # Respond to clicks on the face
  observeEvent(input[['face_for_edit_click']], {
    # Make AOI name
    if(input$aoi_name == "a"){
      #aoi_name = paste0(input$aoi_name,nrow(g$aois)+1)
      aoi_name = paste0(input$aoi_name, sample(LETTERS,1), sample(0:9,1))
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
    #browser()
    g$fixrep_with_annotation = g$fixrep_with_annotation %>% bind_rows(g$fixrep_this_face)
    g$aois_all = g$aois_all %>% bind_rows(g$aois)
    g$summary_file <- g$fixrep_with_annotation %>% 
      group_by(subject_id, trial_id, condition1, face_jpeg, tile_name) %>% 
      summarise(
        dwell_time=sum(fix_dur),
        n_fixations = n(), 
        avge_dwell_time_per_fixation=dwell_time/n_fixations) %>% 
      group_by(condition1, tile_name) %>% 
      summarise(
        mean_dwell_time=mean(dwell_time), 
        mean_n_fixations=mean(n_fixations), 
        mean_avge_dwell_time_per_fixation = mean(avge_dwell_time_per_fixation)
      )
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
  observeEvent(input[['write_all_rds']], {
    mytimestamp = safe_timestamp()
    saveRDS(g$fixrep_with_annotation, file.path("outputs", paste0(mytimestamp, "-fixrep_with_annotation",  ".rds")))
    saveRDS(g$summary_file,           file.path("outputs", paste0(mytimestamp, "-summary-file",            ".rds")))
  })

  # Respond to download_annotated
  output$download_annotated <- downloadHandler(
    filename = function() {
      paste("data-", safe_timestamp(), "-fixrep_with_annotation", ".csv", sep="")
    },
    content = function(file) {
      write.csv(g$fixrep_with_annotation, file)
    }
  )
  
  # Respond to download_summary
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("data-", safe_timestamp(), "-summary", ".csv", sep="")
    },
    content = function(file) {
      write.csv(g$summary_file, file)
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
