library(shiny)
library(shinyjs) 
library(bslib)
library(DT)
library(deldir)
library(jpeg)
library(dplyr)
library(renv)
library(readr)

##### UI #####

ui <- page_fillable(
  useShinyjs(),
  layout_columns(
    col_widths = c(4, 8),
    navset_card_pill(
      nav_panel("Instructions",
                h3("Developer Notes"),
                tags$div(
                         HTML("
                          <ul>
                          <li>This is v 0.1 with tabbed interface</li>
                          <li> I live at <a href='https://uruguay.bournemouth.ac.uk/shiny/users/mgreen/eyetracking_ae_shiny/' > https://uruguay.bournemouth.ac.uk/shiny/users/mgreen/eyetracking_ae_shiny/  </a> </li>
                          <li> My github is <a href='https://github.com/mjgreen/eyetracking_ae_shiny' > https://github.com/mjgreen/eyetracking_ae_shiny </a> </li>
                          <li> Use the Debug tab to inspect the internal state of the program but only when running from RStudio, not on the server</li>
                          </ul>
                          ")),
                hr(),
                h3("User Instructions"),
                tags$div(
                  HTML("
                    <ol> 
                    <li> Use the <strong>Fixation Report</strong> tab to upload your fixation report. Then dropdown boxes will appear and you should use those to say which columns correspond to which eye-tracking variables. You can download a suitable test fixation report <a href = 'https://mjgreen.github.io/eyetracking_ae_shiny/fixation_report.csv' > here </a> </li>
                    <li> Use the <strong>Faces upload</strong> tab to upload and annotate faces. Instructions for annotation are given in that tab. You can download two suitable faces as a zip file <a href = 'https://mjgreen.github.io/eyetracking_ae_shiny/sample_faces.zip' > here </a> </li>
                    <li> Check the <strong>Annotated fixation report</strong> tab to verify that the annotation is working: it will be empty until after you have finished the first face though. </li>
                    <li> Use the <strong>Save and exit</strong> tab to save the processed Fixation report </li>
                    </ol>
                    ")),
                hr()
      ),
      nav_panel("Fixation Report", 
                fileInput("upload_fixrep", "Upload fixation report", accept = c(".csv", ".xls", ".xlsx")),
                uiOutput("variable_selectors")
                
      ), 
      nav_panel("Faces Upload", 
                fileInput("upload_face", "Upload face", accept = "image/jpeg"),
                input_switch("toggle_fixations", "Toggle fixation visibility on/off"),
                p(paste0( "You can supply a name (like 'nose') for an AOI by writing it in the box below, ", "or let the app name the AOIs for you by not typing anything.")), p(paste0("Then use the mouse to single-click on the left-hand-side face to indicate the centroid ", "of an AOI (double-click removes the nearest centroid).")), p(paste0("When there are 2 or more AOIs, the right-hand-side face ", "will create and display AOI areas using Voronoi tesselation.")), textInput("aoi_name", "Type name for this AOI, then click in AOI", "a"), p( "You ", strong("MUST"), "click ", em('Annotate Current Face'), "before doing ", em('Start next face'), "or any ", em('Save and Exit'), "options. As soon as you have clicked 'Annotate Current Face', an annotated fixation report will be available in the tab ", strong("Annotated fixation report"), "on the right-hand-side next to", strong("Faces upload") ),
                actionButton("annotate", "Annotate current face"),
                actionButton("next_face", "Start next face")
      ), 
      nav_panel("Save and exit options",
                downloadButton("download_annotated", "Download annotated fixation report as .csv"),
                downloadButton("download_summary",   "Download summary report as .csv"),
                actionButton("write_all_rds", "Write annotated fixrep and summary locally as .rds (developer use only)"),
                actionButton("exit_app", "Exit App")
      ),
      nav_panel("To Do",
                h3("Developer notes"),
                p(paste0("Handle automatially running 'annotate this face' before either ", "(1) doing another face, or (2) saving the data and exiting.")),
                p(paste0("If we just call 'annotate this face' naively, then the current face ","would be duplicated across rows in the event that the user","did in fact use 'annotate this face' appropriately. ","However dplyr::distinct() might be good for this as it removes duplicate rows - ","Need to check whether rows are unique enough for this - I think if fiaxtion id is used, then using distinct should work.")),
                p(paste0("FIXED: double-click doesn't do anything")),
                p(paste0("selecting origin doesn't do anything"))
      ),
      nav_panel("Debug",
                h3("Developer buttons, not for use by Users"),
                actionButton("debug", "Debug (for developer use only)")
      )
    ),
    navset_card_pill(
      nav_panel("Faces edit", card(fluidRow(column(6, plotOutput("face_for_edit", click = "face_for_edit_click", dblclick = "face_for_edit_dblclick")),column(6, plotOutput("face_for_markup"))))),
      nav_panel("Annotated fixation report", dataTableOutput('table'))
    )
    )
  )
