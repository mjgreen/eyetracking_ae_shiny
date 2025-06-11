output[['face_for_edit']] <- renderPlot({
  if(!is.null(input[['upload_face']])){
    edit_face = ggplot()+
      scale_y_reverse(limits=c(800, 0))+
      scale_x_continuous(limits=c(0, 600))+
      theme(aspect.ratio = (800/600))+
      labs(x=NULL, y=NULL)+
      annotation_raster(
        raster=readJPEG(input[['upload_face']]$datapath, native=TRUE), 
        xmin=0, xmax=600, ymin=(-1*800), ymax=0)
    
    # if there are any clicks on the edit_face
    if(length(myglobals$click_x) > 0){
      aois = tibble()
      for (i in 1:length(myglobals$click_x)){
        this_aoi_x =  myglobals$click_x[i]
        this_aoi_y =  myglobals$click_y[i]
        this_aoi = tibble(x=this_aoi_x, y=this_aoi_y)
        aois = aois %>% bind_rows(this_aoi)
        myglobals$aois = aois
      }
      
      # update edit_face with clicks as red marks
      edit_face = edit_face +
        geom_point(aes(x=myglobals$click_x, y=myglobals$click_y), color="red", size=4)
    }
    # put the edit face, with any red dots if any
    edit_face 
  }
})

output[['face_with_markup']] <- renderPlot({
  if(!is.null(input[['upload_face']])){
    # if(between(length(myglobals$click_x), 0, 1)){
    # ggplot()+
    #   scale_y_reverse(limits=c(800, 0))+
    #   scale_x_continuous(limits=c(0, 600))+
    #   theme(aspect.ratio = (800/600))+
    #   labs(x=NULL, y=NULL)+
    #   annotation_raster(
    #     raster=readJPEG(input[['upload_face']]$datapath, native=TRUE), 
    #     xmin=0, xmax=600, ymin=(-1*800), ymax=0)+
    #     geom_point(aes(x=myglobals$click_x, y=myglobals$click_y), color="red", size=4)
    # }
    
    if(length(myglobals$click_x) > 1){
      vor = deldir(
        x = myglobals$aois$x, 
        y = myglobals$aois$y, 
        #id = as.vector(myglobals$clicked_name),
        rw = c(xleft=0, xright=600, ybottom=0, ytop=800)
      )
      #xmin, xmax, ymin, ymax.
      rectangle=c(0, 600, 0, 800)
      ggplot(myglobals$aois, aes(x=x, y=y, group=-1L))+
        #scale_y_reverse(limits=c(800, 0))+
        scale_y_reverse()+
        #scale_x_continuous(limits=c(0, 600))+
        coord_fixed(ratio=1.333, xlim=c(0,600), ylim=c(800,0), clip='off')+
        theme(aspect.ratio = (800/600))+
        labs(x=NULL, y=NULL)+
        annotation_raster(
          raster=readJPEG(input[['upload_face']]$datapath, native=TRUE), 
          xmin=0, xmax=600, ymin=(-1*800), ymax=0)+
        geom_voronoi_tile(fill=NA, bound=rectangle)+
        geom_voronoi_segment()+
        geom_point(aes(x=x, y=y), color="red", size=4)
      
    }
  }
})