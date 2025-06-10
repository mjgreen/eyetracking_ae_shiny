ggplot(iris, aes(Sepal.Length, Sepal.Width, group = -1L)) +
  geom_voronoi_tile(aes(fill = Species)) +
  geom_voronoi_segment() +
  geom_text(aes(label = after_stat(nsides), size = after_stat(vorarea)),
            stat = 'delvor_summary', switch.centroid = TRUE
  )

ggplot(aois, aes(x, y, group = -1L)) +
  geom_voronoi_tile() +
  geom_voronoi_segment() +
  geom_text(aes(label = after_stat(nsides), size = after_stat(vorarea)),
            stat = 'delvor_summary', switch.centroid = TRUE
  )


ggplot(aois, aes(x, y, group = -1L)) +
  geom_voronoi_tile() +
  geom_voronoi_segment()



theplot = ggplot(na.omit(myglobals$aois), aes(x=click_x, y=click_y))+
  scale_y_reverse(limits=c(800, 0))+
  scale_x_continuous(limits=c(0, 600))+
  theme_void()+
  theme(aspect.ratio = (800/600))+
  labs(x=NULL, y=NULL)+
  annotation_raster(
    raster=myjpeg(), 
    xmin=0, xmax=600, ymin=(-1*800), ymax=0)+
  geom_point(size=4, color="red")

theplot