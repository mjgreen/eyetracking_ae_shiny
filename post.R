f=readRDS("fixrep.rds") %>% filter(jpg=="C1_041.jpg")
a=readRDS("aois_all.rds") %>% filter(jpg=="C1_041.jpg")



for( i in 1:nrow(f)){
  x=f[i, 'fix_x'] %>% pull()
  y=f[i, 'fix_y'] %>% pull()
  t = a[1,6] %>% unlist(recursive=FALSE)
  f[i, 'tile2'] = which.tile(x, y, t)
}



