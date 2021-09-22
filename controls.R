controls <- function(dem, ep_study, c){
Finland2 <- list()
dem <- dem[!is.na(dem$reg_type),]
a <- unique(dem$country_name)
b <- as.vector(ep_study[ep_study$country_name==c,"dem_ep_start_year"])
b <- b$dem_ep_start_year

for(i in a){
  if (length(dem[dem$year==b & dem$country_name==i, "reg_type"]==0)>1 | 
      length(dem[dem$year==b+1 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+2 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+3 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+4 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+5 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+6 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+7 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+8 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+9 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b+10 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-1 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-2 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-3 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-4 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-5 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-6 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-7 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-8 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-9 & dem$country_name==i, "reg_type"]==0)>1 |
      length(dem[dem$year==b-10 & dem$country_name==i, "reg_type"]==0)>1 
      )
    next
  if (length(dem[dem$year==b & dem$country_name==i, "reg_type"]==0)==0 | 
      length(dem[dem$year==b+1 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+2 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+3 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+4 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+5 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+6 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+7 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+8 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+9 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b+10 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-1 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-2 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-3 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-4 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-5 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-6 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-7 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-8 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-9 & dem$country_name==i, "reg_type"]==0)==0 |
      length(dem[dem$year==b-10 & dem$country_name==i, "reg_type"]==0)==0 )
    next
  if (dem[dem$year==b & dem$country_name==i, "reg_type"]==1)
    next

  if (dem[dem$year==b & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+1 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+2 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+3 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+4 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+5 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+6 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+7 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+8 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+9 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b+10 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-1 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-2 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-3 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-4 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-5 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-6 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-7 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-8 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-9 & dem$country_name==i, "reg_type"]==0 &
      dem[dem$year==b-10 & dem$country_name==i, "reg_type"]==0
      )
    Finland2[[paste0("c_", i)]] <- assign(paste0("country_",i),
                                          dem[dem$country_name==i &
                                                dem$year>(b-11) &
                                                dem$year<(b+11),])
}

return(Finland2)
}
