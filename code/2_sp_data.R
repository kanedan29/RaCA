d <- d %>%
  mutate(Loc.clust.point = paste(Location, Cluster, Point, sep = "."))
  

d.points <- d %>%
  dplyr::select(State, Location, Cluster, Point, Loc.clust.point, long, lat) %>%
  distinct()

View(duplicated(d.points$Loc.clust.point))

### Transform into a spatial points data frame and identify CRS

coordinates(d.points) = ~ long + lat
proj4string(d.points) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')

### Call map unit information from SDA ####
mu.info <- SDA_query_features(d.points, id='Loc.clust.point') 

test <- mu.info %>%
  left_join(dplyr::select(d,Loc.clust.point, Slope, Elevation)) %>%
  distinct()




### Call table data from SDA for components of each mukey, extract most representative components

test3 <- SDA_query(paste("SELECT *
                FROM  component
                         WHERE mukey IN (", paste(unique(mu.info$mukey), collapse=","),")")) 

test4 <- test %>%
  right_join(test3) %>%
  mutate(slope.diff = abs(Slope - slope_r)) %>%
  group_by(Loc.clust.point) %>%
  filter(Slope > slope_l | Slope < slope_h, 
         slope.diff == min(slope.diff),
         majcompflag == "Yes",
         comppct_r == max(comppct_r))

test5 <- test %>%
  right_join(test3) %>%
  filter(!Loc.clust.point %in% test4$Loc.clust.point) %>%
  mutate(elev.diff = abs(Elevation - elev_r)) %>%
  group_by(Loc.clust.point) %>%
  filter(Elevation > elev_l | Elevation < elev_h,
         elev.diff == min(elev.diff),
         majcompflag == "Yes",
         comppct_r == max(comppct_r)) 

test6 <- test %>%
  right_join(test3) %>%
  filter(!Loc.clust.point %in% test5$Loc.clust.point) %>%
  group_by(Loc.clust.point) %>%
  filter(majcompflag == "Yes",
         comppct_r == max(comppct_r)) 


test7 <- dplyr::bind_rows(test4, test5, test6) %>%
  filter(!duplicated(Loc.clust.point))
  

test2 <- SDA_query(paste("SELECT *
                FROM  chorizon
                         WHERE cokey IN (", paste(unique(test7$cokey), collapse=","),")")) 




depths(test2) <- cokey ~ hzdept_r + hzdepb_r

soil.slabs <- slab(test2, 
                   fm= cokey ~ sandtotal_r+silttotal_r+claytotal_r+
                     om_r+caco3_r+gypsum_r+cec7_r+
                     ph1to1h2o_r, 
                   slab.structure=c(0,15,30), slab.fun=mean, na.rm=TRUE) %>%
  dcast(., cokey + bottom ~ variable, value.var = 'value') %>%
  rename_all(.funs = function(x) paste("SSURGO", x, sep = "_")) %>%
  rename("Depth" = SSURGO_bottom) %>%
  dplyr::bind_rows(
    slab(test2, 
         fm= cokey ~ sandtotal_r+silttotal_r+claytotal_r+
           om_r+caco3_r+gypsum_r+cec7_r+
           ph1to1h2o_r, 
         slab.structure=c(0,20), slab.fun=mean, na.rm=TRUE) %>%
      dcast(., cokey + bottom ~ variable, value.var = 'value') %>%
      rename_all(.funs = function(x) paste("SSURGO", x, sep = "_")) %>%
      rename("Depth" = SSURGO_bottom)
  )
  
test7 <- test7 %>%
  dplyr::select(Loc.clust.point, mukey, muname, compname, cokey, starts_with("tax")) %>%
  ungroup()


s.colors <- list() 

for(i in 1:length(unique(test7$compname))) {
  s.colors[[i]] <- tryCatch(fetchOSD(unique(test7$compname)[i],colorState ='dry', extended=FALSE), 
                            error = function(e) NULL)
}

s.colors <- plyr::compact(s.colors)
s.color.slice.15 <- list() 

for(i in 1:length(s.colors)) {
  s <- slice(s.colors[[i]], c(15, 30) ~ id+soil_color)
  s$slice <- paste0(s$top)
  s$slice <- as.character(s$slice, levels=guessGenHzLevels(s, 'slice')$levels)
  s$sliceID <- paste(s$id, s$slice, sep = "_")
  a <- aggregateColor(s, groups = 'sliceID', col = 'soil_color')[[2]] %>%
    separate(sliceID, into = c("compname", "Depth")) %>%
    dplyr::select(-n)
  s.color.slice.15[[i]] <- a
}

s.color.slice.15 <- plyr::ldply(s.color.slice.15)

s.color.slice.20 <- list() 

for(i in 1:length(s.colors)) {
  s <- slice(s.colors[[i]], c(0,20) ~ id+soil_color)
  s$slice <- paste0(s$top)
  s$slice <- as.character(s$slice, levels=guessGenHzLevels(s, 'slice')$levels)
  s$sliceID <- paste(s$id, s$slice, sep = "_")
  a <- aggregateColor(s, groups = 'sliceID', col = 'soil_color')[[2]] %>%
    separate(sliceID, into = c("compname", "Depth")) %>%
    dplyr::select(-n)
  s.color.slice.20[[i]] <- a
}

s.color.slice.20 <- plyr::ldply(s.color.slice.20) %>%
  filter(Depth == "20")

s.color.slices <- dplyr::bind_rows(s.color.slice.15, s.color.slice.20) %>%
  mutate(compname = str_to_title(compname, locale = "en")) %>%
  rename_all(.funs = function(x) paste("SSURGO", x, sep = "_")) %>%
  rename("Depth" = SSURGO_Depth) 

test8 <- test7 %>%
  rename_all(.funs = function(x) paste("SSURGO", x, sep = "_")) %>%
  rename("Loc.clust.point" = SSURGO_Loc.clust.point) %>%
  mutate(SSURGO_cokey = as.character(SSURGO_cokey)) %>%
  left_join(soil.slabs) %>%
  dplyr::select(Loc.clust.point, Depth, everything()) %>%
  mutate(SSURGO_cokey = as.character(SSURGO_cokey),
         Depth = as.character(Depth)) %>%
  left_join(s.color.slices) %>%
  dplyr::select(Loc.clust.point, Depth, everything()) %>%
  distinct()

d <- d %>%
  dplyr::select(Loc.clust.point, everything(),
                -starts_with("SSURGO")) %>%
  rename("REMOTE_Aspect" = Aspect,
         "REMOTE_Elevation" = Elevation,
         "REMOTE_NDVI" = NDVI,
         "REMOTE_Slope" = Slope) %>%
  left_join(test8)

rm(list = setdiff(ls(), "d"))  
  

write_rds(d, "data/d_all_2020_01_07.rds")

