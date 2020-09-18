library(tidyverse)
test <- read_rds("data/fetchRaCA_data/set_1.rds")[[4]]
View(test$pedons@horizons)





# Items to slab from soil profile collection
# @horizon data
# RGB colors, dry
# Soil texture components
# pH



soil.slabs <- slab(test2, 
                   fm= rcapid ~ d_r+d_g+d_b, 
                   slab.structure=c(0,15,30), slab.fun=mean, na.rm=TRUE) %>%
  reshape2::dcast(., rcapid + bottom ~ variable, value.var = 'value') %>%
  rename_all(.funs = function(x) paste("SSURGO", x, sep = "_")) %>%
  rename("Depth" = SSURGO_bottom) %>%
  mutate(d_r = SSURGO_d_r/255,
         d_g = SSURGO_d_g/255,
         d_b = SSURGO_d_b/255)




# from the site data table
# CaCO3

# @site data
# Taxonomy (starts_with tax)
# topo (elev, slope, aspect, ends_with _field)
# 
  