source("code/0_libraries.R")
# Fetch soil profile objects

rcasiteids <- unique(d$rcasiteid)
rcasiteids.list <- split(rcasiteids, ceiling(seq_along(rcasiteids)/30))

for(i in 51:90){
  temp <- lapply(rcasiteids.list[[i]], function(x) 
    tryCatch(fetchRaCA(rcasiteid = x, get.vnir = FALSE), error = function(e) NULL))
  write_rds(temp, path = paste("data/fetchRaCA_data/set_",i,".rds",sep = ""))
  Sys.sleep(120)
}

