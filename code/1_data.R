source("code/0_libraries.R")

d <- data.table::fread("data/RaCA_Download/RaCA_Download/Data/RaCA_samples.csv")

# Import soil scan data
d.scans <- data.table::fread("data/raca_scans_merged.csv")
d.scans <- d.scans %>%
  rename_at(.vars = vars(3:2153), .funs = function(x) paste("NM", x, sep = "_")) %>%
  dplyr::select(-scan_id) %>% 
  mutate(smp_id = as.character(smp_id))

# collate fetchRaCA data
fetch.raca.data <- plyr::compact(
  unlist(
    lapply(list.files("data/fetchRaCA_data/", full.names = TRUE), read_rds), recursive = FALSE))

fetch.raca.data.list <- list()

for(i in 1:length(fetch.raca.data)){
  fetch.raca.data.list[[i]] <- as(fetch.raca.data[[i]]$pedons, 'data.frame') %>%
    left_join(fetch.raca.data[[i]]$sample)
}


fetch.raca.data <- plyr::ldply(fetch.raca.data.list) %>%
  rename(sample.id = sample_id)


# Join dataframes

d <- d %>%
  mutate(smp_id = as.character(smp_id)) %>%
  left_join(fetch.raca.data) %>%
  right_join(d.scans) %>%
  filter(!is.na(c_tot_ncs)) %>%
  distinct(.)

fetchOSD("Croghan")




          