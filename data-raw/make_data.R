proj0 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +towgs84=0,0,0")
projVN <- sp::CRS("+proj=utm +zone=48 +ellps=WGS84 +units=m +no_defs")
devtools::use_data(proj0, projVN, overwrite = TRUE)
