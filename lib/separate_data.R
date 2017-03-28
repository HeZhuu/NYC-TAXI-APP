data = read.csv('data/2016_abridged_cleaned_grouped_FpD_weekOftheDay.csv')
data = data %>% subset(select = c('Pickup_longitude', 'Pickup_latitude', 'business_day', 'pickup_hour', 'farePerDistance'))
# data$WWH = ifelse(data$weekend, 2, 1 + 2 * !data$business_day)
data$WWH = ifelse(data$business_day, 1, 2)

count_result = array(dim = c(195,24,2))
FPD_result = array(dim = c(195,24,2))
load('output/myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))
count_rank = rank(subdat@data$NTACode)

for (j in 1:2){
  for (h in 1:24){
    data.sel = data %>% subset(pickup_hour == h - 1 & WWH == j)
    
    dat = data.frame(Longitude = data.sel$Pickup_longitude, Latitude = data.sel$Pickup_latitude)
    coordinates(dat) <- ~ Longitude + Latitude
    proj4string(dat) <- CRS("+proj=longlat")
    dat <- spTransform(dat, proj4string(myShape1))
    r = over(dat, myShape1)
    r = r %>% subset(select = c('NTACode')) %>% cbind(data.sel)
    
    count = table(r$NTACode)[count_rank]
    FPD = tapply(r$farePerDistance, r$NTACode, mean)
    count_result[,h,j] = as.vector(count)
    FPD_result[,h,j] = as.vector(FPD)
  }
}

save(count_result, file = 'output/count_seperated.RData')
save(FPD_result, file = 'output/FPD_seperated.RData')
