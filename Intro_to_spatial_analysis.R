## ----setup, include=FALSE------------------------------------------------
library(raster)
library(tidyverse)
library(sf)

## ---- eval=FALSE---------------------------------------------------------
## devtools::install_github('edzer/sfr') # development version

## ---- eval=FALSE---------------------------------------------------------
## install.packages('sf') # stable version

## ---- message=FALSE------------------------------------------------------
library('sf')

## ------------------------------------------------------------------------
wrld <- st_read('data/wrld.shp')

## ------------------------------------------------------------------------
ham <- st_read('data/hamilton_county.gpkg')

## ------------------------------------------------------------------------
ham_cities <- read.csv('data/hamiltion_cities.csv')

## ------------------------------------------------------------------------
ham_cities_sf <- st_as_sf(ham_cities, coords = c("X","Y"))
ham_cities_sf

## ---- warning=FALSE------------------------------------------------------
st_write(wrld, 'data/new_wrld.shp')

## ---- warning=FALSE------------------------------------------------------
st_write(wrld, 'data/new_wrld.gpkg')

## ------------------------------------------------------------------------
class(wrld)

## ------------------------------------------------------------------------
wrld[1:2, 1:3]

## ---- eval=FALSE---------------------------------------------------------
## head(wrld)
## nrow(wrld)
## ncol(wrld)
## wrld[, c(1, 3)]
## wrld[1:5, 2]
## wrld[c(5, 10, 15), ]

## ------------------------------------------------------------------------
wrld_df <- st_set_geometry(wrld, NULL)
class(wrld_df)

## ---- message=FALSE------------------------------------------------------
library('dplyr')

## ------------------------------------------------------------------------
wrld_sel <- select(wrld, name_long, area_km2)

## ---- echo=FALSE---------------------------------------------------------
wrld_sel[1:4, ]

## ------------------------------------------------------------------------
wrld_arr <- arrange(wrld, area_km2)

## ---- echo=FALSE---------------------------------------------------------
wrld_arr[1:3, ]

## ------------------------------------------------------------------------
wrld_fil <- filter(wrld, pop < 297517)

## ---- echo=FALSE---------------------------------------------------------
wrld_fil[1:3, ]

## ------------------------------------------------------------------------
wrld_mut <- mutate(wrld, pop_density = pop/area_km2)

## ---- echo=FALSE---------------------------------------------------------
wrld_mut[1:3, ]

## ------------------------------------------------------------------------
wrld_sum1 <- summarize(wrld, pop_sum = sum(pop, na.rm = TRUE), 
                      pop_mean = mean(pop, na.rm = TRUE), 
                      pop_median = median(pop, na.rm = TRUE))

## ---- echo=FALSE---------------------------------------------------------
wrld_sum1[1, ]

## ------------------------------------------------------------------------
wrld_sum1 <- wrld %>% 
        group_by(continent) %>% 
        summarize(pop_sum = sum(pop, na.rm = TRUE), 
                      pop_mean = mean(pop, na.rm = TRUE), 
                      pop_median = median(pop, na.rm = TRUE))

## ---- echo=FALSE---------------------------------------------------------
wrld_sum1[1:3, ]

## ---- warning=FALSE------------------------------------------------------
wrld_set3410 <- st_set_crs(wrld, 3410)
st_crs(wrld_set3410)

## ------------------------------------------------------------------------
st_crs(ham_cities_sf)

## ------------------------------------------------------------------------
ham_cities_sf <- st_set_crs(ham_cities_sf, 4326)
st_crs(ham_cities_sf)

## ------------------------------------------------------------------------
wrld_3410 <- st_transform(wrld, 3410)
st_crs(wrld_3410)

## ---- eval=FALSE---------------------------------------------------------
## plot(wrld[0])

## ---- eval=FALSE---------------------------------------------------------
## plot(wrld["pop"])

## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------
png('figs/plot_compare.png', width = 800, height = 300)
par(mfrow = c(1, 2), mar=c(0,0,1,0))
plot(wrld[0]);plot(wrld["pop"])
dev.off()

## ------------------------------------------------------------------------
wrld_sp <- as(wrld, 'Spatial')
class(wrld_sp)
wrld_sf <- st_as_sf(wrld_sp)
class(wrld_sf)

## ---- message=FALSE------------------------------------------------------
library('raster')

## ------------------------------------------------------------------------
dem <- raster('data/srtm.tif')
dem

## ---- warning=FALSE, results='hide', echo=FALSE--------------------------
file.remove(c('data/new_dem.tif', 'data/new_dem2.tif'))

## ------------------------------------------------------------------------
writeRaster(dem, 'data/new_dem.tif')

## ------------------------------------------------------------------------
writeRaster(dem, 'data/new_dem2.tif', 
            datatype = 'FLT4S', options=c("COMPRESS=DEFLATE"))

## ---- warning=FALSE, results='hide', echo=FALSE--------------------------
file.remove(c('data/new_dem.tif', 'data/new_dem2.tif'))

## ------------------------------------------------------------------------
writeFormats()

## ------------------------------------------------------------------------
dem

## ------------------------------------------------------------------------
inMemory(dem)

## ------------------------------------------------------------------------
values_dem <- getValues(dem)

## ---- echo=FALSE---------------------------------------------------------
values_dem[1:50]

## ---- eval=FALSE---------------------------------------------------------
## getValues(dem, row = 5)

## ------------------------------------------------------------------------
new_values <- runif(864000, min=150, max=300) # pseudo-random number generator
new_dem <- setValues(dem, new_values)

## ---- fig.align='center', fig.height=5, echo=FALSE-----------------------
plot(new_dem)

## ------------------------------------------------------------------------
new_dem2 <- dem
new_dem2[new_dem2 < 0] <-  NA

## ---- fig.align='center', fig.height=5, echo=FALSE-----------------------
plot(new_dem2)

## ------------------------------------------------------------------------
new_dem3 <- dem + 50

## ---- fig.align='center', fig.height=5, echo=FALSE-----------------------
plot(new_dem3)

## ------------------------------------------------------------------------
new_dem4 <- dem * new_dem

## ---- fig.align='center', fig.height=5, echo=FALSE-----------------------
plot(new_dem4)

## ------------------------------------------------------------------------
dem_set3410 <- dem
crs(dem_set3410) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs"

## ------------------------------------------------------------------------
crs(dem_set3410)

## ------------------------------------------------------------------------
dem3410 <- projectRaster(dem, crs="+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs")

## ---- fig.align='center'-------------------------------------------------
plot(dem)

## ------------------------------------------------------------------------
ham_cities_sp <- as(ham_cities_sf, 'Spatial')
raster::extract(dem, ham_cities_sp)

## ------------------------------------------------------------------------
ham_cities_sf$dem <- raster::extract(dem, ham_cities_sp)
ham_cities_sf

## ---- echo=FALSE, message=FALSE------------------------------------------
library('tigris')
oh <- counties(state='OH')
ham0 <- oh %>% 
        st_as_sf(.) %>% 
        filter(NAME=='Hamilton')
ham <- st_transform(ham0, 4326)

## ------------------------------------------------------------------------
ham84 <- st_transform(ham, 4326)
ham_sp <- as(ham84, 'Spatial')

## ------------------------------------------------------------------------
dem_crop <- crop(dem, ham_sp)

## ---- echo=FALSE---------------------------------------------------------
plot(dem_crop)

## ------------------------------------------------------------------------
dem_mask <- mask(dem_crop, ham_sp)

## ---- echo=FALSE---------------------------------------------------------
plot(dem_mask)

## ---- message=FALSE, fig.align='center', fig.height=4--------------------
library('rasterVis')
my_theme <- rasterTheme(region=brewer.pal('RdYlGn', n = 9))
p <- levelplot(dem_crop, margin = FALSE, par.settings = my_theme)
p <- p + layer(sp.lines(ham_sp, lwd = 3, col = 'darkgrey'))
p + layer(sp.points(ham_cities_sp, pch = 19, col = 'black'))

## ---- fig.align='center', fig.height=4-----------------------------------
library('tmap')
tm_shape(wrld, projection="wintri") +
        tm_polygons("lifeExp", style="pretty", palette="RdYlGn",
                    auto.palette.mapping=FALSE, title=c("Life expactancy")) +
        tm_style_grey()

## ---- eval=FALSE---------------------------------------------------------
## library('leaflet')
## leaflet(ham_sp) %>%
##         addProviderTiles(providers$Stamen.Watercolor) %>%
##         # addTiles() %>%
##         addPolygons() %>%
##         addMarkers(data=ham_cities_sp, popup=~as.character(name))
