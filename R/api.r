################################################################################
#                  Using API to make maps in R
#                  Milos Popovic
#                  2022/01/30
################################################################################

if(!require("httr")) install.packages("httr") 
if(!require("jsonlite")) install.packages("jsonlite")  
if(!require("sf")) install.packages("sf")
if(!require("tidyverse")) install.packages("tidyverse")  
if(!require("grid")) install.packages("grid")  
if(!require("gridExtra")) install.packages("gridExtra") 
if(!require("giscoR")) install.packages("giscoR") 
if(!require("classInt")) install.packages("classInt")   

library(httr, quietly=T) #send request to API
library(jsonlite, quietly=T) #JSON parser and generator    
library(tidyverse, quietly=T) #data wrangling
library(sf, quietly=T) #geospatial analysis
library(grid, quietly=T) #make grid
library(gridExtra, quietly=T) #make grid
library(giscoR, quietly=T) #shapefile of Europe
library(classInt, quietly=T) #bins

# 1. API
#---------
res <- GET("https://ghoapi.azureedge.net/api/Indicator")
res

indicators <- fromJSON(rawToChar(res$content))
str(indicators)

data <- indicators$value
head(data)

# 2. DATA
#----------

rtd <- grep("road traffic death", data$IndicatorName, perl=T)
sel_rows <- data[rtd,]
sel_rows

traffic_deaths <- GET("https://ghoapi.azureedge.net/api/RS_198")
traffic_d <- fromJSON(rawToChar(traffic_deaths$content))

td <- traffic_d$value
head(td)

td <- td %>%
	  filter(Dim1 == "BTSX") %>%
	  select(SpatialDim, TimeDim, NumericValue, Low, High) %>%
	  group_by(SpatialDim) %>% 
	  slice(which.max(TimeDim)) %>%
	  rename(ISO3 = SpatialDim,
         year = TimeDim,
         value = NumericValue)

head(td)

# 3. SPATIAL DATA
#----------

europe <- giscoR::gisco_get_countries(
  year = "2016",
  epsg = "4326",
  resolution = "10",
  region = "Europe"
)

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"


bb <- st_sfc(
  st_polygon(list(cbind(
    c(-10.6600, 33.00, 33.00, -10.6600, -10.6600),
    c(32.5000, 32.5000, 71.0500, 71.0500, 32.5000) 
    ))),
  crs = crsLONGLAT)

laeabb <- st_transform(bb, crs = crsLAEA)
b <- st_bbox(laeabb)

c <- right_join(td, europe, by=c("ISO3"="ISO3_CODE")) %>%
     st_as_sf() %>%
     st_transform(crs = crsLAEA) %>%
     drop_na()

# bins
brk <- round(classIntervals(c$value, 
              n = 6, 
              style = 'equal')$brks, 0)

# define the color palette
cols = rev(c('#05204d', '#004290', '#6458ae', '#dd98d1', '#eab0a2'))
newcol <- colorRampPalette(cols)
ncols <- 7
cols2 <- newcol(ncols)
vmin <- min(c$value, na.rm=T)
vmax <- max(c$value, na.rm=T)

#centroids
cents <- c %>% st_centroid() %>%  
  as_Spatial() %>%                  
  as.data.frame()

#centroids for France and Russia
moscow <- data.frame(long = 37.61556, lat = 55.75222)
paris <- data.frame(long = 2.3488, lat = 48.85341)

mc <- st_as_sf(x = moscow,                         
           coords = c("long", "lat"),
           crs = 4326) %>%
     st_transform(crs = crsLAEA) %>%  
     as_Spatial() %>%                  
     as.data.frame()

pa <- st_as_sf(x = paris,                         
           coords = c("long", "lat"),
           crs = 4326) %>%
     st_transform(crs = crsLAEA) %>%  
     as_Spatial() %>%                  
     as.data.frame()

#plug new values into centroids
cn <- cents %>% 
  mutate(coords.x1=ifelse(CNTR_ID=="RU", mc$coords.x1,coords.x1),
         coords.x2=ifelse(CNTR_ID=="RU", mc$coords.x2,coords.x2)) %>%
  mutate(coords.x1=ifelse(CNTR_ID=="FR", pa$coords.x1,coords.x1),
         coords.x2=ifelse(CNTR_ID=="FR", pa$coords.x2,coords.x2)) 

# 4. MAPPING
#----------

p1 <- 
ggplot() +
geom_sf(data = c, 
  aes(fill=value), 
  color="white", 
  size=0.25) +
coord_sf(crs = crsLAEA, 
  xlim = c(b["xmin"], b["xmax"]), 
  ylim = c(b["ymin"], b["ymax"])) +
labs(y="", 
    subtitle="",
    x = "",
    title="Estimated road traffic death rate (2019)",
    caption="©2022 Milos Popovic https://milospopovic.net\nSource: WHO")+
scale_fill_gradientn(name="deaths per 100,000 people",
                       colours=cols2,
                       breaks=brk,
                       labels=brk,
					   limits = c(min(brk),max(brk)))+
geom_text(data = filter(cn, CNTR_ID=="HR"), 
    aes(coords.x1, coords.x2, 
      label=round(value, 1)),
      size=2.75,
      vjust=-1.5,
      color="grey20",
      family="georg") + 
geom_text(data = filter(cn, CNTR_ID=="EL"), 
    aes(coords.x1, coords.x2, 
      label=round(value, 1)),
      size=2.75,
      hjust=1.5,
      color="grey20",
      family="georg") +
geom_text(data = filter(cn, CNTR_ID=="AL"), 
    aes(coords.x1, coords.x2, 
      label=round(value, 1)),
      size=2.75,
      hjust=1.5,
      color="grey20",
      family="georg") +      
guides(fill=guide_legend(
            direction = "horizontal",
            keyheight = unit(1.15, units = "mm"),
            keywidth = unit(12, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "bottom"
          )
    ) +
theme_minimal() +
theme(text = element_text(family = "georg"),
panel.background = element_blank(), 
legend.background = element_blank(),
legend.position = c(.45, .02),
panel.border = element_blank(),
panel.grid.minor = element_blank(),
panel.grid.major = element_blank(),
plot.title = element_text(size=16, color='#6458ae', hjust=0.5, vjust=0),
plot.subtitle = element_text(size=14, color='#ac63a0', hjust=0.5, vjust=0),
plot.caption = element_text(size=7, color="grey60", hjust=0.5, vjust=10),
axis.title.x = element_text(size=10, color="grey20", hjust=0.5, vjust=-6),
legend.text = element_text(size=9, color="grey20"),
legend.title = element_text(size=10, color="grey20"),
strip.text = element_text(size=12),
plot.margin     =   unit(c(t=1, r=-2, b=-1, l=-2),"lines"),
axis.title.y = element_blank(),
axis.ticks = element_blank(),
axis.text.x = element_blank(),
axis.text.y = element_blank())

print(p1)

p2 <- 
p1 +
geom_text(data = filter(cn, !CNTR_ID%in%c("AL", "HR", "EL") & value < 9), 
    aes(coords.x1, coords.x2, 
      label=round(value, 1)),
      size=2.75,
      color="grey20",
      family="georg") + 
geom_text(data = filter(cn, !CNTR_ID%in%c("AL", "HR", "EL") & value >= 9), 
    aes(coords.x1, coords.x2, 
      label=round(value, 1)),
      size=2.75,
      color="grey80",
      family="georg")

print(p2)
