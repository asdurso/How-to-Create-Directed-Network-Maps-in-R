#### How to Create Directed Network Maps in R #### 
#### By Amanda Sahar d'Urso ####
################################



#### Set Up ####

### Load Libraries 
library(maps)
library(tools)
library(lwgeom)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(sf)
library(tidyverse)

### Load Data 
state_sim <- read_csv("Data/state_sim.csv")

### Load Functions 
state_coord <- function(state_name, lon_or_lat) {
  state_sim %>%
    filter(money_from_state == state_name) %>%
    select(lon_or_lat) %>%
    as.matrix() %>%
    as.vector()
}

state_money <- function(state_from, state_to) {
  state_sim %>%
    filter(money_from_state == state_from) %>%
    select(state_to) %>%
    as.matrix() %>%
    as.vector()
}

#### Set up Maps ####
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

states <- cbind(states, st_coordinates(st_centroid(states)))

states$ID <- toTitleCase(states$ID)

### If you want all states 
all_states <- ne_states(country = "united states of america", returnclass = "sf")

### Add Data
sites <- st_as_sf(
  data.frame(longitude = state_sim$lon,
             latitude = state_sim$lat),
  coords = c("longitude", "latitude"),
  crs = 4326, #the projection we want to use
  agr = "constant"
)

#### Plot Maps ####
states %>% 
  ggplot() +
  geom_sf()

### Add Color 
states %>% 
  ggplot() +
  geom_sf(fill = "antiquewhite1")

### Add Key Points 
states %>% 
  ggplot() +
  geom_sf(fill = "antiquewhite1") + 
  geom_sf(data = sites, size = 2, shape = 16, color = "salmon4", alpha = 0.5)

### Alternative Projections 
all_states %>% 
  ggplot() +
  geom_sf(fill = "antiquewhite1") + 
  geom_sf(data = sites, size = 2, shape = 16, color = "salmon4", alpha = 0.5) +
  coord_sf(crs = st_crs(2163), xlim = c(-6000000, 2500000))

#### Map Directed Network ####
states %>%
  ggplot() +
  geom_sf(fill = "antiquewhite1") +
  geom_spatial_segment(
    aes(
      #use our `state_coord` fx for state locations 
      x = state_coord("Virginia", "lon"), 
      y = state_coord("Virginia", "lat"),
      xend = state_coord("Texas", "lon"),
      yend = state_coord("Texas", "lat"),
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  )

### Repeat the Process
states %>% 
  ggplot() +
  geom_sf(data = states, fill = "antiquewhite1") +
  geom_spatial_segment(
    aes(
      x = state_coord("Virginia", "lon"),
      y = state_coord("Virginia", "lat"),
      xend = state_coord("Texas", "lon"),
      yend = state_coord("Texas", "lat")
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Washington", "lon"),
      y = state_coord("Washington", "lat"),
      xend = state_coord("Texas", "lon"),
      yend = state_coord("Texas", "lat"),
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Illinois", "lon"),
      y = state_coord("Illinois", "lat"),
      xend = state_coord("Virginia", "lon"),
      yend = state_coord("Virginia", "lat")
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Michigan", "lon"),
      y = state_coord("Michigan", "lat"),
      xend = state_coord("North Dakota", "lon"),
      yend = state_coord("North Dakota", "lat")
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  )

### Space Between Arrows
states %>% 
  ggplot() +
  geom_sf(data = states, fill = "antiquewhite1") +
  geom_spatial_segment(
    aes(
      x = state_coord("Virginia", "lon"),
      y = state_coord("Virginia", "lat") - 0.5, #manual segment adjustment
      xend = state_coord("Texas", "lon") + 0.5,
      yend = state_coord("Texas", "lat")
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Washington", "lon"),
      y = state_coord("Washington", "lat"),
      xend = state_coord("Texas", "lon") - 0.5,
      yend = state_coord("Texas", "lat"),
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Illinois", "lon"),
      y = state_coord("Illinois", "lat"),
      xend = state_coord("Virginia", "lon") - 1.5,
      yend = state_coord("Virginia", "lat")
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Michigan", "lon"),
      y = state_coord("Michigan", "lat"),
      xend = state_coord("North Dakota", "lon"),
      yend = state_coord("North Dakota", "lat") + 1.5
    ),
    color = "tomato",
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) 

### Line Thickness to Values 
states %>% 
  ggplot() +
  geom_sf(data = states, fill = "antiquewhite1") +
  geom_spatial_segment(
    aes(
      x = state_coord("Virginia", "lon"),
      y = state_coord("Virginia", "lat") - 0.5, #manual segment adjustment
      xend = state_coord("Texas", "lon") + 0.5,
      yend = state_coord("Texas", "lat")
    ),
    color = "tomato",
    size = state_money("Virginia", "Texas"), #adding funds granted data using our function
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Washington", "lon"),
      y = state_coord("Washington", "lat"),
      xend = state_coord("Texas", "lon") - 0.5,
      yend = state_coord("Texas", "lat"),
    ),
    color = "tomato",
    size = state_money("Washington", "Texas"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Illinois", "lon"),
      y = state_coord("Illinois", "lat"),
      xend = state_coord("Virginia", "lon") - 1.5,
      yend = state_coord("Virginia", "lat")
    ),
    color = "tomato",
    size = state_money("Illinois", "Virginia"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Michigan", "lon"),
      y = state_coord("Michigan", "lat"),
      xend = state_coord("North Dakota", "lon"),
      yend = state_coord("North Dakota", "lat") + 1.5
    ),
    color = "tomato",
    size = state_money("Michigan", "North Dakota"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) 

### Label Network 
states %>% 
  ggplot() +
  geom_sf(data = states, fill = "antiquewhite1") +
  geom_spatial_segment(
    aes(
      x = state_coord("Virginia", "lon"),
      y = state_coord("Virginia", "lat") - 0.5, #manual segment adjustment
      xend = state_coord("Texas", "lon") + 0.5,
      yend = state_coord("Texas", "lat")
    ),
    color = "tomato",
    size = state_money("Virginia", "Texas"), #adding funds granted data using our function
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Washington", "lon"),
      y = state_coord("Washington", "lat"),
      xend = state_coord("Texas", "lon") - 0.5,
      yend = state_coord("Texas", "lat"),
    ),
    color = "tomato",
    size = state_money("Washington", "Texas"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Illinois", "lon"),
      y = state_coord("Illinois", "lat"),
      xend = state_coord("Virginia", "lon") - 1.5, 
      yend = state_coord("Virginia", "lat")
    ),
    color = "tomato",
    size = state_money("Illinois", "Virginia"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Michigan", "lon"),
      y = state_coord("Michigan", "lat"),
      xend = state_coord("North Dakota", "lon"),
      yend = state_coord("North Dakota", "lat") + 1.5
    ),
    color = "tomato",
    size = state_money("Michigan", "North Dakota"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_label(aes(
    label = "$1.13 VA to TX", #what the label should say
    x = state_coord("Virginia", "lon") + 4, #manual label adjustment
    y = state_coord("Virginia", "lat")
  ), size = 2.5) +
  geom_label(aes(
    label = "$2.38 WA to TX",
    x = state_coord("Washington", "lon") + 0.5,
    y = state_coord("Washington", "lat") + 1
  ), size = 2.5) +
  geom_label(aes(
    label = "$2.01 IL to VA",
    x = state_coord("Illinois", "lon")-4,
    y = state_coord("Illinois", "lat")
  ), size = 2.5) +
  geom_label(aes(
    label = "$0.99 MI to ND",
    x = state_coord("Michigan", "lon") + 4,
    y = state_coord("Michigan", "lat") + 0.5
  ), size = 2.5) 


### Clean-up Plot Features 
states %>% 
  ggplot() +
  geom_sf(data = states, fill = "antiquewhite1") +
  geom_spatial_segment(
    aes(
      x = state_coord("Virginia", "lon"),
      y = state_coord("Virginia", "lat") - 0.5, #manual segment adjustment
      xend = state_coord("Texas", "lon") + 0.5,
      yend = state_coord("Texas", "lat")
    ),
    color = "tomato",
    size = state_money("Virginia", "Texas"), #adding funds granted data using our function
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Washington", "lon"),
      y = state_coord("Washington", "lat"),
      xend = state_coord("Texas", "lon") - 0.5,
      yend = state_coord("Texas", "lat"),
    ),
    color = "tomato",
    size = state_money("Washington", "Texas"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Illinois", "lon"),
      y = state_coord("Illinois", "lat"),
      xend = state_coord("Virginia", "lon") - 1.5, 
      yend = state_coord("Virginia", "lat")
    ),
    color = "tomato",
    size = state_money("Illinois", "Virginia"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_spatial_segment(
    aes(
      x = state_coord("Michigan", "lon"),
      y = state_coord("Michigan", "lat"),
      xend = state_coord("North Dakota", "lon"),
      yend = state_coord("North Dakota", "lat") + 1.5
    ),
    color = "tomato",
    size = state_money("Michigan", "North Dakota"),
    crs = 4326,
    arrow = grid::arrow(length = unit(0.15, "inches"))
  ) +
  geom_label(aes(
    label = "$1.13 VA to TX",
    x = state_coord("Virginia", "lon") + 4, #manual label adjustment
    y = state_coord("Virginia", "lat")
  ), size = 2.5) +
  geom_label(aes(
    label = "$2.38 WA to TX",
    x = state_coord("Washington", "lon") + 0.5,
    y = state_coord("Washington", "lat") + 1
  ), size = 2.5) +
  geom_label(aes(
    label = "$2.01 IL to VA",
    x = state_coord("Illinois", "lon")-4,
    y = state_coord("Illinois", "lat")
  ), size = 2.5) +
  geom_label(aes(
    label = "$0.99 MI to ND",
    x = state_coord("Michigan", "lon") + 4,
    y = state_coord("Michigan", "lat") + 0.5
  ), size = 2.5) +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA)
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggtitle("Medical Relief Granted per Million Dollars")