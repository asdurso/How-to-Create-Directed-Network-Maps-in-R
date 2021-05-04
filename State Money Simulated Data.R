## State Money Simulated Data Script

#Load library
library(tidyverse)

#Set seed
set.seed(1776)

#Load State Capital Data
state_caps <-
  read_delim(file = "_data/us_states_lonlat.txt", ",") %>% #load data that includes lat/lon of state capitals
  rename(state = name, #rename for ease of use
         capital = description, 
         lon = longitude, 
         lat = latitude)

#Create Simulated Data
state_sim <- state_caps %>%  
  select(state) %>% 
  mutate(val = NA) %>% #need some data to make the next step work
  spread(key = state, val = val) %>% #reshape data so each state is a column
  bind_rows(state_caps %>% select(state)) %>% #add on each state as a row (so we can eventually have each state interacting in some way with every other state)
  filter(!is.na(state)) %>% #remove the NA row we created for `spread`
  select(-state) %>% 
  mutate_all(~sample(51:100, replace =  T) %>% scales::rescale(to = c(0.5,2.5))) #randomly give each cell a value between 51-100 (ie money) and rescale from 0.5 to 2.5. This rescaling also helps with the line width for the directed network maps. This is our simulated data.


#Remove Cases of States Giving Money to Themselves
##Now we have the data, but it shows each state giving money to itself. We don't want this, so we want to include an NA for the cases where the state from is the same as the state to
for(i in 1:nrow(state_sim)){  
  for (j in 1:nrow(state_sim))      
    if (i == j){
      state_sim[i,j] <- NA;  #for the cases where the row of interest is the same value as the column of interest, NA
    }
}

#Clean up the Simulated Data and add Lat/Lon
#Save Data
state_simulation <- state_sim %>% 
  bind_cols(state_caps) %>% #now we can add back in the state name, along with the capital and lat/lon
  select(state, capital, lat, lon, everything()) %>% 
  rename(money_from_state = state) #%>% 
  #write_csv("Data/state_sim.csv") #saves the data
