#Honors Project
#Jacob Holmes

# LOADING PACKAGES AND DATA ####

if(!require("pacman")) install.packages("pacman")

pacman::p_load(magrittr, #for different pipe operators
               rio,      #for importing data
               tidyverse,#for graphics and data manipulation
               reshape2, #for making data different formats
               agricolae,#for stats?
               ggplot2)#for making multiple graphs in one plot

#importing the Alaska Area data formatted in ArcGIS
dfarea <- import("Alaska Shp to Area.csv") %>%
  as_tibble() %>%
  print()

#importing Alaskan Election Results
df <- import("2018gepctresults-cleaned.csv") %>%
  as_tibble() %>%
  print()

#merging the two dataframes based on common attributes (aka district id)
union <- merge(df, dfarea, all.x = T, all.y = T) %>%
  as_tibble() %>%
  mutate_if(is.character, as.factor) %>%
  mutate(state_districtid = state_districtid %>% as_factor(), #reformatting some numeric items as factors
         ap_canidx = ap_canidx %>% as_factor(),
         ap_precinct = ap_precinct %>% as_factor()) %>%
  print()

# EDA ####

#seeing different columns and column types, along with some data
union %>%
  glimpse()

#seeing size of the data set
union %>% size_sum()

#seeing column names
union %>% names()

#seeing if there are any missing values
union %>% is.na() %>% colSums()

#seeing correlation matrix with different numeric columns
union %>% 
  select_if(is.numeric) %>%
  cor()

## Graphs of Interest ====

#column graph of the votes by district and by candidate
union %>% ggplot(aes(x = state_districtid, 
                     y = votes,
                     fill = ap_full_can_nam)) +
  geom_col() + 
  labs(title = "Votes for Each Party by District in the 2018 Alaskan Gubernatorial Election",
       x = "District",
       y = "Votes") +
  scale_fill_discrete(name = "Candidate")

#seeing what levels there are for this column factor
union$vote_method %>%
  levels()

#plotting the votes by district for each of the voting methods
union %>% 
  ggplot(aes(x = state_districtid,
                     y = votes,
                     fill =ap_full_can_nam)) +
  geom_col() +
  facet_wrap(.~vote_method) +#facet wrap creates a new graph for each of the levels within the voting method factor
  labs(title = "Votes for Each Party by District and Voting Method",
       x = "District",
       y = "Votes") +
  scale_fill_discrete(name = "Candidate")

#plotting votes by method and by candidate 100% graph
union %>%
  ggplot(aes(x = vote_method, y = votes, fill = ap_full_can_nam)) +
  geom_col(position = "fill") +
  labs(title = "Percentage of Votes for Each Candidate by Voting Method",
       x = "Voting Method",
       y = "Vote Percentage") +
  scale_fill_discrete(name = "Candidate")

# ANALYSIS ####

#making an ANOVA model of the votes by the two factors below
model <- union %$%
  aov(votes ~ vote_method * state_districtid) %>%
  print()

model %>% summary()

#making an interaction plot of the specific data used in the model above
union %$%
  interaction.plot(x.factor = ap_maj_ptycod_align,
                   trace.factor = vote_method,
                   response = votes,
                   col = c("blue", "red", "green", "orange"),
                   xlab = "Candidate Party",
                   ylab = "Mean of Votes",
                   trace.label = "Voting Method")

#reversing the trace and x-axis data
union %$%
  interaction.plot(x.factor = vote_method,
                   trace.factor = ap_maj_ptycod_align,
                   response = votes,
                   col = c("blue", "red", "green", "orange"),
                   xlab = "Voting Method",
                   ylab = "Mean of Votes",
                   trace.label = "Candidate Party")

#making an interaction plot for the factors below
union %$%
  interaction.plot(x.factor = state_districtid,
                   trace.factor = vote_method,
                   response = votes,
                   col = c("blue", "red", "green", "orange"),
                   xlab = "District",
                   ylab = "Mean of Votes",
                   trace.label = "Voting Method")

#making an ANOVA model of the district area on the voting method factor
model2 <- union %$%
  aov(dist_area ~ vote_method) %>%
  print()

model2 %>% summary()

#making an interaction plot of the factors above
union %$%
  interaction.plot(x.factor = dist_area,
                   trace.factor = vote_method,
                   response = votes,
                   col = c("blue", "red", "green", "orange"),
                   xlab = "District Area",
                   ylab = "Mean of Votes",
                   trace.label = "Voting Method")

#Clean up


# Clear data
rm(list = ls())

# Clear packages
p_unload(all)

# Clear plots
graphics.off()

# Clear console
cat("\014")

