#*****************************************
#              Assignment_1
#            
#*****************************************

##-1-Packages used ---------

#load my libraries
library(tidyverse)
#Using filter function form tidyvese package
conflicted::conflict_prefer("filter",  "dplyr")

library(viridis)

theme_set(theme_light())

library(ggthemes)

#to create Venn diagram
#install.packages("VennDiagram") ++ New packages ++ 
library(VennDiagram)
library(grid)
library(futile.logger)
#to use the function intersect form the base R
conflicted::conflicts_prefer(base::intersect)

#To change the style of the code
library(styler)

##-2- Getting the data ----

#Set my working directory
#session/set working directory/choose directory/select and open the directory

#open and read the data file
dfBOLD <- read_tsv("../data/Chaetognatha_BOLD_data.tsv")

##-3- Code part 1 - Data Exploration and manipulation ------

#Know the type of the data
class(dfBOLD)

#Know the dimensions of the data frame (number of rows and columns)
#dim(dfBOLD)

#To know the structure,  dimension,  and get a preview of the contents
str(dfBOLD)

#Summary Statistics- to see the minimum,  maximum,  mean,  median (of the numeric data) and number of NA of each variable
summary(dfBOLD)

#See the variable names to use for selecting teh avriables and indexing the data 
names(dfBOLD)

#create a new data frame dfBOLD2 that contains a subset of columns from the existing data frame "dfBOLD" which will include only the variable used for the answering the research question
dfBOLD2 <- dfBOLD[,  c("processid",  "bin_uri",  "species_name",  "country",  "lat")]


#Number of unique of each of the following (to be used in the writing section): 

#bin_uri = 277
length(unique(dfBOLD2$bin_uri))

#species_name = 115
length(unique(dfBOLD2$species_name))

#country = 35
length(unique(dfBOLD2$country))

#lat = 136
length(unique(dfBOLD2$lat))

## Edit 1:Redundancy Reduction and Code Efficiency ------
#The original code manually calculates unique counts for each column using length(unique(...)), which is simple but not scalable. The edited code uses a count_unique function with sapply to calculate unique counts for multiple columns at once, improving efficiency, scalability, and code modularity.
#Calculate the unique count
count_unique <- function(df, columns) {
  sapply(columns, function(col) length(unique(df[[col]])))
}
unique_counts <- count_unique(dfBOLD2, c("bin_uri", "species_name", "country", "lat"))
print(unique_counts)

#Visual normality for "lat" 
qqnorm(dfBOLD2$lat)
qqline(dfBOLD2$lat)

#Histogram for "lat" normality distribution #2
hist(dfBOLD2$lat,  main = "Histogram of Latitude",  xlab = "Latitude",  breaks = 30)

#shapiro test to statically examine the normal distribution of "lat"
shapiro.test(dfBOLD2$lat)

#Test of normality after removing the missing value
dfBOLD2 <- filter(dfBOLD2, !is.na(lat))
shapiro.test(dfBOLD2$lat)

# Calculate mean,  max,  and min latitude for countries (Canada,  United States, india) 
lat_summary <- dfBOLD2%>%
  filter(country %in% c("Canada",  "United States", "India"))%>%  # Filter for specific countries
  group_by(country)%>%  # Group by country
  summarise(
    mean_latitude = mean(lat,  na.rm = TRUE),   # Calculate mean latitude
    max_latitude = max(lat,  na.rm = TRUE),     # Calculate max latitude
    min_latitude = min(lat,  na.rm = TRUE)     # Calculate min latitude
  )

##Edit 2: Adding a Statistical Test and Enhancing Data Summary------
# The original code called the Shapiro-Wilk test twice, causing redundancy. The revised code stores the result in a variable and defines countries_to_analyze as a separate vector, improving efficiency and reusability.
shapiro_test_result <- shapiro.test(dfBOLD2$lat)
print(shapiro_test_result)

countries_to_analyze <- c("Canada", "United States", "India")
lat_summary <- dfBOLD2 %>%
  filter(country %in% countries_to_analyze) %>%
  group_by(country) %>%
  summarise(
    mean_latitude = mean(lat, na.rm = TRUE),
    max_latitude = max(lat, na.rm = TRUE),
    min_latitude = min(lat, na.rm = TRUE)
  )
print(lat_summary)

#All tests showed that the "lat" data does not follow the normal distribution so data need to be transformed

#Data Manipulation:  please note that more data manipulation (i.e.,  filtering data,  grouping,  summarizing) will be find included in the code of each graph

##4- Code part 2 - Analysis to Address the Question-----------------------------

###---Figure 1------- 
# BIN,  Record and Species distribution by country for data exploration 

dfBOLD2%>%  
  #filter the data to remove missing observations (not available data)
  filter(!is.na(country))%>% 
  filter(!is.na(species_name))%>% 
  filter(!is.na(processid))%>% 
  filter(!is.na(bin_uri))%>% 
  
  #group the data by country
  group_by(country)%>% 
  
  #summarize the filtered selected variables in a tibble by counting distinct species, records, and BINs
  summarise(
    Number_of_species = n_distinct(species_name), 
    Number_of_records = n_distinct(processid), 
    Number_of_BIN = n_distinct(bin_uri)
  )%>%
  
  #transform data from a wide format to a long format for restructuring the dataset where the columns become values under a new column named "category", with their corresponding counts in another column called "count"
  pivot_longer(
    cols = c("Number_of_records",  "Number_of_BIN",  "Number_of_species"), 
    names_to = "category", 
    values_to = "count"
  )%>%
  
  #Plotting the data
  #remove "-" from "reorder(country,  -count)" in y = reorder(country,  -count) to arrange the countries descending
  
  ggplot(aes(y = reorder(country, count),  x = count,  fill = category)) + 
  geom_bar(stat = "identity",  position = "dodge") + 
  
  #axes titles and labels
  labs(
    y = "Country",   
    x = "Count (BIN-Records-Species)",   
    fill = " ",  
    title = "BIN,  Record and Species distribution by country"
  ) + 
  
  # Increase label font size and make it black and bold 
  theme(
    axis.title.x = element_text(size = 16,  color = "black",  face = "bold"),   
    axis.title.y = element_text(size = 16,  color = "black",  face = "bold"),   
    axis.text.x = element_text(size = 12,  color = "black"),    
    axis.text.y = element_text(size = 12,  color = "black")    
  )
#save figure with dpi = 300
ggsave("my_plot.png",  plot = last_plot(),  width = 8,  height = 6,  dpi = 300)

###Figure 3 ------------------------------------------------------------------
#Venn diagram showing species distribution among Canada,  United States and India 

#Summarize and filtering the data 
df_summary6 <- dfBOLD2%>%
  filter(!is.na(country) & country %in% c("India",  "Canada",  "United States"))%>%
  filter(!is.na(species_name))%>%
  group_by(country)%>%
  summarise(Name_of_species = list(unique(species_name)))

# Extract species lists for each country
canada_species <- df_summary6$Name_of_species[df_summary6$country == "Canada"][[1]]
India_species <- df_summary6$Name_of_species[df_summary6$country == "India"][[1]]
united_states_species <- df_summary6$Name_of_species[df_summary6$country == "United States"][[1]]

# Find common species between Canada,  United States,  and China
common_species <- Reduce(intersect,  list(canada_species,  united_states_species,  India_species))

# Find unique species for each country
unique_canada_species <- setdiff(canada_species,  common_species)
unique_India_species <- setdiff(India_species,  common_species)
unique_united_states_species <- setdiff(united_states_species,  common_species)

# Create Venn diagram using VennDiagram package

venn.plot <- venn.diagram(
  x = list(Canada = canada_species,  United_States = united_states_species,  India = India_species), 
  category.names = c("Canada",  "United States",  "India"), 
  filename = NULL, 
  fill = c("red",  "blue",  "green"), 
  alpha = 0.5, 
  cex = 1.5, 
  cat.cex = 1.5, 
  cat.fontfamily = "Arial", 
  cat.pos = 0,  # 0 to move the label above the circles, 180 to move labels below the circles
  cat.dist = 0.06      # Adjust label distance from circles
)

# Add the title to the plot
grid.text(
  "Species Distribution among India,  Canada and United States",  
  x = 0.2,  y = 0.95,  gp = gpar(fontsize = 16,  fontfamily = "Arial"),   # Position title at the top
  just = "left",  default.units = "npc"
)
# # Clear the current plotting device to avoid overwriting the previous plot
grid.newpage()

# Plot the diagram
grid.draw(venn.plot)

ggsave("my_plot.png",  plot = last_plot(),  width = 8,  height = 6,  dpi = 300)

###-Figure2 ------------------------------------------------------------------ 
#Number of species and number of barcodes recorded along latitude (degrees)

# Summarize the data
df_summary2 <- dfBOLD2%>%
  
  #Remove NAs
  filter(!is.na(lat))%>%
  filter(!is.na(species_name))%>%
  filter(!is.na(bin_uri))%>%
  
  group_by(lat)%>%
  summarise(
    Number_of_species = n_distinct(species_name), 
    Number_of_BIN = n_distinct(bin_uri)
  )%>%
  
  # Linear regression for species
  species_lm <- lm(Number_of_species ~ lat,  data = df_summary2)
  species_summary <- summary(species_lm)

  ##Edit 3: Corrected Linear Regression for Species Analysis------
  # The original code caused an error by attempting linear regression within a pipe sequence. The revised code separates the summarization and regression steps, stores the data in df_summary2, and adds a ggplot2 scatter plot with a regression line.

# Summarize the data by grouping by latitude and calculating unique species and BINs
df_summary2 <- dfBOLD2 %>%
  
  # Remove rows with missing latitude, species name, or BIN URI
  filter(!is.na(lat)) %>%
  filter(!is.na(species_name)) %>%
  filter(!is.na(bin_uri)) %>%
  
  # Group the data by latitude
  group_by(lat) %>%
  
  # Summarize the number of unique species and BINs at each latitude
  summarise(
    Number_of_species = n_distinct(species_name), 
    Number_of_BIN = n_distinct(bin_uri)
  )

# Perform linear regression to assess the relationship between species richness and latitude
species_lm <- lm(Number_of_species ~ lat, data = df_summary2)

# Get the summary of the linear regression results
species_summary <- summary(species_lm)

# Display the results of the regression model
print(species_summary)

## Visualize the relationship between latitude and species richness
library(ggplot2)
ggplot(df_summary2, aes(x = lat, y = Number_of_species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Number of Species by Latitude",
    x = "Latitude",
    y = "Number of Species"
  )

# Extract F-statistic and p-value for species linear model
species_f_value <- species_summary$fstatistic[1]
species_p_value <- species_summary$coefficients[2,  4]

#Constructs a string representation of a linear regression equation, including the coefficients for the terms, the F-statistic, and the p-value
species_eq <- paste0("Y_species = ",  round(coef(species_lm)[1],  4), 
                     " + ",  round(coef(species_lm)[2],  4),  "*X", 
                     " (F = ",  round(species_f_value,  2), 
                     ",  p = ",  format.pval(species_p_value,  digits = 3),  ")")

# Quadratic regression for BINs
BIN_lm <- lm(Number_of_BIN ~ poly(lat,  2),  data = df_summary2)
BIN_summary <- summary(BIN_lm)

# Extract F-statistic and p-value for BIN quadratic model
BIN_f_value <- BIN_summary$fstatistic[1]
BIN_p_value <- pf(BIN_f_value,  BIN_summary$fstatistic[2],       BIN_summary$fstatistic[3],  lower.tail = FALSE)

#Construct a string representation of a quadratic regression equation, including the coefficients for the terms, the F-statistic, and the p-value
BIN_eq <- paste0("Y_BIN = ",  round(coef(BIN_lm)[1],  4), 
                 " + ",  round(coef(BIN_lm)[2],  4),  "*X + ", 
                 round(coef(BIN_lm)[3],  4),  "*X^2", 
                 " (F = ",  round(BIN_f_value,  2), 
                 ",  p = ",  format.pval(BIN_p_value,  digits = 3),  ")")

# Create the plot
ggplot(df_summary2,  aes(x = lat)) +
  
  # Species (filled dots and linear trend)
  geom_point(aes(y = Number_of_species),  shape = 16,  size = 3,  color = "black") + 
  
  #Generates a linear regression line on a ggplot to represent the relationship between x and Number_of_specie using a black line.
  geom_smooth(aes(y = Number_of_species),  method = "lm",  se = FALSE,  color = "black") + 
  
  # BINs (empty dots and quadratic trend)
  geom_point(aes(y = Number_of_BIN),  shape = 1,  size = 3,  color = "black") +
  
  #Generate a quadratic regression line on a ggplot to represent the relationship between x and Number_of_BIN, using a dashed black line.
  geom_smooth(aes(y = Number_of_BIN),  method = "lm",  formula = y ~ poly(x,  2),  se = FALSE,  linetype = "dashed",  color = "black") +
  
  # Axis labels (x and left y axis)
  labs(x = "Latitude",  
       y = "Number of species (filled dots)",  
       title = "Number of Species and BIN Trends across Latitude") +
  
  # Secondary y-axis (right y axis)
  scale_y_continuous(sec.axis = sec_axis(~ .,  name = "Number of BIN (empty dots)")) +
  
  # Customize x-axis to add more latitude ticks (Latitude from -90 to 90 in increments of 10 degrees)
  scale_x_continuous(breaks = seq(-90,  90,  by = 10)) +  
  
  # Add the regression equations as text on the plot
  annotate("text",  x = 10,  y = 45,  label = species_eq,  size = 5,  hjust = 0,  color = "black") +
  annotate("text",  x = 10,  y = 35,  label = BIN_eq,  size = 5,  hjust = 0,  color = "black") +
  
  # Formatting axes labels and graph title
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 16,  color = "black"), 
    axis.title.y.right = element_text(size = 16,  color = "black"), 
    axis.title.x = element_text(size = 16,  color = "black"), 
    axis.text = element_text(size = 14,  color = "black"), 
    plot.title = element_text(hjust = 0.5,  face = "bold")
  )
#save figure with dpi = 300
ggsave("my_plot.png",  plot = last_plot(),  width = 8,  height = 6,  dpi = 300)

