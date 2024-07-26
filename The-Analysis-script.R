## 30.12.2023
## Written by Nargiz Safaraliyeva
## for an Interview Presentation
## Data Scientist and Visualization


#### # Function to create an input dialog ####
# Library(tcltk)
# get_input <- function(prompt) {
#   # Create a variable to store user input
#   user_input <- tclVar("")
#   
#   # Create the dialog box
#   dlg <- tktoplevel()
#   tkwm.title(dlg, "Input Required")
#   
#   # Add a label and entry box
#   tkgrid(tklabel(dlg, text = prompt))
#   entry <- tkentry(dlg, textvariable = user_input)
#   tkgrid(entry)
#   
#   # Add OK and Cancel buttons
#   tkgrid(tkbutton(dlg, text = "OK", command = function() tkdestroy(dlg)))
#   tkgrid(tkbutton(dlg, text = "Cancel", command = function() {
#     tclvalue(user_input) <- ""
#     tkdestroy(dlg)
#   }))
#   
#   # Wait for the dialog to close
#   tkwait.window(dlg)
#   
#   # Return the user input
#   return(tclvalue(user_input))
# }
# # Get the working directory from the user
# working_directory <- get_input("Please enter the working directory:")
# 
# # Get the pati_main path from the user
# pati_main <- get_input("Please enter the path for pati_main:")

## Set working directory
#setwd("C:/Users/.../")
## Set path
pati_main <- "./.../interview_materials/"

## Import Libraries
#install.packages("readxl")
library(readxl)
#install.packages("knitr")
library(knitr)
#install.packages("corrplot")
library(corrplot)
library(openxlsx)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)


## Import open-source data
data_avia <- read_xlsx("./.../DataSet_ASRS_2022.xlsx")

## Converting xlsx into csv file
#write.csv(data_avia, "DataSet_ASRS_2022.csv", row.names = FALSE)

## Check column names
colnames(data_avia)
# Check dimension of the database
dim(data_avia)
# See the classes of each columns
print(sapply(data_avia, class))
# Print the dataframe columns' classes names table using kable
kable(sapply(data_avia, class), caption = "Column Classes")
# Display the unique values for each column
unique(data_avia$`Primary Problem`)

## Find the count of each unique combination of 'Date' and 'Primary Problem'
problem_counts <- table(data_avia$Date) # , data_avia$`Primary Problem`) #, data_avia$`Local Time Of Day`
# Convert the result to a data frame
problem_counts_df <- as.data.frame(as.table(problem_counts))
colnames(problem_counts_df) <- c("Date", "Count") # "Primary Problem",  #"Local Time Of Day", 
# Order the data frame by count in descending order
problem_counts_df <- problem_counts_df[order(-problem_counts_df$Count), ]
# Order the data frame by 'Date' in descending order
problem_counts_df <- problem_counts_df %>%
  arrange(Date)
# Display the data frame
print(problem_counts_df)

## Get unique Primary Problem values
unique_problems <- unique(data_avia$`Primary Problem`)
#detach(package:plyr)
#detach("package:plyr", character.only = TRUE, unload = TRUE)
# Reshape the data_avia dataframe
data_avia_reshaped <- data_avia %>%
  group_by(Date, `Primary Problem`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = `Primary Problem`, values_from = Count, values_fill = 0)
# Merge the reshaped data with problem_counts_df
problem_counts_df <- merge(problem_counts_df, data_avia_reshaped, by = "Date", all.x = TRUE)
# Fill missing values with 0
problem_counts_df[is.na(problem_counts_df)] <- 0
# Reoder columns
desired_order <- c("Date", "Count", "Aircraft", "Human Factors",
                   "Ambiguous", "Environment - Non Weather Related",
                   "Procedure","ATC Equipment / Nav Facility / Buildings","Weather",
                   "Airport","Equipment / Tooling",
                   "Chart Or Publication", "Airspace Structure",
                   "Company Policy","MEL","Incorrect / Not Installed / Unavailable Part",
                   "NA", "Software and Automation","Manuals")
problem_counts_df <- problem_counts_df[, desired_order]
# Print the updated problem_counts_df
print(problem_counts_df)


# # 'data_avia' is your dataframe
# for (col in names(data_avia)) {
#   # Display the unique values for each column
#   cat("Unique values in column", col, ":\n")
#   print(unique(data_avia[[col]]))
#   
#   # Find the count of each unique value in a column and rank them by frequency
#   counts <- table(data_avia[[col]])
#   
#   # Convert the result to a data frame
#   counts_df <- data.frame(Value = names(counts), Count = as.numeric(counts))
#   
#   # Order the data frame by count in descending order
#   counts_df <- counts_df[order(-counts_df$Count), ]
#   
#   # Display the data frame
#   cat("\nCounts for column", col, ":\n")
#   print(counts_df)
#   
#   cat("\n------------------------\n")
# }


## 'data_avia' is the dataframe
# 'character_cols' is a vector containing the names of character value columns
character_cols <- c(   "ACN", "Date", "Local Time Of Day", "Locale Reference", "State Reference",
                       "Flight Conditions", "Weather Elements / Visibility", "Work Environment Factor",
                       "Light", "Ceiling", "ATC / Advisory", "Aircraft Operator", "Make Model Name",
                       "Crew Size", "Flight Plan", "Mission", "Flight Phase", "Airspace",
                       "Cabin Lighting", "Aircraft Component", "Problem", "Human Factors",
                       "ASRS Report Number.Accession Number", "Anomaly",
                       "Were Passengers Involved In Event", "Detector", "When Detected", "Result",
                       "Contributing Factors / Situations", "Primary Problem", "Narrative",
                       "Callback", "Synopsis")  # Replace with your actual column names
## Function to check for "fume" or "smoke" in any column for each row
check_fume_smoke <- function(row) {
  # Convert to lowercase for case-insensitive search
  row_lower <- tolower(row)
  # Check if "fume" or "smoke" exists in any column for the row
  has_fume_or_smoke <- any(grepl("fume|smoke", row_lower))
  return(has_fume_or_smoke)
}
# Apply the function to each row
result <- apply(data_avia[character_cols], 1, check_fume_smoke)
# Create a new dataframe with rows where "fume" or "smoke" is present
filtered_datam <- data_avia[result, ]
# Define the values to filter
values_to_filter <- c("Routine Inspection", "Aircraft In Service At Gate",
                      "Other Gate Pushback", "Other Takeoff Roll", 
                      "Other Not stated", "Other Post Flight")
## Create a new data frame containing only rows with all the specified values (not associated with events during the flight)
filtered_data_on_ground <- subset(filtered_datam, `When Detected` %in% values_to_filter)

## Create a new data frame excluding rows with the specified values (associated with events during the flight)
filtered_data <- subset(filtered_datam, !(`When Detected` %in% values_to_filter))
# Print or inspect the filtered data
print(filtered_data)
# Write the dataframe to an Excel file
# write.xlsx(filtered_data, "./DataSet_ASRS_2022-filtered_data.xlsx", rowNames = FALSE)

## Find the count of each unique combination of 'Date' and 'Primary Problem'
problem_counts_filtered <- table(filtered_data$Date) # , filtered_data$`Primary Problem`) #, filtered_data$`Local Time Of Day`
# Convert the result to a data frame
problem_counts_filtered_df <- as.data.frame(as.table(problem_counts_filtered))
colnames(problem_counts_filtered_df) <- c("Date", "Count") # "Primary Problem",  #"Local Time Of Day", 
# Order the data frame by count in descending order
problem_counts_filtered_df <- problem_counts_filtered_df[order(-problem_counts_filtered_df$Count), ]
# Order the data frame by 'Date' in descending order
problem_counts_filtered_df <- problem_counts_filtered_df %>%
  arrange(Date)
# Display the data frame
print(problem_counts_filtered_df)

## Get unique Primary Problem values
unique_problems_filtered <- unique(filtered_data$`Primary Problem`)
unique_times_filtered <- unique(filtered_data$`Local Time Of Day`)
# Reshape the filtered_data by time
filtered_time_reshaped <-  filtered_data %>%
       group_by(Date, `Local Time Of Day`) %>%
       summarise(Count = n(), .groups = 'drop') %>%
       pivot_wider(names_from = `Local Time Of Day`, values_from = Count, values_fill = 0)
# Reshape the filtered_data by Aircraft type
filtered_aircraft_reshaped <- filtered_data %>%
  group_by(Date, `Make Model Name`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = `Make Model Name`, values_from = Count, values_fill = 0)
colnames(filtered_aircraft_reshaped)
filtered_aircraft_reshaped[,6] <- filtered_aircraft_reshaped[,6] + filtered_aircraft_reshaped[,7]
filtered_aircraft_reshaped <- filtered_aircraft_reshaped[, -c(7)] #Airbus Undifferentiated
filtered_aircraft_reshaped[,7] <- filtered_aircraft_reshaped[,7]+filtered_aircraft_reshaped[,8]+filtered_aircraft_reshaped[,13]+filtered_aircraft_reshaped[,19]+filtered_aircraft_reshaped[,20]+filtered_aircraft_reshaped[,26]+filtered_aircraft_reshaped[,38]
filtered_aircraft_reshaped <- filtered_aircraft_reshaped[, -c(8,13,19,20,26,38)] #B737
filtered_aircraft_reshaped[,25] <- filtered_aircraft_reshaped[,25]+ filtered_aircraft_reshaped[,27] + filtered_aircraft_reshaped[,34]
filtered_aircraft_reshaped <- filtered_aircraft_reshaped[, -c(27,34)] #B787
filtered_aircraft_reshaped[,12] <- filtered_aircraft_reshaped[,12]+ filtered_aircraft_reshaped[,17] + filtered_aircraft_reshaped[,28]
filtered_aircraft_reshaped <- filtered_aircraft_reshaped[, -c(12,17,28)] #B777
filtered_aircraft_reshaped[,24] <- filtered_aircraft_reshaped[,24]+ filtered_aircraft_reshaped[,33] + filtered_aircraft_reshaped[,28]
filtered_aircraft_reshaped <- filtered_aircraft_reshaped[, -c(33)] #B767
filtered_aircraft_reshaped[,5] <- filtered_aircraft_reshaped[,6]+ filtered_aircraft_reshaped[,5]
filtered_aircraft_reshaped <- filtered_aircraft_reshaped[, -c(6)] #Airbus Undifferentiated
filtered_aircraft_reshaped[,12] <- rowSums(filtered_aircraft_reshaped[,c(12:14,16:17,19,21,24:27,29:34)]) #Other
colnames(filtered_aircraft_reshaped)[12] <- "Other"
filtered_aircraft_reshaped <- filtered_aircraft_reshaped[, -c(13:14,16:17,19,21,24:27,29:34)]
colnames(filtered_aircraft_reshaped)
# Reshape the filtered_data dataframe
filtered_data_reshaped <- filtered_data %>%
  group_by(Date, `Primary Problem`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = `Primary Problem`, values_from = Count, values_fill = 0)
# Merge the reshaped data with problem_counts_filtered_df
problem_counts_filtered_df <- merge(problem_counts_filtered_df, filtered_data_reshaped, by = "Date", all.x = TRUE)
problem_counts_filtered_df <- merge(problem_counts_filtered_df, filtered_time_reshaped, by = "Date", all.x = TRUE)
colnames(problem_counts_filtered_df)[14] <- "Local_Time_NA"
problem_counts_filtered_df <- merge(problem_counts_filtered_df, filtered_aircraft_reshaped, by = "Date", all.x = TRUE)

# Fill missing values with 0
problem_counts_filtered_df[is.na(problem_counts_filtered_df)] <- 0
# Rename columns
colnames(problem_counts_filtered_df)[1:9] <- c("Date","Count_smoke_fume","Aircraft_smoke_fume",
                                          "Human Factors_smoke_fume","Procedure_smoke_fume",
                                          "Weather_smoke_fume","Ambiguous_smoke_fume",
                                          "Company Policy_smoke_fume","Incorrect / Not Installed / Unavailable Part_smoke_fume")
# Print the updated problem_counts_filtered_df
print(problem_counts_filtered_df)

# # Find the count of each unique value in a column and rank them by frequency
# # 'Primary Problem' is the column of interest in 'filtered_data'
# problem_counts_filtered <- table(filtered_data$`Primary Problem`)
# # Convert the result to a data frame
# problem_counts_df_filtered <- data.frame(Problem = names(problem_counts_filtered), Count = as.numeric(problem_counts_filtered))
# # Order the data frame by count in descending order
# problem_counts_df_filtered <- problem_counts_df_filtered[order(-problem_counts_df_filtered$Count), ]
# # Display the data frame
# print(problem_counts_df_filtered)

# problem_counts_df and problem_counts_df_filtered are the dataframes
# Perform a full outer join and fill missing values with 0
merged_df <- merge(
  x = problem_counts_df,
  y = problem_counts_filtered_df,
  by = "Date",
  all = TRUE
)
# Display the merged dataframe
print(merged_df)


# 
# # 'filtered_data' is your dataframe
# for (col in names(filtered_data)[5:33]) {
#   # Display the unique values for each column
#   cat("Unique values in column", col, ":\n")
#   unique_values <- unique(filtered_data[[col]])
#   print(unique_values)
#   
#   # Find the count of each unique value in a column and rank them by frequency
#   counts <- table(filtered_data[[col]])
#   
#   # Convert the result to a data frame
#   counts_df <- data.frame(Value = names(counts), Count = as.numeric(counts))
#   
#   # Order the data frame by count in descending order
#   counts_df <- counts_df[order(-counts_df$Count), ]
#   
#   # Display the data frame
#   cat("\nCounts for column", col, ":\n")
#   print(counts_df)
#   
#   cat("\n------------------------\n")
#   
#   # Save the data frame to a variable with the column name
#   assign(paste0("counts_", col), counts_df)
# }
# 
# # List all the saved dataframes
# # You can access them using counts_ColumnName
# ls(pattern = "counts_")

## Overview - Area Plot
problem_counts_df
problem_counts_filtered_df

# Assuming data is stored in a matrix named "data_matrix"
data_matrix <- cbind(problem_counts_df$Count, problem_counts_filtered_df$Count_smoke_fume)
# Calculate the percentage
percentage <- round(((data_matrix[, 2] / data_matrix[, 1]) * 100),0)
# Print the result
print(percentage)

# Melt the data frame to long format for ggplot
library(reshape2)
melted_df <- melt(merged_df, id.vars = "Date", measure.vars = c("Count", "Count_smoke_fume"))
# Convert Date to a factor for proper x-axis order
melted_df$Date <- as.factor(melted_df$Date)

# Plot the line graph with area
overall_incidents <-  ggplot(melted_df, aes(x = Date, y = value, color = variable, group = variable)) +
                        geom_line() +
                        geom_ribbon(data = subset(melted_df, variable == "Count_smoke_fume"),
                                    aes(ymin = 0, ymax = value),
                                    fill = alpha("#cc0000", 0.3), color = NA) +
                        geom_ribbon(data = subset(melted_df, variable == "Count"),
                                    aes(ymin = 0, ymax = value),
                                    fill = alpha("#f89649", 0.3), color = NA) +
                        geom_point() +
                        labs(title = "Overall and Smoke/Fume related flight incidents per month in 2022",
                             y = "The Number of Flight Incidents",
                             x = "Month") +
                        scale_color_manual(values = c("Count" = "#f89649", "Count_smoke_fume" = "#cc0000"),
                                           labels = c("Count" = "Overall", "Count_smoke_fume" = "Smoke & Fume related")) +
                        scale_x_discrete(breaks = unique(melted_df$Date), labels = month.abb, expand = c(0.01, 0.01)) +
                        scale_y_continuous(breaks = seq(0, max(melted_df$value), 30), expand = c(0.01, 0.01)) +
                        labs(color = "Incidents:") +  # Modify the legend title here
                        theme(axis.text.y = element_text(hjust = 1, size=rel(1.1)),
                              axis.text.x = element_text(angle = 45,hjust = 1, size=rel(1.1)),
                              plot.title=element_text(size=rel(1.1), face="bold", color="black", hjust = 0.5),
                              axis.title.x = element_text(size = rel(1.1), face = "bold"),
                              axis.title.y = element_text(size = rel(1.1), face = "bold"),
                              legend.position = "top",
                              legend.title = element_text(size = rel(1),face = "bold"),
                              legend.text = element_text(size = rel(1)),
                              legend.margin = margin(t = 0, unit = "cm"),
                              plot.margin = unit(c(0.5, 0.5, 0, 0.5), "cm")) +
                        theme(panel.background = element_blank(),
                              panel.border = element_blank(),
                              plot.margin = unit(c(0.15, 0.25, 0.15, 0.15), "cm"),
                              panel.grid.minor = element_blank())
ggsave(filename = file.path(paste0(pati_main,"/", "overall_incidents_2022.pdf"))
       , plot=overall_incidents, device="pdf", width = 20, height = 12, units = "cm", dpi = 300)
ggsave(filename = file.path(paste0(pati_main,"/", "overall_incidents_2022.png"))
       , plot=overall_incidents, device="png", width = 20, height = 12, units = "cm", dpi = 300)
overall_incidents
graphics.off()


## Barplot - Aircraft type
# Melt the data frame to long format for ggplot barplot
melted_df_bar <- melt(problem_counts_filtered_df, id.vars = "Date", 
                      measure.vars = c("A319","A320","A321","Airbus 318/319/320/321 Undifferentiated",
                                       "B737 Undifferentiated or Other Model","B757 Undifferentiated or Other Model",
                                       "Commercial Fixed Wing","EMB ERJ 170/175 ER/LR","Regional Jet 200 ER/LR (CRJ200)",
                                       "EMB ERJ 145 ER/LR","Other","EMB ERJ 190/195 ER/LR","Regional Jet 900 (CRJ900)",
                                       "Regional Jet 700 ER/LR (CRJ700)","B787 Dreamliner Undifferentiated or Other Model",
                                       "B767 Undifferentiated or Other Model","Regional Jet CL65, Undifferentiated or Other Model"))

# Convert Date to a factor for proper x-axis order
melted_df_bar$Date <- as.factor(melted_df_bar$Date)
colors_bar <- c("#1f78b4", # Blue
            "#33a02c", # Green
            "#e31a1c", # Red
            "#ff7f00", # Orange
            "#6a3d9a", # Purple
            "#b15928", # Brown
            "#a6cee3", # Light Blue
            "#b2df8a", # Light Green
            "#fb9a99", # Light Red
            "#fdbf6f", # Light Orange
            "#cab2d6", # Light Purple
            "#ff7f7f", # Light Brown
            "#ccebc5", # Pale Green
            "#fdbf85", # Pale Orange
            "#8dd3c7", # Turquoise
            "#ffffb3", # Pale Yellow
            "#bebada", # Light Purple-Blue
            "#fb8072")  # Salmon Pink

p5a <- ggplot(melted_df_bar, aes(x = Date, y = value, fill = variable)) +
          geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
          labs(title = "Bar plot representation of smoke or fume related incidents per month and aircraft type in 2022.",
               y = "Count",
               x = "Month",
               fill = "Aircraft type",
               color = "Incidents:") +
          scale_x_discrete(breaks = unique(melted_df_bar$Date), labels = month.abb, expand = c(0.01, 0.01)) +
          scale_y_continuous(breaks = seq(0,50,2), expand = c(0.01, 0.01)) +
          theme_minimal() +
          theme(axis.text.y = element_text(hjust = 1, size=rel(1.1)),
                axis.text.x = element_text(angle = 45,hjust = 1, size=rel(1.1)),
                plot.title=element_text(size=rel(1.1), face="bold", color="black", hjust = 0.5),
                axis.title.x = element_text(size = rel(1.1), face = "bold"),
                axis.title.y = element_text(size = rel(1.1), face = "bold"),
                legend.position = "right",
                legend.box ="vertical",
                legend.title = element_text(size = rel(1),face = "bold"),
                legend.text = element_text(size = rel(0.8)),
                legend.margin = margin(t = 0, unit = "cm"),
                plot.margin = unit(c(0.5, 0.5, 0, 0.5), "cm")) +
          theme(panel.background = element_blank(),
                panel.border = element_blank(),
                plot.margin = unit(c(0.15, 0.25, 0.15, 0.15), "cm"),
                panel.grid.minor = element_blank())+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5a



p5b <- ggplot(melted_df_bar, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "identity") +
  labs(title = "Bar plot representation of smoke or fume related incidents per aircraft type in 2022.",
       y = "Count",
       x = "Aircraft Type",
       fill = "Variable") +
  scale_x_discrete(breaks = unique(melted_df_bar$variable), labels = c("A319","A320","A321","Airbus Undifferentiated",
                                                                       "B737 Undifferentiated","B757 Undifferentiated",
                                                                       "Commercial Fixed Wing","EMB ERJ 170/175 ER/LR",
                                                                       "Regional Jet 200 ER/LR","EMB ERJ 145 ER/LR",
                                                                       "Other","EMB ERJ 190/195 ER/LR",
                                                                       "Regional Jet 900","Regional Jet 700 ER/LR",
                                                                       "B787","B767","Regional Jet CL65")) +
  scale_y_continuous(breaks = seq(0, 18, 2), expand = c(0.01, 0.01)) +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1, size=rel(1)),
        axis.text.x = element_text(angle = 45, hjust = 1, size=rel(1)),
        plot.title=element_text(size=rel(1.1), face="bold", color="black", hjust = 0.5),
        axis.title.x = element_text(size = rel(1.1), face = "bold"),
        axis.title.y = element_text(size = rel(1.1), face = "bold"),
        legend.position = "none",
        legend.title = element_text(size = rel(1),face = "bold"),
        legend.text = element_text(size = rel(1)),
        legend.margin = margin(t = 0, unit = "cm"),
        plot.margin = unit(c(0.5, 0.5, 0, 0.5), "cm")) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.15, 0.25, 0.15, 0.15), "cm"),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5b



##Circular Barplot
# library
library(tidyverse)

# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(1,18), 60, replace=T)
)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(melted_df_bar$Date), ncol(melted_df_bar)) )
colnames(to_add) <- colnames(melted_df_bar)
to_add$Date <- rep(levels(melted_df_bar$Date), each=empty_bar)
melted_df_bar <- rbind(melted_df_bar, to_add)
melted_df_bar <- melted_df_bar %>% arrange(Date)
melted_df_bar$id <- seq(1, nrow(melted_df_bar))

# Get the name and the y position of each label
label_melted_df_bar <- melted_df_bar
number_of_bar <- nrow(label_melted_df_bar)
angle <- 90 - 360 * (label_melted_df_bar$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_melted_df_bar$hjust <- ifelse( angle < -90, 1, 0)
label_melted_df_bar$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_melted_df_bar <- melted_df_bar %>% 
  group_by(Date) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_melted_df_bar <- base_melted_df_bar
grid_melted_df_bar$end <- grid_melted_df_bar$end[ c( nrow(grid_melted_df_bar), 1:nrow(grid_melted_df_bar)-1)] + 1
grid_melted_df_bar$start <- grid_melted_df_bar$start - 1
grid_melted_df_bar <- grid_melted_df_bar[-1,]

# Make the plot
p <- ggplot(melted_df_bar, aes(x = as.factor(id), y = value, fill = Date)) +
  geom_bar(aes(x = as.factor(id), y = value, fill = Date), stat = "identity", alpha = 0.7) +
  geom_segment(data = grid_melted_df_bar, aes(x = end, y = 5, xend = start, yend = 5), color = "grey", alpha = 0.8, size = 0.3, inherit.aes = FALSE) +
  geom_segment(data = grid_melted_df_bar, aes(x = end, y = 10, xend = start, yend = 10), color = "grey", alpha = 0.8, size = 0.3, inherit.aes = FALSE) +
  geom_segment(data = grid_melted_df_bar, aes(x = end, y = 15, xend = start, yend = 15), color = "grey", alpha = 0.8, size = 0.3, inherit.aes = FALSE) +
  #annotate("text", x = rep(max(melted_df_bar$id), 3), y = c(15, 10, 5), label = c("15", "10", "5"), color = "grey", size = 3, angle = 0, fontface = "bold", hjust = 1) +
  coord_polar() +
  ylim(-25, 30) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  geom_text(data = label_melted_df_bar, aes(x = id, y = value + 7, label = variable, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 1.5, angle = label_melted_df_bar$angle, inherit.aes = FALSE) +
  geom_text(data = base_melted_df_bar, aes(x = title, y = -10, label = Date), color = "black", alpha = 0.8, size = 3, inherit.aes = FALSE)
p
graphics.off()

#### END
