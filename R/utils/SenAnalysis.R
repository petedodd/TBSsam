
### Sensitivity analysis ###


# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)


## Copy tICERtable.csv as our baseline ICER data

# Define the path of the source file (the file you want to copy)
source_file <- "C:/Users/md21/Dropbox/UBx/TB-SPEED/Modelling/O3SAM/TBSsam/data/tICERtable.csv"

# Read the CSV file into R
data <- read.csv(source_file)

# Remove the first column
data <- data %>% select(-1)  # Remove the first column using dplyr's select

# Define the destination file names
destination_file_LQ <- "C:/Users/md21/Dropbox/UBx/TB-SPEED/Modelling/O3SAM/TBSsam/data/SA/tICERtable_baseline_LQ.csv"
destination_file_UQ <- "C:/Users/md21/Dropbox/UBx/TB-SPEED/Modelling/O3SAM/TBSsam/data/SA/tICERtable_baseline_UQ.csv"

# Save the data to the new files without the first column
write.csv(data, destination_file_LQ, row.names = FALSE)
write.csv(data, destination_file_UQ, row.names = FALSE)

# Confirm the files are saved and the first column is removed
if (file.exists(destination_file_LQ) & file.exists(destination_file_UQ)) {
  print("Files successfully copied, first column removed, and renamed to tICERtable_baseline_LQ.csv and tICERtable_baseline_UQ.csv!")
} else {
  print("File processing failed.")
}


# Set working directory to your specified path
setwd("C:/Users/md21/Dropbox/UBx/TB-SPEED/Modelling/O3SAM/TBSsam/data/SA")

# List all CSV files that contain "tICERtable_" in their name
csv_files <- list.files(pattern = "tICERtable_.*\\.csv")

# Your predefined output types
output_types <- c("100x ATT per child, SOC", "100x ATT per child, WHO", "100x ATT per child, TBS1", "100x ATT per child, TBS2",
                  "% FP, SOC", "% FP, WHO", "% FP, TBS1", "% FP, TBS2", "% FN, SOC", "% FN, WHO", "% FN, TBS1", "% FN, TBS2",
                  "100x incremental ATT, WHO v SOC", "100x incremental ATT, TBS1 v SOC", "100x incremental ATT, TBS2 v SOC",
                  "100x incremental ATT, TBS1 v WHO", "100x incremental ATT, TBS2 v WHO", "100x incremental ATT, TBS2 v TBS1",
                  "100x deaths per child, SOC", "100x deaths per child, WHO", "100x deaths per child, TBS1", "100x deaths per child, TBS2",
                  "100x incremental deaths, WHO v SOC", "100x incremental deaths, TBS1 v SOC", "100x incremental deaths, TBS2 v SOC",
                  "100x incremental deaths, TBS1 v WHO", "100x incremental deaths, TBS2 v WHO", "100x incremental deaths, TBS2 v TBS1",
                  "100x undiscounted LYS, WHO v SOC", "100x undiscounted LYS, TBS1 v SOC", "100x undiscounted LYS, TBS2 v SOC",
                  "100x undiscounted LYS, TBS1 v WHO", "100x undiscounted LYS, TBS2 v WHO", "100x undiscounted LYS, TBS2 v TBS1",
                  "100x DALYs averted, WHO v SOC", "100x DALYs averted, TBS1 v SOC", "100x DALYs averted, TBS2 v SOC",
                  "100x DALYs averted, TBS1 v WHO", "100x DALYs averted, TBS2 v WHO", "100x DALYs averted, TBS2 v TBS1",
                  "cost per child, SOC", "cost per child, WHO", "cost per child, TBS1", "cost per child, TBS2",
                  "incremental cost, WHO v SOC", "incremental cost, TBS1 v SOC", "incremental cost, TBS2 v SOC",
                  "incremental cost, TBS1 v WHO", "incremental cost, TBS2 v WHO", "incremental cost, TBS2 v TBS1",
                  "ICER (no discounting), WHO v SOC", "ICER (no discounting), TBS1 v SOC", "ICER (no discounting), TBS2 v SOC",
                  "ICER (no discounting), TBS1 v WHO", "ICER (no discounting), TBS2 v WHO", "ICER (no discounting), TBS2 v TBS1",
                  "ICER, WHO v SOC", "ICER, TBS1 v SOC", "ICER, TBS2 v SOC", "ICER, TBS1 v WHO", "ICER, TBS2 v WHO", "ICER, TBS2 v TBS1")

# Create a data frame for output types
output_types_df <- data.frame(output_types = output_types)

# Check the number of rows in output types
num_output_rows <- nrow(output_types_df)

# Initialize an empty list to store the data frames
csv_list <- list()

# Read each CSV file and merge with the corresponding output type
for (file in csv_files) {
  data <- read.csv(file)  # Read the CSV file
  
  # Ensure all CSV files have the same number of rows
  if (nrow(data) != num_output_rows) {
    stop(paste("Mismatch in row count for file:", file))
  }
  
  # Add the corresponding output type column
  data$output_type <- output_types_df$output_types
  
  # Add the file name as a new column
  data$file_name <- file  # Store the file name in a new column
  
  # Decompose the file name to create 'parm' and 'quartile'
  split_name <- strsplit(file, "_")[[1]]  # Split the file name by "_"
  
  # Extract 'parm' and 'quartile'
  data$parm <- paste(split_name[-c(1, length(split_name))], collapse = ".")  # Join all but the first and last parts
  data$quartile <- gsub("\\.csv", "", split_name[length(split_name)])  # Remove ".csv" from the last part
  
  # Append the data frame to the list
  csv_list[[file]] <- data
}

# Merge all data frames row-wise
merged_data <- do.call(rbind, csv_list)

# Remove specified columns from merged data
columns_to_remove <- c("Cambodia", "Cameroon", "Côte.d.Ivoire", "Mozambique", "Sierra.Leone")
merged_data <- merged_data[, !(names(merged_data) %in% columns_to_remove)]

# View the merged data
head(merged_data)

# Write the merged data to a new CSV file
write.csv(merged_data, "SAnoncosts_merged_output.csv", row.names = FALSE)


## Filter ICER data

# Read the merged data from the CSV file
merged_data <- read.csv("SAnoncosts_merged_output.csv")

# Filter the data to only keep the specified output_type values
filtered_data <- merged_data %>%
  filter(output_type %in% c('ICER, WHO v SOC', 'ICER, TBS1 v SOC', 'ICER, TBS2 v SOC'))

# View the filtered data
head(filtered_data)

# Optionally, write the filtered data to a new CSV file
write.csv(filtered_data, "SAnoncosts_ICER_output.csv", row.names = FALSE)


## Reshape 

# Read the merged data from the CSV file
filtered_data <- read.csv("SAnoncosts_ICER_output.csv")

# Reshape the data for Uganda and Zambia based on the quartile
reshaped_data <- filtered_data %>%
  # Select relevant columns
  select(parm, quartile, Uganda, Zambia, output_type) %>%
  # Pivot the data wider to create separate columns for LQ and UQ
  pivot_wider(
    names_from = quartile,  # Use the 'quartile' values (LQ, UQ) to create new columns
    values_from = c(Uganda, Zambia),  # Spread values from Uganda and Zambia columns
    names_glue = "{.value}_{quartile}"  # Custom column names Uganda_LQ, Uganda_UQ, etc.
  )

# View the reshaped data
head(reshaped_data)

# Optionally, write the reshaped data to a new CSV file
write.csv(reshaped_data, "SAnoncosts_ICER_reshaped.csv", row.names = FALSE)


## prepare dataset for tornado graph 


# Read the CSV file into R
data <- read.csv("SAnoncosts_ICER_reshaped.csv")

# Rename the values in the 'output_type' column
data <- data %>%
  mutate(output_type = recode(output_type,
                              'ICER, WHO v SOC' = 'WHO',
                              'ICER, TBS1 v SOC' = 'TBS1',
                              'ICER, TBS2 v SOC' = 'TBS2'))

# Create Uganda_mean and Zambia_mean as the mean of LQ and UQ values
data <- data %>%
  mutate(Uganda_mean = (Uganda_LQ + Uganda_UQ) / 2,
         Zambia_mean = (Zambia_LQ + Zambia_UQ) / 2)


# Identify the baseline values for Uganda and Zambia
baseline_Uganda <- subset(data, parm == "baseline")$Uganda_mean
baseline_Zambia <- subset(data, parm == "baseline")$Zambia_mean


# Merge baseline values with the main data based on output_type
data <- merge(data, subset(data, parm == "baseline")[, c("output_type", "Uganda_mean", "Zambia_mean")], 
              by = "output_type", suffixes = c("", "_baseline"))

# Calculate the difference between mean and baseline for Uganda and Zambia
data <- data %>%
  mutate(
    Uganda_diff = Uganda_mean - Uganda_mean_baseline,
    Zambia_diff = Zambia_mean - Zambia_mean_baseline
  )

# Calculate the effect sizes for Uganda and Zambia
data <- data %>%
  mutate(
    Uganda_effect_size = abs(Uganda_UQ - Uganda_LQ),
    Zambia_effect_size = abs(Zambia_UQ - Zambia_LQ)
  )

# View the updated data
head(data)

# Optionally, save the modified data to a new CSV file
write.csv(data, "SAnoncosts_ICER_reshaped_ready.csv", row.names = FALSE)


## create the tornado diagram

# Load the data
data <- read.csv("SAnoncosts_ICER_reshaped_ready.csv")

# List of parameters to exclude for each output_type
exclusions <- list(
  "WHO" = c("baseline", "c.s.tbs1step.diag.clin", "c.s.tbs1step.diag.test", "c.s.tbs2step.diag", "c.s.tbs2step.scre"), # Exclude "parm1" and "parm2" for output_type_1
  "TBS1" = c("baseline", "c.s.who.scre", "c.s.who.hiv.diag", "c.s.who.diag", "c.s.tbs2step.diag", "c.s.tbs2step.scre"), 
  "TBS2" = c("baseline", "c.s.who.scre", "c.s.who.hiv.diag", "c.s.who.diag", "c.s.tbs1step.diag.clin", "c.s.tbs1step.diag.test") 
)

# Function to create tornado diagram for a given country and output_type
create_tornado_plot <- function(df, country_prefix, country_name, output_type_value) {
  # Filter the data for the specific output_type
  df_plot <- df %>%
    filter(output_type == output_type_value) %>%
    # Exclude specific parameters based on the output_type
    filter(!(parm %in% exclusions[[output_type_value]])) %>%
    mutate(LQ = get(paste0(country_prefix, "_LQ")),
           UQ = get(paste0(country_prefix, "_UQ")),
           baseline = get(paste0(country_prefix, "_mean_baseline")),
           range_width = abs(UQ - LQ)) %>% # Calculate the range width
    arrange(range_width) %>% # Sort by range width in descending order
    mutate(parm = factor(parm, levels = parm)) # Reorder the factor levels for parm
  
  # Plot tornado diagram 
  p <- ggplot(df_plot, aes(x = parm)) +
    geom_segment(aes(x = parm, xend = parm, y = LQ, yend = baseline, color = "Lower quartile"), size = 2) +
    geom_segment(aes(x = parm, xend = parm, y = baseline, yend = UQ, color = "Upper quartile"), size = 2) +
    geom_hline(aes(yintercept = baseline), linetype = "dashed", color = "red") +
    annotate("text", x = 1, y = unique(df_plot$baseline), label = "Baseline ICER", color = "red", hjust = -0.1, size = 3) +
    coord_flip() +
    scale_color_manual(values = c("Lower quartile" = "rosybrown1", "Upper quartile" = "rosybrown4")) +
    labs(title = paste("Tornado Diagram for", country_name, "-", output_type_value, "vs SOC"),
         x = "Parameters",
         y = "ICER",
         color = "Legend") + # Add legend title
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.background = element_rect(fill = "white", color = NA), # Set panel background to white
      plot.background = element_rect(fill = "white", color = NA),  # Set plot background to white
      panel.grid.major = element_line(color = "gray90"), # Optional: lighten the grid lines
      panel.grid.minor = element_line(color = "gray95")  # Optional: lighten minor grid lines
    )
  
  return(p)
}

# Get unique output_types
output_types <- unique(data$output_type)

# Create tornado diagrams for each output_type for Uganda and Zambia
plots <- list()
for (output in output_types) {
  uganda_plot <- create_tornado_plot(data, "Uganda", "Uganda", output)
  zambia_plot <- create_tornado_plot(data, "Zambia", "Zambia", output)
  
  # Store plots in a list
  plots[[paste("SA_Uganda", output)]] <- uganda_plot
  plots[[paste("SA_Zambia", output)]] <- zambia_plot
}

# Display and save the plots
for (plot_name in names(plots)) {
  # Display the plot
  print(plots[[plot_name]])
  
  # Save the plot
  ggsave(
    filename = paste0(plot_name, ".png"), 
    plot = plots[[plot_name]], 
    width = 15, 
    height = 10, 
    dpi = 300
  )
}


