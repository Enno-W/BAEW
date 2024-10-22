##### Packages ####
if (!requireNamespace("needs", quietly = TRUE)) {
  install.packages("needs")
}
library(needs)
needs(xfun, tidyverse)
xfun::install_github("Enno-W/excelbib")
library(excelbib)
# Create .bib file from the excel list
xlsx_to_bib(magic_path("References_BAEW.xlsx"))

#### Average two numbers if there is a hyphen####
handle_hyphen <- function(data, column_name) {
  data %>%
    mutate(
      {{column_name}} := ifelse(
        is.na(.[[column_name]]), 
        NA,  # If the value is NA, keep it as NA
        ifelse(
          grepl("-", .[[column_name]]), 
          sapply(strsplit(.[[column_name]], "-"), function(x) mean(as.numeric(x), na.rm = TRUE)), 
          ifelse(
            .[[column_name]] == "", NA,  # Handle empty strings explicitly
            as.character(.[[column_name]])  # Convert other values to numeric, suppressing warnings
          )
        )
      )
    )
}


df <- handle_hyphen(df, "WeeklyKM_base") # example use
#### Group similar words in a character variable ####
# Define the function
replace_patterns <- function(data, column_name, patterns) {
  # Dynamically evaluate the column and apply the replacements
  data %>%
    mutate(
      !!column_name := case_when(
        # Loop through the patterns and replacements
        !!!map2(patterns, names(patterns), function(pattern, replacement) {
          # Create case_when conditions: if the pattern matches, replace it
          grepl(pattern, .[[column_name]], ignore.case = TRUE) ~ replacement
        }),
        # Add a fallback to keep original values if no pattern matches
        TRUE ~ .[[column_name]]
      )
    )
}

## Example usage
## Define the patterns and their replacements
#patterns <- c(  "Kraftsport" = "kraft",   "Laufen" = "lauf")

## Apply the function to the 'Sport' column
#df <- replace_patterns(df, "Sport", patterns)

# Now df will have the patterns replaced in the 'Sport' column

#### Create Correlation Table #####
generate_correlation_table <- function(df, display_names) {
  library(Hmisc)
  library(flextable)
  # Compute correlation matrix
  correlation_matrix <- rcorr(as.matrix(df))
  correlation_matrix_r <- round(correlation_matrix$r, digits = 2)
  
  # Extract lower triangle of the correlation matrix
  lower_triangle <- correlation_matrix_r[lower.tri(correlation_matrix_r)]
  
  # Create a clean correlation matrix
  correlation_matrix_clean <- matrix(NA, nrow = ncol(correlation_matrix_r), ncol = ncol(correlation_matrix_r))
  correlation_matrix_clean[lower.tri(correlation_matrix_clean)] <- lower_triangle
  
  # Compute significance stars
  stars_matrix <- matrix("", nrow = ncol(correlation_matrix_clean), ncol = ncol(correlation_matrix_clean))
  stars_matrix[correlation_matrix$P < 0.01 & correlation_matrix$P > 0] <- "**"
  stars_matrix[correlation_matrix$P >= 0.01 & correlation_matrix$P < 0.05 & correlation_matrix$P > 0] <- "*"
  
  # Append stars to the lower triangle of the correlation matrix
  correlation_matrix_clean[lower.tri(correlation_matrix_clean)] <- paste(correlation_matrix_clean[lower.tri(correlation_matrix_clean)], stars_matrix[lower.tri(stars_matrix)], sep = "")
  # Compute mean and standard deviation of variables
  means <- colMeans(df, na.rm = T)
  sds <- apply(df, 2, sd, na.rm = T)# 2 stands for "colums" here
  
  # Create data frame
  correlation_df <- data.frame(Measure = display_names, Mean = means,SD = sds, correlation_matrix_clean)
  
  colnames(correlation_df)[4:ncol(correlation_df)] <- as.character(1:ncol(correlation_matrix_clean))
  
  # Create flextable
  flextable(correlation_df) %>%
    theme_apa() %>%
    line_spacing(part = "all") %>%
    padding(padding.top = 5, padding.bottom = 5)
}

## Example usage
#correlation_names <- c("Age", "Gender","Weekly Kilometers")
#x<-generate_correlation_table(df[,c("Age","Gender","WeeklyKM_base")], correlation_names)
#### Generate mean values for values wit 

# Define the function
means_by_patterns <- function(data, column_name, patterns) {
  data %>%
    rowwise() %>%
    mutate(
      # Iterate over patterns to match columns and calculate the mean
      !!column_name := case_when(
        !!!map(patterns, function(pattern) {
          # Select columns that match the pattern
          matching_cols <- select(data, matches(pattern))
          # Calculate the row-wise mean of the matching columns, ignoring NAs
          if (ncol(matching_cols) > 0) {
            return(mean(c_across(matching_cols), na.rm = TRUE))
          } else {
            return(NA_real_)
          }
        }),
        # Fallback if no pattern matches
        TRUE ~ .[[column_name]]
      )
    ) %>%
    ungroup()  # Ungroup after row-wise operation
}
