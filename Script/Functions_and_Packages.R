##### Packages ####
if (!requireNamespace("needs", quietly = TRUE)) {
  install.packages("needs")
}
library(needs)
needs(xfun, tidyverse, psych, remotes, Hmisc, flextable, gtsummary, cardx, svglite, pwr, pastecs, nlme, performance, lmerTest, HLMdiag, lmtest, jtools, mice, lattice, huxtable, broom.mixed, patchwork, sjPlot)
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
            as.character(.[[column_name]])  # Keep the rest as characters
          )
        )
      )
    )
}


#df <- handle_hyphen(df, "WeeklyKM_base") # example use
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
  library(officer)
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
  means <- colMeans(df, na.rm = T) %>% round(2)
  sds <- apply(df, 2, sd, na.rm = T) %>% round(2)# 2 stands for "colums" here
  
  # Create data frame
  correlation_df <- data.frame(Measure = display_names, Mean = means,SD = sds, correlation_matrix_clean)
  
  colnames(correlation_df)[4:ncol(correlation_df)] <- as.character(1:ncol(correlation_matrix_clean))
  
  # Create flextable
  flextable(correlation_df) %>%
    set_header_labels(
      Measure = "Measure", 
      Mean = "Mean", 
      SD = "SD"
    ) %>%
    add_header_row(
      values = c("", "Descriptive Statistics", "Correlations"), 
      colwidths = c(1, 2, ncol(correlation_matrix_clean))
    ) %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::autofit() %>%
    flextable::bold(part = "header") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 12, part = "all") %>%
    flextable::padding(padding.top = 3, padding.bottom = 3, part = "all") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = fp_border(width = 1.5), part = "header") %>%
    flextable::hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
    flextable::hline(border = fp_border(width = 1), part = "header")
}


generate_correlation_table2 <- function(df, display_names) {
  library(Hmisc)
  library(flextable)
  library(officer)
  
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
  
  # Create data frame
  correlation_df <- data.frame(Measure = display_names, correlation_matrix_clean)
  
  colnames(correlation_df)[2:ncol(correlation_df)] <- as.character(1:ncol(correlation_matrix_clean))
  
  # Create flextable
  flextable(correlation_df) %>%
    set_header_labels(
      Measure = "Measure"
    ) %>%
    add_header_row(
      values = c("", "Correlations"), 
      colwidths = c(1, ncol(correlation_matrix_clean))
    ) %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::autofit() %>%
    flextable::bold(part = "header") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 12, part = "all") %>%
    flextable::padding(padding.top = 3, padding.bottom = 3, part = "all") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = fp_border(width = 1.5), part = "header") %>%
    flextable::hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
    flextable::hline(border = fp_border(width = 1), part = "header")
}

## Example usage
#correlation_names <- c("Age", "Gender","Weekly Kilometers")
#x<-generate_correlation_table(df[,c("Age","Gender","WeeklyKM_base")], correlation_names)
#### Generate mean values for values wit 

# Define the function
#df$mean_goals <- rowMeans(df[, grepl("goal", names(df),ignore.case = T)], na.rm = TRUE)
mean_by_pattern<-function(df,searchstring){
  new_var <- rowMeans (df[,grepl(searchstring, names (df), ignore.case = T)], na.rm = T)
  return(new_var)
}
#df$meannew<-mean_by_pattern(df,"goal") #example use
####Descriptives-Funktion: Berechnet Typische deskriptive Werte für alle Variablen eines gegebenen Datensatzes: ######
#Calculate mean, sd, range, min, max of all variables. 
library(dplyr)

mean_sd_median_min_max <- function(df) {
  result <- df %>%
    # Select only numeric columns
    select(where(is.numeric)) %>%
    # Summarise with the desired statistics
    summarise(across(everything(), 
                     list(mean = ~round(mean(., na.rm = TRUE), digits = 2), 
                          sd = ~round(sd(., na.rm = TRUE), digits = 2),
                          median = ~round(median(., na.rm = TRUE), digits = 2),
                          min = ~min(., na.rm = TRUE),
                          max = ~max(., na.rm = TRUE))))
  
  # Create named list
  result_list <- setNames(as.list(result), paste(names(result), sep = ""))
  
  return(result_list)
}

#### Return all variables that are not normally distribute in the dataset####
which_var_not_normal<- function(df) {
  names<-df %>% select(where(is.numeric)) %>% stat.desc (basic=F, norm=T) %>% as.data.frame() %>%.["normtest.p",] %>%   .[, . < 0.05 ] %>% names()
  return(names)
}

which_var_not_normal_p<- function(df) {
  names<-df %>% select(where(is.numeric)) %>% stat.desc (basic=F, norm=T) %>% as.data.frame() %>%.["normtest.p",] %>%   .[, . < 0.05 ] %>% names()
  not_normal_data<-df[,names]  %>% stat.desc (basic=F, norm=T) %>% as.data.frame() %>%.["normtest.p",]
  return(not_normal_data)
}

##### Show Histograms of all variables #####
print_all_histograms <- function(df, bins_n=20) {
  df_long <- df %>%
    pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") %>% filter(!is.na(value))
  
  plot<- ggplot(df_long, aes(value)) +
    geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white", bins = bins_n) +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(guide = "none") +
    facet_wrap(~variable, scales = "free") + # Create separate panels for each variable
    stat_function(fun = dnorm,
                  args = list(mean = mean(df_long$value, na.rm = TRUE),
                              sd = sd(df_long$value, na.rm = TRUE)),
                  colour = "black", linewidth = 1)
  
  print (plot)
}


#### Print violin Boxplots####
print_all_violin_boxplots <- function(df, group_col = NULL, dodge_width = 1, facet_nrow = 2, facet_ncol = NULL, point_jitter = 0.1, custom_labels = NULL) {
  # Ensure the required libraries are loaded
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Convert the data to a long format, keeping only numeric columns
  df_long <- df %>%
    pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))
  
  # Preserve the original order of variables
  variable_order <- colnames(df)[sapply(df, is.numeric)]
  df_long <- df_long %>%
    mutate(variable = factor(variable, levels = variable_order))
  
  # Add group column to the long format if provided
  if (!is.null(group_col)) {
    df_long <- df_long %>%
      mutate(Group = as.factor(df[[group_col]]))
  } else {
    df_long$Group <- "1" # Default group if no grouping is provided
  }
  
  # Create a named vector for custom labels if provided
  if (!is.null(custom_labels)) {
    label_mapping <- custom_labels
  } else {
    label_mapping <- setNames(unique(df_long$variable), unique(df_long$variable)) # Default to current names
  }
  
  # Create the plot
  plot <- ggplot(df_long, aes(x = variable, y = value, fill = Group)) +
    # Violin plot
    geom_violin(aes(fill = Group), linewidth = 1, color = "black", 
                show.legend = FALSE, position = position_dodge(width = dodge_width)) +
    # Boxplot
    geom_boxplot(aes(fill = Group), outlier.size = 2, outlier.shape = 18, outlier.colour = "blue", 
                 width = 0.1, position = position_dodge(width = dodge_width), show.legend = FALSE) +
    # Raw data points with horizontal jitter
    geom_point(position = position_jitter(width = point_jitter, height = 0), 
               size = 1.5, alpha = 0.6, aes(color = Group), show.legend = FALSE) +
    # Summary mean points
    stat_summary(mapping = aes(color = Group), fun = mean, geom = "point", shape = 4, size = 3, 
                 position = position_dodge(width = dodge_width), show.legend = FALSE) +
    # Custom scales
    scale_color_manual(values = c("black", "black")) +
    scale_fill_manual(values = c("1" = "white", "2" = "grey"),
                      labels = c("1" = "Group 1", "2" = "Group 2"),
                      name = "Group") +
    # Theme settings
    theme_classic(base_size = 14, base_family = "sans") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    # Faceting with custom labels
    facet_wrap(~variable, scales = "free", as.table = TRUE, nrow = facet_nrow, ncol = facet_ncol,
               labeller = labeller(variable = label_mapping))
  
  # Print the plot
  print(plot)
}

#### function to format gt tables apa style####
apa <- function(x) {
  gt(x) %>%
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 2,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) %>%
    cols_align(align="center") %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    )  %>%
    opt_table_font(
      font = list(
        "Times New Roman",
        "serif" # Fallback font
      )
    )
}

##Generate a talbe with descriptives

get_descriptive_table <- function(df, language = "German") {
  library(dplyr)
  library(pastecs)
  
  # Compute statistics
  df_stat <- df %>% 
    stat.desc(basic = FALSE, norm = TRUE) %>% 
    t() %>% 
    as.data.frame() %>% 
    select(-var, -coef.var, -SE.mean, -kurt.2SE, -normtest.W, -skew.2SE)
  
  # Adjust normtest.p formatting
  df_stat <- df_stat %>%
    mutate(
      normtest.p = ifelse(
        normtest.p < 0.001,
        "< .001",
        as.character(round(normtest.p, 3))
      )
    )
  
  # Round all numeric values
  df_stat <- df_stat %>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  # Add rownames as a variable
  df_stat <- df_stat %>%
    mutate(Variable = rownames(df_stat)) %>%
    select(Variable, everything())
  
  # Rename columns based on language
  if (language == "German") {
    df_stat <- df_stat %>% 
      rename(
        Median = median,
        Schiefe = skewness,
        Exzess = kurtosis,
        Mittelwert = mean,
        "95% KI" = CI.mean.0.95,
        "SD" = std.dev,
        "p-Wert " = normtest.p
      )
  } else if (language == "English") {
    df_stat <- df_stat %>% 
      rename(
        Median = median,
        Skewness = skewness,
        Kurtosis = kurtosis,
        Mean = mean,
        "95% CI" = CI.mean.0.95,
        "SD" = std.dev,
        "p-Value" = normtest.p
      )
  } else {
    stop("Unsupported language. Please choose either 'German' or 'English'.")
  }
  return(df_stat)}