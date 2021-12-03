## Script to prepare training file for naive bayes classifier
##

# Load packages, set directories
library(tidyverse)

proj_dir <- getwd()
classifier_dir <- file.path(proj_dir, "resources/bayes_classifier")


# Read in the CSVs; convert to input for bayes classifier

# List of csv
csv_list <- list.files(path = classifier_dir, pattern = ".csv", full.names = TRUE)

# Tibble of file information
file_info <- tibble(filename = csv_list) %>%
  mutate(class = str_extract(filename, "_[A-Za-z0-9]*.csv"),
         class = str_remove_all(class, "_|.csv"),
         data = list(NULL))

# Iterate over the files
for (i in seq_along(csv_list)) {
  data <- read_csv(file = csv_list[i]) %>%
    rename(pixel = X1)

  # Find locations of red/green/blue/etc labels
  label_locs <- data$Label
  labels <- c("Red", "Green", "Blue", "(R+G+B)/3", "0.299R+0.587G+0.114B")
  which_labels <- c(0, which(label_locs %in% labels))

  # Add the labels to the column
  label_index <- list()
  for (j in seq_len(length(which_labels) - 1)) {
    data$Label[seq(which_labels[j] + 1, which_labels[j+1])] <- labels[j]
  }

  # Reformat the data
  data1 <- data %>%
    filter(Label %in% c("Red", "Green", "Blue")) %>%
    mutate(Label = fct_inorder(Label)) %>%
    select(Label, Mean) %>%
    split(.$Label) %>%
    map("Mean") %>%
    do.call("cbind", .)

  # Add to the df
  file_info$data[[i]] <- as.data.frame(data1)

}

# Combine the data
pixel_list <- file_info %>%
  unnest(data) %>%
  unite(rgb, Red, Green, Blue, sep = ",") %>%
  select(class,rgb) %>%
  split(.$class) %>%
  map("rgb")

# Select list element with fewest entries
min_length <- min(sapply(pixel_list, length))
pixel_list1 <- lapply(X = pixel_list, "[", seq_len(min_length))


# Save as tsv
write_tsv(x = as.data.frame(pixel_list1), file = file.path(classifier_dir, "bayes_classifier_training.txt"))

