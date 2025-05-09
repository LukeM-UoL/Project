# Install required packages if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(readxl)
library(ggplot2)

# Step 1: Load and Prepare the Data
# Load the TSV file
data_prev <- read.delim("V2_250.Pseudomonas_prev_tidied.3.tsv", sep = "\t")

# Load the Excel file
metadata <- read_excel("cipro_metadata.xlsx")

# Normalize the sample names for merging
data_prev$sample <- tolower(gsub("_", "-", data_prev$sample))  # e.g., t88_d113 -> t88-d113
metadata$`Laboratory name` <- tolower(metadata$`Laboratory name`)  # e.g., T88-D113 -> t88-d113

# Merge the datasets
data_merged <- merge(data_prev, metadata, by.x = "sample", by.y = "Laboratory name")

# Clean the data
# Convert DNA concentration to numeric, handling "NA" values
data_merged$`DNA concentration (ng/ul, nanodrop)` <- as.numeric(
  ifelse(data_merged$`DNA concentration (ng/ul, nanodrop)` == "NA", 
         NA, 
         data_merged$`DNA concentration (ng/ul, nanodrop)`)
)

# Standardize Visit names
data_merged$Visit <- gsub("V14/ETD", "V14-D", data_merged$Visit)
data_merged$Visit <- gsub("SCREENRTST", "SCREENING", data_merged$Visit)

# Order Visit levels chronologically
visit_order <- c("SCREENING", "V1DAY1", "V2DAY14", "V3DAY28", "V4DAY57", "V5DAY84", 
                 "V6DAY113", "V9DAY196", "V10DAY225", "V13DAY308", "V14-D337", "V15DAY364")
data_merged$Visit <- factor(data_merged$Visit, levels = visit_order)

# Convert Patient number to factor for grouping
data_merged$`Patient number` <- as.factor(data_merged$`Patient number`)

# Step 2: Generate Plots with Lighter Background
# 2.1 Boxplot: Prevalence by Visit
p1 <- ggplot(data_merged, aes(x = Visit, y = prevalence, fill = Visit)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Pseudomonas Prevalence Across Visits",
       x = "Visit",
       y = "Prevalence") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        panel.background = element_rect(fill = "#F5F5F5"),  # Light gray panel background
        plot.background = element_rect(fill = "#FFFFFF")) +  # White plot background
  scale_fill_brewer(palette = "Purples")

ggsave("prevalence_by_visit_boxplot.png", plot = p1, width = 10, height = 6, dpi = 300)

# 2.2 Boxplot: Prevalence by Patient (Top 10 patients)
top_patients <- names(sort(table(data_merged$`Patient number`), decreasing = TRUE))[1:10]
data_top_patients <- data_merged[data_merged$`Patient number` %in% top_patients, ]

p2 <- ggplot(data_top_patients, aes(x = `Patient number`, y = prevalence, fill = `Patient number`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Pseudomonas Prevalence by Patient (Top 10)",
       x = "Patient Number",
       y = "Prevalence") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "none",
        panel.background = element_rect(fill = "#F5F5F5"),  # Light gray panel background
        plot.background = element_rect(fill = "#FFFFFF")) +  # White plot background
  scale_fill_brewer(palette = "Purples")

ggsave("prevalence_by_patient_boxplot.png", plot = p2, width = 8, height = 6, dpi = 300)

# 2.3 Scatterplot: Prevalence vs. DNA Concentration
p3 <- ggplot(data_merged, aes(x = `DNA concentration (ng/ul, nanodrop)`, y = prevalence)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  theme_minimal() +
  labs(title = "Prevalence vs. DNA Concentration",
       x = "DNA Concentration (ng/ul, Nanodrop)",
       y = "Prevalence") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        panel.background = element_rect(fill = "#F5F5F5"),  # Light gray panel background
        plot.background = element_rect(fill = "#FFFFFF"))  # White plot background

ggsave("prevalence_vs_dna_concentration.png", plot = p3, width = 8, height = 6, dpi = 300)

