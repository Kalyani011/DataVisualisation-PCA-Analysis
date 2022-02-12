# Section 1: PCA

## Section 1 Solution
# Loading libraries
library(readr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(colorblindr)

# Reading data from csv file
DIGITS <- read_csv(file = 'pendigits.csv', col_names=FALSE)

# Renaming target column to digit
names(DIGITS)[17] <- "digit"

# Displaying first 5 rows of DIGITS
head(DIGITS)

# Using dplyr select method to select all dependent factors from data set,
# except for the target factor "digit".
# The selected factors are piped into PCA method of "FactoMineR" library
# Applying PCA to DIGITS data-set and saving the components in "pca" object
select(DIGITS, -digit) %>% PCA(graph = FALSE) -> pca

# Plotting scree plot
fviz_screeplot(pca, choice="variance", addlabels = TRUE)

# Plotting loadings_plot
loadings_plot <- fviz_pca_var(pca, col.var = "contrib", 
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                              repel = TRUE) +  xlab("PC1") + 
  ylab("PC2") +
  # removing the custom title, plot fig is captioned below
  ggtitle("") +
  theme(
    legend.position = c(1.10, 0.85),
    legend.box.margin = margin(0, 0.05, 0.05, 0.05)
  )

loadings_plot

# Section 2: Custom Palette

## Section 2 Solution

# Saving the hex values obtained from colorizer.org and 
# adjusted with hcl_color_picker in "custom_palette"
custom_palette <- c('#9F69E1', '#59C7DB',  '#F1954D', '#E562A5','#6C7AEA',
                    '#59DCB2', '#CB464E',  '#EFD456', '#D065DF', '#4D8FD3')
print("The hex values for the colors in the custom palette are:")
print(custom_palette)

# Plotting the color bar using palette_plot method
palette_plot(custom_palette, label_size=3)


# Section 3: ggplot2 Scatterplot

## Section 3 Solution

# Getting the projection of samples in terms of principle components
data_pca_ind <- get_pca_ind(pca)
# Selecting all rows in data-set with first two principle component values
data_pca <- data_pca_ind$coord[,c(1,2)]
# Converting data into a data frame
data_pca <- as.data.frame(data_pca)
# Naming the columns
names(data_pca)[1] <- "Principle_Component_1"
names(data_pca)[2] <- "Principle_Component_2"
# Combing the PC values with target column for each data point
data_pca<- cbind(data_pca, DIGITS$digit)
# Setting the target column name to digit
names(data_pca)[3] <- "digit"
# Setting digit to factor
data_pca$digit <- as.factor(data_pca$digit)

# ggplot for creating scatterplot for data_pca
digits_scatter <- ggplot(data_pca, aes(x=Principle_Component_1, y=Principle_Component_2, 
                                       color=digit)) +
  geom_point(alpha=0.4) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_colour_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette) +
  stat_ellipse(geom = "polygon",type = "t", size = 0.2,
               aes(fill = digit),
               alpha = 0.08) +
  theme_minimal()
digits_scatter

# ggplot for creating scatterplot for data_pca without colored data points
ggplot(data_pca, aes(x=Principle_Component_1, y=Principle_Component_2, color=digit)) +
  geom_point(alpha = 0.4, colour="black") +
  stat_ellipse(geom = "polygon",type = "t",size = 0.2,
               aes(fill = digit), 
               alpha = 0.08) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_minimal()

# ggplot for creating scatterplot for data_pca without data points 
# and containing only colored ellipses
ggplot(data_pca, aes(x=Principle_Component_1, y=Principle_Component_2, 
                     color=digit)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_colour_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette) +
  stat_ellipse(geom = "polygon",type = "t", size = 0.2,
               aes(fill = digit),
               alpha = 0.3) +
  theme_minimal()

cvd_grid(digits_scatter)
