df <- data.frame(
  name = c("John", "Jane", "Doe"),
  image_url = c(
    "https://upload.wikimedia.org/wikipedia/commons/4/4f/SVG_Logo.svg", 
    "https://upload.wikimedia.org/wikipedia/commons/4/4f/SVG_Logo.svg", 
    "https://upload.wikimedia.org/wikipedia/commons/4/4f/SVG_Logo.svg"
  ),
  stringsAsFactors = FALSE
)
gt_table <- df %>%
  gt() %>%
  text_transform(
    locations = cells_body(image_url),
    fn = function(x) {
      web_image(url = x, height = px(50))
    }
  )
gt_table





# Load the necessary libraries
library(gt)
library(dplyr)

# Create a sample data frame
data <- data.frame(
  category = c("A", "B", "C"),
  description = c(
    "Red, Green, Blue",
    "Yellow, Magenta, Cyan",
    "Black, White, Gray"
  )
)

# Convert the data frame to a gt table
gt_table <- gt(data)

# Add custom styling to specific words in the 'description' column
gt_table <- gt_table %>%
  text_transform(
    locations = cells_body(columns = description),
    fn = function(x) {
      x <- gsub("Red", "<span style='color: red;'>Red</span>", x)
      x <- gsub("Green", "<span style='color: green;'>Green</span>", x)
      x <- gsub("Blue", "<span style='color: blue;'>Blue</span>", x)
      x <- gsub("Yellow", "<span style='color: yellow;'>Yellow</span>", x)
      x <- gsub("Magenta", "<span style='color: magenta;'>Magenta</span>", x)
      x <- gsub("Cyan", "<span style='color: cyan;'>Cyan</span>", x)
      x <- gsub("Black", "<span style='color: black;'>Black</span>", x)
      x <- gsub("White", "<span style='color: white; background-color: black;'>White</span>", x) # white text on black background for visibility
      x <- gsub("Gray", "<span style='color: gray;'>Gray</span>", x)
      x
    }
  )

gt_table

###################################################################################

# Load the necessary libraries
library(gt)
library(dplyr)

# Function to add styling to each letter individually
add_styling_to_letters <- function(word, color) {
  styled_letters <- sapply(strsplit(word, NULL)[[1]], function(letter) {
    sprintf("<span style='color: %s; border: 1px solid black; padding: 1px;'>%s</span>", color, letter)
  })
  paste(styled_letters, collapse = "")
}

# Create a sample data frame
data <- data.frame(
  category = c("A", "B", "C"),
  description = c(
    "Red, Green, Blue",
    "Yellow, Magenta, Cyan",
    "Black, White, Gray"
  )
)

# Convert the data frame to a gt table
gt_table <- gt(data)

# Add custom styling to specific words in the 'description' column, including border around each letter
gt_table <- gt_table %>%
  text_transform(
    locations = cells_body(columns = description),
    fn = function(x) {
      x <- gsub("Red", add_styling_to_letters("Red", "red"), x)
      x <- gsub("Green", add_styling_to_letters("Green", "green"), x)
      x <- gsub("Blue", add_styling_to_letters("Blue", "blue"), x)
      x <- gsub("Yellow", add_styling_to_letters("Yellow", "yellow"), x)
      x <- gsub("Magenta", add_styling_to_letters("Magenta", "magenta"), x)
      x <- gsub("Cyan", add_styling_to_letters("Cyan", "cyan"), x)
      x <- gsub("Black", add_styling_to_letters("Black", "black"), x)
      x <- gsub("White", add_styling_to_letters("White", "white") %>% sprintf("<span style='background-color: black;'>%s</span>", .), x) # white text on black background for visibility
      x <- gsub("Gray", add_styling_to_letters("Gray", "gray"), x)
      x
    }
  )

# Print the table
gt_table












