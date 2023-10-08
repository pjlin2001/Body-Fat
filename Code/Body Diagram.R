# Load the required libraries
library(bdgramR)
library(ggplot2)

dat <- bdgramr(data = data, model = "original_male")

# Create Chest Plot
plot <- ggplot(data = dat, aes(x, y, group = Id)) +
  geom_bdgramr(color = "cyan", aes(fill = Muscle)) +
  scale_fill_manual(values = c(
    "Pec_Major" = "red",       # Color for "Pec_Major"
    "Trapezius" = "red",
    "Infraspinatus" = "red"
  )) 
# Print or display the plot
print(plot)

dat <- bdgramr(data = data, model = "athletesr")

# Create Abs Plot
plot <- ggplot(data = dat, aes(x, y, group = Id)) +
  geom_bdgramr(color = "cyan", aes(fill = Muscle)) +
  scale_fill_manual(values = c(
    "Lower_Back" = "red",
    "Rectus_Abdominis" = "red"
  )) 
# Print or display the plot
print(plot)

# Create Wrist Plot
plot <- ggplot(data = dat, aes(x, y, group = Id)) +
  geom_bdgramr(color = "cyan", aes(fill = Muscle)) +
  scale_fill_manual(values = c(
    "Wrist" = "red"
  )) 
# Print or display the plot
print(plot)
