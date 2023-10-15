outliers_data <- read.csv("BodyFat.csv")
outliers_data<- outliers_data[, c( 'BODYFAT','ABDOMEN', 'CHEST', 'WRIST')]
z_scores <- scale(outliers_data2)
outlier_threshold <- 3
outlier_indices_z <- which(abs(z_scores) > outlier_threshold, arr.ind=TRUE)
print(outlier_indices_z)



