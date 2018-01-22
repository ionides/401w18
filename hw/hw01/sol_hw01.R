###################################
# Author: Naomi Giertych
# Date: 1/2/2018
# Homework 1 Solutions
###################################

# Swirl Exercises
  install.packages('swirl')
  library('swirl')
  swirl()
  # Type 0 to exit swirl

# Data manipulation exercise
  
  # Read in CSV  
  mice <- read.csv("https://ionides.github.io/401w18/hw/hw01/femaleMiceWeights.csv")
  
  # Weight of first mouse
  mice[1,]
  
  # The name of the column containing the weights is Bodyweight.
  
  # Entry of the 12th row and 2nd column
  mice[12,2]
  
  # Weight of the mouse in the 11th row
  mice$Bodyweight[11]
  
  # Number of elements in dataset
  length(mice$Diet)
  
  # Average weight of mice on high fat diet
  mean(mice$Bodyweight[13:24])
  
###################################
# END
###################################