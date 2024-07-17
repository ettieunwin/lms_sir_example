# This file compiles and plots an odin model 
library(odin)
library(tidyverse)

# The file name of the model
model <- "sir_model.R"

# Compiles the model
generator <- odin::odin(model)
mod <- generator$new()

# Set time variable
tt <- seq(0, 20, length.out = 101)

# Runs the model
y <- mod$run(tt)

# Plots the output
y_long = gather(data.frame(y), key = variable, value = value, -t)
p <- ggplot(y_long) +
  geom_line(aes(t, value, col = variable)) + 
  theme_bw()
print(p)