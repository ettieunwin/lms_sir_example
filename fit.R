# This file compiles and plots an odin model 
library(odin)
library(tidyverse)

plot_data = function(params){
  # Sets model parameters
  mod <- generator$new(user = list(sigma = params[1], beta = params[2]))
  # Runs the model
  y <- mod$run(tt)
  
  # Format the data for plotting
  y_long = gather(data.frame(y), key = variable, value = value, -t)
  
  # plot
  p <- ggplot(y_long %>% filter(variable == "I")) +
    geom_line(aes(t, value), col = "red") + 
    geom_point(data = data, aes(t, value), col = "blue")+
    theme_bw()
  
  return (p)
}

error = function(params){
  # Sets model parameters
  mod <- generator$new(user = list(sigma = params[1], beta = params[2]))
  # Runs the model
  y <- mod$run(tt)
  
  subset = data.frame(y) %>% filter(t %in% c(0:20)) %>% 
    select(t, I)
  
  subset = left_join(subset, data, by="t") %>%
    mutate(difference = (I-value)^2)

  return (sum(subset$difference))
}

# The file name of the model
model <- "sir_model.R"
# Compiles the model
generator <- odin::odin(model)
# Set time variable
tt <- seq(0, 20, length.out = 101)

# Reads in data
data = readRDS("data.RDS")

# Plots the output
p = plot_data(params = c(1, 5))
print(p)

print (error(params = c(0.2,3)))

print(optim(par = c(1,3), error))
