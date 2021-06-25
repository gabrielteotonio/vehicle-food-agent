x <- c(0, .6509467)
y <- c(0, 1.606)
approx(x,y)


library(ggplot2)
library(gganimate)
theme_set(theme_bw())
library(tidyverse)

data <- data.frame(role = c("agent", "food"),
                   position_x = c(sample(0:10, 1), sample(0:10, 1)),
                   position_y = c(sample(0:10, 1), sample(0:10, 1)),
                   iteration = c(0, 0))

interp <- approx(data$position_x, data$position_y)

data_path_agent <- data.frame(role = "agent",
                              position_x = rev(interp$x),
                              position_y = rev(interp$y),
                              iteration = 1:length(interp$x),
                              image = "https://image.flaticon.com/icons/png/128/2942/2942667.png")

data_path_food <- data.frame(role = "food",
                             position_x = data[data$role == "food",]$position_x,
                             position_y = data[data$role == "food",]$position_y,
                             iteration = 1:length(interp$x)-1,
                             image = "https://images.vexels.com/media/users/3/143088/isolated/preview/f565debc52083dacca60da22284e4083-iacute-cone-de-coxa-de-frango-by-vexels.png")

data_path <- rbind(data_path_agent, data_path_food)


p <- data_path %>% 
  ggplot(aes(x = position_x, y = position_y, colour = role)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  ylim(0, 10) + xlim(0, 10) + 
  theme_void() +
  transition_time(iteration)

animate(p, renderer = gifski_renderer(loop = F))
