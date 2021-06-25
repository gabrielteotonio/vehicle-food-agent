library(ggplot2)
library(shiny)
library(gganimate)

ui <- fluidPage(
  
  titlePanel("Food seeker!"),
  actionButton("button",
               "Grab the food!"),
  imageOutput("plot1")
    
)

server <- function(input, output) {
  
  data_path <- eventReactive(input$button, 
                             {data <- data.frame(role = c("agent", "food"),
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
                             })
  
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    p <- data_path() %>% 
      ggplot(aes(x = position_x, y = position_y, colour = role)) +
      geom_point(show.legend = FALSE, alpha = 0.7) +
      scale_color_viridis_d() +
      scale_size(range = c(2, 12)) +
      ylim(0, 10) + xlim(0, 10) + 
      theme_void() +
      theme(panel.border = element_rect(fill=NA,color="black", size=0.5, 
                                        linetype="solid")) +
      transition_time(iteration)
    
    animate(p, renderer = gifski_renderer(loop = F))
    
    anim_save("outfile.gif", animate(p, renderer = gifski_renderer(loop = F))) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)}

shinyApp(ui, server)