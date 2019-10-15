#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(ggrepel)

levels <- read_csv(file = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSiqyyYi6jGje-v8u_bOPZ6wFzDOHxxJmsSotTy0RB_jkjTgKjdDoe2Eujh0FU3VDKbx6QIKGp9LYFP/pub?gid=0&single=true&output=csv",
                   col_types = "iii")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The Poke-verse"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("the_xp",
                         "Your total XP",
                         0,
                         min = 0,
                         step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("xpPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$xpPlot <- renderPlot({
        
        req(input$the_xp)
        
        my_xp <- if_else(input$the_xp > max(levels$Cumulative),
                         max(levels$Cumulative),
                         as.integer(floor(input$the_xp)))
        
        if(my_xp < 1) {
            return(NULL)
        }
        
        my_level <- levels %>% filter(Cumulative < my_xp) %>%
            filter(row_number() == n()) %>% pull(Level)
        
        steps_to_go <- round((max(levels$Cumulative) - my_xp) / my_xp, digits=2)
        
        my_df <- tribble(
            ~Level, ~XP, ~label,
            my_level, my_xp, "You are here"
        )
        
        my_labels_df <- tribble(
            ~x, ~y, ~label,
            0, my_level, my_level,
            my_xp, 0, my_xp
        )
        
        my_step_df <- tribble(
            ~Level, ~XP, ~label,
            5,my_xp / 2, 1,
            5, my_xp + (max(levels$Cumulative) - my_xp) / 2, steps_to_go
        )
        
        my_nudge_x <- if_else(my_xp > max(levels$Cumulative) / 2, 
                              -1 * max(levels$Cumulative) / 10,
                              1 * max(levels$Cumulative) / 10)
        
        my_nudge_y <- if_else(my_level > max(levels$Level) / 2, 
                              -.1 * my_level,
                              0.1 * my_level)
        
        p <- ggplot(levels, aes(y=Level, x=Cumulative)) + 
            geom_step() + 
            geom_vline(xintercept = my_xp, linetype=3, color="red" ) +
            geom_hline(yintercept = my_level, linetype=3, color = "red") +
            geom_segment(x = 0, xend = my_xp, y = 5, yend =5, arrow = arrow(ends = "both")) +
            geom_segment(x = my_xp, xend = max(levels$Cumulative), y = 5, yend =5, arrow = arrow(ends = "both")) +
            geom_label_repel(data = my_df, aes(x=XP, y=Level, label=label), 
                             arrow=arrow(type="closed", length = unit(0.15, "inches")), 
                             nudge_x = my_nudge_x, 
                             nudge_y = my_nudge_y)+
            geom_label(data = my_step_df, aes(x=XP, y=Level, label=label)) +
            geom_label(data = my_labels_df, aes(x=x, y=y, label=label)) +
            labs(
                title = "Your Place in the Poke-verse",
                x="Trainer Total XP",
                y="Trainer Level"
            ) + ggthemes::theme_few()
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
