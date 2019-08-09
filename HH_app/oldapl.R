library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Understanding pH and pKa"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("ph",
                        "Choose a pH value:",
                        min = 0.5,
                        max = 14,
                        value = 7),
            sliderInput("pka",
                        "Choose a pKa value:",
                        min = 0.5,
                        max = 14,
                        value = 7),
            br(),
            br(),
            br(),
            "Shiny app created by C.E. Berndsen, 2019",
            br()
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("scatPlot"), plotOutput("barPlot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatPlot <- renderPlot({
        
        # set input variables and make sure that they are considered numbers
        ph <- as.numeric(input$ph)
        pk <- as.numeric(input$pka)
        
        # do the math to calculate the b:a ratio
        ba <- 10^(ph-pk)
        
        # calculate the fraction base and acid
        b <- ba/(1+ba)
        a <- 1/(ba+1)
        
        # make tibbles of acid and base to show relative amounts in 1000 molecules
        # if the fraction of a or b is less than 0.001, assign it a value of 1
        d <- if_else(a > 0.001,
                     a,
                     0.001)
        
        af <- tibble("number" = seq(from = 1, to = d*1000, by = 1))
        
        af <- af %>%
            mutate(specie = "acid",
                   spot = 1)
        
        c <- if_else(b > 0.001,
                     b,
                     0.001)
        
        bf <- tibble("number" = seq(from = 1, to = c*1000, by = 1))
        
        bf <- bf %>%
            mutate(specie = "base",
                   spot = 1)
        
        # make the final dataframe with both acid and base components
        df <- bind_rows(af, bf)
        
        # make the scatter plot
        ggplot(df, aes(x = 1, y = spot, color = specie)) +
            geom_point(position = "jitter", alpha = 0.8, size = 3) +
            scale_color_manual(values = c("purple", "darkgreen")) +
            theme_bw() +
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  panel.grid = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position = "bottom",
                  legend.title = element_text(size = 20, face = "bold"),
                  legend.text = element_text(size = 20, face = "bold")) +
            scale_x_continuous(expand = c(0.01,0.01)) +
            scale_y_continuous(expand = c(0.01,0.01))
    })
    
    output$barPlot <- renderPlot({
        # set input variables and make sure that they are considered numbers
        ph <- as.numeric(input$ph)
        pk <- as.numeric(input$pka)
        
        # do the math to calculate the b:a ratio
        ba <- 10^(ph-pk)
        
        # calculate the fraction base and acid
        b <- ba/(1+ba)
        a <- 1/(ba+1)
        
        # make tibbles of acid and base to show relative amounts in 1000 molecules
        # if the fraction of a or b is less than 0.001, assign it a value of 1
        d <- if_else(a > 0.001,
                     a,
                     0.001)
        
        af <- tibble("number" = seq(from = 1, to = d*1000, by = 1))
        
        af <- af %>%
            mutate(specie = "acid",
                   spot = 1)
        
        c <- if_else(b > 0.001,
                     b,
                     0.001)
        
        bf <- tibble("number" = seq(from = 1, to = c*1000, by = 1))
        
        bf <- bf %>%
            mutate(specie = "base",
                   spot = 1)
        
        # make the final dataframe with both acid and base components
        df <- bind_rows(af, bf)
        
        ggplot(df, aes(x = specie, fill = specie)) +
            geom_bar(alpha = 0.8) +
            scale_fill_manual(values = c("purple", "darkgreen")) +
            theme_bw() +
            theme(axis.title = element_text(size = 24, face = "bold"),
                  axis.text = element_text(size = 20, face = "bold"),
                  axis.title.x = element_blank(),
                  legend.position = "none",
                  panel.grid = element_blank()) +
            labs(x = "specie", y = "relative number of \nmolecules in solution") +
            scale_y_continuous(expand = c(0.01,0), limits = c(0, 1100))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
