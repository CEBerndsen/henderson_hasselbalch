library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Understanding pH and pKa",
    tabPanel("Simulator",
    # Application title
    fluidPage(
        fluidRow(
                        column(5,
                            plotOutput("scatPlot")
                                ),
                        column(3,
                            plotOutput("barPlot")
                                ),
                        column(4,
                            plotOutput("phPlot")
                                )
            ),
        hr(),
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(4,
               offset = 1,
            sliderInput("ph",
                        "Choose a pH value:",
                        min = 0.5,
                        max = 14,
                        value = 7)
        ),
        column(4,
               offset = 1,
            sliderInput("pka",
                        "Choose a pKa value:",
                        min = 0.5,
                        max = 14,
                        value = 7)
)
)
),
    hr(),
    br(),
    "Shiny app created by C.E. Berndsen, 2019",
    br()
),
    tabPanel("More Information",
        includeHTML("H-H_explain.html")
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
                  legend.text = element_text(size = 20, face = "bold"),
                  plot.title = element_text(size = 10, face = "bold")) +
            scale_x_continuous(expand = c(0.01,0.01)) +
            scale_y_continuous(expand = c(0.01,0.01)) +
            labs(title = "Solution contents")
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
                  axis.text.x = element_blank(),
                  legend.position = "none",
                  panel.grid = element_blank(),
                  plot.title = element_text(size = 10, face = "bold")) +
            labs(x = "specie", y = "molecules in solution", title = "Bar plot of \nnumber of \nmolecules in solution") +
            scale_y_continuous(expand = c(0.01,0), limits = c(0, 1100))
    })
    
    output$phPlot <- renderPlot({
        ps <- as.numeric(input$ph)
        pk <- as.numeric(input$pka)
        
        barat <- 10^(ps - pk)
        
        phex <- tibble(btoa = barat)
        
        phlow <- tibble(btoa = seq(from = 1, to = 999, by = 1))
        
        phhigh <- tibble(btoa = seq(from = 1, to = 999, by = 1))
        
        phlow <- phlow %>%
            mutate(ratio = btoa/999)
        
        phhigh <- phhigh %>%
            mutate(ratio = 999/btoa)
        
        phex <- phex %>%
            mutate(ratio = barat)
        
        phdf <- bind_rows(phlow, phhigh, phex)
        
        phdf <- phdf %>%
            mutate(ph = pk + log10(ratio),
                   equiv = ratio/(1+ratio))
        
        where <- phdf %>%
            filter(round(ph, 2) == ps)
        
        ggplot(phdf, aes(x = equiv, y = ph)) +
            geom_line() +
            geom_vline(xintercept = 0.5, linetype = 2) +
            geom_point(data = where, aes(x = equiv, y = ph), alpha = 0.5, color = "red", size = 5) +
            theme_bw() +
            theme(axis.title = element_text(size = 16, face = "bold"),
                  axis.text = element_text(size = 12, face = "bold"),
                  legend.position = "none",
                  plot.title = element_text(size = 10, face = "bold")) +
            labs(x = "Molar equivalents \nof strong base", y = "pH", title = "pH titration curve") +
            scale_x_continuous(expand = c(0,0), breaks = seq(from = 0, to = 1, by = 0.1)) +
            scale_y_continuous(expand = c(0,0), limits = c(0,14), breaks = seq(from = 0, to = 14, by = 1))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
