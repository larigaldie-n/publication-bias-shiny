library(tidyverse)
library(cowplot)
library(shiny)
library(bslib)

add_studies <- function(prior_positive, power, alpha, positive = TRUE, number=1)
{
  
  if(positive==TRUE)
  {
    for(i in seq_len(number))
    {
      prior_negative <- 1-prior_positive
      prior_positive <- (power*prior_positive)/(power*prior_positive+alpha*prior_negative)
    }
    return(prior_positive)
  }
  else
  {
    for(i in seq_len(number))
    {
      prior_negative <- 1-prior_positive
      prior_positive <- ((1-power)*prior_positive)/((1-power)*prior_positive+(1-alpha)*prior_negative)
    }
    return(prior_positive)
  }
}

simulate_literature <- function(p_hypothesis_true, power, alpha, literature_bias, p_hacking, n_positive_studies_observed, n_negative_studies_observed, prior=TRUE)
{
  if(prior==TRUE)
  {
    p_hypothesis_true <- seq(0.001, 0.999, by=0.001)
  }
  else
  {
    power <- seq(0.050, 0.999, by=0.001)
  }
  p_hypothesis_false <- 1-p_hypothesis_true
  
  n_studies <- 1000
  n_studies_true_hypothesis <- p_hypothesis_true*n_studies
  n_studies_false_hypothesis <- p_hypothesis_false*n_studies
  n_studies_true_negative <- (1-alpha)*n_studies_false_hypothesis
  n_studies_false_positive <- n_studies_false_hypothesis-n_studies_true_negative
  n_studies_false_negative <- (1-power)*n_studies_true_hypothesis
  n_studies_true_positive <- n_studies_true_hypothesis-n_studies_false_negative
  
  n_studies_false_positive_hacked <- n_studies_false_hypothesis-n_studies_true_negative+p_hacking*n_studies_true_negative
  n_studies_true_negative_hacked <- n_studies_false_hypothesis-n_studies_false_positive_hacked
  n_studies_true_positive_hacked <- n_studies_true_hypothesis-n_studies_false_negative+p_hacking*n_studies_false_negative
  n_studies_false_negative_hacked <- n_studies_true_hypothesis-n_studies_true_positive_hacked
  
  n_published_positive <- (n_studies_false_positive_hacked + n_studies_true_positive_hacked)
  n_published_negative <- pmin((n_published_positive/literature_bias)*(1-literature_bias), n_studies_false_negative_hacked + n_studies_true_negative_hacked)
  n_published_false_positive <- n_studies_false_positive_hacked
  n_published_true_positive <- n_studies_true_positive_hacked
  n_published_false_negative <- (n_studies_false_negative_hacked/(n_studies_false_negative_hacked+n_studies_true_negative_hacked))*n_published_negative
  n_published_true_negative <- n_published_negative - n_published_false_negative  #(n_studies_true_negative/(n_studies_false_negative+n_studies_true_negative))*n_published_negative
  
  ### Estimate true positives (effective power) and false negative (effective alpha) rates
  p_positive_given_true_studies <- n_studies_true_positive/(n_studies_true_positive + n_studies_false_negative)
  p_positive_given_false_studies <- n_studies_false_positive/(n_studies_true_negative + n_studies_false_positive)
  
  p_hypothesis_true_given_positive_studies <- add_studies(p_hypothesis_true, p_positive_given_true_studies, p_positive_given_false_studies, TRUE, n_positive_studies_observed)
  p_hypothesis_true_given_positive_studies <- add_studies(p_hypothesis_true_given_positive_studies, p_positive_given_true_studies, p_positive_given_false_studies, FALSE, n_negative_studies_observed)
  
  p_positive_given_true_published <- n_published_true_positive/(n_published_true_positive + n_published_false_negative)
  p_positive_given_false_published <- n_published_false_positive/(n_published_true_negative + n_published_false_positive)
  
  p_hypothesis_true_given_positive_published <- add_studies(p_hypothesis_true, p_positive_given_true_published, p_positive_given_false_published, TRUE, number=n_positive_studies_observed)
  p_hypothesis_true_given_positive_published <- add_studies(p_hypothesis_true_given_positive_published, p_positive_given_true_published, p_positive_given_false_published, FALSE, number=n_negative_studies_observed)
  
  return(tibble(p_hypothesis_true=p_hypothesis_true, power=power, p_hypothesis_true_given_positive_published=p_hypothesis_true_given_positive_published, p_hypothesis_true_given_positive_studies=p_hypothesis_true_given_positive_studies))
}




# Define UI ----
ui <- page_sidebar(
  title = "Effects of publication bias, power & malpractice on hypothesis truth inference",
  sidebar = sidebar(card(
                    sliderInput(
                      "Prior",
                      "Prior probability of random hypotheses being true",
                      min = 0.01,
                      max = 0.99,
                      value = 0.5),
                    sliderInput(
                      "Power",
                      "Power",
                      min = 0.05,
                      max = 0.99,
                      value = 0.24)),
                    card(
                    checkboxInput("LiteratureBiasBool", "Include publication bias", value = TRUE),
                    sliderInput(
                      "LiteratureBias",
                      "Proportion of positive studies in the literature",
                      min = 0.01,
                      max = 0.99,
                      value = 0.9)
                    ),
                    card(
                      numericInput("nPositiveStudies", "Number of positive studies observed", value=1, min=0),
                      numericInput("nNegativeStudies", "Number of negative studies observed", value=0, min=0)
                    ),
                    card(
                      checkboxInput("PHackingBool", "Include p-hacking & fraud", value = FALSE),
                      sliderInput(
                        "PHackingBias",
                        "Proportion of negative studies p-hacked or frauded to become positive",
                        min = 0.01,
                        max = 0.99,
                        value = 0.1)
                    )
                    ),
  card(
    card_header(""),
    plotOutput("Plot")
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    p_hypothesis_true <- input$Prior
    p_hypothesis_false <- 1-p_hypothesis_true
    power <- input$Power
    alpha <- 0.05
    n_positive_studies_observed <- input$nPositiveStudies
    n_negative_studies_observed <- input$nNegativeStudies
    literature_bias <- input$LiteratureBias
    include_p_hacking <- input$PHackingBool
    if(include_p_hacking == FALSE)
    {
      p_hacking_bias <- 0
    }
    else
    {
      p_hacking_bias <- input$PHackingBias
    }
    
    df_prior <- simulate_literature(p_hypothesis_true, power, alpha, literature_bias, p_hacking_bias, n_positive_studies_observed, n_negative_studies_observed, prior=TRUE)
    df_power <- simulate_literature(p_hypothesis_true, power, alpha, literature_bias, p_hacking_bias, n_positive_studies_observed, n_negative_studies_observed, prior=FALSE)
    p1 <- df_prior %>% ggplot(aes(x=p_hypothesis_true)) + geom_line(aes(y=p_hypothesis_true_given_positive_studies), size=2) + scale_y_continuous(limits=c(0, 1), expand=c(0,0.05)) + theme_minimal_grid(18) + xlab("Prior probability of random hypotheses being true") + ylab("Posterior probability of an hypothesis being true") + geom_line(aes(x=p_hypothesis_true, y=p_hypothesis_true), size=1, color="blue", linetype="dashed") + geom_vline(xintercept=p_hypothesis_true, size=1.5, linetype="dashed", color="coral")
    p2 <- df_power %>% ggplot(aes(x=power)) + geom_line(aes(y=p_hypothesis_true_given_positive_studies), size=2) + scale_y_continuous(limits=c(0, 1), expand=c(0,0.05)) + theme_minimal_grid(18) + xlab("Statistical power") + ylab("Posterior probability of an hypothesis being true") + geom_line(aes(x=power, y=p_hypothesis_true), size=1, color="blue", linetype="dashed") + geom_vline(xintercept=power, size=1.5, linetype="dashed", color="coral")
    if(input$LiteratureBiasBool==TRUE)
    {
      p1 <- p1 + geom_line(aes(y=p_hypothesis_true_given_positive_published), size=2, color="red")
      p2 <- p2 + geom_line(aes(y=p_hypothesis_true_given_positive_published), size=2, color="red")
    }
    plot_grid(p1, p2, labels = c(paste0('Probability of an hypothesis being true with power ', power), paste0('Probability of an hypothesis being true with prior ', p_hypothesis_true)), label_size = 14)})
}

# Run the app ----
shinyApp(ui = ui, server = server)