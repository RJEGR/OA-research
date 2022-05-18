# devtools::install_github("rsquaredacademy/xplorerr") # tiene muchos problemas de compatibilidad entre versiones de r y paquetes, so not¡


# devtools::install_github("tidymodels/tidymodels")
# https://easystats.github.io/bayestestR/
# https://www.tidymodels.org/learn/
library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker) 

urchins <-
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

urchins

# The urchins data is a tibble. For each of the 72 urchins, we know their:
#   
#   experimental feeding regime group (food_regime: either Initial, Low, or High),
# size in milliliters at the start of the experiment (initial_volume), and
# suture width at the end of the experiment (width).
# As a first step in modeling, it’s always a good idea to plot the data

ggplot(urchins,
  aes(x = initial_volume, 
    y = width, 
    group = food_regime, 
    col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)

# We can see that urchins that were larger in volume at the start of the experiment tended to have wider sutures at the end, but the slopes of the lines look different so this effect may depend on the feeding regime condition.

# BUILD AND FIT A MODE︎L ----
# A standard two-way analysis of variance (ANOVA) model makes sense for this dataset because we have both a continuous predictor and a categorical predictor. Since the slopes appear to be different for at least two of the feeding regimes, let’s build a model that allows for two-way interactions. Specifying an R formula with our variables in this way:

width ~ initial_volume * food_regime

# For this kind of model, ordinary least squares is a good initial approach. With tidymodels, we start by specifying the functional form of the model that we want using the parsnip package. Since there is a numeric outcome and the model should be linear with slopes and intercepts, the model type is “linear regression”. We can declare this with:


linear_reg()

#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm


# # Run the model ----

# That is pretty underwhelming since, on its own, it doesn’t really do much. However, now that the type of model has been specified, we can think about a method for fitting or training the model, the model engine. The engine value is often a mash-up of the software that can be used to fit or train the model as well as the estimation method. The default for linear_reg() is "lm" for ordinary least squares, as you can see above. We could set a non-default option instead:

linear_reg() %>% 
  set_engine("keras")



# The documentation page for linear_reg() lists all the possible engines. https://parsnip.tidymodels.org/reference/linear_reg.html. We’ll save our model object using the default engine as lm_mod.

urchins

#> # A tibble: 72 × 3
#>    food_regime initial_volume width
#>    <fct>                <dbl> <dbl>
#>  1 Initial                3.5 0.01 
#>  2 Initial                5   0.02 
#>  3 Initial                8   0.061
#>  4 Initial               10   0.051

lm_mod <- linear_reg()

lm_fit <- 
  lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)


lm_fit

tidy(lm_fit)


# This kind of output can be used to generate a dot-and-whisker plot of our regression results using the dotwhisker package:
  
tidy(lm_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
    whisker_args = list(color = "black"),
    vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

# USE A MODEL TO PREDICT  ----
# Suppose that, for a publication, it would be particularly interesting to make a plot of the mean body size for urchins that started the experiment with an initial volume of 20ml.

new_points <- expand.grid(initial_volume = 20, 
  food_regime = c("Initial", "Low", "High"))

new_points

# To get our predicted results, we can use the predict() function to find the mean values at 20ml.

mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

# When making predictions, the tidymodels convention is to always produce a tibble of results with standardized column names. This makes it easy to combine the original data and the predictions in a usable format:

conf_int_pred <- predict(lm_fit, 
  new_data = new_points, 
  type = "conf_int")

conf_int_pred

# Now combine: 
plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
    ymax = .pred_upper),
    width = .2) + 
  labs(y = "urchin size")

# MODEL WITH A DIFFERENT ENGINE ----
# Every one on your team is happy with that plot except that one person who just read their first book on Bayesian analysis. They are interested in knowing if the results would be different if the model were estimated using a Bayesian approach. In such an analysis, a prior distribution needs to be declared for each model parameter that represents the possible values of the parameters (before being exposed to the observed data).
