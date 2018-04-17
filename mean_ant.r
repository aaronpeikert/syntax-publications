# title         Ant Colony Optimization (ACO)
# GitHub        https://github.com/ulrich-schroeders/syntax-publications
# date          2016-03-01
# version       1.0.0
# reference     Schroeders, U., Wilhelm, O., & Olaru, G. (2016). Meta-heuristics in short scale construction: Ant Colony Optimization and Genetic Algorithm. PLOS ONE, 11, e0167110. http://doi.org/10.1371/journal.pone.0167110
# Additional information: This script is a revised and adopted version of the script provided by Leite (2007). [Leite, W. L. (2007). Ant Colony Optmization (ACO) Algorithm [Computer software]. Retrieved January 1, 2016, from http://education.ufl.edu/leite/code/]

# set workingDir, read data, etc.
library(lavaan)
library(psych)
library(tidyverse)

items.ppvt <- seq(1, 1000, by = 10) %>%
  map(~rnorm(100, .x)) %>%
  `names<-`(., seq_along(.)) %>%
  bind_cols()
            
nitems <- 15 # number of items in the short scale
iter <- 60   # number of iterations
ants <- 150  # number of iterations (start with a iter/ants ratio of 2/3)
evaporation <- 0.90
pheromone_global <- NULL
#FUNCTION START:
antcolony <- function(evaporation, items.ppvt, nitems, iter, ants, catincrease=TRUE, catpheromone=FALSE, savepheromone = TRUE) {
                               
  best.pheromone <- 0
  best.so.far.pheromone <- 0
  item.vector <- items.ppvt
  
  #creates the table of initial pheromone levels.
  include <- rep(2, length(items.ppvt))
  
  #puts initial best solution (all items selected).
  best.so.far.solution <- include
    
  #creates a list to store factors.
  selected.items <- items.ppvt
  
  #starts counting the iterations
  count <- 1
  
  #starts counting continuous runs regardless of result.
  run <- 1
  
  #defines initial solutions.
  previous.solution <- include
  set.seed(789)
  
  #starts loop through iterations.
  while (count <= iter) { 
    
    #sends a number of ants per time.
    ant  <- 1
    while (ant <= ants) {
      
      mod.1dim <- NULL

      #selects the items for a short form for the factor
      positions <- is.element(item.vector, items.ppvt)
      prob <- include[positions]/ sum(include[positions])
      items <- sample(items.ppvt, size = nitems, replace = F, prob)
        
      #stores selected items 
      selected.items <- items
            
      # specifies CFA model
      mod.1dim <-  paste("lv =~", paste(selected.items, collapse = " + "))
     
      #creates a 0/1 vector of the same length of the long form indicating
      #whether an item was selected or not for the short form.
      select.indicator <- is.element(item.vector, selected.items)
      notselect.indicator <- (select.indicator == FALSE)
      pheromone <- map_dbl(items, mean) %>% mean()
      if(savepheromone)pheromone_global <<- c(pheromone_global, pheromone)
      if(catpheromone)cat(pheromone)
      if(catpheromone)cat("\n")
        
      #adjusts count based on outcomes and selects best solution.
      if (pheromone >= best.pheromone) {
          
        # updates solution.
        best.solution <- select.indicator
        best.pheromone <- pheromone
      } 
        
      #Move to next ant.
      ant <- ant + 1
        
      #ends loop through ants.
    }
    
    #adjusts pheromone only if the current pheromone is better than the previous.
    if (best.pheromone > best.so.far.pheromone) {
      
      #implements pheromone evaporation.
      include <- include * evaporation
      
      #adjusts the pheromone levels.
      include.pheromone <- best.solution * best.pheromone * run * 0.2
      
      #updates pheromone.
      include <- include + include.pheromone
      best.so.far.solution <- best.solution
      best.so.far.pheromone <- best.pheromone
      #re-starts count.
      count <- 1
      
      #end if clause for pheromone adjustment.
    } else {
      
      #advances count.
      count <- count + 1
      if(catincrease)cat("increase count\n")
    }
    
    #ends loop.
    run <- run + 1
    if(catincrease)cat("increase run\n")
  }
  return(best.so.far.solution)
}

# run the function
short <- antcolony(evaporation, items.ppvt, nitems, iter, ants)
short <- names(items.ppvt)[short]
short_real <- map_dbl(items.ppvt, mean) %>% sort() %>% .[86:100] %>% names() 
short_real_value <- map_dbl(items.ppvt[short_real], mean) %>% mean()
short_value <- map_dbl(items.ppvt[short], mean) %>% mean()
