library(R6)

RuralNetwork <- R6Class(
  "RuralNetwork",
  public = list(
    
    initialize = function(layers = list()) {
      private$layers <- layers
    },
    
    print = function(...) {
      cat (sprintf("Neural Network"))
      for (l in seq(self$layers)) {
        print( self$layers[[i]] )
      }
    }
    
  ),
  private = list(
    
    layers = NULL
    
  )
)

nnet <- RuralNetwork$new(list(42,43))