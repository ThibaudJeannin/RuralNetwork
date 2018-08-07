library(R6)
library(magrittr)

RuralNetwork <- R6Class(
  "RuralNetwork",
  public = list(
    
    initialize = function(layers = list()) {
      private$m_layers <- layers
    },
    
    print = function() {
      cat(sprintf('RuralNetwork with %s layers', length(private$m_layers)))
      cat('\n')
      for (i in seq(length(private$m_layers)) ) {
        cat('| ')
        print(private$m_layers[[i]])
        cat('\n')
      }
    },
    
    layer = function(i = 1) {return(private$m_layers[[i]])},
    
    process = function(inputs) {
      for (layer_index in seq(private$m_layers)) {
        inputs %<>% private$m_layers[[layer_index]]$process()
      }
      return(inputs)
    }
    
  ),
  private = list(
    
    m_layers = NULL
    
  )
)

RuralNetworkBuilder <- R6Class(
  "RuralNetworkBuilder",
  public = list(
    
    initialise = function() {
      
    },
    
    build = function() {
      layers <- list()
      for (i in seq(length(private$m_layers)) ) {
        if (i == 1) {
          layers %<>% append(private$m_layers[[i]]$build())
        } else {
          layers %<>% append(private$m_layers[[i]]$build( private$m_layers[[i-1]]$size() ))
        }
      }
      return(RuralNetwork$new(layers = layers))
    },

    print = function() {
      cat(sprintf('RuralNetworkBuilder with %s layers', length(private$m_layers)))
      cat('\n')
      for (i in seq(length(private$m_layers)) ) {
        cat('| ')
        print(private$m_layers[[i]])
        cat('\n')
      }
    },
    
    withLayer = function(layerBuilder) {
      private$m_layers %<>% append(layerBuilder)
      return(self)
    }
    
  ),
  
  private = list(
    
    m_layers = NULL
    
  )
)

Layer <- R6Class(
  "Layer",
  public = list(
    
    initialize = function(size, size_prev_layer, type="simple", activation_function) {
      private$m_type <- type
      private$m_neurons <- matrix(
        nrow = size,
        ncol = 1
      )
      private$m_weights <- matrix(
        runif(size * size_prev_layer, 0, 1), nrow = size, ncol = size_prev_layer)
      
      private$m_activation_function <- switch(activation_function,
                                             linear=function(x){return(x)},
                                             sigmoid=function(x){return(1/(1+exp(-x)))},
                                             relu=function(x){return(ifelse(x<0, 0, x))}
                                             )
      
      },
    
    print = function() {
      cat (sprintf('Layer (%s) with %s neurons\n', private$m_type, nrow(private$m_neurons)))
      print(private$m_weights)
    },
    
    weights = function() {return(private$m_weights)},
    
    process = function(inputs) {
      private$m_neurons <- matrix(inputs, ncol = length(inputs))
      
      if (private$m_type == 'input') {return(private$m_activation_function(inputs))}
      
      #Implicit activation function : y = x
      return(private$m_weights %*% t(private$m_neurons))
    }
    
  ),
  private = list(
    m_type = NULL,
    m_neurons = NULL,
    m_weights = NULL,
    m_activation_function = NULL
  )
)

LayerBuilder <- R6Class(
  "LayerBuilder",
  public = list(

    initialize = function(size = 1, type="simple", activation_function) {
      private$m_type <- type
      private$m_size <- size
      private$m_activation_function <- activation_function

    },

    print = function() {
      cat (sprintf('LayerBuilder (%s/%s) with %s neurons', private$m_type, private$m_activation_function, private$m_size))
    },
    
    build = function(size_prev = 0) {
      return(Layer$new(ifelse(size_prev == 0, 'input', private$m_type), size = private$m_size, size_prev = size_prev, activation_function = private$m_activation_function))
    },
    
    size = function() { return(private$m_size) }

  ),
  private = list(
    
    m_type = NULL,
    m_size = NULL,
    m_activation_function = NULL
    
  )
)

source('R/RuralNetworkTest.r')