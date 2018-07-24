library(magrittr)

Neuron <- setClass(
  "Neuron", 
  slots = c(
    value = "numeric"
  ),
  prototype = prototype(value = 0)
)

setMethod('print','Neuron', function(x){
  cat(sprintf('[%s]', x@value))
})

Layer <- setClass(
  "Layer",
  slots = c(
    neurons = "matrix",
    weights = "matrix",
    type = "character"
  ),
  contains = "matrix",
  prototype = prototype(
    neurons = matrix(nrow = 0, ncol = 0), 
    weights = matrix(nrow = 0, ncol = 0),
    type = "simple"
    )
)

setMethod('print','Layer', function(x){
  cat(sprintf('Layer (%s) with %s neurons\n', x@type, length(x@neurons))) 
  for (i in seq(x@neurons)) {
    print(x@neurons[[i]])
  }
  cat('\n')
})

Network <- setClass(
  "Network",
  slots = c(
    layers = "list"
  ),
  prototype = prototype(layers=list())
)

LayerBuilder <- setClass(
  "LayerBuilder",
  slots = c(
    size = "numeric",
    type = "character"
  ),
  prototype = prototype(size = 1, type = "simple")
)

setMethod('print', "LayerBuilder", function(x){
  cat(sprintf('%s layer with %s neurons', x@type, x@size))
})

NetworkBuilder <- setClass(
  "NetworkBuilder",
  slots = c(
    layers = "list"
  )
)

setMethod('print', "NetworkBuilder", function(x){
  cat("Neural Network Builder\n")
  for (i in seq(x@layers)) {
    cat(sprintf('| Layer #%s : ', i))
    print(x@layers[[i]])
    cat('\n')
  }
})

setGeneric('withLayer', function(self, layerBuilder) {
  self@layers %<>% append(layerBuilder)
  return(self)
})

nnet <- NetworkBuilder()
nnet %<>% 
  withLayer( new("LayerBuilder", size = 2)) %>% 
  withLayer( new("LayerBuilder", size = 3)) %>% 
  withLayer( new("LayerBuilder") )

print(nnet)

setGeneric('build', def = function (self) { standardGeneric("build") })

setMethod('build', "NetworkBuilder", function(self) {
  layers <- list()
  for (i in seq(self@layers)) {
    weigths = NULL
    if (i == 1) {
      weigths = NULL
      type = "input"
      
      
      layer <- new(
        "Layer", 
        neurons = matrix(nrow = 1, ncol = self@layers[[i]]@size),
        type = "input"
      )
    } else {
      layer <- new(
        "Layer",
        neurons = matrix(
          nrow = 1, 
          ncol = self@layers[[i]]@size
        ),
        type = ifelse((i == length(self@layers)), "output", "hidden"),
        weights = matrix (
          runif(self@layers[[i]]@size * self@layers[[i-1]]@size, 0, 1), 
          nrow = self@layers[[i]]@size, 
          ncol = self@layers[[i-1]]@size
        ) 
      )
    }
    layers %<>% append(layer)
  }
  return(new("Network", layers = layers))
})

nnet %<>% build
print (nnet)
