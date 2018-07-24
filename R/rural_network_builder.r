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
    neurons = "list",
    weights = "matrix"
  ),
  prototype = prototype(neurons = list(), weights = matrix(nrow = 0, ncol = 0))
)

setMethod('print','Layer', function(x){
  cat(sprintf('Layer with %s neurons\n', length(x@neurons)))
  for (i in seq(x@neurons)) {
    print(x@neurons[[i]])
  }
  cat('\n')
  # print(x@weights)
})

# layer = Layer(neurons = c(new(Class = "Neuron", value=2), Neuron(value = 42))) #2 méthode pour instancier l'objet
# print(layer)

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