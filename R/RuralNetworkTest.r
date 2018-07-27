source('R/RuralNetwork.r')

nnet <- RuralNetworkBuilder$new()$
  withLayer(LayerBuilder$new(2))$
  withLayer(LayerBuilder$new(3))$
  withLayer(LayerBuilder$new(1))$
  build()

# print(nnet)
# print(nnet$layer(2)$weights())
# print(nnet$layer(3)$weights())