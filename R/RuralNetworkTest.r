nnet <- RuralNetworkBuilder$new()$
  withLayer(LayerBuilder$new(3))$
  withLayer(LayerBuilder$new(4))$
  withLayer(LayerBuilder$new(1))$
  build()

print(nnet)

data <- data.frame(
  col1=c(0,0,1,1),
  col2=c(0,1,0,1),
  col3=c(1,1,1,1),
  res =c(0,1,1,0)
)

predictors = c('col1','col2','col3')
targets = c('res')

for(i in 1:nrow(data)){
  input_row <- data[i,] %>% subset(select=predictors) %>% unlist
  result <- nnet$process(input_row)
  print(result)
}