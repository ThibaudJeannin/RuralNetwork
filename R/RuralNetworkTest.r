nnet <- RuralNetworkBuilder$new()$
  withLayer(LayerBuilder$new(3,activation_function = "linear"))$
  withLayer(LayerBuilder$new(4,activation_function = "relu"))$
  withLayer(LayerBuilder$new(1,activation_function = "sigmoid"))$
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
  output_row <- data[i,] %>% subset(select=targets) %>% unlist
  result <- nnet$process(input_row)
  cat(sprintf('result : %f, ground truth : %f\n', result, output_row))
}