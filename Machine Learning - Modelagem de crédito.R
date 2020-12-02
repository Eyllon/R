
require(caret)
require(ranger)


loan = read.csv('C:\\Users\\fabri\\Desktop\\Projetos\\Modelagem de crédito\\credito.csv' , header = T)

# OBS:  a base não possui missings
str(loan)
# Checando número de defaults
sum(loan$default)
# Transformadno em variável factor
loan$default = factor(loan$default , levels = c(0,1) , labels = c('N','D'))
table(loan$default)

# DIVIDINDO BASE EM TREINAMENTO E TESTE

index = createDataPartition(loan$default, p = 0.75, list = FALSE)

loan.tr = loan[index,]

loan.tst = loan[-index,]


# Criando condições de treinamento padronizada
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)


# PREVENDO DEFAULT USANDO GLMNET
model_glmnet = train(
  default ~.,
  loan.tr,
  method = 'glmnet',
  trControl=myControl,
  tuneGrid = expand.grid(
    alpha = 0:1,
    lambda = seq(.0001 , 1 , length = 20)
  ),
  metric = 'ROC'
)

p.glmnet = predict(model_glmnet , loan.tst , type = 'raw')



# PREVENDO DEFAULT USANDO LOGIT
model_logit = train(
  default ~.,
  loan.tr,
  method = 'glm',
  trControl=myControl,
  metric = 'ROC'
)

p.logit = predict(model_logit , loan.tst , type = 'raw')



# PREVENDO DEFAULT USANDO RANDOM FOREST
model_rf = train(
  default ~.,
  loan.tr,
  method = 'ranger',
  trControl=myControl
)


p.forest = predict(model_rf , loan.tst , type = 'raw')


result = data.frame(
  c(
    sum(diag(table(p.glmnet , loan.tst$default)))/nrow(loan.tst),
    sum(diag(table(p.forest , loan.tst$default)))/nrow(loan.tst),
    sum(diag(table(p.logit , loan.tst$default)))/nrow(loan.tst)))
row.names(result) = c('glmnet' , 'RF' , 'logit')
colnames(result) = 'Acerto'
print(result)
