################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
              "splines","reshape2","PerformanceAnalytics","correlation","see",
              "ggraph","ggplot","ggplot2","psych","nortest","readxl","rgl","car","ggside","tidyquant","olsrr",
              "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
              "equatiomatic","misc3d","plot3D","cluster","factoextra","ade4")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
#               CARREGAMENTO DO DATASET 'INSURANCE'                            #           
################################################################################

insurance <- read_excel("insurance.xlsx")

# Estatísticas Univariadas

summary(insurance)

# Tipos das variáveis
glimpse(insurance)

# Transformando variáveis 'imc' e 'custos_seguro" em tipo numérico

insurance$imc <- as.numeric(insurance$imc)
insurance$custos_seguro <- as.numeric(insurance$custos_seguro)

# Transformando variáveis 'sexo', 'fumante" e 'região' em tipo factor

insurance$sexo <- as.factor(insurance$sexo)
insurance$fumante <- as.factor(insurance$fumante)
insurance$regiao <- as.factor(insurance$regiao)

# Observando os rótulos de de cada variável categórica
levels(glimpse(insurance[, c('sexo','fumante','regiao')]))
# Tabela de Frequencias
table(insurance[, c('sexo','fumante','regiao')])

# Eliminar valores vazios.

insurance <- na.omit(insurance)

################################################################################
#                         ANALISANDO OS DADOS                                  #           
################################################################################

# Histograma da variável dependente

ggplot(insurance, aes(x= custos_seguro)) +geom_histogram()

# Tabela dos dados

insurance %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 24)

# Estudo das Correlações

# Diagrama de inter-relação
insurance %>%
  correlation(method = "pearson") %>%
  plot()

# Chart Correlation

chart.Correlation(insurance[, c("idade",'imc','quantidade_filhos',"custos_seguro")]
                  , histogram =TRUE)


################################################################################
#                         TRANSFORMANDO OS DADOS                               #           
################################################################################

# Procedimento n-1 dummies

insurance_dummies <- dummy_columns(.data = insurance,
                                   select_columns = c("sexo","fumante","regiao"),
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy= T)

# Padronização de variáveis

scale_insurance <- as.data.frame(scale(select(insurance,idade,imc,quantidade_filhos,custos_seguro)))


################################################################################
#                         CRIANDO NOVAS VARIÁVEIS                              #           
################################################################################

####################ESQUEMA DE AGLOMERAÇÃO k-MEANS#############################


# Elaboração da clusterização não hieráquica k-means

cluster_kmeans <- kmeans(scale_insurance,
                         centers = 5)

# Criando variável categórica para indicação do cluster no banco de dados
insurance_dummies$cluster_K <- factor(cluster_kmeans$cluster)

# Método de Elbow para identificação do número ótimo de clusters
fviz_nbclust(scale_insurance, kmeans, method = "wss", k.max = 10)

# Visualização da base de dados
RegionalVarejista %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Análise de variância de um fator (ANOVA)

# ANOVA da variável 'idade'
summary(anova_idade <- aov(formula = idade ~ cluster_K,
                                 data = insurance_dummies))

# ANOVA da variável 'imc'
summary(anova_imc <- aov(formula = imc ~ cluster_K,
                           data = insurance_dummies))

# ANOVA da variável 'quantidade_filhos'
summary(anova_filhos <- aov(formula = quantidade_filhos ~ cluster_K,
                         data = insurance_dummies))

# ANOVA da variável 'custos_seguro'
summary(anova_custo <- aov(formula = custos_seguro ~ cluster_K,
                            data = insurance_dummies))


# Dummificando clusters

insurance_dummies <- dummy_columns(.data = insurance_dummies,
                                   select_columns = c("cluster_K"),
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy= T)


####################ESQUEMA DE AGLOMERAÇÃO BINARIA#############################



################################################################################
#                         ESTIMANDO MODELOS DE REGRESSÃO                       #           
################################################################################


# Modelagem com todas as variáveis 

modelo_insurance_dummies <- lm(custos_seguro ~ ., insurance_dummies)

summary(modelo_insurance_dummies)

# Modelagem após procedimento Step_Wise

modelo_insurance_step <- step(modelo_insurance_dummies, k = 3.841459)

summary(modelo_insurance_step)

# Teste de SHAPIRO-FRANCIA para conferência de aderência dos resíduos à
# à normalidade nos modelos já criados.

sf.test(modelo_insurance_step$residuals)
sf.test(modelo_insurance_dummies$residuals)

# Plotando Gráfico dos resíduos

insurance %>%
  mutate(residuos = modelo_insurance_step$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

# Kernel Density para saber o quanto a distribuição dos resíduos gerados
# se aproximam da distribuição normal.

insurance_dummies %>%
  ggplot() +
  geom_density(aes(x = modelo_insurance_step$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()


#########################################################################
# Os modelos gerados até então não possuem aderência a normalidade de seus
# resíduos gerados o que limita a capacidade de previsão do modelo.
# Teste Shapiro Francia < 0,05 rejeitando a hipótise nula de normalidade
##########################################################################

# Diagnóstico de Heterocedasticidade

ols_test_breusch_pagan(modelo_insurance_step)

#########################################################################
#A variância dos termos de erro não é constante, ou seja, os termos de erro são 
#uma função de uma ou mais variáveis explicativas (erros heterocedásticos).
# Omissão de variável relevante!
# Resultado esperado devido a não aderência à normalidade.
##########################################################################

#Adicionando fitted values e resíduos do modelo 'modelo_insurance_step'
#no dataset 'insurance_dummies'

insurance_dummies$fitted_step <- modelo_insurance_step$fitted.values
insurance_dummies$residuos_step <- modelo_insurance_step$residuals

# Gráfico de comportamento dos resíduos em relação ao fitted values

insurance_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()


################################################################################
#         ESTIMANDO MODELOS DE REGRESSÃO COM TRANSFORMAÇÃO BOX COX             #       #           
################################################################################

# Cálculo do lambda de Box-Cox
lambda_BC <- powerTransform(insurance$custos_seguro)
lambda_BC

# Inserir o lambda na nova base de dados para a estimação de um novo modelo.

insurance_dummies$custos_seguro_box <- (((insurance$custos_seguro ^ lambda_BC$lambda) - 1) / 
                                          lambda_BC$lambda)

# Estimar novo modelo múltiplo com dummies e transformação Box Cox.

modelo_insurance_box <- lm(formula = custos_seguro_box ~. -custos_seguro -fitted_step
                           -residuos_step, 
                           data = insurance_dummies)

# Parâmetros


summary(modelo_insurance_box)

# Aplicar o StepWise no novo modelo

modelo_insurance_box_step <- step(modelo_insurance_box, k = 3.841459)
summary(modelo_insurance_box_step)

# Testar a aderência a  normalidade dos resíduos com Shapiro-Francia

sf.test(modelo_insurance_box_step$residuals)

# Gráfico dos novos resíduos do modelo 

insurance_dummies %>%
  mutate(residuos = modelo_insurance_box_step$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_insurance_box_step$residuals),
                            sd = sd(modelo_insurance_box_step$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()


#Kernel density estimation (KDE)
insurance_dummies %>%
  ggplot() +
  geom_density(aes(x = modelo_insurance_box_step$residuals), fill = "#440154FF") +
  labs(x = "Resíduos do Modelo Stepwise com Transformação de Box-Cox",
       y = "Densidade") +
  theme_bw()

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(modelo_insurance_box_step)

#Adicionando fitted values e resíduos do novo modelo com BOX COX

insurance_dummies$fitted_step_box <- modelo_insurance_box_step$fitted.values
insurance_dummies$yhat_step_modelo_bc <- (((modelo_insurance_box_step$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))
insurance_dummies$residuos_step_box <- modelo_insurance_box_step$residuals

#Gráfico que relaciona resíduos e fitted values do modelo com BOX COX

insurance_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step_box, y = residuos_step_box),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise com Transformação de Box-Cox",
       y = "Resíduos do Modelo Stepwise com Transformação de Box-Cox") +
  theme_bw()


# Analisando os fitted values do modelo

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
insurance_dummies %>%
  ggplot() +
  geom_smooth(aes(x = custos_seguro, y = yhat_step_modelo_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = custos_seguro, y = yhat_step_modelo_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = custos_seguro, y = custos_seguro), method = "lm", formula = y ~ x,
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Real", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


# Comparando gráfico com modelo sem clusterização




insurance_dummies %>%
  ggplot() +
  geom_smooth(aes(x = custos_seguro, y = fitted_step, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = custos_seguro, y = fitted_step),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = custos_seguro, y = custos_seguro), method = "lm", formula = y ~ x,
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Valor Real", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")
