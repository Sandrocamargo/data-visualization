# Autor: Sandro Camargo <sandrocamargo@unipampa.edu.br>
# Disciplina de Visualização de Dados
# Gera gráficos para a aula de visualização de dados multivariados

rm(list = ls())

# Define pasta de trabalho
setwd("~/Documents/unipampa/mestrado/disciplinas/visualizacao")

library(datasets)
data(iris)

str(iris)

pdf("images/multivar1.pdf")
plot(iris$Sepal.Length,iris$Sepal.Width, xlab = "Sepal Length", ylab="Sepal Width")
dev.off()
pdf("images/multivar2.pdf")
plot(iris$Petal.Length,iris$Petal.Width, xlab = "Petal Length", ylab="Petal Width")
dev.off()

pdf("images/multivar3.pdf")
plot(iris$Sepal.Length,iris$Sepal.Width, xlab = "Sepal Length", ylab="Sepal Width", col=as.numeric(iris$Species)+1, pch=as.numeric(iris$Species)+1)
legend("topright",legend=levels(iris$Species), col = unique(as.numeric(iris$Species))+1, pch=unique(as.numeric(iris$Species))+1)
dev.off()
pdf("images/multivar4.pdf")
plot(iris$Petal.Length,iris$Petal.Width, xlab = "Petal Length", ylab="Petal Width", col=as.numeric(iris$Species)+1, pch=as.numeric(iris$Species)+1)
legend("topleft",legend=levels(iris$Species), col = unique(as.numeric(iris$Species))+1, pch=unique(as.numeric(iris$Species))+1)
dev.off()

pdf("images/multivar-matrix1.pdf")
pairs(iris[, c(1:4)], main = "Scatter Plot Matrix for Iris Dataset")
dev.off()
pdf("images/multivar-matrix2.pdf")
pairs(iris[, c(1:4)], main = "Scatter Plot Matrix for Iris Dataset", col = as.numeric(iris$Species)+1, pch = as.numeric(iris$Species)+1)
dev.off()

library(corrplot)
iris.cor = cor(iris[,1:4])
pdf("images/multivar-corrplot1.pdf")
corrplot(iris.cor)
dev.off()
pdf("images/multivar-corrplot2.pdf")
corrplot(iris.cor, method=c("number"), type="upper")
dev.off()

pdf("images/multivar-heatmap1.pdf")
palette = colorRampPalette(c("blue", "white", "red")) (10)
heatmap(iris.cor, col=palette, symm=TRUE, cexRow = 0.6, cexCol = 0.6)
dev.off()

# Análise de Componentes Principais
iris.pc <- prcomp(iris[,1:4])
x <- summary(iris.pc)

pdf("images/multivar-pca.pdf")
plot(iris.pc$x[,1],iris.pc$x[,2], col = as.numeric(iris$Species)+1, xlab=paste("PC1(",100*x$importance[2,1],"%)"), ylab=paste("PC2(",100*x$importance[2,2],"%)"), main="Principal Component Analysis (PCA)", pch=as.numeric(iris$Species)+1)
legend("topright",legend=levels(iris$Species), col = unique(as.numeric(iris$Species))+1, pch=unique(as.numeric(iris$Species))+1)
dev.off()

pdf("images/multivar-pc1.pdf")
barplot(iris.pc$rotation[,1], cex.names = .6, main="PC1")
dev.off()
pdf("images/multivar-pc2.pdf")
barplot(iris.pc$rotation[,2], cex.names = .6, main="PC2")
dev.off()


# MDS
# Calcular a matriz de distâncias euclidianas
dist_matrix <- dist(iris[,1:4])

# Aplicar o MDS para reduzir para 2 dimensões
mds_result <- cmdscale(dist_matrix, k = 2)  # k = 2 para reduzir para 2 dimensões

# Converter o resultado do MDS para um data frame
mds_df <- data.frame(mds_result, Species = iris$Species)

# Nomear as colunas do MDS
colnames(mds_df) <- c("Dim1", "Dim2", "Species")

# Visualizar os resultados do MDS com ggplot2
pdf('images/multivar-mds.pdf')
ggplot(mds_df, aes(x = Dim1, y = Dim2, color = Species)) +
  geom_point(size = 3) +
  labs(title = "MDS applied to Iris Dataset", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()
dev.off()

#dimensões do MDS
pdf('images/multivar-mdsdims.pdf')
pairs(cbind(mds_df, iris[,1:4]), 
      main = "Relationship between the MDS Dimensions and the Original Features",
      col = iris$Species)
dev.off()

# line plots
pdf("images/multivar-lineplot.pdf")
plot(iris[,1], type="l", col="blue", ylim = c(min(iris[,1:4]),max(iris[,1:4])), xlab="Samples", ylab="(cm)", main="Setosa/Versicolor/Virgínica")
lines(iris[,2], col="red")
lines(iris[,3], col="green")
lines(iris[,4], col="magenta")
rect(0,0,50,10, col="#FF00000F", border = 0)
rect(50,0,100,10, col="#00FF000F", border = 0)
rect(100,0,150,10, col="#0000FF1F", border = 0)
legend("topleft",legend=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"), col = c("blue","red","green","magenta"), lty=1)
dev.off()

pdf('images/multivar-lineplot1.pdf')
plot((iris$Sepal.Length-mean(iris$Sepal.Length))/sd(iris$Sepal.Length), type="l", col="red", ylim=c(-3,3), xlab="Samples", ylab="Mean +- SD")
lines((iris$Sepal.Width-mean(iris$Sepal.Width))/sd(iris$Sepal.Width), type="l", col="green")
lines((iris$Petal.Length-mean(iris$Petal.Length))/sd(iris$Petal.Length), type="l", col="blue")
lines((iris$Petal.Width-mean(iris$Petal.Width))/sd(iris$Petal.Width), type="l", col="magenta")
rect(0,-10,50,10, col="#FF00000F", border = 0)
rect(50,-10,100,10, col="#00FF000F", border = 0)
rect(100,-10,150,10, col="#0000FF1F", border = 0)
dev.off()

pdf('images/multivar-density.pdf')
par(mfrow=c(2,2))
plot(density(iris$Sepal.Length[iris$Species=="setosa"]), col="red", xlim=c(0,10), xlab="Sepal Length", main="")
lines(density(iris$Sepal.Length[iris$Species=="versicolor"]), col="green")
lines(density(iris$Sepal.Length[iris$Species=="virginica"]), col="blue")
legend("topright", legend=c("Setosa","Versicolor","Virginica"), col = c("red","green","blue"), lty=1)

plot(density(iris$Sepal.Width[iris$Species=="setosa"]), col="red", xlim=c(0,10), xlab="Sepal Width", main="")
lines(density(iris$Sepal.Width[iris$Species=="versicolor"]), col="green")
lines(density(iris$Sepal.Width[iris$Species=="virginica"]), col="blue")
legend("topright", legend=c("Setosa","Versicolor","Virginica"), col = c("red","green","blue"), lty=1)

plot(density(iris$Petal.Length[iris$Species=="setosa"]), col="red", xlim=c(0,10), xlab="Petal Length", main="")
lines(density(iris$Petal.Length[iris$Species=="versicolor"]), col="green")
lines(density(iris$Petal.Length[iris$Species=="virginica"]), col="blue")
legend("topright", legend=c("Setosa","Versicolor","Virginica"), col = c("red","green","blue"), lty=1)

plot(density(iris$Petal.Width[iris$Species=="setosa"]), col="red", xlim=c(0,10), xlab="Petal Width", main="")
lines(density(iris$Petal.Width[iris$Species=="versicolor"]), col="green")
lines(density(iris$Petal.Width[iris$Species=="virginica"]), col="blue")
legend("topright", legend=c("Setosa","Versicolor","Virginica"), col = c("red","green","blue"), lty=1)
dev.off()

# Radar Chart
# Library
library(fmsb)

# Calcular as médias de cada variável para cada espécie
iris_means <- aggregate(. ~ Species, data = iris, mean)

# Para o radarchart, precisamos adicionar duas linhas:
# Uma para o valor máximo e outra para o valor mínimo de cada variável
# Adicionando os valores máximos e mínimos das variáveis
max_min <- data.frame(
  Sepal.Length = c(8, 4),  # Máximo e mínimo possíveis para Sepal.Length
  Sepal.Width = c(4.5, 2),
  Petal.Length = c(7, 1),
  Petal.Width = c(2.5, 0),
  Species = c("Max", "Min")  # Adicionar uma coluna fictícia para Species
)

# Juntar as médias calculadas com os valores máximo e mínimo
iris_data <- rbind(max_min, iris_means)  # Remover a coluna Species ao juntar

# Renomear as linhas com Max, Min, Setosa, Versicolor e Virginica
row.names(iris_data) <- c("Max", "Min", as.character(iris_means$Species))

# Plotar o radarchart
pdf('images/multivar-radar1.pdf')
radarchart(iris_data[,1:4], axistype = 2, 
           # Customizando o gráfico
           pcol = c("red", "blue", "green"), # Cores das espécies
           pfcol = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0,1,0,0.5)), # Cor de preenchimento
           plwd = 2, # Largura das linhas
           cglcol = "darkgrey", cglty = 1, cglwd = 0.8, # Cor e estilo da grade
           axislabcol = "blue", # Cor dos rótulos dos eixos
           vlcex = 0.8 # Tamanho dos rótulos das variáveis
)

# Adiciona uma legenda para identificar as espécies
legend(x = "topright", legend = c("Setosa", "Versicolor", "Virginica"),
       col = c("red", "blue", "green"), pch = 20, pt.cex = 1)
dev.off()

# Plotar o radarchartcirc
pdf('images/multivar-radar2.pdf')
radarchartcirc(iris_data[,1:4], axistype = 2, 
           # Customizando o gráfico
           pcol = c("red", "blue", "green"), # Cores das espécies
           pfcol = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0,1,0,0.5)), # Cor de preenchimento
           plwd = 2, # Largura das linhas
           cglcol = "darkgrey", cglty = 1, cglwd = 0.8, # Cor e estilo da grade
           axislabcol = "blue", # Cor dos rótulos dos eixos
           vlcex = 0.8 # Tamanho dos rótulos das variáveis
)

legend(x = "topright", legend = c("Setosa", "Versicolor", "Virginica"),
       col = c("red", "blue", "green"), pch = 20, pt.cex = 1)
dev.off()

# Parallel Plot
# Libraries
library(GGally)

# Plot
pdf('images/multivar-parplot.pdf')
ggparcoord(iris,
           columns = 1:4, groupColumn = 5
) 
dev.off()

# Andrews plot
library(pracma)

A <- as.matrix(iris[, 1:4])
f <- as.integer(iris[, 5])
pdf('images/multivar-andrews.pdf')
andrewsplot(A, f, style = "pol", npts=50)
dev.off()

pdf('images/multivar-heatmapamostras.pdf')
heatmap(as.matrix(iris[,1:4]), 
        scale = "none",  # Escalando as colunas
        Colv = NULL,         # Não agrupar colunas (variáveis)
        col = heat.colors(100),  # Paleta de cores
        margins = c(5, 10),  # Ajuste de margens
        main = "Heatmap Iris",
        cexCol = 0.6)  # Título do gráfico
dev.off()

library(ggplot2)

# Resumir os dados: calcular a média das variáveis por espécie
iris_means <- aggregate(. ~ Species, data = iris, mean)

# Transformar os dados para o formato long, necessário para ggplot
iris_long <- reshape2::melt(iris_means, id.vars = "Species", 
                            variable.name = "Feature", 
                            value.name = "Value")

# Criar o stacked barplot
pdf('images/multivar-barplot.pdf')
ggplot(iris_long, aes(x = Species, y = Value, fill = Feature)) + 
  geom_bar(stat = "identity") +
  labs(title = "Means of the features by Iris Species", 
       x = "Specie", 
       y = "Means of the features") +
  scale_fill_brewer(palette = "Set2") +  # Usar uma paleta de cores
  theme_minimal()
dev.off()


library(reshape2)

iris$Sample <- 1:nrow(iris)

# Transformar os dados para o formato longo (long format) usando melt
iris_long <- melt(iris, id.vars = c("Sample", "Species"), 
                  variable.name = "Feature", 
                  value.name = "Value")

# Criar o stacked barplot
pdf('images/multivar-barplot2.pdf')
ggplot(iris_long, aes(x = factor(Sample), y = Value, fill = Feature)) + 
  geom_bar(stat = "identity") +
  labs(title = "Features of Iris Dataset", 
       x = "Samples", 
       y = "Feature Values") +
  facet_wrap(~ Species, scales = "free_x") +  # Separar por espécie
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover rótulos das amostras
        axis.ticks.x = element_blank()) +  # Remover os ticks do eixo X
  scale_fill_brewer(palette = "Set3")  # Usar uma paleta de cores
dev.off()


pdf('images/multivar-barplotcircular.pdf')
# Criar o stacked barplot circular
ggplot(iris_long, aes(x = factor(Sample), y = Value, fill = Feature)) + 
  geom_bar(stat = "identity", width = 1) +  # Largura da barra ajustada
  coord_polar(start = 0) +  # Transformar o gráfico em coordenadas polares
  labs(title = "Circular Stacked Barplot das Variáveis do Conjunto Iris", 
       x = "", 
       y = "Valor das Variáveis") +
  facet_wrap(~ Species, scales = "free_x") +  # Separar por espécie
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remover rótulos das amostras
        axis.ticks.x = element_blank(),  # Remover ticks do eixo X
        panel.grid = element_blank(),  # Remover grades do fundo
        plot.title = element_text(hjust = 0.5)) +  # Centralizar o título
  scale_fill_brewer(palette = "Set3")  # Usar uma paleta de cores
dev.off()

pdf('images/multivar-hist.pdf')
par(mfrow=c(2,2))
hist(iris$Sepal.Length[iris$Species=="setosa"], col ="#FF000033", xlim=c(4,8), ylim=c(0,20), xlab="Sepal.Length", main="")
hist(iris$Sepal.Length[iris$Species=="versicolor"], col ="#00FF0033", add=TRUE)
hist(iris$Sepal.Length[iris$Species=="virginica"], col ="#0000FF33", add=TRUE)
legend("topright",legend=levels(iris$Species), col = c("#FF000033","#00FF0033","#0000FF33"), pch=15)

hist(iris$Sepal.Width[iris$Species=="setosa"], col ="#FF000033", xlim=c(2,5), ylim=c(0,25), xlab="Sepal.Width", main="")
hist(iris$Sepal.Width[iris$Species=="versicolor"], col ="#00FF0033", add=TRUE)
hist(iris$Sepal.Width[iris$Species=="virginica"], col ="#0000FF33", add=TRUE)
legend("topright",legend=levels(iris$Species), col = c("#FF000033","#00FF0033","#0000FF33"), pch=15)

hist(iris$Petal.Length[iris$Species=="setosa"], col ="#FF000033", xlim=c(1,7), ylim=c(0,25), xlab="Petal.Length", main="")
hist(iris$Petal.Length[iris$Species=="versicolor"], col ="#00FF0033", add=TRUE)
hist(iris$Petal.Length[iris$Species=="virginica"], col ="#0000FF33", add=TRUE)
legend("topright",legend=levels(iris$Species), col = c("#FF000033","#00FF0033","#0000FF33"), pch=15)

hist(iris$Petal.Width[iris$Species=="setosa"], col ="#FF000033", xlim=c(0,3), ylim=c(0,35), xlab="Petal.Width", main="")
hist(iris$Petal.Width[iris$Species=="versicolor"], col ="#00FF0033", add=TRUE)
hist(iris$Petal.Width[iris$Species=="virginica"], col ="#0000FF33", add=TRUE)
legend("topright",legend=levels(iris$Species), col = c("#FF000033","#00FF0033","#0000FF33"), pch=15)
dev.off()

pdf('images/multivar-densityarea.pdf')
par(mfrow=c(2,2))
plot(density(iris$Sepal.Length[iris$Species=="setosa"]), col="#FF000033", xlim=c(4,9), xlab="Sepal Length", main="")
lines(density(iris$Sepal.Length[iris$Species=="versicolor"]), col="#00FF0055")
lines(density(iris$Sepal.Length[iris$Species=="virginica"]), col="#0000FF44")
polygon(density(iris$Sepal.Length[iris$Species=="setosa"]), col = "#FF000033", border=FALSE)
polygon(density(iris$Sepal.Length[iris$Species=="versicolor"]), col = "#00FF0055", border=FALSE)
polygon(density(iris$Sepal.Length[iris$Species=="virginica"]), col = "#0000FF44", border=FALSE)
legend("topright", legend=c("setosa","versicolor","virginica"), col = c("#FF000033","#00FF0055","#0000FF44"), pch=15)

plot(density(iris$Sepal.Width[iris$Species=="setosa"]), col="#FF000033", xlim=c(1,5), ylim=c(0,1.3), xlab="Sepal Width", main="")
lines(density(iris$Sepal.Width[iris$Species=="versicolor"]), col="#00FF0055")
lines(density(iris$Sepal.Width[iris$Species=="virginica"]), col="#0000FF44")
polygon(density(iris$Sepal.Width[iris$Species=="setosa"]), col = "#FF000033", border=FALSE)
polygon(density(iris$Sepal.Width[iris$Species=="versicolor"]), col = "#00FF0055", border=FALSE)
polygon(density(iris$Sepal.Width[iris$Species=="virginica"]), col = "#0000FF44", border=FALSE)
legend("topright", legend=c("setosa","versicolor","virginica"), col = c("#FF000033","#00FF0055","#0000FF44"), pch=15)

plot(density(iris$Petal.Length[iris$Species=="setosa"]), col="#FF000033", xlim=c(0,8), xlab="Petal Length", main="")
lines(density(iris$Petal.Length[iris$Species=="versicolor"]), col="#00FF0055")
lines(density(iris$Petal.Length[iris$Species=="virginica"]), col="#0000FF44")
polygon(density(iris$Petal.Length[iris$Species=="setosa"]), col = "#FF000033", border=FALSE)
polygon(density(iris$Petal.Length[iris$Species=="versicolor"]), col = "#00FF0055", border=FALSE)
polygon(density(iris$Petal.Length[iris$Species=="virginica"]), col = "#0000FF44", border=FALSE)
legend("topright", legend=c("setosa","versicolor","virginica"), col = c("#FF000033","#00FF0055","#0000FF44"), pch=15)

plot(density(iris$Petal.Width[iris$Species=="setosa"]), col="#FF000033", xlim=c(0,3), xlab="Petal Width", main="")
lines(density(iris$Petal.Width[iris$Species=="versicolor"]), col="#00FF0055")
lines(density(iris$Petal.Width[iris$Species=="virginica"]), col="#0000FF44")
polygon(density(iris$Petal.Width[iris$Species=="setosa"]), col = "#FF000033", border=FALSE)
polygon(density(iris$Petal.Width[iris$Species=="versicolor"]), col = "#00FF0055", border=FALSE)
polygon(density(iris$Petal.Width[iris$Species=="virginica"]), col = "#0000FF44", border=FALSE)
legend("topright", legend=c("setosa","versicolor","virginica"), col = c("#FF000033","#00FF0055","#0000FF44"), pch=15)
dev.off()


# Criar o gráfico de densidade 2D com Petal.Length e Petal.Width
pdf('images/multivar-density2d.pdf')
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_raster(aes(fill = Species), interpolate = TRUE) + 
  geom_point(alpha = 0.5) +  # Adicionar pontos com transparência
  geom_density_2d() +  # Adicionar as linhas de contorno da densidade 2D
  labs(title = "Density Plot 2D: Petal Length vs Petal Width",
       x = "Petal Length",
       y = "Petal Width") +
  theme_minimal() +
  scale_fill_manual(values = c("setosa" = "red", 
                               "versicolor" = "green", 
                               "virginica" = "blue")) +
  scale_color_manual(values = c("setosa" = "red", 
                                "versicolor" = "green", 
                                "virginica" = "blue")) 
dev.off()

pdf('images/multivar-density2d2.pdf')
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_raster(aes(fill = Species), interpolate = TRUE) + 
  geom_point(alpha = 0.5) +  # Adicionar pontos com transparência
  geom_density_2d() +  # Adicionar as linhas de contorno da densidade 2D
  labs(title = "Density Plot 2D: Sepal Length vs Sepal Width",
       x = "Sepal Length",
       y = "Sepal Width") +
  theme_minimal() +
  scale_fill_manual(values = c("setosa" = "red", 
                               "versicolor" = "green", 
                               "virginica" = "blue")) +
  scale_color_manual(values = c("setosa" = "red", 
                                "versicolor" = "green", 
                                "virginica" = "blue")) 
dev.off()