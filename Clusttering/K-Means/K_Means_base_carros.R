library(factoextra)
library(ggplot2)

#Carregamento dos dados
data("mtcars")
df=scale(mtcars)
head(df, n=3)

# Número ótimo de clusters
fviz_nbclust(df, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

# Clusterização k-means
set.seed(123)
km.res=kmeans(df, 4, nstart=25)
print(km.res)

aggregate(mtcars, by=list(cluster=km.res$cluster), mean)

mtcars2=cbind(mtcars, cluster=km.res$cluster)
head(mtcars2)

km.res$centers

# Vizualizando os clusters
fviz_cluster(km.res, data=mtcars2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)