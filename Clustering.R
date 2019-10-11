# importing library

library(dplyr)
library(ggplot2)
library(factoextra)
library(clValid)

# importing dataset into R

spine<-read.csv('2Dataset_spine.csv')

# Data Exploration

str(spine)
summary(spine)

## correlation plot

corrplot::corrplot(cor(spine),method = "pie", type = "upper")
windows(width = 40,height = 40)
psych::pairs.panels(x = spine,method = "pearson",ellipses = TRUE,density = TRUE,digits = TRUE,
                    hist.col = "blue",smoother = TRUE)

spine$tilt_slope <- spine$pelvic_tilt + spine$sacral_slope
head(spine)
spine %>% select(c(pelvic_incidence,tilt_slope)) %>% reshape2::melt() %>%
  ggplot(aes(x=value,color = variable)) + geom_histogram(positio = "dodge") + 
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value") +
  theme_minimal()

# removing last column 
spine <- spine[,1:12]

## column 1 to 6 have correlation
## distribution plot

spine %>% select(c(pelvic_incidence,pelvic_tilt,lumbar_lordosis_angle,sacral_slope,pelvic_radius,
                   degree_spondylolisthesis,pelvic_slope,Direct_tilt,thoracic_slope,cervical_tilt,
                   sacrum_angle,scoliosis_slope)) %>% reshape2::melt() %>%
  ggplot(aes(x=value,color = variable)) + geom_histogram() + 
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(x = "Variable Value") +
  theme_minimal()

## Outlier Detection

spine %>% select(c(pelvic_incidence,pelvic_tilt,lumbar_lordosis_angle,sacral_slope,pelvic_radius,
                   degree_spondylolisthesis,pelvic_slope,Direct_tilt,thoracic_slope,cervical_tilt,
                   sacrum_angle,scoliosis_slope)) %>% reshape2::melt() %>% 
  ggplot(aes(x=variable,y = value, color = variable)) +   
  geom_boxplot(outlier.color = "black")+  geom_jitter() + 
  facet_wrap(~variable, scales = "free", ncol = 6) +
  labs(x = "Variable Value") +
  theme_minimal()

boxplot(spine$degree_spondylolisthesis)
stripchart(spine$degree_spondylolisthesis,method="jitter",add = TRUE, pch = 20, col = 'orange',
           cex=1.5,vertical = TRUE, 
           xlab = "Degree of Spondylolisthesis", ylab ="Degree")

## bivariate outlier detetecion in degree of spondylolisthesis

spine %>% select(c(pelvic_incidence,pelvic_tilt,lumbar_lordosis_angle,sacral_slope,pelvic_radius,
                   degree_spondylolisthesis)) %>% reshape2::melt(id.vars ="degree_spondylolisthesis") %>%
  ggplot(aes(x=value,y = degree_spondylolisthesis, color = variable)) + 
  geom_point(alpha = 0.7,position = "jitter")+ 
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Degree of Spondylolisthesis") +
  theme_minimal()

## As seen from plots one point is outlier for degree of spondylolisthesis and not for other variables
## removing 1 row as this maybe an entry error 

spine <- subset(spine, degree_spondylolisthesis < 400)

## not removing any outliers to consider as they maybe extreme cases of abnormality

# scaling dataset

spine_scale<-scale(spine,center = TRUE, scale = TRUE)
spine_scale <- as.data.frame(spine_scale)

# checking if data is clusterable or not using hopkins statistic

get_clust_tendency(spine_scale, n = 50, gradient = list(low = "orange",  high = "blue"))

# Dataset is clusterable as hopkins statistic value is below 0.5 
# finding number of cluster

# internal comparsion between different clustering method on various metrics

validation_test <- clValid::clValid(spine_scale, nClust = 2:4, clMethods = c("hierarchical","kmeans","pam"),
                                    method = "complete" ,
                                    validation = c("stability","internal"))
summary(validation_test)


# nbclust method for wss, silhouette and gap statistic

fviz_nbclust(spine_scale, kmeans, method = "wss") +  geom_vline(xintercept = 2, linetype = 2) + 
  labs(subtitle = "Elbow method")

fviz_nbclust(spine_scale, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

set.seed(123)
fviz_nbclust(spine_scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 100) + 
  labs(subtitle = "Gap statistic method")

## optimal number of cluster is 2 and optimal method is Kmeans 
## Computing dissimilarity matrix

diss <- get_dist(spine_scale, method = "euclidean")

# Kmeans clustering

set.seed(150)
kmeans_model <- kmeans(spine_scale, 2)
summary(kmeans_model)
kmeans_model$size
factoextra::fviz_cluster(kmeans_model, spine_scale,  geom = "point", ellipse= T, show.clust.cent = T,
                         palette = "jco", ggtheme = theme_minimal())

# checking silhoutte width 

sp_kmeans <- silhouette(kmeans_model$cluster,diss) 
plot(sp_kmeans) 

# as output labels are not available we select model with highest silhouette width 
## it is metric which measures the cluster integrity based on inter cluster variance and intra cluster variance
## value should be greater than 0.5
