# # LDA
# library(topicmodels)
# thing_tm <- LDA(dtm.issues, k=5, method="Gibbs",
#                 control = list(seed = 1234, burnin = 1000,
#                                thin = 100, iter = 1000))
# thing_tm1 <- LDA(dtm.issues, k=3, method="VEM",
#                  control = list(seed=1234, estimate.alpha=FALSE))
# topics(thing_tm1, 1)
# terms(thing_tm1, 10)
# 
# topics.imputed <- data_frame(topic = 1:4,
#                              title = c("Human rights, policy, and research",
#                                        "Health and development", 
#                                        "Health and advocacy",
#                                        "Advocacy"))
# 
# issue.topics <- issue.corpus.df %>%
#   mutate(topic = topics(thing_tm1, 1)) %>%
#   left_join(topics.imputed, by="topic")
# document.topics <- topics(thing_tm1, 1)
# 
# VEM_fixed = LDA(JSS_dtm, k = k,
#                 + control = list(estimate.alpha = FALSE, seed = SEED))
# 
# # Latent semantic analysis
# # https://meefen.github.io/blog/2013/03/11/analyze-text-similarity-in-r-latent-semantic-analysis-and-multidimentional-scaling/
# library(lsa)
# tdm.lsa <- lw_bintf(tdm.issues.mat) * gw_idf(tdm.issues.mat)
# lsa.space <- lsa(tdm.lsa)
# dist.mat.lsa <- dist(t(as.textmatrix(lsa.space)))  # compute distance matrix
# 
# fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
# points <- bind_cols(issue.corpus.df, 
#                     data_frame(x = fit$points[, 1], y = fit$points[, 2]))
# ggplot(points, aes(x = x, y = y)) + 
#   geom_point()
# 
# # K-means clustering
# # http://randyzwitch.com/rsitecatalyst-k-means-clustering/
# # Find optimal number of k clusters
# # http://stackoverflow.com/a/15376462/120898
# # Different cost values
# possible.clusters <- 1:25
# 
# # Sum of squared error for kmeans + elbow method (look for where slope changes)
# kmeans.cost <- possible.clusters %>%
#   map(function(i) kmeans(x=tdm.issues, centers=i, iter.max=100)$tot.withinss)
# 
# # Largest average silhouette width for pam (look for max avg.width)
# pam.cost <- possible.clusters %>%
#   map(function(i) pam(x=tdm.issues, k=i)$silinfo$avg.width)
# 
# # Combine costs for each algorithm into one dataframe
# cost.df <- data_frame(cluster = possible.clusters, 
#                       kmeans.cost = unlist(kmeans.cost),
#                       pam.cost = c(NA, unlist(pam.cost))) %>%
#   mutate(possible.group = case_when(
#     .$cluster < 5 ~ 1,
#     .$cluster >=5 & .$cluster <= 12 ~ 2,
#     .$cluster > 12 ~ 3
#   )) %>%
#   mutate(possible.group = factor(possible.group))
# 
# # kmeans
# ggplot(cost.df, aes(x=cluster, y=kmeans.cost)) + 
#   geom_line() +
#   geom_smooth(aes(colour=possible.group), method="lm", se=FALSE) +
#   labs(x="Clusters", y="Within-cluster sum of squares",
#        title="Optimal cluster size (kmeans)") + 
#   guides(colour=FALSE) +
#   theme_ath()
# 
# # pam
# ggplot(na.omit(cost.df), aes(x=cluster, y=pam.cost)) +
#   geom_line() +
#   coord_cartesian(xlim=c(0, 25)) +
#   labs(x="Clusters", y="Average silouette width",
#        title="Optimal cluster size (pam)") +
#   theme_ath()
# 
# # Create dataframe of clusters and term frequency
# final.cluster <- pam(tdm.issues, k=4)$clustering
# issues.clustered <- data_frame(cluster = final.cluster,
#                                term = names(final.cluster)) %>%
#   right_join(term.frequency, by="term")
