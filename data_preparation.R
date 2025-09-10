# libraries for data manipulation and dataviz
library(foreign)
library(tidyverse)
library(Rtsne)
library(uwot)
library(dbscan)
library(NbClust)

# uploading .por file 
# use original IPIP-NEO-300 data from the Johnson's IPIP-NEO data repository available at https://osf.io/wxvth/files/osfstorage 
data <- foreign::read.spss("data/IPIP300.por", to.data.frame = TRUE)

# assigning items to individual traits
o_items <- c(3, 8, 13, 18, 53, 28, 33, 68, 43, 138, 203, 58, 63, 188, 223, 168, 233, 148, 93, 218, 283, 288, 263, 268, 23, 38, 48, 73, 78, 83, 88, 98, 103, 108, 113, 118, 123, 128, 133, 143, 153, 158, 163, 173, 178, 183, 193, 198, 208, 213, 228, 238, 243, 248, 253, 258, 273, 278, 293, 298)
c_items <- c(5, 40, 45, 50, 55, 120, 35, 160, 105, 140, 145, 150, 65, 190, 165, 260, 205, 210, 155, 220, 195, 290, 265, 270, 10, 15, 20, 25, 30, 60, 70, 75, 80, 85, 90, 95, 100, 110, 115, 125, 130, 135, 170, 175, 180, 185, 200, 215, 225, 230, 235, 240, 245, 250, 255, 275, 280, 285, 295, 300)
e_items <- c(2, 7, 12, 17, 22, 27, 62, 37, 42, 47, 52, 57, 212, 157, 132, 77, 142, 147, 272, 247, 162, 167, 172, 177, 32, 67, 72, 82, 87, 92, 97, 102, 107, 112, 117, 122, 127, 137, 152, 182, 187, 192, 197, 202, 207, 217, 222, 227, 232, 237, 242, 252, 257, 262, 267, 277, 282, 287, 292, 297)
a_items <- c(4, 99, 74, 169, 144, 29, 34, 159, 104, 199, 174, 59, 64, 249, 194, 229, 204, 149, 184, 279, 284, 259, 264, 239, 9, 14, 19, 24, 39, 44, 49, 54, 69, 79, 84, 89, 94, 109, 114, 119, 124, 129, 134, 139, 154, 164, 179, 189, 209, 214, 219, 224, 234, 244, 254, 269, 274, 289, 294, 299)
n_items <- c(1, 6, 11, 76, 111, 26, 31, 36, 41, 106, 171, 56, 61, 126, 71, 136, 201, 86, 91, 216, 251, 256, 231, 176, 16, 21, 46, 51, 66, 81, 96, 101, 116, 121, 131, 141, 146, 151, 156, 161, 166, 181, 186, 191, 196, 206, 211, 221, 226, 236, 241, 246, 261, 266, 271, 276, 281, 286, 291, 296)

# adding 'I' prefix
o_items <- paste0("I", o_items)
c_items <- paste0("I", c_items)
e_items <- paste0("I", e_items)
a_items <- paste0("I", a_items)
n_items <- paste0("I", n_items)

# selecting a random sample of 10,000 respondents and computing scores for all five major scales  
set.seed(2025)
mydata_sample <- data %>% 
  dplyr::select(all_of(c(o_items, c_items, e_items , a_items, n_items))) %>% 
  tidyr::drop_na() %>% 
  dplyr::rename_with(~ str_replace_all(., "I", "O"), .cols = all_of(o_items)) %>% 
  dplyr::rename_with(~ str_replace_all(., "I", "C"), .cols = all_of(c_items)) %>% 
  dplyr::rename_with(~ str_replace_all(., "I", "E"), .cols = all_of(e_items)) %>% 
  dplyr::rename_with(~ str_replace_all(., "I", "A"), .cols = all_of(a_items)) %>% 
  dplyr::rename_with(~ str_replace_all(., "I", "N"), .cols = all_of(n_items)) %>% 
  dplyr::sample_n(10000,replace = FALSE) %>% 
  dplyr::mutate(
    Openness = rowSums(select(., starts_with("O"))),
    Conscientiousness = rowSums(select(., starts_with("C"))),
    Extraversion = rowSums(select(., starts_with("E"))),
    Agreeableness = rowSums(select(., starts_with("A"))),
    Neuroticism = rowSums(select(., starts_with("N")))
  ) %>% 
  dplyr::select(
    Openness, Conscientiousness, Extraversion, Agreeableness, Neuroticism
  )


# glimpse(mydata_sample)

# reducing data dimensionality
reduction_method <- "UMAP" # UMAP, tSNE, PCA

# standardizing the data
scaled_data <- scale(mydata_sample)

if (reduction_method == "PCA") {
  res <- prcomp(scaled_data)
  reduced_data <- as.data.frame(res$x[, 1:3])
} else if (reduction_method == "UMAP") { # Preserves local neighborhoods and more global topology; typically preserves inter-cluster relations and continuums better.
  res <- uwot::umap(scaled_data, n_components = 3, n_neighbors = 5, min_dist = 0, seed = 2025) # setting params to get well-separated clusters
  reduced_data <- as.data.frame(res)
} else if (reduction_method == "tSNE") { # Preserve local neighborhoods; global layout often unreliable; excellent local separation but inter-cluster distances are not meaningful.
  set.seed(2025)
  res <- Rtsne::Rtsne(unique(scaled_data), dims = 3, perplexity = 30, check_duplicates = FALSE) # setting param to balance local and global view
  reduced_data <- as.data.frame(res$Y)
} else {
  stop("Invalid reduction method selected.")
}
colnames(reduced_data) <- c("Dim1", "Dim2", "Dim3")


# finding clusters
clustering_method <- "hdbscan" # hdbscan, kmeans, hclust

if (clustering_method == "kmeans") {
  # K-Means requires pre-specifying k - finding optimal value of k using one of the following approaches:
  # silhouette — interpretable, but computationaly demanding (maximize).
  # ch (Calinski–Harabasz) — strong variance-ratio criterion; often tracks true k (maximize).
  # db (Davies–Bouldin) — complements the above; penalizes overlap (minimize).
  # ratkowsky - variance-based; reasonable with roughly spherical clusters (maximize).
  res_nb <- NbClust(reduced_data, distance = "euclidean", min.nc = 2, max.nc = 30, method = "kmeans", index = "ch")
  optimal_k <- res_nb$Best.nc[['Number_clusters']]
  res <- kmeans(reduced_data, centers = optimal_k, nstart = 25)
  cluster_assignments <- res$cluster
  
} else if (clustering_method == "hclust") {
  # Hierarchical clustering also requires cutting the tree at a specified k.
  res_nb <- NbClust(reduced_data, distance = "euclidean", min.nc = 2, max.nc = 30, method = "ward.D2", index = "ch")
  optimal_k <- res_nb$Best.nc[['Number_clusters']]
  res <- hclust(dist(reduced_data), method = "ward.D2")
  cluster_assignments <- cutree(res, k = optimal_k)
  
} else if (clustering_method == "hdbscan") {
  # HDBSCAN determines the number of clusters automatically.
  # The 'minPts' parameter is important to tune - here 1% of the population/sample
  res <- hdbscan(reduced_data, minPts = nrow(mydata_sample)*0.01)
  cluster_assignments <- res$cluster
  # Note: Cluster '0' represents points classified as noise.
  
} else {
  stop("Invalid clustering method selected. Choose from 'kmeans', 'hclust', 'hdbscan'.")
}


# combining all data
final_data <- mydata_sample %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    Dim1 = reduced_data$Dim1,
    Dim2 = reduced_data$Dim2,
    Dim3 = reduced_data$Dim3,
    cluster = as.factor(cluster_assignments)
  )

# pre-calculating cluster percentile profiles
percentile_data <- final_data %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::mutate(across(
    all_of(names(mydata_sample)),
    ~ ecdf(mydata_sample[[cur_column()]])(.x) * 100
  ))

# saving the datasets for dataviz
saveRDS(object = final_data, file = "data/final_data.RDS")
saveRDS(object = percentile_data, file = "data/percentile_data.RDS")
