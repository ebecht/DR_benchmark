library(uwot)
library(Rtsne)
library(MASS)
library(future)
options("bitmapType" = "cairo")
options("scipen" = 999)

####################
## Datasets simulation and embeddings
####################

cores = ifelse(interactive(), 6, availableCores(methods = "Slurm"))
cat("Using", cores, "cores\n")

epochs = 10^(2:6)
neighbors = c(25, 35, 50, 60, 75, 100, 200)

for(nn in neighbors){
    for(iters in epochs){
        ## Circle with subclusters (like a "pearl necklace")
        benchmark = "01"
        set.seed(123 * as.numeric(benchmark))

        n = 30
        r = 15
        n_points = 200
        thetas = seq(0, 2*pi*(n-1)/n, length.out = n)
        centers = cbind(rep(r*cos(thetas), n_points), rep(r*sin(thetas), n_points))
        cols = rep(rgb(thetas, 2*pi - thetas, thetas, maxColorValue = 2*pi), n_points)
        data = cbind(rnorm(n_points*n), rnorm(n_points*n)) + centers
        random_init = cbind(rnorm(nrow(data), sd = 0.0001), rnorm(nrow(data), sd = 0.0001))

        dir_out = file.path("output", benchmark)
        dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
        png(file.path(dir_out, "data.png"), height = 1000, width = 2000, res = 150)
        par(mfrow = c(1, 2))
        plot(data[,1], data[, 2], col = cols, pch = 16, cex = 0.1, xlab = "", ylab = "")
        image(kde2d(data[,1],data[,2], n = 200, h = 1.5), col = c("white", rev(c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"))))
        saveRDS(data, file = file.path(dir_out, "data.Rds"))
        saveRDS(cols, file = file.path(dir_out, "cols.Rds"))
        dev.off()

        umap_le = umap(data, n_neighbors = nn, n_epochs = 1000, ret_model = TRUE, ret_nn = TRUE, n_threads = cores, n_sgd_threads = cores)
        umap_random = umap(data, n_neighbors = nn, init = random_init, n_epochs = iters, ret_model = TRUE, ret_nn = TRUE, n_threads = cores, n_sgd_threads = cores)
        tsne_pca = Rtsne(data, perplexity = nn, Y_init = prcomp(data)$x[,1:2])
        tsne_random = Rtsne(data, perplexity = nn, Y_init = random_init, max_iter = iters)

        png(file.path(subdir_out, "embeddings.png"), height = 2000, width = 2000, res = 150)
        par(mfrow = c(2, 2))
        plot(umap_le$embedding, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "UMAP LE init")
        plot(umap_random$embedding, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "UMAP random init")
        plot(tsne_pca$Y, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "t-SNE PCA init")
        plot(tsne_random$Y, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "t-SNE random init")
        dev.off()

        res = list(umap_le = umap_le, umap_random = umap_random, tsne_pca = tsne_pca, tsne_random = tsne_random)
        saveRDS(res, file.path(subdir_out, "embeddings.Rds"))

        ## Circle as per Kobak and Linderman (very low noise)
        benchmark = "02"
        set.seed(123 * as.numeric(benchmark))

        n_points = 7000
        r = 1
        thetas = runif(0, 2*pi, n = n_points)
        sigma = 0.0001

        centers = cbind(r*cos(thetas), r*sin(thetas))
        cols = rep(rgb(thetas, 2*pi - thetas, thetas, maxColorValue = 2*pi), 1)
        data = cbind(r*cos(thetas) + rnorm(n = n_points, sd = sigma), r*sin(thetas) + rnorm(n = n_points, sd = sigma))
        random_init = cbind(rnorm(n_points, sd = 0.0001), rnorm(n_points, sd = 0.0001))

        dir_out = file.path("output", benchmark)
        dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
        png(file.path(dir_out, "data.png"), height = 1000, width = 2000, res = 150)
        par(mfrow = c(1, 2))
        plot(data[,1], data[, 2], col = cols, pch = 16, cex = 0.1, xlab = "", ylab = "")
        image(kde2d(data[,1],data[,2], n = 200, h = 0.1), col = c("white", rev(c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"))))
        saveRDS(data, file = file.path(dir_out, "data.Rds"))
        saveRDS(cols, file = file.path(dir_out, "cols.Rds"))
        dev.off()

        umap_le = umap(data, n_neighbors = nn, n_epochs = 1000, ret_model = TRUE, ret_nn = TRUE, n_threads = cores, n_sgd_threads = cores)
        umap_random = umap(data, n_neighbors = nn, init = random_init, n_epochs = iters, ret_model = TRUE, ret_nn = TRUE, n_threads = cores, n_sgd_threads = cores)
        tsne_pca = Rtsne(data, perplexity = nn, Y_init = prcomp(data)$x[,1:2])
        tsne_random = Rtsne(data, perplexity = nn, Y_init = random_init, max_iter = iters)

        png(file.path(subdir_out, "embeddings.png"), height = 2000, width = 2000, res = 150)
        par(mfrow = c(2, 2))
        plot(umap_le$embedding, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "UMAP LE init")
        plot(umap_random$embedding, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "UMAP random init")
        plot(tsne_pca$Y, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "t-SNE PCA init")
        plot(tsne_random$Y, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "t-SNE random init")
        dev.off()

        res = list(umap_le = umap_le, umap_random = umap_random, tsne_pca = tsne_pca, tsne_random = tsne_random)
        saveRDS(res, file.path(subdir_out, "embeddings.Rds"))
        
        ## Circle with large Gaussian Noise
        benchmark = "07"
        set.seed(123 * as.numeric(benchmark))

        n_points = 7000
        r = 1
        thetas = runif(0, 2*pi, n = n_points)
        sigma = 0.2

        centers = cbind(r*cos(thetas), r*sin(thetas))
        cols = rep(rgb(thetas, 2*pi - thetas, thetas, maxColorValue = 2*pi))
        data = cbind(r*cos(thetas) + rnorm(n = n_points, sd = sigma), r*sin(thetas) + rnorm(n = n_points, sd = sigma))
        random_init = cbind(rnorm(n_points, sd = 0.0001), rnorm(n_points, sd = 0.0001))

        dir_out = file.path("output", benchmark)
        dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
        png(file.path(dir_out, "data.png"), height = 1000, width = 2000, res = 150)
        par(mfrow = c(1, 2))
        plot(data[,1], data[, 2], col = cols, pch = 16, cex = 0.1, xlab = "", ylab = "")
        image(kde2d(data[,1],data[,2], n = 200, h = 0.1), col = c("white", rev(c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"))))
        saveRDS(data, file = file.path(dir_out, "data.Rds"))
        saveRDS(cols, file = file.path(dir_out, "cols.Rds"))
        dev.off()
        subdir_out = file.path(dir_out, paste0("epochs_", iters, "_neighbors_", nn))
        dir.create(subdir_out, showWarnings = FALSE, recursive = TRUE)

        umap_le = umap(data, n_neighbors = nn, n_epochs = 1000, ret_model = TRUE, ret_nn = TRUE, n_threads = cores, n_sgd_threads = cores)
        umap_random = umap(data, n_neighbors = nn, init = random_init, n_epochs = iters, ret_model = TRUE, ret_nn = TRUE, n_threads = cores, n_sgd_threads = cores)
        tsne_pca = Rtsne(data, perplexity = nn, Y_init = prcomp(data)$x[,1:2])
        tsne_random = Rtsne(data, perplexity = nn, Y_init = random_init, max_iter = iters)

        png(file.path(subdir_out, "embeddings.png"), height = 2000, width = 2000, res = 150)
        par(mfrow = c(2, 2))
        plot(umap_le$embedding, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "UMAP LE init")
        plot(umap_random$embedding, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "UMAP random init")
        plot(tsne_pca$Y, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "t-SNE PCA init")
        plot(tsne_random$Y, col = cols, xlab = "", ylab = "",xaxt = "n", yaxt ="n", main = "t-SNE random init")
        dev.off()

        res = list(umap_le = umap_le, umap_random = umap_random, tsne_pca = tsne_pca, tsne_random = tsne_random)
        saveRDS(res, file.path(subdir_out, "embeddings.Rds"))
    }
}

####################
## Figure and sup figure
####################
    
experiments = paste0("output/", c("02", "01", "07"), "/")
nn = paste0("epochs_", c(10000, 100000, 1000000), "_")
iters = paste0("neighbors_", c(50, 100))

paths = apply(expand.grid(experiments, nn, iters), 1, paste0, collapse = "")

embeddings = lapply(
    paths,
    function(x){
        res = readRDS(file.path(x, "embeddings.Rds"))
        list(
            umap_random = res$umap_random$embedding,
            tsne_random = res$tsne_random$Y
        )
    }
)
names(embeddings) = paths
plot_ncol = 2*length(nn)*length(iters) + 1

## Supplementary Figure
png("sup_figure.png", width = 500 * plot_ncol, height = length(experiments)*500, res = 100)
cex = 0.1
par(mfrow = c(length(experiments), plot_ncol))
par(bty = "n", xaxt = "n", yaxt = "n", mar = c(0.1, 0.1, 0.1, 0.1))
for(i in 1:length(experiments)){
    data = readRDS(file.path(experiments[i], "data.Rds"))
    cols = readRDS(file.path(experiments[i], "cols.Rds"))
    plot(data, pch = 16, col = cols, cex = cex*5)

    path = file.path(experiments[i])
    for(neighbors in nn){
        for(iter in iters){
            plot(
                embeddings[[paste0(path, neighbors, iter)]]$umap_random,
                col = cols,
                cex = cex
            )
        }
    }
    for(neighbors in nn){
        for(iter in iters){
            plot(
                embeddings[[paste0(path, neighbors, iter)]]$tsne_random,
                col = cols,
                cex = cex
            )
        }
    }
    
}
dev.off()

## Figure panels C and D
experiments = paste0("output/", c("01", "07"), "/")
plot_ncol = 3
png("figure.png", width = 500 * plot_ncol, height = length(experiments)*500, res = 100)
cex = 0.1
par(mfrow = c(length(experiments), plot_ncol))
par(bty = "n", xaxt = "n", yaxt = "n", mar = c(0.1, 0.1, 0.1, 0.1))
for(i in 1:length(experiments)){
    data = readRDS(file.path(experiments[i], "data.Rds"))
    cols = readRDS(file.path(experiments[i], "cols.Rds"))
    plot(data, pch = 16, col = cols, cex = cex*5)

    path = file.path(experiments[i])

    iter = "neighbors_50" #woops
    neighbors = "epochs_100000_"
    plot(
        embeddings[[paste0(path, neighbors, iter)]]$umap_random,
        col = cols,
        cex = cex
    )

    iter = "neighbors_100" #woops
    neighbors = "epochs_1000000_"
    plot(
        embeddings[[paste0(path, neighbors, iter)]]$tsne_random,
        col = cols,
        cex = cex
    )    
}
dev.off()
