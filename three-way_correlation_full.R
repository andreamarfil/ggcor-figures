# install.packages("ggcor.tar.gz", repos = NULL, type = "source")

pacman::p_load(tidyverse, vegan, ggcor, yarrr)

data("varechem", package = "vegan")
data("varespec", package = "vegan")


# apply correlation method (spearman, pearson, mantel, etc.) 
# and generate a data frame with columns: var1, var2, p, r, range p (Positive, Negative, ns), range r (<0.2, 0.2-0.4, >=0.4)
mantel <- mantel_test(varespec, varechem,
                      spec.select = list(M1 = 1:7,
                                         M2 = 8:18,
                                         M3 = 19:37,
                                         M4 = 38:44)) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4"))) %>% 
  add_column(., pd = ifelse(.$r > 0, "Positive", "Negative")) %>% 
  mutate(., pd = ifelse(.$p.value > 0.05, "ns", pd))

# plot parameters
extra.params <- extra_params(
  env.point = point_params(colour = "#595959", fill = "#595959", size = 2),
  spec.point = point_params(colour = "#595959", fill = "#595959", size = 2),
  link.params = link_params(spec.point.hjust = 1),
  spec.label = text_params(size = 5))

# not show edge of circles
colour_transparent <- transparent("white", 1)

# to use a different correlation method, instead of the one from the package:
## 1. make correlation matrix with desired method
## 2. need to convert matrix to cor_tbl: mat <- cor_tbl(extra.mat = list(mat = matrix))
## 3. then in quickcor: mapping = aes(fill = mat)
## rest of options remain the same

# plot
quickcor(varechem, varespec[, 1:10], type = "full", cor.test = T, method = "spearman", axis.y.position = "right") +
  ### colour circle instead of whole cell
  geom_circle2(colour = colour_transparent) +
  ### show stars in signif cells
  geom_mark(sig.thres = 0.05,
            color = "white",
            fontface = "bold",
            mark = c("*", "**", "***"),
            size = 7,
            r = NA,
            vjust = 0.75) +
  ### add extra correlations with lines
  add_link(mantel, mapping = aes(colour = pd,
                               size = rd),
           extra.params = extra.params,
           curvature = F, 
           on.left = T) +
  ### cross-out non-significant correlations (redundant)
  # geom_cross(sig.level = 0.05, colour = "#8c8c8c", alpha = 0.5) +
  ### ggplot2 options
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = c("Positive" = "#29a649","Negative" = "#d67600","ns" = "grey80")) +
  scale_fill_gradient2(low = "#2d69ab", 
                       mid = "white", 
                       high = "#b42c3a", 
                       breaks = c(-1, -0.5, 0, 0.5, 1), 
                       limits = c(-1, 1)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Significance", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colourbar(title = "Spearman's r", 
                               order = 3)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        text = element_text(size = 13),
        legend.key=element_blank())
