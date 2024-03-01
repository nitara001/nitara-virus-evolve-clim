##calculate ED
library(dplyr)
library(ape)
library("caper")
library(ggplot2)
library(magrittr)
library(mgcv)
library(gratia)


# west nile data
wnd = west_nile_data

colnames(wnd)[colnames(wnd) == "species"] <- "Accession_number"

# 1. SUBSET TO REGION (NORTH AMERICA)
wnd =  wnd %>%
  dplyr::filter(lon < -50 & lat > 20) 

# plotting
wnd %>%
  ggplot() + 
  geom_jitter(aes(lon, lat, size=ED, col=ED), width=1, height=1, alpha=0.3) + 
  coord_fixed() + 
  theme_bw() +
  scale_color_viridis_c()

##log?
hist(wnd$DR, 50) #possibly 2 main clades?

# 2. REMOVE ANY OUTLIERS IF NEEDED
# n.b. large outlier in ED but these are not usual in terms of DR due to inverse;
# so potentially not necessary to remove?
#wnd = wnd %>%
  dplyr::filter(ED < 0.04)


# 3. OPTIMISE SPATIAL TERM RANGE PARAM

# vals = values of range param to test for Matern covar func
# aic and ubre store these values per model
aic = c()
ubre = c()
vals = c(0.05, 0.1, 0.2, 0.5, 1, 5, 10, 20)
#vals = seq(from=0.1, to=3, length.out=20) # use later for closer look

# for each value fit a "baseline" model with the effort covar and space term
for(i in vals){
  
  w_i <- mgcv::gam(scale(DR) ~ s(lon, lat, bs = "gp", m=c(3, i)) + log(num_occurrences+1),
                  data=wnd,
                  method = "REML")
  
  # extract AIC and generalised cross val UBRE scores
  aic = c(aic, w_i$aic)
  ubre = c(ubre, w_i$gcv.ubre)
  
}

# plot the minimal for the ubre score
plot(vals, ubre, "o")
data.frame(vals = vals, aic=aic, ubre = ubre)


# 4. FIT MODEL WITH TUNED GAUSSIAN PROCESS EFFECT

# n.b. from visualising the covars and response var
# I have scaled the response var (DR), logged the num_occurrences, and scaled all the covariates

# a. baseline model (no covariates)
mod_base <- mgcv::gam(scale(DR) ~ s(lon, lat, bs = "gp", m=c(3, 1)) + log(num_occurrences+1),
                 data=wnd, 
                 method = "REML")

# plot spatial term
gratia::draw(mod_base)

# b. adding covariates

# i. check for collinearity (nothing really needs excluding)
corrplot::corrplot(cor(wnd %>% dplyr::select(extracted_temp, extracted_precip, heterogeneity)))

# fit model with all 3 covars
mod_full <- mgcv::gam(scale(DR) ~ s(lon, lat, bs = "gp", m=c(3, 1)) + log(num_occurrences+1)
                      + scale(extracted_temp)
                      + scale(heterogeneity)
                      + scale(extracted_precip),
                      data=wnd, 
                      method = "REML")

summary(mod_base)
summary(mod_full)



# 5. SPATIAL CLUSTER SENSITIVITY ANALYSIS (using hclust)

# distances matrix between all point based on haversine distance
dists_mat = geosphere::distm(x = wnd %>% dplyr::select(lon, lat), 
                             y = wnd %>% dplyr::select(lon, lat), 
                             fun = geosphere::distHaversine)

# hierarchical clustering to create dendrogram based on distances
hc = hclust(as.dist(dists_mat))

# split into k clusters (here I've said 8 but could be more/fewer; 8 seems a decent start)
hc_clusts = cutree(hc, k=8)

# add to data
wnd_c = wnd %>%
  dplyr::mutate(clust = hc_clusts)

# visualise
wnd_c %>%
  ggplot() + 
  geom_point(aes(lon, lat, color=factor(clust))) + 
  theme_bw() + 
  coord_fixed()


# run param sensitivity check - fit k models holding out one cluster at a time
sens_result = data.frame()

for(i in unique(wnd_c$clust)){
  
  # exclude cluster i
  mod_i = mgcv::gam(scale(DR) ~ s(lon, lat, bs = "gp", m=c(3, 0.9)) + log(num_occurrences+1)
                    + scale(extracted_temp)
                    + scale(heterogeneity)
                    + scale(extracted_precip),
                    data=wnd_c[ wnd_c$clust != i, ], 
                    method = "REML")
  
  # extract coefs
  coefs_i = as.data.frame(summary(mod_i)$p.table)
  names(coefs_i) = c("est", "se", "t", "p")
  coefs_i$param = row.names(coefs_i)
  row.names(coefs_i) = c()
  coefs_i$clust = i
  
  # add to df
  sens_result = rbind(sens_result, coefs_i)
}

# plot result (just param point estimate)
sens_result %>%
  dplyr::filter(!param %in% c("(Intercept)", "log(num_occurrences + 1)")) %>%
  ggplot() + 
  geom_point(aes(param, est, col=factor(clust)), size=5, alpha=0.6, pch=16) + 
  geom_hline(yintercept=0, lty=2) + 
  coord_flip()+
  ggtitle("WNV")

# plot result and 95% confidence interval (1.96 * standard error)
# gives a sense for where params are most sensitive
sens_result %>%
  dplyr::filter(!param %in% c("(Intercept)", "log(num_occurrences + 1)")) %>%
  ggplot() + 
  geom_point(aes(param, est, col=factor(clust)), size=3, alpha=0.8, pch=16, position=position_dodge(width=0.3)) + 
  geom_linerange(aes(param, ymin=est-(1.96*se), ymax=est+(1.96*se), col=factor(clust)), linewidth=0.8, alpha=0.8, position=position_dodge(width=0.3)) + 
  geom_hline(yintercept=0, lty=2) + 
  ylab("estimate + 95% CI") +
  coord_flip() +
  ggtitle("WNV")


# or a nice way to visualise; plot the full model with CI, and then submodels on top

coefs_full = as.data.frame(summary(mod_full)$p.table)
names(coefs_full) = c("est", "se", "t", "p")
coefs_full$param = row.names(coefs_full)
row.names(coefs_full) = c()

ggplot() + 
  geom_point(data = sens_result[ !sens_result$param %in% c("(Intercept)", "log(num_occurrences + 1)"), ], aes(param, est), size=5, alpha=0.4, pch=16) + 
  geom_point(data = coefs_full[ !coefs_full$param %in% c("(Intercept)", "log(num_occurrences + 1)"), ], aes(param, est), size=3, pch=23, fill="red") + 
  geom_linerange(data = coefs_full[ !coefs_full$param %in% c("(Intercept)", "log(num_occurrences + 1)"), ],
                 aes(param, ymin=est-(1.96*se), ymax=est+(1.96*se)), linewidth=0.6, color="red") + 
  geom_hline(yintercept=0, lty=2) + 
  coord_flip() + 
  ylab("Estimate + 95% confidence interval") + 
  xlab("Parameter") + 
  theme_classic()+
  ggtitle("WNV")





  
