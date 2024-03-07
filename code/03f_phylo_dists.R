source("01_requirements.R")

gray_2009_mcct <- ape::read.tree("data/trees/gray_et_al_tree_pruned_newick_mcct.txt")

gray_2009_mcct$edge.length = gray_2009_mcct$edge.length / 1000

# https://github.com/grambank/grambank-analysed/blob/main/R_grambank/spatiophylogenetic_modelling/analysis/make_precisionmatrices.R

## Calculate precision using typical variance rescaling
# Here, we calculate the precison matrix, including all nodes and tips
# By including the nodes, we create a sparse matrix, which results in significant
# time improvements within INLA. Note we don't want to scale the phylogeny
# because we are doing that ourselves in a moment
phy_inv_nodes = MCMCglmm::inverseA(tree,
                                   nodes = "ALL",
                                   scale = FALSE)$Ainv

# Next, we invert the precison matrix - creating the covariance matrix
# and standardize by the typical variance, to ensure variance is scaled to 1
phy_covar_nodes = solve(phy_inv_nodes)
typical_phylogenetic_variance = exp(mean(log(diag(phy_covar_nodes))))
phy_cov_std = phy_covar_nodes / typical_phylogenetic_variance
