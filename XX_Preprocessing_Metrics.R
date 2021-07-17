##Pretext Analysis

#Matthew J. Denny, and Arthur Spirling (2018). "Text Preprocessing For Unsupervised Learning: Why It Matters, When It Misleads, And What To Do About It". 
#[ssrn.com/abstract=2849145] or [https://doi.org/10.1017/pan.2017.44]

library(preText)
library(quanteda)

build_distance_objects <- function(cur_dfm, distance_method, dimensions) {
  if (distance_method == "cosine") {
    simil <- quanteda.textstats::textstat_simil(cur_dfm, method = distance_method)
    simil[is.na(simil)] <- 0
  } else {
    simil <- proxy::simil(as.matrix(cur_dfm), method = distance_method)
  }
  simil <- as.matrix(simil)
  distances2 <- proxy::pr_simil2dist(simil)
  distances <- as.matrix(distances2)
  pos <- stats::cmdscale(distances, k = dimensions)
  out <- list('matrices' = distances,
              'objects' = proxy::as.dist(distances2),
              'positions' = pos)
  return(out)
}

scaling_comparison <- function(dfm_object_list,
                               dimensions = 2,
                               distance_method = "cosine",
                               verbose = TRUE,
                               cores = 1){
  
  if (cores > 1) { # multi-core
    if (verbose) {
      warning('verbose = TRUE only works with single-core computation.')
    }
    cl <- parallel::makeCluster(getOption("cl.cores", cores)) # start cluster
    tmp <- parallel::parLapplyLB(cl = cl,
                                 fun = build_distance_objects,
                                 X = dfm_object_list,
                                 distance_method = distance_method,
                                 dimensions = dimensions)
    distance_matrices <- lapply(tmp, function(x) x[['matrices']])
    distance_objects <- lapply(tmp, function(x) x[['objects']])
    scaled_positions <- lapply(tmp, function(x) x[['positions']])
    parallel::stopCluster(cl) # stop cluster
  } else { # single-core
    # get the number of dfms
    num_dfms <- length(dfm_object_list)
    
    # ceate data structures to store information
    distance_matrices <- vector(mode = "list", length = num_dfms)
    distance_objects <- vector(mode = "list", length = num_dfms)
    scaled_positions <- vector(mode = "list", length = num_dfms)
    
    for (i in 1:num_dfms) {
      if (verbose) {
        ptm <- proc.time()
        cat("Currently working on dfm",i,"of",num_dfms,"\n")
      }
      
      # compute
      tmp <- build_distance_objects(dfm_object_list[[i]], distance_method, dimensions)
      
      # store
      distance_matrices[[i]] <- tmp[['matrices']]
      distance_objects[[i]] <- tmp[['objects']]
      scaled_positions[[i]] <- tmp[['positions']]
      
      if (verbose) {
        t2 <- proc.time() - ptm
        cat("Complete in:",t2[[3]],"seconds...\n")
      }
    }
  }
  
  return(list(distance_matrices = distance_matrices,
              scaled_positions = scaled_positions,
              dist_objects = distance_objects))
  
}

preText <- function(preprocessed_documents,
                    dataset_name = "Documents",
                    distance_method = "cosine",
                    num_comparisons = 50,
                    parallel = FALSE,
                    cores = 1,
                    verbose = TRUE){
  
  ptm <- proc.time()
  # extract teh dfm object list from preprocessed_documents
  dfm_object_list <- preprocessed_documents$dfm_list
  
  cat("Generating document distances...\n")
  # get document distances
  scaling_results <- scaling_comparison(dfm_object_list,
                                        dimensions = 2,
                                        distance_method = distance_method,
                                        verbose = verbose,
                                        cores = cores)
  
  # extract distance matrices
  distance_matrices <- scaling_results$distance_matrices
  cat("Generating preText Scores...\n")
  
  preText_results <- preText_test(
    distance_matrices,
    choices = preprocessed_documents$choices,
    labels = preprocessed_documents$labels,
    baseline_index = length(preprocessed_documents$labels),
    text_size = 1,
    num_comparisons = num_comparisons,
    parallel = parallel,
    cores = cores,
    verbose = verbose)
  
  preText_scores <- preText_results$dfm_level_results_unordered
  cat("Generating regression results..\n")
  
  reg_results <- preprocessing_choice_regression(
    Y = preText_scores$preText_score,
    choices = preprocessed_documents$choices,
    dataset = dataset_name,
    base_case_index = length(preprocessed_documents$labels))
  
  cat("Regression results (negative coefficients imply less risk):\n")
  # create temporary results os we can round coefficients
  reg_results2 <- reg_results
  reg_results2[,1] <- round(reg_results2[,1],3)
  reg_results2[,2] <- round(reg_results2[,2],3)
  print(reg_results2[,c(3,1,2)])
  
  t2 <- proc.time() - ptm
  cat("Complete in:",t2[[3]],"seconds...\n")
  #extract relevant info
  return(list(preText_scores = preText_scores,
              ranked_preText_scores = preText_results$dfm_level_results,
              choices = preprocessed_documents$choices,
              regression_results = reg_results))
  
}



corpus_wordfish <- quanteda::corpus(df_base, 
                                    text_field = "speechContent")



preprocessed_documents <- factorial_preprocessing(
  documents,
  use_ngrams = TRUE,
  infrequent_term_threshold = 0.2,
  verbose = FALSE, 
  language = 'german')


preText_results <- preText(
  preprocessed_documents,
  dataset_name = "Covid-19 debates in the German Bundestag",
  distance_method = "cosine",
  num_comparisons = 20,
  verbose = FALSE)

preText_score_plot(preText_results)

pretextplot <- regression_coefficient_plot(preText_results,
                            remove_intercept = TRUE)
ggplot2::ggsave("./Figures/Figures/preText.png", plot = pretextplot, height = 7)

