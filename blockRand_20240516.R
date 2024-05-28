# define all available TMT channels
tmt_channels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C","131N",
                  "131C","132N","132C","133N","133C","134N","134C","135N")

# batch shift adding method 
batch_shift_adding <- function(initial_batch_design_matrix, n_samples_f, n_samples_batches) {
  
  n_batches <- dim(initial_batch_design_matrix)[1] 
  
  # 1. calculate the inconsistency
  initial_batch_colsum <- apply(initial_batch_design_matrix, 2, sum)
  n_less <- n_samples_f - initial_batch_colsum
  n_less[n_less < 0] = 0
  
  # 2. if a group lack n samples, add one sample in a batch, and then shift to next batch
  
  # define the temp vector
  n_f <- length(n_samples_f)
  temp_vec <- vector(length = n_f)
  for (i in 1:n_f) {
    temp_vec[i] <- sum(n_less[1:i])  
  }
  temp_vec <- c(0, temp_vec)
  
  # define the adding vector
  adding_vector <- rep(1:n_batches, ceiling(sum(n_less)/n_batches))
  
  # generate final batch design matrix
  new_batch_design_matrix <- initial_batch_design_matrix
  for (i in 1:n_f) {
    if (temp_vec[i+1] == 0) {
      i = i+1
    } else if ((temp_vec[i]+1) <= temp_vec[i+1]) {
      adding_index <- adding_vector[(temp_vec[i]+1):temp_vec[i+1]]
      new_batch_design_matrix[adding_index, i] <- initial_batch_design_matrix[adding_index, i] + 1
    }
  }
  
  # 3. check if there are still inconsistency
  # for columns
  new_batch_colsum <- apply(new_batch_design_matrix, 2, sum)
  n_incosis_col <- n_samples_f - new_batch_colsum
  
  # further correction when there are still inconsistency
  if (sum(abs(n_incosis_col)) != 0) {
    incosis_index <- sample(1:n_batches, sum(abs(n_incosis_col[n_incosis_col < 0])))
    new_batch_design_matrix[incosis_index, (n_incosis_col > 0)] <- new_batch_design_matrix[incosis_index, (n_incosis_col > 0)] + 1
    new_batch_design_matrix[incosis_index, (n_incosis_col < 0)] <- new_batch_design_matrix[incosis_index, (n_incosis_col < 0)] - 1
  }
  
  # for rows
  new_batch_rowsum <- apply(new_batch_design_matrix, 1, sum)
  n_incosis_row <- n_samples_batches - new_batch_rowsum
  
  # further correction when there are still inconsistency
  if (sum(abs(n_incosis_row)) !=0) {
    
    from_batches = names(n_incosis_row)[n_incosis_row < 0] 
    to_batches = names(n_incosis_row)[n_incosis_row > 0] 
    
    from_cols_all = c()
    for (from_batch in from_batches) {
      n_switch_samples = abs(n_incosis_row[from_batch])
      from_cols = colnames(new_batch_design_matrix)[new_batch_design_matrix[from_batch, ] >= 1][1:n_switch_samples]
      for (from_col in from_cols) {
        new_batch_design_matrix[from_batch, from_col] <- new_batch_design_matrix[from_batch, from_col] - 1
      }
      from_cols_all = c(from_cols_all, from_cols)
    }
    
    for (to_batch in to_batches) {
      n_switch_samples = abs(n_incosis_row[to_batch])
      from_cols = from_cols_all[1:n_switch_samples]
      for (from_col in from_cols) {
        new_batch_design_matrix[to_batch, from_col] <- new_batch_design_matrix[to_batch, from_col] + 1
      }
      from_cols_all <- from_cols_all[-(1:n_switch_samples)]
    }
    
  }
  
  return(new_batch_design_matrix)
}


# generate a batch design matrix based on one factor
batch_design_matrix_one_factor <- function(n_samples, batch_volume, n_samples_f) {
  
  # 1. calculate # of batches needed and # of batches full with samples
  n_batches <- ceiling(n_samples/batch_volume) 
  n_full_batches <- floor(n_samples/batch_volume)
  print(paste0("No. of batches needed = ", n_batches))
  
  # 2. calculate frequency distribution of the factor
  pctVector <- n_samples_f/n_samples 
  n_groups_f <- length(pctVector)  # No. of groups in the factor
  
  # 3. For a single batch, calculate the minimum # of samples in each group 
  #    based on the batch volume
  n_samples_f_one_batch <- floor(batch_volume*pctVector)
  
  # 4. generate the initial design matrix
  if (n_full_batches == n_batches) { # when there are no extra samples
    initial_batch_design <- matrix(data = rep(n_samples_f_one_batch, n_batches), 
                                   nrow = n_batches, ncol = n_groups_f, byrow = T)
    n_samples_batches <- rep(batch_volume, n_batches)
  } else { # when there are extra samples
    ## calculate No. of samples in the last batch
    n_samples_in_last_batch <- n_samples - (n_full_batches * batch_volume)
    n_samples_f_last_batch <- round(n_samples_in_last_batch*pctVector)
    initial_batch_design <- matrix(data = c(rep(n_samples_f_one_batch, n_full_batches), n_samples_f_last_batch), 
                                   nrow = n_batches, ncol = n_groups_f, byrow = T)
    n_samples_batches <- c(rep(batch_volume, n_full_batches), n_samples_in_last_batch)
  }
  
  colnames(initial_batch_design) <- names(pctVector)
  rownames(initial_batch_design) <- stringr::str_c("batch", 1:n_batches)
  print("Initial batch design matrix:")
  print(initial_batch_design)
  
  # 5. generate the final design matrix
  final_batch_design <- batch_shift_adding(initial_batch_design, n_samples_f, n_samples_batches)
  
  if (sum(n_samples_f == apply(final_batch_design, 2, sum)) == n_groups_f) {
    print("Final batch design matrix:")
    print(final_batch_design)
    print("Completed!")
  } else {
    print("Unsuccessful!")
  }
  
  return(final_batch_design)
}


# generate a batch design matrix considering 2 factors by using the batch 
# design matrix of factor 1 and frequency distribution of factor 2
batch_design_2factors <- function(batch_design_matrix_f1, f1_f2_relation, n_samples) {
  
  n_batches <- dim(batch_design_matrix_f1)[1]
  n_groups_f1 <- dim(batch_design_matrix_f1)[2]
  names_f1 <- colnames(batch_design_matrix_f1)
  n_samples_f1 <- apply(batch_design_matrix_f1, 2, sum)
  
  # define an initial matrix 
  batch_design_matrix_f1_f2 <- matrix(nrow = n_batches, ncol = 1)
  rownames(batch_design_matrix_f1_f2) <- rownames(batch_design_matrix_f1)
  # define an initial vector
  n_samples_f1_f2 <- vector()
  
  
  for (i in 1:n_groups_f1) {
    # for each group of factor 1
    sample_distri_f2 <- table(f1_f2_relation[f1_f2_relation[,1] == names_f1[i], 2])
    names(sample_distri_f2) <- paste0(names_f1[i], "|", names(sample_distri_f2))
    
    n_samples_f1_f2 <- c(n_samples_f1_f2, sample_distri_f2)
    
    # calculate the frequency distributions of factor 2
    pctVector_f2 <- sample_distri_f2/n_samples_f1[i]
    n_f2_in_batches <- floor(batch_design_matrix_f1[,i] %o% pctVector_f2)
    
    batch_design_matrix_f1_f2 <- cbind(batch_design_matrix_f1_f2, n_f2_in_batches)
  }
  
  batch_design_matrix_f1_f2 <- batch_design_matrix_f1_f2[,-1]
  print("Batch design matrix considering two factors (initial):")
  print(batch_design_matrix_f1_f2)
  
  # batch shift adding
  n_samples_batches <- apply(batch_design_matrix_f1, 1, sum) 
  new_batch_design_matrix_f1_f2 <- batch_shift_adding(batch_design_matrix_f1_f2, n_samples_f1_f2, n_samples_batches)
  
  new_n_samples_batches <- apply(new_batch_design_matrix_f1_f2, 1, sum)
  # see if samples in each batch exceed the volume
  temp_vec <- new_n_samples_batches <= max(n_samples_batches)
  if (sum(temp_vec) == n_batches) {
    print("Batch design matrix considering two factors:")
    print(new_batch_design_matrix_f1_f2)
    print("Completed!")
  } else {
    print("Unsuccessful!")
  }
  
  return(new_batch_design_matrix_f1_f2)
}
  

# allocate samples based on a batch design matrix 
allocate_samples <- function(sample_group, batch_design_matrix) {
  # sample_group contain two columns: one is sampleID, and the other is groupID.
  # The group names in groupID should be the same as the column names of batch_design_matrix
  colnames(sample_group) <- c('sampleID', 'groupID')
  
  batchID <- rownames(batch_design_matrix)
  groupID <- colnames(batch_design_matrix)
  
  n_batches <- dim(batch_design_matrix)[1]
  n_groups <- dim(batch_design_matrix)[2] 
  
  # define an initial dataframe
  sample_batch <- data.frame(matrix(nrow = 1, ncol = 2))
  colnames(sample_batch) <- c("sampleID", "batchID")
  # allocate samples based on batch design matrix
  for (i in 1:n_groups) {
      sample_batch_temp <- data.frame(sampleID = sample(x = sample_group$sampleID[sample_group$groupID == groupID[i]], 
                                                        size = sum(batch_design_matrix[,i]), replace = F),                                                        
                                      batchID = rep(rownames(batch_design_matrix), batch_design_matrix[,i]))
      sample_batch <- rbind(sample_batch, sample_batch_temp)
  }
  
  sample_batch <- na.omit(sample_batch) 
  return(sample_batch)
}


# calculate variation index for a factor
calculate_variation_index <- function(batch_factor_table, n_samples, n_samples_f) {
  # batch_factor_table: rows for batches, and columns for groups of factor
  
  # 1. frequency distribution of the factor in total population
  pctVector_f <- n_samples_f/n_samples
  
  # 2. frequency distribution of the factor in each batch
  pctVector_f_batches <- batch_factor_table/apply(batch_factor_table, 1, sum)
  
  # 3. calculate variations
  variation_matrix <- abs(sweep(pctVector_f_batches, 2, pctVector_f))
  
  variation_mean <- mean(variation_matrix)
  variation_sd <- sd(variation_matrix)
  
  variation_index <- sum(variation_mean, variation_sd)
  
  return(variation_index)
}


block_rand_one_factor <- function(sample_factors_df, batch_volume, n_IRs, n_iterations = 100) {
  # sample_factors_df: a dataframe, first column should be sample ID
  # The second to last columns should be the factors that need randomization 
  
  # see if No. of factors and No. of columns of sample_factors_df match each other
  if (ncol(sample_factors_df) < 2) {
    print("No. of factors don't match! Please check the data!")
  } else {
  
    sample_factors_df <- sample_factors_df[,1:2]
    colnames(sample_factors_df) <- c("sampleID", "factor1")
    n_samples <- dim(sample_factors_df)[1]
    print(paste0("No. of samples = ", n_samples))
    
    # calculate batch volume 
    batch_volume <- batch_volume - n_IRs
    
    ############################# Block Randomization ##########################
    
    # Step 1: Generate batch design matrix based on factor 1
    n_samples_f1 <- table(sample_factors_df[,2])
    batch_design_matrix_f1 <- batch_design_matrix_one_factor(n_samples, batch_volume, n_samples_f1)
    
    # Step 2: Randomly allocate samples based on the batch design matrix
    
    # allocate samples
    sample_batch <- allocate_samples(sample_factors_df, batch_design_matrix_f1)
    sample_batch_factors <- merge(sample_factors_df, sample_batch, by = "sampleID")

    return(sample_batch_factors)
  }
}


block_rand_two_factors <- function(sample_factors_df, batch_volume, n_IRs, n_iterations = 100) {
  # sample_factors_df: a dataframe, first column should be sample ID
  # The second to last columns should be the factors that need randomization 
  
  # see if No. of factors and No. of columns of sample_factors_df match each other
  if (ncol(sample_factors_df) < 3) {
    print("No. of factors don't match! Please check the data!")
  } else {
    
    sample_factors_df <- sample_factors_df[,1:3]
    colnames(sample_factors_df) <- c("sampleID", paste0("factor", c(1:2)))
    n_samples <- dim(sample_factors_df)[1]
    print(paste0("No. of samples = ", n_samples))
    
    # calculate batch volume 
    batch_volume <- batch_volume - n_IRs
    
    ############################# Block Randomization ##########################
    
    # Step 1: Generate batch design matrix based on factor 1
    n_samples_f1 <- table(sample_factors_df[,2])
    batch_design_matrix_f1 <- batch_design_matrix_one_factor(n_samples, batch_volume, n_samples_f1)
    
    # Step 2: Generate a batch design matrix considering both factor 1 and 2 
    f1_f2_relation <- sample_factors_df[, 2:3]
    batch_design_matrix_f1_f2 <- batch_design_2factors(batch_design_matrix_f1, f1_f2_relation, n_samples)
    
    # Step 3: Randomly allocate samples based on the batch design matrix
    
    # combine the group info of factor 1 and 2
    sample_group <- sample_factors_df[,1:3]
    sample_group$groupID <- stringr::str_c(sample_group[,2], "|", sample_group[,3])
    sample_group <- sample_group[,c(1,4)]
    colnames(sample_group) <- c("sampleID", "groupID")
      
    # allocate samples
    sample_batch <- allocate_samples(sample_group, batch_design_matrix_f1_f2)
    sample_batch_factors <- merge(sample_factors_df, sample_batch, by = "sampleID")
    
    return(sample_batch_factors)
  }
}


block_rand_3orMore_factors <- function(sample_factors_df, n_factors, batch_volume, n_IRs, n_iterations = 100) {
  # sample_factors_df: a dataframe, first column should be sample ID
  # The second to last columns should be the factors that need randomization 
  
  # see if No. of factors and No. of columns of sample_factors_df match each other
  if (ncol(sample_factors_df) < (n_factors + 1)) {
    print("No. of factors don't match! Please check the data!")
  } else {
    
    sample_factors_df <- sample_factors_df[,1:(n_factors+1)]
    colnames(sample_factors_df) <- c("sampleID", paste0("factor", c(1:n_factors)))
    n_samples <- dim(sample_factors_df)[1]
    print(paste0("No. of samples = ", n_samples))
    
    # calculate batch volume 
    batch_volume <- batch_volume - n_IRs
    
    ############################# Block Randomization ##########################
    
    # Step 1: Generate batch design matrix based on factor 1
    n_samples_f1 <- table(sample_factors_df[,2])
    batch_design_matrix_f1 <- batch_design_matrix_one_factor(n_samples, batch_volume, n_samples_f1)
    
    # Step 2: Generate a batch design matrix considering both factor 1 and 2 
    f1_f2_relation <- sample_factors_df[, 2:3]
    batch_design_matrix_f1_f2 <- batch_design_2factors(batch_design_matrix_f1, f1_f2_relation, n_samples)
    
    # Step 3: Randomly allocate samples based on the batch design matrix
    #         and also evaluate the variation for factor 3 ... etc.
    
    # combine the group info of factor 1 and 2
    sample_group <- sample_factors_df[,1:3]
    sample_group$groupID <- stringr::str_c(sample_group[,2], "|", sample_group[,3])
    sample_group <- sample_group[,c(1,4)]
    colnames(sample_group) <- c("sampleID", "groupID")
    
    print(paste0("No. of iterations = ", n_iterations))
    
    # for each iteration
    sample_batch_list <- vector(mode = "list", length = n_iterations)
    variation_index_matrix <- matrix(nrow = n_iterations, ncol = n_factors-2)
    for (i in 1:n_iterations) {
      
      # allocate samples
      sample_batch <- allocate_samples(sample_group, batch_design_matrix_f1_f2)
      sample_batch_factors <- merge(sample_factors_df, sample_batch, by = "sampleID")
      sample_batch_list[[i]] <- sample_batch_factors
      
      # calculate variation index for each additional factor
      for (j in 3:n_factors) {
        batch_factor_table <- table(sample_batch_factors$batchID, sample_batch_factors[,j+1])
        n_samples_factor <- apply(batch_factor_table, 2, sum)
        variation_index_matrix[i, j-2] <- calculate_variation_index(batch_factor_table, n_samples, n_samples_factor)
      }
    }
    variation_index_all_fs <- apply(variation_index_matrix, 1, sum)
    
    # find the optimal results
    optimal_iteration <- which.min(variation_index_all_fs)
    sample_batch_final <- sample_batch_list[[optimal_iteration]]
    
    return(sample_batch_final)
  }
}


# add TMT channels
add_TMT_channels <- function(sample_batch_final, n_IRs, batch_volume, n_batches, n_factors) {
  
  used_tmt_channels = tmt_channels[1:batch_volume]
  # extract TMT channels used for samples
  if (n_IRs == 0) {
    tmt_channels_used_for_samples <- used_tmt_channels
  } else if (n_IRs > 0) {
    tmt_channels_used_for_samples <- used_tmt_channels[1:(batch_volume-n_IRs)]
    IRchannels = used_tmt_channels[(batch_volume-n_IRs+1):length(used_tmt_channels)]
  }
  
  sample_batch_final$TMT_channel <- NA
  batchID <- unique(sample_batch_final$batchID)
  for (i in 1:n_batches) {
    batch_index <- sample_batch_final$batchID == batchID[i]
    n_samples_batch <- sum(batch_index)
    sample_batch_final$TMT_channel[batch_index] <- sample(tmt_channels_used_for_samples, n_samples_batch)
  }
  
  # if there are channels left empty, add "Other" samples to fill all channels
  batch_tmt_relation <- table(sample_batch_final$batchID, sample_batch_final$TMT_channel)
  added_index <- which(batch_tmt_relation == 0, arr.ind = T)
  if (nrow(added_index) != 0) {
    other_samples <- data.frame(matrix(nrow = (dim(added_index)[1]), ncol = n_factors + 3))
    colnames(other_samples) <- colnames(sample_batch_final)
    other_samples[,1] <- "Other"
    other_samples$batchID <- c(rownames(batch_tmt_relation)[added_index[,1]])
    other_samples$TMT_channel <- c(colnames(batch_tmt_relation)[added_index[,2]])
  } else {
    other_samples <- data.frame(matrix(nrow = (dim(added_index)[1]), ncol = n_factors + 3))
    colnames(other_samples) <- colnames(sample_batch_final)
  }

  
  if (n_IRs == 0) {
    sample_batch_final_add <- rbind(sample_batch_final, other_samples)
  } else if (n_IRs > 0) {
    IR_samples <- data.frame(matrix(nrow = n_batches*n_IRs, ncol = n_factors + 3))
    colnames(IR_samples) <- colnames(sample_batch_final)
    IR_samples[,1] <- "IR"
    IR_samples$batchID <- rep(batchID, each = n_IRs)
    IR_samples$TMT_channel <- rep(IRchannels, n_batches)
    
    sample_batch_final_add <- rbind(sample_batch_final, other_samples, IR_samples)
  }
  
  return(sample_batch_final_add)
}  

# sample_factors_df = read.csv("www/example_input_for_block_randomization.csv")
# n_factors = 3
# experiment_type = "Label-Free"
# batch_volume = 20
# n_IRs = 0
# n_iterations = 100

# generate final output results
final_outputs <- function(sample_factors_df, experiment_type, batch_volume, n_IRs, n_iterations = 100) {
  
  n_factors = ncol(sample_factors_df) - 1
  # batch assign
  if (n_factors == 1) {
    sample_batch_final <- block_rand_one_factor(sample_factors_df, batch_volume, n_IRs, n_iterations)
  } else if (n_factors == 2) {
    sample_batch_final <- block_rand_two_factors(sample_factors_df, batch_volume, n_IRs, n_iterations)
  } else if (n_factors >= 3) {
    sample_batch_final <- block_rand_3orMore_factors(sample_factors_df, n_factors, batch_volume, n_IRs, n_iterations)
  }
  
  batches <- unique(sample_batch_final$batchID)
  batches <- batches[order(as.numeric(gsub("batch", "", batches)))]
  n_batches <- length(batches)
  sample_batch_final$batchID = factor(sample_batch_final$batchID, levels = batches)
  
  if (experiment_type == "TMT") {
    # add other and IR samples
    sample_batch_final_add <- add_TMT_channels(sample_batch_final, n_IRs, batch_volume, n_batches, n_factors)
    # sort by batch ID and TMT channels
    sample_batch_final_add$TMT_channel = factor(sample_batch_final_add$TMT_channel, levels = tmt_channels[1:batch_volume])
    sample_batch_final_add = dplyr::arrange(sample_batch_final_add, batchID, TMT_channel)
  } else if (experiment_type == "Label-Free") {
    sample_batch_final_add = dplyr::arrange(sample_batch_final, batchID)
  }

  # add a column indicating order
  sample_batch_final_add = cbind(Sample_order = seq(nrow(sample_batch_final_add)), sample_batch_final_add)
  
  # generate final outputs
  batch_design_factos <- vector(mode = "list", length = n_factors)
  channel_design_factos <- vector(mode = "list", length = n_factors)
  for (i in 1:n_factors) {
    batch_factor_table <- as.data.frame.matrix(table(sample_batch_final_add$batchID, sample_batch_final_add[,(i+2)])) 
    batch_design_factos[[i]] <- batch_factor_table[stringr::str_order(rownames(batch_factor_table), numeric = T), ]
    if (experiment_type == "TMT") {
      tmt_factor_table <- as.data.frame.matrix(table(sample_batch_final_add$TMT_channel, sample_batch_final_add[,(i+2)]))
      channel_design_factos[[i]] <- tmt_factor_table[stringr::str_order(rownames(tmt_factor_table), numeric = T),]
    }
  }
  
  if (experiment_type == "TMT") {
    final_results <- list(sample_batch_final_add, batch_design_factos, channel_design_factos)
  } else if (experiment_type == "Label-Free") {
    final_results <- list(sample_batch_final_add, batch_design_factos)
  }
  
  return(final_results)
}
