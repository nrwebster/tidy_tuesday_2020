#=======================================================================#
# Purpose: determinant of 4x4 matrix in straight algebra
# Inputs: generate 4x4 matrix
# Outputs: function returns determinant terms
#
# Notes: example from https://byjus.com/maths/determinant-of-4x4-matrix/
#        +-+-
#=======================================================================#

  # load packages
  library(data.table)
  
  # generate data
  my_dt <- t(matrix(c(4,  3,  2, 2,
                      0,  1, -3, 3,
                      0, -1,  3, 3, 
                      0,  3,  1, 1),
                    ncol = 4))
  
  # give dimensions' names
  colnames(my_dt) <- paste("c", 1:4, sep = "_")
  rownames(my_dt) <- paste("r", 1:4, sep = "_")


# function to compute determinant terms
compute_four_by_four_determinant_terms <- function(cofactor_row, cofactor_column){
  
  # isolate each term
  cofactor <- my_dt[cofactor_row, cofactor_column]
  
  # find the 3x3
  three_by_three <- my_dt[-cofactor_row, -cofactor_column]
  
  # convert to single row
  single_vector <- data.table(c(three_by_three))
  
  # make colnames
  single_vector[, col := rep(1:3, each  = 3)]
  single_vector[, row := rep(1:3, times = 3)]
  single_vector[, varname := paste0("v_", row, col)]
  
  # remove row and cols intermediate names
  single_vector[, c("row", "col") := NULL]
  
  # cast wide to be 1 row
  wide_dt <- dcast.data.table(single_vector, ... ~ varname, value.var = "V1")[, -1]
  
  # forwards diagonal products
  wide_dt[, fdp_1 := v_11 * v_22 * v_33]
  wide_dt[, fdp_2 := v_12 * v_23 * v_31]
  wide_dt[, fdp_3 := v_13 * v_21 * v_32]
  
  # backwards diagonal products
  wide_dt[, bdp_1 := v_11 * v_23 * v_32]
  wide_dt[, bdp_2 := v_12 * v_21 * v_33]
  wide_dt[, bdp_3 := v_13 * v_22 * v_31]
  
  # sum forwards 
  wide_dt[, forward_sum := fdp_1 + fdp_2 + fdp_3]
  
  # sum backwards 
  wide_dt[, backward_sum := bdp_1 + bdp_2 + bdp_3]
  
  # subtract
  wide_dt[, difference := forward_sum - backward_sum]
  
  # cofactor times difference
  wide_dt[, determinant_term := cofactor * difference]
  
  # identify cofactor
  wide_dt[, cofactor_index := paste(cofactor_row, cofactor_column, sep = "_")]
  
  return(wide_dt[, .(cofactor_index, determinant_term)])
  
}

  # call function for each cofactor
  stacked_terms <- rbindlist(lapply(1:nrow(my_dt), function(x) compute_four_by_four_determinant_terms(x, 1)))

  # cast wide to get the +- pattern right  
  predeterminant <- dcast.data.table(stacked_terms, ... ~ paste0("cf_", cofactor_index), value.var = "determinant_term")[, -1]
  
  # compute determinant
  predeterminant[, determinant := cf_1_1 - cf_2_1 + cf_3_1 - cf_4_1]
  
  
                      