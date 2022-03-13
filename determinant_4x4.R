#=======================================================================#
# Purpose: determinant of 4x4 matrix in straight algebra
# Inputs: generate 4x4 matrix
# Outputs: function returns determinant terms
#
# Notes: example from https://byjus.com/maths/determinant-of-4x4-matrix/
#        +-+-
#
#       https://semath.info/src/inverse-cofactor-ex4.html
#
#        for inverse of 4x4 which is 1 over the determinant times the adjugate
#=======================================================================#

  # load packages
  library(data.table)

  # generate data
  # my_dt <- t(matrix(c(4,  3,  2, 2,
  #                     0,  1, -3, 3,
  #                     0, -1,  3, 3,
  #                     0,  3,  1, 1),
  #                   ncol = 4))
  
  my_dt1 <- data.table(tv = c(4,  3,  2, 2,
                    0,  1, -3, 3,
                    0, -1,  3, 3,
                    0,  3,  1, 1))
  
  my_dt2 <- data.table(tv = c(1,  1,  1, -1,
                            1,  1, -1,  1,
                            1, -1,  1,  1, 
                           -1,  1,  1,  1))
  
  # name variables
  my_dt1[, row := rep(1:4, each  = 4)]
  my_dt1[, col := rep(1:4, times = 4)]
  my_dt2[, row := rep(1:4, each  = 4)]
  my_dt2[, col := rep(1:4, times = 4)]
  
  # add student id
  my_dt1[, student_id := "1"]
  my_dt2[, student_id := "2"]
  
  # cast wide
  wide_dt1 <- dcast.data.table(my_dt1, student_id ~ paste("tv", row, col, sep = "_"), value.var = "tv")
  wide_dt2 <- dcast.data.table(my_dt2, student_id ~ paste("tv", row, col, sep = "_"), value.var = "tv")

  # stacked student level
  stacked_student <- rbind(wide_dt1, wide_dt2)
  

# function to compute determinant terms
compute_four_by_four_determinant_terms <- function(in_data, cofactor_row, cofactor_column){
  
  # isolate each term #note assumes variables names
  varname_cofactor <- grep(paste(cofactor_row, cofactor_column, sep = "_"), colnames(in_data), value = TRUE)
  
  # find 3x3 element varnames
  three_by_three_varnames <- grep(paste(paste0("_", cofactor_row, "_"), paste0(cofactor_column, "$"), sep = "|"), colnames(in_data), value = TRUE, invert = TRUE)
  
  # find the 3x3 by removing the cofactor's shared elements
  three_by_three <- subset(in_data, select = c(three_by_three_varnames))
  
  # rename colnames
  setnames(three_by_three, three_by_three_varnames, c("student_id", paste0("v_", rep(1:3, each = 3), rep(1:3, times  = 3))))
  
  # forwards diagonal products
  three_by_three[, fdp_1 := v_11 * v_22 * v_33]
  three_by_three[, fdp_2 := v_12 * v_23 * v_31]
  three_by_three[, fdp_3 := v_13 * v_21 * v_32]
  
  # backwards diagonal products
  three_by_three[, bdp_1 := v_11 * v_23 * v_32]
  three_by_three[, bdp_2 := v_12 * v_21 * v_33]
  three_by_three[, bdp_3 := v_13 * v_22 * v_31]
  
  # sum forwards 
  three_by_three[, forward_sum := fdp_1 + fdp_2 + fdp_3]
  
  # sum backwards 
  three_by_three[, backward_sum := bdp_1 + bdp_2 + bdp_3]
  
  # subtract
  three_by_three[, difference := forward_sum - backward_sum]
  
  # include cofactor to compute 4x4 determinant outside of function
  three_by_three[, cofactor := in_data[, get(varname_cofactor)]]
  
  # identify cofactor
  three_by_three[, cofactor_row    := cofactor_row]
  three_by_three[, cofactor_column := cofactor_column]
  
  #note keep the differences for filling in the adjugate
  return(three_by_three[, .(student_id, cofactor_row, cofactor_column, cofactor, difference)])
  
}

  # call function for each cofactor
  determinant_stacked_terms <- rbindlist(lapply(1:4, function(x) compute_four_by_four_determinant_terms(stacked_student, x, 1)))
  second_adjugate_terms     <- rbindlist(lapply(1:4, function(x) compute_four_by_four_determinant_terms(stacked_student, x, 2)))
  third_adjugate_terms      <- rbindlist(lapply(1:4, function(x) compute_four_by_four_determinant_terms(stacked_student, x, 3)))
  fourth_adjugate_terms     <- rbindlist(lapply(1:4, function(x) compute_four_by_four_determinant_terms(stacked_student, x, 4)))
  
  # stack adjugate terms
  stacked_adjugate <- rbind(determinant_stacked_terms, second_adjugate_terms, third_adjugate_terms, fourth_adjugate_terms)
  
  # compute cofactor products
  determinant_stacked_terms[, determinant_term := cofactor * difference]
  
  # cast wide to get the +- pattern right  
  wide_determinant <- dcast.data.table(determinant_stacked_terms[, -c("cofactor", "difference")], student_id ~ paste0("cf_", cofactor_row, "_", cofactor_column), value.var = "determinant_term")
  
  # compute determinant
  wide_determinant[, determinant := cf_1_1 - cf_2_1 + cf_3_1 - cf_4_1]
  
  # first term of adjugate depends on even/odd dimsums
  stacked_adjugate[, sign := (-1) ^ (cofactor_row + cofactor_column)]
  
  # merge determinant onto adjugate
  setkey(wide_determinant, "student_id")
  setkey(stacked_adjugate, "student_id")
  inverse_four_by_four <- merge(wide_determinant, stacked_adjugate, by = "student_id")
  
  # the inverse is sign times difference times 1 / determinant
  inverse_four_by_four[, inverse_element := 1 / determinant * sign * difference]




