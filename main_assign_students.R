source("./functions/solve_problem.R")
source("./functions/create_input_data.R")


# file <- "./data/student_info/Attributes to Assist in Practicum Team Creation - dummy input data np edits.xlsx"
file <- "./data/student_info/Attributes to Assist in Practicum Team Creation - input 2022 08 28 scrubbed (1).xlsx"
section = 1
lp_input <- create_input_file(file, section = section)

minStudents = 4
maxStudents = 6
china_const = 3
india_const = 4

lp_ob <- solve_problem(lp_input, minStudents, maxStudents, china_const, india_const)
lp_ob$lp_result$solution
lp_ob$lp_result$objval
lp_ob$lp_result$status

section = 2
lp_input <- create_input_file(file, section = section)

minStudents = 4
maxStudents = 6
china_const = 3
india_const = 4

lp_ob <- solve_problem(lp_input, minStudents, maxStudents, china_const, india_const)
lp_ob$lp_result$solution
lp_ob$lp_result$objval
lp_ob$lp_result$status

