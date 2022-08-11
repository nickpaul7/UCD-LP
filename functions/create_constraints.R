create_student_to_project_constr <- function(student_num, project_num, numProjects, numStudents){
    

    
    mat <- matrix(0, numProjects, numStudents)
    mat[project_num, student_num] <- 1
    mat    
    vec <- c(mat)
    
    constr <- t(matrix(vec))
    
    return(constr)
    
}


create_student_conflict_constr <- function(student1, student2, numProjects, numStudents){
    
    vec <- rep(0, numStudents)
    vec[c(student1, student2)] <- 1
    
    matrix_list <- lapply(vec, diag, numProjects, numProjects)
    
    constr <- do.call(cbind, matrix_list)

    return(constr)
    
}


create_project_constraint <- function(df, numProjects, col_name, var){
    
    vec <- ifelse(df[,col_name] == var, 1, 0)
    matrix_list <- lapply(vec, diag, numProjects, numProjects)
    constr <- do.call(cbind, matrix_list)
    df_constr <- data.frame(constr)
    
    return(df_constr)
}
