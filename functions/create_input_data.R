

create_input_file <- function(file, section, preference = NULL){

df <- openxlsx::read.xlsx(file)
df <- df[df$Section == section,]
df_projects <- openxlsx::read.xlsx(file, sheet = "Projects")
df_projects <- df_projects[df_projects$Section == section,]

chars <- unlist(lapply(df_projects$Project_Name, nchar))

df_projects$Project_Name2 <- substr(df_projects$Project_Name, 
                                     3,
                                     chars) 
df_projects$Project_Letter <- paste("Project", substr(df_projects$Project_Name, 
                                      1,
                                      1) )

numStudents = nrow(df)
numProjects = nrow(df_projects)
df_projects$Project_Name

studentLead <- df[,c('Student_Number','Student_Name', 'Leadership')]
studentLead$Leader[studentLead$Leadership == "Yes"] <- 1
studentLead$Leader[studentLead$Leadership == "No"] <- 0

studentGender <- df[,c('Student_Number','Student_Name', 'Gender')]

studentpref <- df[,c('Student_Number','Student_Name')]


#randomly assign preference
if(is.null(preference)){
vec <- ifelse(runif(numStudents*numProjects)<=.4,1,0) 
vec <- rep(0, numStudents *numProjects)

df_dummy <- data.frame(matrix(vec, nrow = numStudents, ncol = numProjects))
colnames(df_dummy) <- df_projects$Project_Name
studentpref <- cbind(studentpref, df_dummy)


}else{
    
    df_preference <- openxlsx::read.xlsx(preference)
    
    name_matches <- !(df$Student_Name %in% df_preference$`Your.Name.(Select.from.dropdown.menu)`)
    
    if(sum(name_matches) > 0 ){
        
        missing_names <- df$Student_Name[name_matches]
        
        stop(cat("The following students are missing names:", missing_names))
    }
    
    # need to grab projects in right order and order students correctly
    
    df_preference2 <- merge(studentpref, df_preference, by.x = "Student_Name", 
              by.y="Your.Name.(Select.from.dropdown.menu)",
              sort = FALSE)
    
    project_cols <- colnames(df_preference2)[grepl("Are.you.Interested.in.Project", colnames(df_preference2))]
    
    df_preference3 <- df_preference2[,c(
        "Student_Number","Student_Name",
        project_cols
       )]
    
    
    df_preference3[df_preference3 == "No"] <- 0
    df_preference3[df_preference3 == "Yes"] <- 1
    df_preference3[is.na(df_preference3)] <- 0
    
    
    colnames(df_preference3)[colnames(df_preference3) %in% project_cols] <- sub("\\."," ",regmatches(project_cols, regexpr('Project\\.\\w', project_cols)))
    real_project_names <- colnames(df_preference3)[!colnames(df_preference3) %in% c("Student_Name", "Student_Number")]
    
    missing_projects <- df_projects$Project_Name[!(df_projects$Project_Letter %in% real_project_names)]
    
    if(length(missing_projects) > 0 ){
        
        stop(cat("The following projects are missing preferences:", missing_projects))
    }

    # vec <- as.numeric(c(t(as.matrix(df_preference3))) )
    # 
    # df_preference3    
    # 
    # vec <- rep(0, numStudents *numProjects)
    
    studentpref <- df_preference3[c("Student_Number",
                                       "Student_Name",
                                       df_projects$Project_Letter)]
    
    colnames(studentpref)[colnames(studentpref) %in% df_projects$Project_Letter]<- df_projects$Project_Name
}


studentTech <- df[,c('Student_Number','Student_Name', 'TechSkill')]

projectTech <- df_projects[,c('Project_Number', 'Project_Name', 'Project_Tech_Req')]

df_nation <- df[,c("Student_Number","Country.of.Citizenship")]

# handle taiwan and china conflicts by adding to student conflict sheet
china_students <- df$Student_Number[df$Country.of.Citizenship == "China"]
tai_students <- df$Student_Number[df$Country.of.Citizenship == "Taiwan"]

if(length(china_students) > 0 & length(tai_students) > 0){
student1 = rep(china_students, each = length(tai_students))
student2 = rep(tai_students, length(china_students))

df_conflict <- data.frame(student1=student1, student2=student2)
} else {
    df_conflict <- data.frame(student1="", student2="")
}
df_student_to_project <- data.frame(studentNumber="", projectNumber="")

data_list <- list("Student Leadership Skills" = studentLead,
                  "Student Gender" = studentGender,
                  "Student Pref" = studentpref,
                  "Student Tech Skill" = studentTech,
                  "Project Tech Requirements" = projectTech,
                  "Student Nationality" = df_nation,
                  "Student Conflict"= df_conflict,
                  "Pre-assign Student" = df_student_to_project
                  # "Student Section" = studentSection
                  )

file_date <- format(Sys.Date(),"%Y%m%d")
output_file <- paste0("./data/solve_problem_input/lp_input_section_",section,"_", file_date,".xlsx")
openxlsx::write.xlsx(data_list, output_file)

return(output_file)

}
