

create_input_file <- function(file, section, random_preference = TRUE){

df <- openxlsx::read.xlsx(file)
df <- df[df$Section == section,]
df_projects <- openxlsx::read.xlsx(file, sheet = "Projects")
df_projects <- df_projects[df_projects$Section == section,]


numStudents = nrow(df)
numProjects = nrow(df_projects)
df_projects$Project_Name

studentLead <- df[,c('Student_Number','Student_Name', 'Leadership')]
studentLead$Leader[studentLead$Leadership == "Yes"] <- 1
studentLead$Leader[studentLead$Leadership == "No"] <- 0

studentGender <- df[,c('Student_Number','Student_Name', 'Gender')]

studentpref <- df[,c('Student_Number','Student_Name')]


#randomly assign preference
if(random_preference == TRUE){
vec <- ifelse(runif(numStudents*numProjects)<=.4,1,0) 
}else{
    vec <- rep(0, numStudents *numProjects)
}
df_dummy <- data.frame(matrix(vec, nrow = numStudents, ncol = numProjects))
colnames(df_dummy) <- df_projects$Project_Name
studentpref <- cbind(studentpref, df_dummy)

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
output_file <- paste0("./data/solve_probem_input/lp_input_section_",section,"_", file_date,".xlsx")
openxlsx::write.xlsx(data_list, output_file)

return(output_file)

}
