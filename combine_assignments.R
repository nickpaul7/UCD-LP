
section1 <- "./data/solve_problem_output/lp_output_section_1_20220906.xlsx_student_assignments.csv"
section2 <- "./data/solve_problem_output/lp_output_section_2_20220906.xlsx_student_assignments.csv"
file <-  "./data/student_info/Attributes to Assist in Practicum Team Creation - input 2022 08 28 scrubbed (1).xlsx"

df_projects <- openxlsx::read.xlsx(file, sheet = "Projects")

df_projects2 <- df_projects[,c('Project_Name','Project_Tech_Req')]


sections <- list(section1,section2)

all_assignments_list <- lapply(sections, read.csv, row.names = NULL)

all_assignments <- do.call(rbind,all_assignments_list)

all_assignments <- all_assignments[!(names(all_assignments) %in% ('X'))]



df <- openxlsx::read.xlsx(file)

df_final <- merge(df, all_assignments, all = TRUE, sort= FALSE)
df_final2 <- merge(df_final, df_projects2, all.x =TRUE, sort = FALSE, by.x = "project",
                   by.y = "Project_Name")

wb <- openxlsx::loadWorkbook("./data/combine_assignments_output/lp_report.xlsx")
openxlsx::deleteData(wb, sheet = "Data", rows = 1:500, cols = 1:500, gridExpand = TRUE )
openxlsx::writeData(wb, sheet = "Data", df_final2)
tday <- gsub("-","", Sys.Date())
openxlsx::saveWorkbook(wb,
                       stringr::str_c("./data/combine_assignments_output/",
                       tday,
                             "lp_report.xlsx")
                       ,overwrite = T)


