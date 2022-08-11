source("./functions/solve_problem.R")
source("./functions/create_input_data.R")

minStudents = 5
maxStudents = 6
china_const = 3
india_const = 4
input1 <- "./data/solve_probem_input/lp_input_section_1_20220809.xlsx"
lp_ob <- solve_problem(input1, minStudents, maxStudents, china_const, india_const)

# constraint 4 tech requirement-------------------------------------------------

x <- lp_ob$lp_result
x$objval
tech <- as.matrix(lp_ob$constraints$const.mat4)
cbind(lp_ob$input_data$projectTech, solution = tech %*% x$solution)

# change tech skills so the constraint is binding 
df_assignments <- read.csv("./data/solve_problem_output/lp_input_section_1_20220809.xlsx_student_assignments.csv")
assign_tech <- cbind(lp_ob$input_data$studentTech, project = df_assignments$project)
assign_tech[assign_tech$TechSkill == 100 & assign_tech$project == "Project C",]

df_assignments[df_assignments$Student_Number == "19",]
lp_ob$input_data$studentTech[lp_ob$input_data$studentTech$TechSkill == 100,]

# change tech skills so that student 19 has to be reassigned

input2 <- "./test/lp_input_section_1_20220809_tech_test.xlsx"
lp_ob_tech <- solve_problem(input2, minStudents, maxStudents, china_const, india_const)

x2 <- lp_ob_tech$lp_result
x2$solution
x2$objval
x2$status
all(x2$solution == x$solution)

# the tech skills are changed so only 3 students have 100
lp_ob_tech$input_data$studentTech[lp_ob_tech$input_data$studentTech$TechSkill == 100,]

# tech constraint is satisfied
tech <- as.matrix(lp_ob_tech$constraints$const.mat4)
cbind(tech %*% x2$solution, lp_ob_tech$rhs$const.rhs4)

# check student 19 
df_assignments_tech <- read.csv("./test/lp_input_section_1_20220809_tech_test.xlsx_student_assignments.csv")
df_assignments_tech$tech <- df_assignments_tech$project
df_assignments_tech <- df_assignments_tech[,!(names(df_assignments_tech) %in% c("project"))]
df_tech_check <- merge(df_assignments, df_assignments_tech, all = TRUE)
df_tech_check[df_tech_check$Student_Number == 19,]

# constraint 5 Leadership -----------------------------------------------------------------

leader <- as.matrix(lp_ob$constraints$const.mat5)
cbind( solution = leader %*% x$solution,lp_ob$rhs$const.rhs5 )

# Make 4 leaders on project 1

df_leader_assign <- cbind(df_assignments, lp_ob$input_data$studentLead)

df_leader_assign[df_leader_assign$project == "Project A",]
#left off here need to fix the leader -----
# Make Student 11 a leader

leader_input <- "./test/lp_input_section_1_20220809_leader.xlsx"

lp_ob_leader <- solve_problem(leader_input, minStudents, maxStudents, china_const, india_const)

x3 <- lp_ob_leader$lp_result
x3$solution
x3$objval
x3$status
all(x3$solution == x$solution)

as.matrix(lp_ob_leader$constraints$const.mat5) %*% x3$solution

df_assignments_leader <- read.csv("./test/lp_input_section_1_20220809_leader.xlsx_student_assignments.csv")

# student 45 and 47 are no longer on Project A
df_assignments[df_assignments$project == "Project A",]
df_assignments_leader[df_assignments_leader$project == "Project A",]


# constraint 6 - two students not on same project-------------------------------

df_assignments[c(1,13),]

# 2 and 16 are on the same project 

input_conflict <- "./test/lp_input_section_1_20220809_student_conflict.xlsx"
lp_ob_conflict <- solve_problem(input_conflict, minStudents, maxStudents, china_const, india_const)

lp_ob_conflict$lp_result$status

df_assignments_conflict <- read.csv("./test/lp_input_section_1_20220809_student_conflict.xlsx_student_assignments.csv")

df_assignments[df_assignments$Student_Number %in% c(2, 20),]
df_assignments_conflict[df_assignments_conflict$Student_Number %in% c(2, 20),]

# constraint 8 - at least 2 females --------------------------------------------

as.matrix(lp_ob$constraints$const.mat8) %*% x$solution

df_assignments[df_assignments$project == "Project B",]

# assign all students on Project B as males

input_female = "./test/lp_input_section_1_20220809_female.xlsx"
lp_ob_female <- solve_problem(input_female, minStudents, maxStudents, china_const, india_const)

lp_ob_female$lp_result$status
# number of females for each project
as.matrix(lp_ob_female$constraints$const.mat8) %*% lp_ob_female$lp_result$solution

df_assignments_female <- read.csv("./test/lp_input_section_1_20220809_female.xlsx_student_assignments.csv")

df_assignments[df_assignments$project == "Project B",]
df_assignments_female[df_assignments_female$project == "Project B",]
df_assignments_female$female <- df_assignments_female$project
df_assignments_female <- df_assignments_female[!(names(df_assignments_female) %in% c('project'))]

# two males are reassigned from the all male team
df_female_check <- merge(df_assignments, df_assignments_female, all = TRUE)
df_female_check[df_female_check$project == "Project B",]

# constraint 8 b ---------------------------------------------------------------
# assign all students on Project B as females

input_male = "./test/lp_input_section_1_20220809_male.xlsx"
lp_ob_male <- solve_problem(input_male, minStudents, maxStudents, china_const, india_const)

lp_ob_male$lp_result$status

# number of males per project
as.matrix(lp_ob$constraints$const.mat8) %*% lp_ob_male$lp_result$solution

df_assignments_male <- read.csv("./test/lp_input_section_1_20220809_male.xlsx_student_assignments.csv")

df_assignments[df_assignments$project == "Project B",]
df_assignments_male[df_assignments_male$project == "Project B",]
df_assignments_male$male <- df_assignments_male$project
df_assignments_male <- df_assignments_male[!(names(df_assignments_male) %in% c('project'))]

# one female is reassigned from the all female team
df_male_check <- merge(df_assignments, df_assignments_male, all = TRUE)
df_male_check[df_male_check$project == "Project B",]

# india constraint -------------------------------------------------------------

as.matrix(lp_ob$constraints$const.mat.india) %*% x$solution
df_assignments[df_assignments$project == "Project A",]

input_india <- "./test/lp_input_section_1_20220809_india.xlsx"

lp_ob_india <- solve_problem(input_india, minStudents, maxStudents, china_const, india_const)

lp_ob_india$lp_result$status

as.matrix(lp_ob_india$constraints$const.mat.india) %*% lp_ob_india$lp_result$solution
df_assignments_india <- read.csv("./test/lp_input_section_1_20220809_india.xlsx_student_assignments.csv")

df_assignments_india$india <- df_assignments_india$project
df_assignments_india <- df_assignments_india[!(names(df_assignments_india) %in% c('project'))]

# three from india are reassigned
df_india_check <- merge(df_assignments, df_assignments_india, all = TRUE)
df_india_check[df_india_check$project == "Project A",]
# china constraint -------------------------------------------------------------

as.matrix(lp_ob$constraints$const.mat.china) %*% x$solution

df_assignments[df_assignments$project == "Project B",]

# make all members of Project B from china
input_china <- "./test/lp_input_section_1_20220809_china.xlsx"

lp_ob_china <- solve_problem(input_china, minStudents, maxStudents, china_const, india_const)

lp_ob_china$lp_result$status

#number from chin on each project
as.matrix(lp_ob_china$constraints$const.mat.china) %*% lp_ob_china$lp_result$solution
df_assignments_china <- read.csv("./test/lp_input_section_1_20220809_china.xlsx_student_assignments.csv")

df_assignments_china$china <- df_assignments_china$project
df_assignments_china <- df_assignments_china[!(names(df_assignments_china) %in% c('project'))]

# reassignments
df_china_check <- merge(df_assignments, df_assignments_china, all = TRUE)
df_china_check[df_china_check$project == "Project B",]

# preassign student to project -------------------------------------------------

df_assignments[1,]

# assign student 2 to project C

input_preassign <- "./test/lp_input_section_1_20220809_preassign.xlsx"

lp_ob_preassign <- solve_problem(input_preassign, minStudents, maxStudents, china_const, india_const)

lp_ob_preassign$lp_result$status

df_assignments_preassign <- read.csv("./test/lp_input_section_1_20220809_preassign.xlsx_student_assignments.csv")
df_assignments_preassign[1,]
