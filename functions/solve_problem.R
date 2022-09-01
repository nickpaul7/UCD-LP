


solve_problem <- function(file_name, minStudents, maxStudents, china_const, india_const){

# This does following constraints:
#  Each student on 1 project
#  Between min and max students on each project
#  Assigns minimum tech power to each project
#  Assigns maximum of 1 leader per project
#  Works for scale (100 students, 20 projects) - v 08, v 09, v 10 input files
#  Works for manual assignment of conflicting students within the code v 14
#  Works for test sets of up to 50 (so Section 1 or Section 2); does not run for 100 x 20 (too big!)

#  Working on:  gender - read in, implement >= 2 females per project
#Have it reading in,  need to test that it's working

#  Working on:  input student attributes, print out gender and section in assignments (can manually adjust)
#  Working on:   output to state which student will lead which project (assignment working, just needs printout)
#  Working on:  full output for each project:  leader, team member names, tech required/assigned, women, head  count, leader count


# UC Davis MSBA Practicum Assignment
# Linear Programming written by Carrie Beam
# Started 7/12/22

# Starter code
# 
# getwd()
# setwd("C:/Users/Carrie/Documents/C UC Davis/Assign students to Practicum LP")

#install.packages("lpSolve")
library(lpSolve)
source("./functions/create_constraints.R")

options(stringsAsFactors = FALSE)

# Formulation
# Decision Variables
# Let xij = 1 if student i is assigned to project j, 0 otherwise
# Let cij = 1 if student i 'likes' project j, 0 otherwise
# Let ti = tech skills of student i (1 = good programming, 0 = otherwise)
# Let rj = tech skills required by project j (1 = need 1 programmer, 2 = need 2 programmers)
# Let bi = our assessment of student i's leadership skills (1=good leader, 0=otherwise)

# Input files:
# LP Input Vxx.csv - contains cij matrix, student i's preference for project j
# LP Input Vxx Student Tech Skills.csv - contains tech skill evaluation per student
# LP Input Vxx Project Tech Requirements.csv - contains tech skill requirement per project
# LP Input Vxx STudent Leadership Skills.csv - contains leadership skill evaluation of each student per staff

# Objective function
# Maximize (student happiness with projects) = sum(i,j) cij xij
# 
# Subject to
# Each student on exactly one project
#    sum(j) xij = 1
#
# Between minStudents and maxStudents (generally 5 and 6) students on each project
#    5 <= sum(i) xij <= 6

# changed for input v14 - allow min of 3 students


#
# Sufficient technical skill on each project
#  sum(i) ti xij >= rj
#
# At least one leader on each project
#  sum(i) bi xij >= 1
#
# Any students who must not be on certain projects?
# xij = 0 if student i must not be on project j
#
# Any conflicts between students?
# xaj + xbj = 0 for all j (student a and student b must not be on project j together)

# Now we are going to use R code to set up the various parts of our linear program
# Then we will run this code to get our output
# lp("max", f.obj, f.con, f.dir, f.rhs)
# Then we will read various output bits and write them to files

# Set coefficients of the objective function
# f.obj <- c(5, 7)
# These are read in from an external file

# Read in the input files-------------------------------------------------------
# students to projects file
# temp <- read.csv("LP Input v15 Student Pref.csv")

library(openxlsx)



studentLead <- read.xlsx(file_name, "Student Leadership Skills")
studentGender <- read.xlsx(file_name, "Student Gender")
temp <- read.xlsx(file_name, "Student Pref")
studentTech <- read.xlsx(file_name, "Student Tech Skill")
projectTech <- read.xlsx(file_name, "Project Tech Requirements")
studentNation <- read.xlsx(file_name, "Student Nationality")

projectConflict <- read.xlsx(file_name, "Student Conflict")
projectAssign <- read.xlsx(file_name, "Pre-assign Student")


# check if there are conflicts in the data and get the row number for each student
if(nrow(projectConflict) > 0 & projectConflict$student1[1] != ""){

projectConflict$student1_row <-  unlist(lapply(projectConflict$student1, function(x) which(temp$Student_Number %in% x)))
projectConflict$student2_row <-  unlist(lapply(projectConflict$student2, function(x) which(temp$Student_Number %in% x)))
}

# check if there are pre-assignments in the data and get the row number for each 
# student and project
pa_constraint = FALSE

if(nrow(projectAssign) > 0){
    if(projectAssign$studentNumber[1] != ""){
        pa_constraint = TRUE
    }
    }

if( pa_constraint){

projectAssign$student_row <-  unlist(lapply(projectAssign$studentNumber, function(x) which(temp$Student_Number %in% x)))
projectAssign$project_row <-  unlist(lapply(projectAssign$projectNumber, function(x) which(projectTech$Project_Number %in% x)))

# studentSection <- read.xlsx(file_name, "Student Section")
}


# Pull off the project names and numbers from the first row
# temp2 <- t(temp[1,])
# temp3 <- as.data.frame(cbind(c(-1:(nrow(temp2)-2)), temp2))
# temp4 <- temp3[3:nrow(temp3),]
# row.names(temp4)<-NULL
# colnames(temp4)<-c("Project_Number", "Project_Name")

temp4 <- projectTech

# Make sure we don't have any duplicate projects
n_occur<-data.frame(table(temp4$Project_Number))
n_occur2<-data.frame(table(temp4$Project_Name))

if(max(n_occur$Freq>1) || max(n_occur2$Freq >1)) {
  cat("Error:  we have a duplicate project number or name - check n_occur or n_occur2 for details")
} else { 
  cat("Good news - Project names and numbers are all unique.")
}

# Load our projects numbers and project names
df.projects <- temp4

# Pull off the student names and numbers from the left columns
temp2 <- temp[,1:2]

# remove the first line as blank
# temp3 <- temp2[2:nrow(temp2),]
# colnames(temp3)<-c("Student_Number", "Student_Name")

# Make sure we don't have any duplicate students
n_occur<-data.frame(table(temp2$Student_Number))
n_occur2<-data.frame(table(temp2$Student_Name))

if(max(n_occur$Freq>1) || max(n_occur2$Freq>1)) {
  cat("Error:  we have a duplicate student number or name - check n_occur or n_occur2 for details")
} else { 
  cat("Good news - Student names and numbers are all unique.")
}


# Load our student numbers and student numbers
row.names(temp2)<- temp2$Student_Number
df.students <- temp2

# Pull out our cij (student preferences) cost matrix from the input file 
# start from our temp again
# temp2 <- temp
# names(temp2)<-lapply(temp[1,], as.character)
# colnames(temp2)[1]<-"Student_Number"
# # drop the student name column and the project name row
# temp3<-temp2[-1,-2]
# rownames(temp3)<-temp3$Student_Number

df.costs <- temp[,!colnames(temp)== "Student_Name"]


# Make sure the cost matrix matches row-wise the students

if (sum(df.students$Student_Number!= df.costs$Student_Number)!= 0) {
  cat("Error - student numbers in df.projects do not match those in df.costs")
}

# peel off the student number column and check column names
df.costs <- df.costs[,-1]

# Make sure the cost matrix matches columnwise the projects 

#need to fix the "." in column names when it happens - 
if (sum(gsub(" ",".", t(df.projects)[2,]) != colnames(df.costs))!= 0) {
  cat("Error - project names in df.projects do not match those in df.costs")
}

# Make sure costs are either 1 or 0 only
temp <- sum(df.costs ==1) + sum(df.costs == 0)

if(temp != (nrow(df.costs) * ncol(df.costs))) {
  cat("Error!  We have entries in the student costs matrix which are not 1 or 0")
}

cat("We should have ",nrow(df.costs) * ncol(df.costs), " total student expressions. Of these,")
cat("We have ",sum(df.costs==1), " students preferences expressed for some project")
cat("We have ",sum(df.costs==0), " students preferences not for some project")

# OK, we made it!  Now we should have
# df.projects[j] - a data frame listing the projects in it
# df.students[i] - a data frame listing the students in it
# df.costs[i][j] - a data frame listing student i's preference for project j

cat("Getting ready to generate LP formulation")
cat("We have ", nrow(df.projects), " projects and ")
cat("We have ", nrow(df.students), " students ")
cat("Our cost matrix reflects ",nrow(df.costs)," student preferences for ",ncol(df.costs)," projects")

cat("Here is how many students preferred each project")
colSums(df.costs == 1)

temp<-cbind(df.students, as.data.frame(rowSums(df.costs==1)))
colnames(temp)[3]<-"Number of Projects Preferred by this Student"
cat("Here is how many projects were preferred by a given student")
temp

# Clean up a little bit
remove(temp, temp2, temp4, n_occur, n_occur2)


ncol(df.costs)

# Assign each student to exactly one project
# Each student on exactly one project
# Assumes students are numbered starting at 1 and going up by 1 not missing any
#    sum(j) xij = 1

numStudents = nrow(df.costs)
numProjects = ncol(df.costs)

# Make sure our minStudents and maxStudents are feasible with problem size
if (minStudents * numProjects > numStudents) {
  stop("Error:  not enough students to staff each project at minimum levels")
} 
  

# Set up our objective function
# objective is to maximize our happiness
# Convert our costs to numeric then unspool into a single dimensional cost vector
d <- as.data.frame(sapply(df.costs, as.numeric))
objective.in = as.vector(t(as.matrix(d)))

# Line up our constraints

# Constraint #1:  
# each student is on one project - student xij assignment
const.mat1 <- as.data.frame(
    outer(c(1:numStudents), 
          c(rep(1:numStudents, each=numProjects)), 
          "==")
    )

# constraint is equality
const.dir1 <- rep("==", numStudents)

# RHS is 1
const.rhs1 = rep(1, numStudents)


# Constraints #2 and #3:
# Between 5 and 6 students on each project
#    5 <= sum(i) xij <= 6
# Start with a data frame of all zeros
const.mat2 <- data.frame(matrix(0, ncol=numStudents*numProjects, nrow=numProjects))

for (j in 1:numProjects) {
  for (c in seq(j, numStudents*numProjects, by=numProjects))
  {
#    cat("  j=", j, ", c is", c)
    const.mat2[j,c]=1
  }
#  cat("\n\n")
}
# the other constraint matrix for <= 6 is exactly the same
const.mat3 <- const.mat2

# constraint is greater than or equal to minStudents
const.dir2 <- rep(">=", numProjects)

# constraint is less than or equal to maxStudents
const.dir3 <- rep("<=", numProjects)

# RHS is minStudents
const.rhs2 = rep(minStudents, numProjects)

# RHS is maxStudents
const.rhs3 = rep(maxStudents, numProjects)

# Constraint #4:  
# Sufficient technical skill on each project
#  sum(i) ti xij >= rj

# studentTech <- read.csv("LP Input v15 Student Tech Skills.csv")
# projectTech <- read.csv("LP Input v15 Project Tech Requirements.csv")

# Check that the students are the same as in the costs matrix
x1 <- sum(df.students$Student_Name == studentTech$Student_Name)
x2 <- sum(df.students$Student_Number == studentTech$Student_Number)
if (x1+ x2 != 2*numStudents) {
  cat("Error with Student Tech Skills - number or name of students does not match cost matrix input file")
}

# Check that the projects are the same as in the projects matrix
x1 <- sum(df.projects$Project_Number == projectTech$Project_Number)
x2 <- sum(df.projects$Project_Name == projectTech$Project_Name)
if (x1+ x2 != 2*numProjects) {
  cat("Error with Project Tech requirements - number or name of projects does not match cost matrix input file")
}



# Want sum(ti xij) >= tech requirement of Project j
# Get the tech skills for each student spooled out
temp1 <- rep(studentTech$TechSkill, each=numProjects)

# Because of how data frame multiplication works, need to transpose
# our constraint matrix, multiply it by the tech skill of the student,
# then transpose it back 

const.mat4 <- as.data.frame(t(t(const.mat2) * temp1))

# constraint is tech ability of students must be greater than or equal to
const.dir4 <- rep(">=", numProjects)

# RHS is Tech Skills required by project
const.rhs4 = projectTech$Project_Tech_Req


# Constraint #5:  ideally only one leader per team
# studentLead <- read.csv("LP Input v15 Student Leadership Skills.csv")

# Check that the students Leadership are the same as in the costs matrix
x1 <- sum(df.students$Student_Name == studentLead$Student_Name)
x2 <- sum(df.students$Student_Number == studentLead$Student_Number)
if (x1+ x2 != 2*numStudents) {
  cat("Error with Student Leadership Skills - number or name of students does not match cost matrix input file")
}

total_leaders <- sum(as.numeric(studentLead$Leader))

# Want sum(Lead(i) xij) <= 1 leader on Project j
# Get the leadership skills for each student spooled out
temp1 <- rep(as.numeric(studentLead$Leader), each=numProjects)

# Because of how data frame multiplication works, need to transpose
# our constraint matrix, multiply it by the leadership skill of the student,
# then transpose it back 

const.mat5 <- as.data.frame(t(t(const.mat2) * temp1))

# constraint is tech ability of students must be greater than or equal to
const.dir5 <- rep("<=", numProjects)

# RHS is Leadership Skills required by project
const.rhs5 = rep(ceiling(total_leaders / numProjects), numProjects)


const.mat5a <- as.data.frame(t(t(const.mat2) * temp1))

# constraint is tech ability of students must be greater than or equal to
const.dir5a <- rep(">=", numProjects)

# RHS is Leadership Skills required by project
const.rhs5a = rep(floor(total_leaders / numProjects), numProjects)


# Constraint #6:  Individual students A and B not on the same project

# Function to implement constraints, dir, and rhs for Student A and B not on same project
# Will have j rows (one for each project) for each AB student pair

if(projectConflict$student1[1] != ""){
    
    projectConflict$constr <- lapply(1:nrow(projectConflict), function(x)create_student_conflict_constr(student1 = projectConflict[x,"student1_row"],
                                                                                                      student2 = projectConflict[x,"student2_row"],numProjects, numStudents) )

    const.mat.proj_conflict <- do.call(rbind, projectConflict$constr)
    const.dir.proj_conflict  <-  rep("<=", nrow(projectConflict) * numProjects)
    const.rhs.proj_conflict  <- rep(1, nrow(projectConflict) * numProjects)
    
} else {
    const.mat.proj_conflict <- matrix(0,1, numStudents*numProjects)
    const.dir.proj_conflict <-  rep("<=", 1)
    const.rhs.proj_conflict  <- rep(1, 1)
}

# end my new code 

# student_conflict <- function(student_a, student_b,  numStudents, numProjects) {
# #  cat("Hello from student_conflict function\n")
# #  cat(paste("Conflict between student ", student_a," and student ", student_b,"\n"))
# #  cat(paste("We had ", numStudents," students on ", numProjects," projects\n"))
#   
#   const.mat.temp <- data.frame(matrix(0, ncol=numStudents *numProjects, nrow=numProjects))
#   
#   # Iterate through projects j to numProjects
#   for (j in 1:numProjects) {
#     
#     # Compute index of xaj in our constraint matrix
#     # Remember xij is for student i on project j
#     # There are a total of (numStudents * numProjects) columns in this matrix
#     # The ith student's interaction with the jth project is given in column ((i-1)*numProjects)+j
#     
#     a = ((student_a - 1) * numProjects) + j
#     b = ((student_b - 1) * numProjects) + j
# #    cat(paste("\n --------------- \nWe are now on Project ",j,"\n"))
# #    cat(paste("Student ", student_a," on project ", j," is at column ",a,"\n"))  
# #    cat(paste("Student ", student_b," on project ", j," is at column ",b,"\n"))  
#     
#     # implement a constraint that student a and student b are not both on project j
#     const.mat.temp[j,a] = 1
#     const.mat.temp[j,b] = 1
#     
#   }
#   
# #  cat("Our const.mat.temp is now\n")
# #  print(const.mat.temp)
# #  cat("Returning from student_conflict function now\n")
#   return(const.mat.temp)
# }
# 
# # Call our student_conflict function and consolidate the results
# # Set up the containers with dummy data (will remove after matrix is fully compiled)
# const.mat6 <- data.frame(matrix(99, ncol=numStudents *numProjects, nrow=1))
# 
# # Student 1 and 11 in conflict
# temp <- student_conflict(1, 11, numStudents, numProjects)
# const.mat6 <- rbind(const.mat6, temp)
# 
# # Student 3 and 4 in conflict
# #temp <- student_conflict(3, 4, numStudents, numProjects)
# #const.mat6 <- rbind(const.mat6, temp)
# 
# # Student 4 and 11 in conflict
# #temp<-student_conflict(4, 11, numStudents, numProjects)
# #const.mat6 <- rbind(const.mat6, temp)
# 
# # Remove the dummy first row from the const.mat6
# const.mat6 <- const.mat6[-1,]
# 
# # All of these are <= (we have at most one of the conflicting students on a project)
# const.dir6 <- rep("<=", nrow(const.mat6))
# 
# # Right hand side is at most one of the conflicting students per project row
# const.rhs6 <- rep(1, nrow(const.mat6))

# np edit - removed all of constraint 7

# Constraint #7:  Each project staffed entirely from Section 1 or entirely from Section 2
# This is the same as Student A has conflict with Student B if sections are not the same
# Going to piggyback off of student_conflict function from above

# Read in student section data
# studentSection <- read.csv("LP Input v15 Student Section.csv")

# Check that the students are the same as in the costs matrix
# x1 <- sum(df.students$Student_Name == studentSection$Student_Name)
# x2 <- sum(df.students$Student_Number == studentSection$Student_Number)
# if (x1+ x2 != 2*numStudents) {
#   cat("Error with Student Sections - number or name of students does not match cost matrix input file")
# }

# Check that every student is assigned to either section 1 or section 2
# if (sum(studentSection$Section==1) + sum(studentSection$Section==2)!=numStudents) {
#   cat("Error - all students must be in Section 1 or Section 2")
#   
# }

# We want to implement a conflict between all Students A and B
# when Student A is in one section and Student B is in another section

# # Set up the containers with dummy data (will remove after matrix is fully compiled)
# const.mat7 <- data.frame(matrix(99, ncol=numStudents *numProjects, nrow=1))
# 
# # Call our student_conflict function for each conflicting pair
# for (a in 1:(numStudents-1)) {
#   for (b in (a+1):numStudents) {
# #    cat("\nComparing Student ", a," and Student ",b)
#     
#     if(studentSection$Section[a] != studentSection$Section[b]) {
#       # Student a and b in conflict
# #      cat("\n  Student ",a," is in Section ",studentSection$Section[a])
# #      cat("\n  Student ",b," is in Section ",studentSection$Section[b])
# #      cat("\tThe students are not in the same section, calling student_conflict function\n")
# 
#       temp <- student_conflict(a, b, numStudents, numProjects)
#       const.mat7 <- rbind(const.mat7, temp)
#     }
#     else {
# #      cat("\nStudent ", a," and Student ",b," are in same section, no conflict\n")
#     }
#   }
# }
# 
# # Remove the dummy first row from the const.mat7
# const.mat7 <- const.mat7[-1,]
# 
# # All of these are <= (we have at most one of the conflicting students on a project)
# const.dir7 <- rep("<=", nrow(const.mat7))
# 
# # Right hand side is at most one of the conflicting students per project row
# const.rhs7 <- rep(1, nrow(const.mat7))

# -------------------------------
# Constraint #8:  gender (at least 2 females on each team)
# For our purposes, they are either Female or Not Female (==Male, missing, did not state)
# studentGender <- read.csv("LP Input v15 Student Gender.csv")

# Check that the students Gender are the same as in the costs matrix
x1 <- sum(df.students$Student_Name == studentGender$Student_Name)
x2 <- sum(df.students$Student_Number == studentGender$Student_Number)
if (x1+ x2 != 2*numStudents) {
  cat("Error with Student Gender - number or name of students does not match cost matrix input file")
}

# Convert the gender to 1 if female, 0 otherwise
studentGender$GenderCoded <- ifelse(studentGender$Gender=="Female", 1, 0)

# How many females?
cat("We have ", sum(studentGender$GenderCoded), " females\n")

# Want sum(GenderCoded (i) xij) >= 2 females on Project j
# Get the gender  for each student spooled out
temp1 <- rep(studentGender$GenderCoded, each=numProjects)

# Because of how data frame multiplication works, need to transpose
# our constraint matrix, multiply it by the gender coding of the student,
# then transpose it back 

const.mat8 <- as.data.frame(t(t(const.mat2) * temp1))

# constraint is genderCoded sum of students must be greater than or equal to
const.dir8 <- rep(">=", numProjects)

# RHS is GenderCoded number of females required by project (generally 2)
const.rhs8 = rep(2, numProjects)

# Constraint 8b: more than 1 male per team -------------------------------------

const.mat8b <- create_project_constraint(studentGender, numProjects, "Gender", "Male")
const.dir8b <- rep(">=", numProjects)
const.rhs8b = rep(1, numProjects)

# constraint 9: nationality ----------------------------------------------------

# India Constraint

const.mat.india <- create_project_constraint(studentNation, numProjects, "Country.of.Citizenship", "India")
const.dir.india <- rep("<=", numProjects)
const.rhs.india = rep(india_const, numProjects)

# china constraint

const.mat.china <- create_project_constraint(studentNation, numProjects, "Country.of.Citizenship", "China")
const.dir.china <- rep("<=", numProjects)
const.rhs.china = rep(china_const, numProjects)

# Constraint 10: pre-assign student to project----------------------------------

if(pa_constraint){

projectAssign$constr <- lapply(1:nrow(projectAssign), function(x)create_student_to_project_constr(student_num = projectAssign[x,"student_row"],
                                                                                                  project_num = projectAssign[x,"project_row"],numProjects, numStudents) )

# const.student1project3 <- create_student_to_project_constr(student_num = 1,
#                                                            project_num = 3,numProjects, numStudents)

const.mat.stu_to_proj <- do.call(rbind, projectAssign$constr)

const.dir.stu_to_proj  <-  rep("=", nrow(projectAssign))
const.rhs.stu_to_proj  <- rep(1, nrow(projectAssign))

} else {
    const.mat.stu_to_proj <- matrix(0,1, numStudents*numProjects)
    const.dir.stu_to_proj  <-  rep("<=", 1)
    const.rhs.stu_to_proj  <- rep(1, 1)
}

# -------------------------------
# Bundle our constraints, directions, and RHS together

# use the same column names for all constraint matrices 
names(const.mat2) <- names(const.mat1)
names(const.mat3) <- names(const.mat1)
names(const.mat4) <- names(const.mat1)
names(const.mat5) <- names(const.mat1)
names(const.mat5a) <- names(const.mat1)
# names(const.mat6) <- names(const.mat1)
# names(const.mat7) <- names(const.mat1)
names(const.mat8) <- names(const.mat1)
names(const.mat8b) <- names(const.mat1)
names(const.mat.india)  <- names(const.mat1)
names(const.mat.china)  <- names(const.mat1)
names(const.mat.stu_to_proj) <- names(const.mat.stu_to_proj)

const.mat <- rbind(const.mat1, const.mat2, const.mat3, const.mat4, const.mat5,
                   const.mat5a,
                   const.mat.proj_conflict,
                   const.mat8,
                   const.mat8b,
                   const.mat.india, const.mat.china,
                   const.mat.stu_to_proj
                   )

const.dir <- c(const.dir1, const.dir2, const.dir3, const.dir4, const.dir5, 
               const.dir5a,
               const.dir.proj_conflict,
               const.dir8,
               const.dir8b,
               const.dir.india, const.dir.china,
               const.dir.stu_to_proj)

const.rhs <- c(const.rhs1, const.rhs2, const.rhs3, const.rhs4, const.rhs5, 
               const.rhs5a,
               const.rhs.proj_conflict,
               const.rhs8,
               const.rhs8b,
               const.rhs.india, const.rhs.china,
               const.rhs.stu_to_proj)



# run our linear program
#x <- lp(direction="max", objective.in, const.mat1, const.dir1, const.rhs1,all.bin=TRUE)
x <- lp(direction="max", objective.in, const.mat, const.dir, const.rhs,all.bin=TRUE)
x$solution
x$objval
x$status

# add constraints, input data, and solution to a list to return at the end of the function
return_list <- list()
return_list$lp_result <- x
return_list$constraints <- list(const.mat1 = const.mat1, 
                                const.mat2 = const.mat2, 
                                const.mat3 = const.mat3, 
                                const.mat4 = const.mat4, 
                                const.mat5 = const.mat5, 
                                const.mat5a = const.mat5a,
                    const.mat.proj_conflict = const.mat.proj_conflict ,
                    const.mat8 = const.mat8,
                    const.mat8b = const.mat8b,
                    const.mat.india = const.mat.india, 
                    const.mat.china = const.mat.china,
                    const.mat.stu_to_proj = const.mat.stu_to_proj )

return_list$rhs <- list(const.rhs1 = const.rhs1, 
                        const.rhs2 = const.rhs2, 
                        const.rhs3 = const.rhs3, 
                        const.rhs4 = const.rhs4, 
                        const.rhs5 = const.rhs5, 
                        const.rhs5a = const.rhs5a,
                        const.rhs.proj_conflict = const.rhs.proj_conflict,
                        const.rhs8 = const.rhs8,
                        const.rhs8b = const.rhs8b,
                        const.rhs.india = const.rhs.india, 
                        const.rhs.china = const.rhs.china,
                        const.rhs.stu_to_proj = const.rhs.stu_to_proj)

return_list$input_data$df.projects <- df.projects
return_list$input_data$df.students <- df.students
return_list$input_data$studentTech <- studentTech
return_list$input_data$projectTech <- projectTech
return_list$input_data$studentLead <- studentLead
return_list$input_data$studentGender <- studentGender


# create data frame that has the project each student is assigned to

assignments <- matrix(x$solution, nrow=numStudents, byrow=TRUE)
assignments2 <- assignments == 1
proj_num <- apply(assignments2, 1, which)
project_names <- t(df.projects)[2,]
project_assingments <- project_names[proj_num]
df.students$project <- project_assingments

output_file <- paste0( file_name,"_student_assignments.csv") 

output_file <- gsub("input","output", output_file)
print(output_file)

write.csv(df.students, output_file)

return(return_list)

}


