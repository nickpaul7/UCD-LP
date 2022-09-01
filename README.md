-   <a href="#purpose" id="toc-purpose">Purpose</a>
-   <a href="#overview-of-components"
    id="toc-overview-of-components">Overview of Components</a>
    -   <a href="#overview-of-code-components"
        id="toc-overview-of-code-components">Overview of Code Components</a>
    -   <a href="#overview-of-data-sets" id="toc-overview-of-data-sets">Overview
        of Data Sets</a>
-   <a href="#workflow" id="toc-workflow">Workflow</a>
    -   <a href="#prepare-the-data" id="toc-prepare-the-data">Prepare the
        Data</a>
        -   <a href="#student-updates" id="toc-student-updates">Student Updates</a>
        -   <a href="#project-updates" id="toc-project-updates">Project Updates</a>
    -   <a href="#create-the-lp-input-file"
        id="toc-create-the-lp-input-file">Create the LP Input File</a>
    -   <a href="#run-solve-problem" id="toc-run-solve-problem">Run Solve
        Problem</a>
    -   <a href="#combine-assignments" id="toc-combine-assignments">Combine
        Assignments</a>
    -   <a href="#review-results" id="toc-review-results">Review Results</a>

# Purpose

This document covers the basic components of the UCD LP code and data
and describes a workflow to complete the assignment process.

# Overview of Components

## Overview of Code Components

1.  `main_assign_students.R`: R Script to create assignments for both
    sections.

    -   Input Data: A file strucutred like
        `Attributes to Assist in Practicum Team Creation - dummy input data np edits.xlsx`

    -   Dependencies:

        1.  `create_input_data.R`: Function that takes student data and
            creates input file with one sheet for each constraint
            matrix.
        2.  `solve_problem.R`: Function that takes an excel workbook
            with sheets for each constraint matrix, solves the
            assignment problem, and returns a list with input data,
            constraints, right hide side values, and the lp object with
            solution. It also writes a csv with the assignments for each
            student. The file name for this csv will containt the input
            the file name and append `student_assignments`.
        3.  `create_constraints.R`: Helper functions used by
            `solve_problem.R`

2.  `combine_assignments.R`: R Script that takes the output CSVs from
    `solve_problem.R`, combines them into one data set and writes to
    `lp_report.xlsx`.

3.  `test_constraints.R`: R Script that tests each constraint.

## Overview of Data Sets

1.  `Attributes to Assist in Practicum Team Creation - dummy input data np edits.xlsx`:
    Formatted data set that contains data about students and projects.
2.  `lp_input_section_x_YYYYMMDD.xlsx`: Input file for
    `solve_problem.R`. Contains a sheet for each of constraint matrix.
3.  `lp_input_section_x_YYYYMMDD.xlsx_student_assignments.xlsx`: Output
    of `solve_problem.R`. Contains student name, student number, and
    project assignment.
4.  `lp_report.xlsx`: Final output of process with student assignments
    from both sections, all student data, and pivot tables that show the
    composition of each project team.

# Workflow

## Prepare the Data

To begin, a file with the same structure as
`Attributes to Assist in Practicum Team Creation - dummy input data np edits.xlsx`
needs to created.

### Student Updates

    1. Tech Skills
    2. Leadership Indicator

### Project Updates

    1. Required Tech Skills

## Create the LP Input File

1.  To create the LP input file for each section, open
    `main_assign_students.R`.
2.  Update input script to use the file you prepared in the previous
    section.
3.  Run `create_input_file()`
4.  Update the output from `create_input_file()`
    1.  Student Conflicts
    2.  Student pre-assignments
    3.  Student preferences

## Run Solve Problem

1.  Update the arguments for `solve_problem()`
    1.  minStudents = 4
    2.  maxStudents = 6
    3.  china\_const = 3
    4.  india\_const = 4
2.  Run `solve_problem`
3.  Repeat for second section

## Combine Assignments

1.  Open `combine_assignments.R`
2.  Update section1 and section2 file names.
3.  Run script

## Review Results

1.  Open `./data/combine_assigments_output/[today's date]lp_report.xlsx`
2.  Refresh pivot tables
3.  Inspect Results
