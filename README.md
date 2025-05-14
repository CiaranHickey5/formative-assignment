# Programming Assignment - Functional Programming

## Student Information

| Field          | Value         |
| -------------- | ------------- |
| Name           | Ciaran Hickey |
| Student Number | 20088959      |

# University Timetabling System

## Overview
This project implements a student timetabling system using functional programming principles in Haskell. It provides a framework for managing courses, lecturers, rooms, student groups, and creating timetables.

## Features
- **Entity Management**: Comprehensive data structures for courses, lecturers, rooms, students, and student groups
- **Timetable Generation**: Create and validate schedules for classes
- **Validation Rules**: Validation of all entities and their relationships
- **Conflict Detection**: Automatically detects scheduling conflicts including:
  - Room double-bookings
  - Lecturer double-bookings
  - Student group schedule conflicts
  - Lecturer workload balancing
- **Report Generation**: Generates CSV reports for all entities and validation errors

## Implementation Details

### Data Model
The system is built around the following core entities:
- **Course**: Courses offered by the university/college
- **Lecturer**: Members teaching the courses
- **Room**: Teaching spaces with capacity constraints
- **Student**: Individual students enrolled in programs
- **StudentGroup**: Groups of students in specific programs and years
- **Timetable**: Scheduled classes linking courses, lecturers, rooms, and student groups
- **Allocations**: Assignments of lecturers to specific courses and groups

### Architecture
The project follows a modular design with clear separation of concerns:
- Strong typed Entity definitions
- CSV data import/export using cassava
- Validation rules implemented as functions
- Report generation that shows validation errors

### Validation Rules
The system enforces several validation rules, including:
- Foreign key constraints between entities
- Checking for double-bookings of rooms
- Preventing lecturer schedule conflicts
- Ensuring student groups aren't scheduled for multiple classes simultaneously
- Validating that lecturers aren't over-allocated beyond their available hours
- Checking that modules have the correct number of scheduled hours

### Data Storage
The CSV files are used for data storage, storing each entity:
- courses.csv - Course information
- lecturers.csv - Lecturer details and available hours
- rooms.csv - Room information and capacity
- students.csv - Student records
- student_groups.csv - Student group information
- timetable.csv - Scheduled classes
- allocations.csv - Lecturer-Course-Group assignments

## Implementation Approach
The project is built entirely in Haskell and demonstrates functional programming concepts:
- Immutable data structures
- Pure functions for validation logic
- Strong typing with record types
- Algebraic data types for validation results
- Parsing and serialization using typeclasses