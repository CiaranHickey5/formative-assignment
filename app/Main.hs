import Lecturer
import Course
import Room
import Student
import StudentGroup
import Timetable
import TimetableValidator (ValidationIssue(..), validateTimetableSystem, 
                          checkLecturerOverscheduling, checkRoomDoubleBookings)
import Data.List (find, partition)
import Data.Csv (decodeByName, Header)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V (Vector, toList)
import qualified Data.Csv as Csv (encodeDefaultOrderedByName)

main :: IO ()
main = do
  result <- readLecturers "data/lecturers.csv"
  case result of
    Left err  -> putStrLn $ "Error: " ++ err
    Right lecturers -> do  
      -- Print all lecturers
      putStrLn "=== All Lecturers ==="
      mapM_ print lecturers
      putStrLn $ "Total lecturers: " ++ show (length lecturers)
      
      -- Separate valid and invalid lecturers
      let (validLecturers, invalidLecturers) = separateValidation lecturers
      
      -- Print valid lecturers
      putStrLn "\n=== Valid Lecturers ==="
      if null validLecturers
        then putStrLn "No valid lecturers found."
        else mapM_ print validLecturers
      putStrLn $ "Total valid lecturers: " ++ show (length validLecturers)
      
      -- Print invalid lecturers with reasons
      putStrLn "\n=== Invalid Lecturers ==="
      if null invalidLecturers
        then putStrLn "No invalid lecturers found."
        else mapM_ printInvalidLecturer invalidLecturers
      putStrLn $ "Total invalid lecturers: " ++ show (length invalidLecturers)

      -- Write valid lecturers to CSV
      writeValidLecturers validLecturers
      
      -- Check timetable rule
      putStrLn "\n=== Timetable Rule Check ==="
      putStrLn $ checkLecturerAvailability validLecturers
      
      -- Load other entities
      putStrLn "\n=== Reading Courses ==="
      courseResult <- readCourses "data/courses.csv"
      case courseResult of
        Left err  -> putStrLn $ "Error: " ++ err
        Right courses -> do
          putStrLn "=== All Courses ==="
          mapM_ print courses
          putStrLn $ "Total courses: " ++ show (length courses)
      
      putStrLn "\n=== Reading Rooms ==="
      roomResult <- readRooms "data/rooms.csv"
      case roomResult of
        Left err  -> putStrLn $ "Error: " ++ err
        Right rooms -> do
          putStrLn "=== All Rooms ==="
          mapM_ print rooms
          putStrLn $ "Total rooms: " ++ show (length rooms)

      putStrLn "\n=== Reading Students ==="
      studentResult <- readStudents "data/students.csv"
      case studentResult of
        Left err  -> putStrLn $ "Error: " ++ err
        Right students -> do
          putStrLn "=== All Students ==="
          mapM_ print students
          putStrLn $ "Total students: " ++ show (length students)
      
      putStrLn "\n=== Reading Student Groups ==="
      groupResult <- readStudentGroups "data/student_groups.csv"
      case groupResult of
        Left err  -> putStrLn $ "Error: " ++ err
        Right groups -> do
          putStrLn "=== All Student Groups ==="
          mapM_ print groups
          putStrLn $ "Total student groups: " ++ show (length groups)
      
      putStrLn "\n=== Reading Timetable ==="
      timetableResult <- readTimetable "data/timetable.csv"
      case timetableResult of
        Left err  -> putStrLn $ "Error: " ++ err
        Right entries -> do
          putStrLn "=== All Timetable Entries ==="
          mapM_ print entries
          putStrLn $ "Total timetable entries: " ++ show (length entries)
      
          -- Perform validation
          putStrLn "\n=== System-Level Validation ==="
      
          -- Check lecturer overscheduling
          let lecturerIssues = checkLecturerOverscheduling validLecturers entries
          putStrLn "=== Lecturers Scheduled for More Hours Than Available ==="
          if null lecturerIssues
            then putStrLn "No overscheduled lecturers found."
            else mapM_ (\issue -> putStrLn $ "  - " ++ issueDescription issue) lecturerIssues
      
          -- Check room double bookings
          let roomIssues = checkRoomDoubleBookings entries
          putStrLn "\n=== Room Double Bookings ==="
          if null roomIssues
            then putStrLn "No room double bookings found."
            else mapM_ (\issue -> putStrLn $ "  - " ++ issueDescription issue) roomIssues
          
          -- Only proceed with full validation if we have all necessary data
          case (courseResult, roomResult, groupResult) of
            (Right courses, Right rooms, Right groups) -> do
              -- Full system validation
              let systemIssues = validateTimetableSystem validLecturers courses rooms groups entries
              putStrLn "\n=== Complete System Validation ==="
              if null systemIssues
                then putStrLn "No system-level issues found."
                else do
                  putStrLn $ "Found " ++ show (length systemIssues) ++ " system-level issues:"
                  mapM_ (\issue -> putStrLn $ "  - [" ++ issueType issue ++ "] " ++ 
                                             issueDescription issue) systemIssues
            
            _ -> putStrLn "Cannot perform complete system validation due to data loading errors."
  
readLecturers :: FilePath -> IO (Either String [Lecturer])
readLecturers filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, result) -> return $ Right (V.toList result)

separateValidation :: [Lecturer] -> ([Lecturer], [(Lecturer, [String])])
separateValidation = foldr categorize ([], [])
  where
    categorize lecturer (valid, invalid) = 
      case validateLecturer lecturer of
        Valid _ -> (lecturer : valid, invalid)
        Invalid errors -> (valid, (lecturer, errors) : invalid)

printInvalidLecturer :: (Lecturer, [String]) -> IO ()
printInvalidLecturer (lecturer, errors) = do
  putStrLn $ "Lecturer: " ++ show lecturer
  putStrLn "Validation errors:"
  mapM_ (\err -> putStrLn $ "  - " ++ err) errors
  putStrLn ""

writeValidLecturers :: [Lecturer] -> IO ()
writeValidLecturers lecturers = do
  let csv = Csv.encodeDefaultOrderedByName lecturers
  BL.writeFile "output/valid_lecturers.csv" csv
  putStrLn $ "Wrote " ++ show (length lecturers) ++ " valid lecturers to output/valid_lecturers.csv"

checkLecturerAvailability :: [Lecturer] -> String
checkLecturerAvailability lecturers = 
  "Checking lecturer availability for " ++ show (length lecturers) ++ " lecturers.\n" ++
  "In a full implementation, this would verify if lecturers are scheduled for\n" ++
  "more hours than their availability allows."

readCourses :: FilePath -> IO (Either String [Course])
readCourses filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, result) -> return $ Right (V.toList result)

readRooms :: FilePath -> IO (Either String [Room])
readRooms filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, result) -> return $ Right (V.toList result)

readStudents :: FilePath -> IO (Either String [Student])
readStudents filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, result) -> return $ Right (V.toList result)

readStudentGroups :: FilePath -> IO (Either String [StudentGroup])
readStudentGroups filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, result) -> return $ Right (V.toList result)

readTimetable :: FilePath -> IO (Either String [Timetable])
readTimetable filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, result) -> return $ Right (V.toList result)