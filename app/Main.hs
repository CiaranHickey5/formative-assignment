import Lecturer
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
  let csv = encodeDefaultOrderedByName lecturers
  BL.writeFile "output/valid_lecturers.csv" csv
  putStrLn $ "Wrote " ++ show (length lecturers) ++ " valid lecturers to output/valid_lecturers.csv"

checkLecturerAvailability :: [Lecturer] -> String
checkLecturerAvailability lecturers = 
  "Checking lecturer availability for " ++ show (length lecturers) ++ " lecturers.\n" ++
  "In a full implementation, this would verify if lecturers are scheduled for\n" ++
  "more hours than their availability allows."
