import Data.List (nub, (\\), intercalate)

module ERDiagram
  ( Entity(..)
  , Attribute(..)
  , Relationship(..)
  , validateERDiagram
  , checkDiagramCompleteness
  ) where

data Entity = Entity 
  { entityName :: String
  , attributes :: [Attribute]
  } deriving (Show)

data Attribute = Attribute
  { attributeName :: String
  , isPrimaryKey :: Bool
  , isForeignKey :: Bool
  , referencedEntity :: Maybe String
  } deriving (Show)

data Relationship = Relationship
  { relationshipName :: String
  , entityFrom :: String
  , entityTo :: String
  , cardinality :: String  -- "1:1", "1:N", "M:N"
  } deriving (Show)

validateERDiagram :: [Entity] -> [Relationship] -> [String]
validateERDiagram entities relationships = 
  let
    entityErrors = concatMap validateEntity entities
    relationshipErrors = validateRelationships entities relationships
  in
    entityErrors ++ relationshipErrors

validateEntity :: Entity -> [String]
validateEntity entity =
  let
    name = entityName entity
    attrs = attributes entity
    
    -- Check for primary key
    hasPrimaryKey = any isPrimaryKey attrs
    primaryKeyError = if hasPrimaryKey 
                      then [] 
                      else ["Entity " ++ name ++ " has no primary key"]
    
    -- Check for duplicate attribute names
    attrNames = map attributeName attrs
    duplicates = attrNames \\ nub attrNames
    duplicateErrors = if null duplicates 
                      then [] 
                      else ["Entity " ++ name ++ " has duplicate attributes: " ++ 
                            intercalate ", " duplicates]
  in
    primaryKeyError ++ duplicateErrors

validateRelationships :: [Entity] -> [Relationship] -> [String]
validateRelationships entities relationships =
  let
    entityNames = map entityName entities
    
    validateRelationship r = 
      let
        fromExists = entityFrom r `elem` entityNames
        toExists = entityTo r `elem` entityNames
        fromError = if fromExists 
                    then [] 
                    else ["Relationship " ++ relationshipName r ++ 
                          " references non-existent entity " ++ entityFrom r]
        toError = if toExists 
                  then [] 
                  else ["Relationship " ++ relationshipName r ++ 
                        " references non-existent entity " ++ entityTo r]
      in
        fromError ++ toError
  in
    concatMap validateRelationship relationships

checkDiagramCompleteness :: [Entity] -> [Relationship] -> [String]
checkDiagramCompleteness entities relationships =
  let
    -- Check if all essential entities are present
    requiredEntities = ["Course", "Room", "Lecturer", "Student", "StudentGroup", "Timetable"]
    missingEntities = requiredEntities \\ map entityName entities
    entityErrors = if null missingEntities 
                   then [] 
                   else ["Missing essential entities: " ++ intercalate ", " missingEntities]
    
    -- Check if all essential relationships are represented
    hasRelationship from to = any (\r -> (entityFrom r == from && entityTo r == to) || 
                                         (entityFrom r == to && entityTo r == from)) relationships
    essentialRelationships = [("Lecturer", "Timetable"), ("Course", "Timetable"), 
                             ("Room", "Timetable"), ("StudentGroup", "Timetable")]
    missingRelationships = filter (uncurry hasRelationship) essentialRelationships
    relationshipErrors = if null missingRelationships 
                         then [] 
                         else ["Missing essential relationships: " ++ 
                               intercalate ", " (map (\(a, b) -> a ++ "-" ++ b) missingRelationships)]
  in
    entityErrors ++ relationshipErrors