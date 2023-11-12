data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show

--1)
exampleJSON :: JSON
exampleJSON =
  JArray
    [ JObject
        [ ("name", JString "meier")
        , ("besuchte_kurse", JArray [JString "Logik", JString "Programmierung", JString "Compilerbau"])
        , ("bachelor_note", JNull)
        , ("zugelassen", JBool True)
        ]
    , JObject
        [ ("name", JString "schmidt")
        , ("besuchte_kurse", JArray [JString "Programmierung", JString "Informationssysteme"])
        , ("bachelor_note", JFloat 2.7)
        , ("zugelassen", JBool False)
        ]
    ]

foldJSON ::
  b ->                         -- JNull
  (Bool -> b) ->               -- JBool
  (Int -> b) ->                -- JInt
  (Float -> b) ->              -- JFloat
  (String -> b) ->             -- JString
  ([b] -> b) ->                -- JArray
  ([(String, b)] -> b) ->      -- JObject
  JSON -> b                    -- input JSON value
foldJSON jnull jbool jint jfloat jstring jarray jobject jvalue = case jvalue of
  JNull -> jnull
  JBool val1 -> jbool val1
  JInt val2 -> jint val2
  JFloat val3 -> jfloat val3
  JString val4 -> jstring val4
  JArray val5 -> jarray (map (foldJSON jnull jbool jint jfloat jstring jarray jobject) val5)
  JObject val6 -> jobject (map (\(key, value) -> (key, foldJSON jnull jbool jint jfloat jstring jarray jobject value)) val6)

-- Beispiel-Nutzung der foldJSON-Funktion
countTrue :: JSON -> Int
countTrue = foldJSON 0 (\b -> if b then 1 else 0) (const 0) (const 0) (const 0) sum (\xs -> sum (map snd xs))

--4)

prettyJSON :: JSON -> String
prettyJSON = foldJSON
  "null"                            -- JNull
  (\b -> if b then "true" else "false")  -- JBool
  show                              -- JInt
  show                              -- JFloat
  (\s -> '"' : s ++ "\"")           -- JString
  (\arr -> '[' : commaSep arr ++ "]")     -- JArray
  (\obj -> '{' : commaSep (map prettyKV obj) ++ "}")   -- JObject
  where
    prettyKV (k, v) = '\"' : k ++ "\": " ++ v

-- Transform a list of strings into a comma-separated string
commaSep :: [String] -> String
commaSep [] = ""
commaSep s = foldr1 (\s1 s2 -> s1 ++ ", " ++ s2) s
