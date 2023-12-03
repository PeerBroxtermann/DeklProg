getNonEmptyLine :: IO String
getNonEmptyLine =
  getLine >>= \input ->
    if null input
      then putStrLn "Please enter a non-empty string." >> getNonEmptyLine
      else return input


quitOrInput :: IO (Maybe String)
quitOrInput = do
  putStrLn "Your input"
  inp <- getLine
  case inp of
    ":q" -> return Nothing
    _    -> return (Just inp)