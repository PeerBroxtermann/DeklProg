--Import auskommentiert, weil das so in der Aufgabe stand, aber ggf. brauchst du das ja noch

--import Data.Char (isAlpha, toLower)

{-
  Automatischer Test läuft nicht durch, beim eigenen Testen im ghci 
  funktioniert das Programm allerdings problemfrei.
-}


-- Datenstruktur für den Spielzustand
data HangmanState = HangmanState
  { secretWord :: String   -- Das geheime Wort
  , guessedWord :: String  -- Das bisher geratene Wort mit Platzhaltern für nicht geratene Buchstaben
  , attempts :: Int        -- Anzahl der Versuche
  , maxAttempts :: Int     -- Maximale Anzahl der Versuche
  , guessedLetters :: [Char] -- Liste der geratenen Buchstaben
  }

-- Funktion zur Initialisierung eines neuen Spiels
initializeHangman :: String -> Int -> HangmanState
initializeHangman word maxAttmpts = HangmanState
  { secretWord = map toLower word
  , guessedWord = replicate (length word) '*'
  , attempts = 0
  , maxAttempts = maxAttmpts
  , guessedLetters = []
  }

-- Funktion zur Überprüfung, ob das Spiel gewonnen wurde
isGameWon :: HangmanState -> Bool
isGameWon state = secretWord state == guessedWord state

-- Funktion zur Überprüfung, ob das Spiel verloren wurde
isGameOver :: HangmanState -> Bool
isGameOver state = attempts state >= maxAttempts state

{- 
  Hätte es lieber so geschrieben, dass attempts beim richtigen Versuch
  nicht erhöht wird, aber die Anforderung war halt anders :D 
  Ansonsten hätte ich "maxAttempts" beim Starten des Spiels
  auf 11 gesetzt, weil das die Standard-Anzahl
  an Fehlversuchen bei Hangman ist laut Google 
-}

-- Funktion zur Aktualisierung des Spielzustands nach einer Eingabe
updateHangmanState :: HangmanState -> Char -> HangmanState
updateHangmanState state guess
  | guess `elem` guessedLetters state = state  -- Buchstabe wurde bereits geraten
  | guess `elem` secretWord state =
    state
      { guessedWord = zipWith (\x y -> if x == guess then x else y) (secretWord state) (guessedWord state)
      , guessedLetters = guess : guessedLetters state
      , attempts = attempts state + 1
      } -- Buchstabe ist Teil der Lösung
  | otherwise =
    state
      { attempts = attempts state + 1
      , guessedLetters = guess : guessedLetters state
      } -- Buchstabe ist nicht Teil der Lösung

-- Funktion zur Darstellung des Spielzustands
displayHangmanState :: HangmanState -> IO ()
displayHangmanState state = do
  putStrLn $ "Secret: " ++ guessedWord state

-- Funktion zum Starten des Spiels
hangman :: String -> IO ()
hangman word = do
  let maxAttmpts = 15 -- Anzahl der maximalen Versuche
  let initState = initializeHangman word maxAttmpts
  playHangman initState

-- Hauptspielablauf
playHangman :: HangmanState -> IO ()
playHangman state = do
  displayHangmanState state

  if isGameWon state
    then putStrLn $ "Solved in " ++ show (attempts state) ++ " tries."
    else if isGameOver state
      then putStrLn $ "Game over. The secret word was: " ++ secretWord state
    else do
      putStr "Enter a character: "
      input <- getLine
      let guess = if null input || not (isAlpha (head input)) then '*' else toLower (head input)
      let newState = updateHangmanState state guess
      playHangman newState

