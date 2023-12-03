import Data.Char (isAlpha, toLower)

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

-- Funktion zur Aktualisierung des Spielzustands nach einer Eingabe
updateHangmanState :: HangmanState -> Char -> HangmanState
updateHangmanState state guess
  | guess `elem` guessedLetters state = state  -- Buchstabe wurde bereits geraten
  | guess `elem` secretWord state =
    state
      { guessedWord = zipWith (\x y -> if x == guess then x else y) (secretWord state) (guessedWord state)
      , guessedLetters = guess : guessedLetters state
      , attempts = attempts state + 1
      }
  | otherwise =
    state
      { attempts = attempts state + 1
      , guessedLetters = guess : guessedLetters state
      }

-- Funktion zur Darstellung des Spielzustands
displayHangmanState :: HangmanState -> IO ()
displayHangmanState state = do
  putStrLn $ "Secret: " ++ guessedWord state

-- Funktion zur Durchführung des Hangman-Spiels
hangman :: String -> IO ()
hangman word = do
  let maxAttmpts = 11 -- Anzahl der maximalen Versuche
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

