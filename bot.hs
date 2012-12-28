import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import TokyoForecast
import Data.Char

server = "servercentral.il.us.quakenet.org"
port   = 6667
chan   = "#J5OP"
nick   = "tokiobot"

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" 0 * :tutorial bot")
    write h "JOIN" chan
    listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

perspirationToHandle :: Handle -> IO ()
perspirationToHandle h = do
    weather <- acquireTodayWeather
    mapM_ (handlePersp h) (snd weather)
    print $ snd weather

handlePersp h (t, p)
    | filter isAlphaNum p == "" = return ()
    | otherwise = privmsg h ("Aikavälillä "++ t ++" todennäköisyys sateeseen on: "++ p )

weatherToHandle :: Handle -> IO ()
weatherToHandle h = do
    weather <- acquireTodayWeather
    print $  snd weather
    write h "PRIVMSG" (chan ++ " :" ++ fst weather)

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else eval h (clean s)
    putStrLn s
  where
    forever a = a >> forever a

    clean     = drop 1 . dropWhile (/= ':') . drop 1

    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)

eval :: Handle -> String -> IO ()
eval h    "!quit"                = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h    "!join"                = write h "JOIN" chan
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval h "!ilma"                   = weatherToHandle h
eval h "!sade"                   = perspirationToHandle h
eval _   _                       = return () -- ignore everything else

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)