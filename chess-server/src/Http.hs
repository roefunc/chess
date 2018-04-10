module Http (
    parseHttpRequest,
    logHeaders,
    parseCommand,
    commandContents
) where

import HttpTypes

fork::(a->b)->(a,a)->(b,b)
fork f (a1,a2) = (f a1, f a2)

stripLeadingChars::String->String
stripLeadingChars s = dropWhile (\x -> x == ' ' || x == ':') s

commandContents::HttpCommand->String
commandContents Post{url=_,content=c} = c
commandContents _ = ""

parseCommand::String->HttpCommand
parseCommand command = case (words command) of
                                ("GET":u:_) -> Get { url=u }
                                ("POST":u:_) -> Post {url=u, content="" }
                                ("HEAD":u:_) -> Head {url=u }
                                _ -> error ("Unknown command " ++ command)

parseHttpRequest::String->(HttpCommand,[(String,String)])
parseHttpRequest headers = let
                            (comm:hs) = lines headers
                            parseLine h = fork stripLeadingChars $ break (==':') h
                            parse [] acc = acc
                            parse (h:hrest) acc = parse hrest ((parseLine h):acc)
                           in
                            (parseCommand comm, parse hs [])

logHeaders::[(String,String)]->String
logHeaders [] =[]
logHeaders xs = let
                    doLog::[(String,String)]->[String]->[String]
                    doLog [] acc = acc
                    doLog ((k,v):vs) acc = doLog vs ((k ++ "," ++ v):acc)
                in
                    unlines $ doLog xs []
