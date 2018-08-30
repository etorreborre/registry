module Test.Data.Box.ReflectionSpec where

{-

data Mod a = Mod a Text deriving (Eq, Show)

mod1 :: Mod Int
mod1 = Mod 1 "hey"

iomod :: IO (Mod Int)
iomod = pure (Mod 1 "hey")

fun0 :: IO Int
fun0 = undefined

fun1 :: IO Int -> IO Int
fun1 = undefined

fun2 :: IO Int -> IO Int -> IO Int
fun2 = undefined

fun3 :: IO Int -> IO Int -> IO Int -> IO Int
fun3 = undefined

fun4 :: IO (Mod Int) -> IO Int -> IO Int -> IO Int
fun4 = undefined

add1 :: Int -> Int
add1 i = i + 1

add2 :: Int -> Int -> Text
add2 i = undefined

funsx = showFunction (1:: Int)
funs0 = showFunction fun0
funs1 = showFunction fun1
funs2 = showFunction fun2
funs3 = showFunction fun3
funs4 = showFunction fun4
funs5 = showFunction add1
funs6 = showFunction add2
alls = (funsx, showValue mod1, funs0, funs1, funs2, "fun3: " <> funs3, funs4, funs5, funs6)

modio :: IO (Mod Int)
modio = pure mod1

-}