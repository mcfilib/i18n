-----------------------------------------------------------------------------
-- |
-- Module      :  Text.I18n.Printf
-- Copyright   :  (c) Eugene Grigoriev, 2008
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  eugene.grigoriev@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- A slightly modified version of Text.Printf module. (with permission)
--
-- This module is internal to Text.I18n.
--
-----------------------------------------------------------------------------
module Text.I18n.Printf(
   printf, hPrintf,
   PrintfType, HPrintfType, PrintfArg, IsChar,
   spr, uprintf
) where

import Prelude
import Data.Char
import Data.Int
import Data.Word
import Numeric(showEFloat, showFFloat, showGFloat)
import System.IO

printf :: (PrintfType r) => String -> r
printf fmts = spr fmts []

hPrintf :: (HPrintfType r) => Handle -> String -> r
hPrintf hdl fmts = hspr hdl fmts []

class PrintfType t where
    spr :: String -> [UPrintf] -> t

class HPrintfType t where
    hspr :: Handle -> String -> [UPrintf] -> t

instance (IsChar c) => PrintfType [c] where
    spr fmts args = map fromChar (uprintf fmts (reverse args))

instance PrintfType (IO a) where
    spr fmts args = do
        putStr (uprintf fmts (reverse args))
        return undefined

instance HPrintfType (IO a) where
    hspr hdl fmts args = do
        hPutStr hdl (uprintf fmts (reverse args))
        return undefined

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \ a -> spr fmts (toUPrintf a : args)

instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
    hspr hdl fmts args = \ a -> hspr hdl fmts (toUPrintf a : args)

class PrintfArg a where
    toUPrintf :: a -> UPrintf

instance PrintfArg Char where
    toUPrintf c = UChar c

instance (IsChar c) => PrintfArg [c] where
    toUPrintf = UString . map toChar

instance PrintfArg Int where
    toUPrintf = uInteger

instance PrintfArg Int8 where
    toUPrintf = uInteger

instance PrintfArg Int16 where
    toUPrintf = uInteger

instance PrintfArg Int32 where
    toUPrintf = uInteger

instance PrintfArg Int64 where
    toUPrintf = uInteger

-- if on NHC, exclure this declaration using CPP
instance PrintfArg Word where
    toUPrintf = uInteger

instance PrintfArg Word8 where
    toUPrintf = uInteger

instance PrintfArg Word16 where
    toUPrintf = uInteger

instance PrintfArg Word32 where
    toUPrintf = uInteger

instance PrintfArg Word64 where
    toUPrintf = uInteger

instance PrintfArg Integer where
    toUPrintf = UInteger 0

instance PrintfArg Float where
    toUPrintf = UFloat

instance PrintfArg Double where
    toUPrintf = UDouble

uInteger :: (Integral a, Bounded a) => a -> UPrintf
uInteger x = UInteger (toInteger $ minBound `asTypeOf` x) (toInteger x)

class IsChar c where
    toChar :: c -> Char
    fromChar :: Char -> c

instance IsChar Char where
    toChar c = c
    fromChar c = c

-------------------

data UPrintf = UChar Char | UString String | UInteger Integer Integer | UFloat Float | UDouble Double

uprintf :: String -> [UPrintf] -> String
uprintf ""       []       = ""
uprintf ""       (_:_)    = fmterr
uprintf ('%':'%':cs) us   = '%':uprintf cs us
uprintf ('%':_)  []       = argerr
uprintf ('%':cs) us@(_:_) = fmt cs us
uprintf (c:cs)   us       = c:uprintf cs us

fmt :: String -> [UPrintf] -> String
fmt cs us =
    let (width, prec, ladj, zero, plus, cs', us') = getSpecs False False False cs us
        adjust (pre, str) = 
            let lstr = length str
                lpre = length pre
                fill = if lstr+lpre < width then take (width-(lstr+lpre)) (repeat (if zero then '0' else ' ')) else ""
            in  if ladj then pre ++ str ++ fill else if zero then pre ++ fill ++ str else fill ++ pre ++ str
        adjust' ("", str) | plus = adjust ("+", str)
        adjust' ps = adjust ps
    in case cs' of
        []     -> fmterr
        c:cs'' -> case us' of
                    []     -> argerr
                    u:us'' -> (case c of
                                'c' -> adjust  ("", [toEnum (toint u)])
                                'd' -> adjust' (fmti u)
                                'i' -> adjust' (fmti u)
                                'x' -> adjust  ("", fmtu 16 u)
                                'X' -> adjust  ("", map toUpper $ fmtu 16 u)
                                'o' -> adjust  ("", fmtu 8  u)
                                'u' -> adjust  ("", fmtu 10 u)
                                'e' -> adjust' (dfmt' c prec u)
                                'E' -> adjust' (dfmt' c prec u)
                                'f' -> adjust' (dfmt' c prec u)
                                'g' -> adjust' (dfmt' c prec u)
                                'G' -> adjust' (dfmt' c prec u)
                                's' -> adjust  ("", tostr u)
                                _   -> perror ("bad formatting char " ++ [c])
                                ) ++ uprintf cs'' us''

fmti :: UPrintf -> (String, String)
fmti (UInteger _ i) = if i < 0 then ("-", show (-i)) else ("", show i)
fmti (UChar c)      = fmti (uInteger (fromEnum c))
fmti _          = baderr

fmtu :: Integer -> UPrintf -> String
fmtu b (UInteger l i) = itosb b (if i < 0 then -2*l + i else i)
fmtu b (UChar c)      = itosb b (toInteger (fromEnum c))
fmtu _ _              = baderr

toint :: UPrintf -> Int
toint (UInteger _ i) = fromInteger i
toint (UChar c)      = fromEnum c
toint _          = baderr

tostr :: UPrintf -> String
tostr (UString s) = s
tostr _       = baderr

itosb :: Integer -> Integer -> String
itosb b n = 
    if n < b then 
        [intToDigit $ fromInteger n]
    else
        let (q, r) = quotRem n b in
        itosb b q ++ [intToDigit $ fromInteger r]

stoi :: Int -> String -> (Int, String)
stoi a (c:cs) | isDigit c = stoi (a*10 + digitToInt c) cs
stoi a cs                 = (a, cs)

getSpecs :: Bool -> Bool -> Bool -> String -> [UPrintf] -> (Int, Int, Bool, Bool, Bool, String, [UPrintf])
getSpecs _ z s ('-':cs) us = getSpecs True z s cs us
getSpecs l z _ ('+':cs) us = getSpecs l z True cs us
getSpecs l _ s ('0':cs) us = getSpecs l True s cs us
getSpecs l z s ('*':cs) us = 
        case us of
            [] -> argerr
            nu : us' ->
                let n = toint nu
                    (p, cs'', us'') =
                        case cs of
                            '.':'*':r -> case us' of { [] -> argerr; pu:us''' -> (toint pu, r, us''') }
                            '.':r     -> let (n', cs') = stoi 0 r in (n', cs', us')
                            _         -> (-1, cs, us')
                        in  (n, p, l, z, s, cs'', us'')
getSpecs l z s ('.':cs) us =
    let (p, cs') = stoi 0 cs
    in  (0, p, l, z, s, cs', us)
getSpecs l z s cs@(c:_) us | isDigit c =
    let (n, cs') = stoi 0 cs
        (p, cs'') = case cs' of
            '.':r -> stoi 0 r
            _     -> (-1, cs')
    in  (n, p, l, z, s, cs'', us)
getSpecs l z s cs       us = (0, -1, l, z, s, cs, us)

dfmt' :: Char -> Int -> UPrintf -> (String, String)
dfmt' c p (UDouble d) = dfmt c p d
dfmt' c p (UFloat f)  = dfmt c p f
dfmt' _ _ _           = baderr

dfmt :: (RealFloat a) => Char -> Int -> a -> (String, String)
dfmt c p d =
    case (if isUpper c then map toUpper else id) $
             (case toLower c of
                  'e' -> showEFloat
                  'f' -> showFFloat
                  'g' -> showGFloat
                  _   -> error "Printf.dfmt: impossible"
             )
               (if p < 0 then Nothing else Just p) d "" of
    '-':cs -> ("-", cs)
    cs     -> ("" , cs)

perror :: String -> a
perror s = error ("Printf.printf: "++s)
fmterr, argerr, baderr :: a
fmterr = perror "formatting string ended prematurely"
argerr = perror "argument list ended prematurely"
baderr = perror "bad argument"
