{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser
import Data.Bits
import Data.List

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret x y = do
  file1 <- BS.readFile x
  file2 <- BS.readFile y
  let result = BS.zipWith (xor) file1 file2
  return $ BS.pack $ filter (/= 0) result

-- Exercise 2 -----------------------------------------
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key outFilePath = do
  -- get the file contents
  encryptedFileContents <- BS.readFile (outFilePath ++ ".enc")
  -- unencrypt by xor'ing
  let result = BS.pack $ BS.zipWith (xor) encryptedFileContents (BS.cycle key)
  -- write it out to the current path
  BS.writeFile outFilePath result

-- Exercise 3 -----------------------------------------
-- parseFile "clues/victims.json" :: IO (Maybe [TId])
-- parseFile "clues/transactions.json" :: IO (Maybe [Transaction])
parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
  file <- BS.readFile file
  return $ decode file

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transactions = do
  vs <- parseFile victims :: IO (Maybe [TId])
  ts <- parseFile transactions :: IO (Maybe [Transaction])
  return $ filterTransactions (vs, ts)

filterTransactions :: (Maybe [TId], Maybe [Transaction]) -> Maybe [Transaction]
filterTransactions ((Just victims), (Just transactions)) =
   Just $ filter (\x -> elem (tid x) victims) transactions
filterTransactions _ = Nothing


-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts =
    foldr (\t m-> Map.insertWith (+) (to t) (amount t) $ Map.insertWith (+) (from t) (negate $ amount t) m) (Map.empty) ts

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal amounts =
    fst $ Map.foldrWithKey (\k e r-> if e > (snd r) then (k,e) else r) ("",0) amounts

-- Exercise 7 -----------------------------------------

-- seperate into payers and payees (people who lost and people who gained)
splitCustomers :: Map String Integer -> ((String, [(String, Integer)]),(String, [(String, Integer)]))
splitCustomers ts =
    helper (Map.toList ts) (("payees", []), ("payers", []))
        where
          helper :: [(String, Integer)] -> ((String, [(String, Integer)]),(String, [(String, Integer)])) -> ((String, [(String, Integer)]),(String, [(String, Integer)]))
          helper ((k,v):ts) (ps@(payees, payers))
            | v > 0 = helper ts (((fst payees), ((k,v) : (snd payees))), payers)
            | v < 0 = helper ts (payees, (fst payers, ((k,v) : (snd payers))))
            | otherwise = helper ts ps
          helper [] ps = ps
-- Map.insertWith (++)
-- payee if > 0
-- payer if < 0
-- [ "payee", [("name", 1)], "payer", [("name", -1)] ]

-- sort in descending order - payers who are owed the most, payees who gained the most
--  hint: sortBy in Data.List
-- for each pair, have a payee pay the payer until payer loss is at 0 and payee debt is at 0

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
