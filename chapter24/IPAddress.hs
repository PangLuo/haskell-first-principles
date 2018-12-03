module IPAddress where

import Control.Applicative
import Data.Char
import Data.List
import Data.Word
import Numeric
import Test.Hspec
import Text.Trifecta

convert' :: Integer -> Integer -> [Integer]
convert' i denom = go i [] where
  go x acc
    | x >= denom =
        let (x', r) = x `divMod` denom
        in go x' (r : acc)
    | otherwise = x : acc

convert :: Integer -> Integer -> Integer -> String -> String
convert i denom base sep = intercalate sep $ replicate (4 - length rs) "0" ++
  fmap (\x -> showIntAtBase base intToDigit x "") rs
  where rs = convert' i denom

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress i) = convert (fromIntegral i) 256 10 "."

parseField :: Parser Integer
parseField = do
  i <- decimal
  case i >= 0 && i < 256 of
    True -> return i
    _ -> fail "each field must be within [0, 255]"

parseIPv4 :: Parser IPAddress
parseIPv4 = do
  xs <- count 3 (parseField <* char '.')
  x <- parseField
  return $ IPAddress $ fromIntegral $ foldl f 0 (xs ++ [x])
    where f acc x = acc * 256 + x

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord)

instance Show IPAddress6 where
  show (IPAddress6 i j) = intercalate ":" $
    fmap (\x -> convert (fromIntegral x) 65536 16 ":") [i, j]

convert4To6 :: IPAddress -> IPAddress6
convert4To6 (IPAddress i) = IPAddress6 0 (fromIntegral i)

parseField6 :: Parser Integer
parseField6 = (fst . head . readHex) <$>
  (try (count 4 hexDigit) <|> try (count 3 hexDigit) <|>
   try (count 2 hexDigit) <|> count 1 hexDigit)

maxFields :: Int
maxFields = 8

parseIPv6' :: Parser ([Integer], [Integer])
parseIPv6' = go [] [] <|> (string "::" >> go' [] []) where
  go xs ys = do
    x <- parseField6
    let xs' = x:xs
    if length xs' == maxFields
      then return (xs', ys)
      else
        (try (string "::" >>
              if length xs' == maxFields - 1
                then return (xs', ys)
                else try (go' xs' ys) <|> return (xs', ys)
             ) <?> "Tried ::") <|>
        (char ':' >> go xs' ys)

  go' xs' ys = do
    y <- parseField6
    let ys' = y:ys
    if length xs' + length ys' == maxFields - 1
      then return (xs', ys')
      else try (char ':' >> go' xs' ys') <|> return (xs', ys')

parseIPv6 :: Parser IPAddress6
parseIPv6 = do
  (xs, ys) <- parseIPv6'
  let nGaps = maxFields - length xs - length ys
      (as, bs) = splitAt 4 $ reverse xs ++ replicate nGaps 0 ++ reverse ys
      go zs = fromIntegral (foldl f 0 zs)
      f acc z = acc * 65536 + z
  return $ IPAddress6 (go as) (go bs) where

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "without ::" $ do
    it "success 1" $ do
      let m = parseString parseIPv6 mempty "0:0:0:0:0:ffff:ac10:fe01"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 281473568538113)

    it "success 2" $ do
      let m = parseString parseIPv6 mempty "0:0:0:0:0:ffff:cc78:f"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 281474112159759)

    it "success 3" $ do
      let m = parseString parseIPv6 mempty
              "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 18338657682652659712 144876050090722089)

    it "success 4" $ do
      let m = parseString parseIPv6 mempty "1:2:3:4:5:6:7:8AbCd"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281483566841860 1407400653851324)

    it "failure 1" $ do
      let m = parseString parseIPv6 mempty "0"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "failure 2" $ do
      let m = parseString parseIPv6 mempty "0:"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "failure 3" $ do
      let m = parseString parseIPv6 mempty "0:a"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "failure 4" $ do
      let m = parseString parseIPv6 mempty "1:2:3:4:5:6:7:"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "failure 5" $ do
      let m = parseString parseIPv6 mempty "1:2:3:4:5:6:7AbCd:"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "failure 6" $ do
      let m = parseString parseIPv6 mempty "1:2:3:4:5:6:7AbC:g"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

  describe "with ::" $ do
    it "success 1" $ do
      let m = parseString parseIPv6 mempty "2001:DB8::8:800:200C:417A"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 2306139568115548160 2260596444381562)

    it "success 2" $ do
      let m = parseString parseIPv6 mempty "2001:DB8::8:800:200C:417AB"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 2306139568115548160 2260596444381562)

    it "success 3" $ do
      let m = parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 18338657682652659712 144876050090722089)

    it "success 4" $ do
      let m = parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329F"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 18338657682652659712 144876050090722089)

    it "success 5" $ do
      let m = parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329::"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 18338657682652659712 144876050090722089)

    it "success 6" $ do
      let m = parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329::FFFF"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 18338657682652659712 144876050090722089)

    it "success 7" $ do
      let m = parseString parseIPv6 mempty "::1AbF"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 6847)

    it "success 8" $ do
      let m = parseString parseIPv6 mempty "::1AbFa"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 6847)

    it "success 9" $ do
      let m = parseString parseIPv6 mempty "::1AbF:"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 6847)

    it "success 10" $ do
      let m = parseString parseIPv6 mempty "::1AbFaB"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 6847)

    it "success 11" $ do
      let m = parseString parseIPv6 mempty "::1AbFa:"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 6847)

    it "success 12" $ do
      let m = parseString parseIPv6 mempty "::1AbF:F"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 448725007)

    it "success 13" $ do
      let m = parseString parseIPv6 mempty "::1AbF::"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 0 6847)

    it "success 14" $ do
      let m = parseString parseIPv6 mempty "::1:2:3:4:5:6"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 65538 844442110328838)

    it "success 15" $ do
      let m = parseString parseIPv6 mempty "::1:2:3:4:5:6:"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 65538 844442110328838)

    it "success 16" $ do
      let m = parseString parseIPv6 mempty "::1:2:3:4:5:6:7"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 4295098371 1125921382072327)

    it "success 17" $ do
      let m = parseString parseIPv6 mempty "::1:2:3:4:5:6:7:"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 4295098371 1125921382072327)

    it "success 18" $ do
      let m = parseString parseIPv6 mempty "::1:2:3:4:5:6:7:8"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 4295098371 1125921382072327)

    it "success 19" $ do
      let m = parseString parseIPv6 mempty "::1:2:3:4:5:6:7::"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 4295098371 1125921382072327)

    it "success 20" $ do
      let m = parseString parseIPv6 mempty "1::"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281474976710656 0)

    it "success 21" $ do
      let m = parseString parseIPv6 mempty "1::2"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281474976710656 2)

    it "success 22" $ do
      let m = parseString parseIPv6 mempty "1::2345f"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281474976710656 9029)

    it "success 23" $ do
      let m = parseString parseIPv6 mempty "1::2345f"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281474976710656 9029)

    it "success 24" $ do
      let m = parseString parseIPv6 mempty "1:2:3:4:5:6:7:8::"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281483566841860 1407400653815816)

    it "success 25" $ do
      let m = parseString parseIPv6 mempty "1:2:3:4:5:6:7::"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281483566841860 1407400653815808)

    it "success 26" $ do
      let m = parseString parseIPv6 mempty "1:2:3:4:5:6:7:8::9"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281483566841860 1407400653815816)

    it "success 27" $ do
      let m = parseString parseIPv6 mempty "1::G"
          r = maybeSuccess m
      print m
      r `shouldBe` (Just $ IPAddress6 281474976710656 0)

    it "failure 1" $ do
      let m = parseString parseIPv6 mempty "::G"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing
