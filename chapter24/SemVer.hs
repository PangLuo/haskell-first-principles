{-# LANGUAGE FlexibleInstances #-}

module SemVer where

import Control.Applicative
import Data.Char
import Data.Maybe
import Test.Hspec
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

instance Ord NumberOrString where
  compare (NOSS s) (NOSS s') = compare s s'
  compare (NOSI i) (NOSI i') = compare i i'
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI _) (NOSS _) = LT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer a b c d _) (SemVer a' b' c' d' _) =
    let r = compare (a, b, c) (a', b', c')
    in case r of
         EQ ->
           case (d, d') of
             ([], []) -> EQ
             ([], _) -> GT
             (_, []) -> LT
             _ -> compare d d'
         _ -> r

parseNat' :: Bool -> String -> Parser Integer
parseNat' leadingZeroLegal xs =
  case (leadingZeroLegal, xs) of
    (False, '0':_:_) ->
      fail "major, minor, patch and numeric identifiers in release must not \
            \include leading zeroes"
    _ -> return $ read xs

parseNat :: Parser Integer
parseNat = do
  xs <- some digit
  parseNat' False xs

parseID :: Bool -> Parser NumberOrString
parseID leadingZeroLegal = do
  xs <- some (alphaNum <|> char '-')
  case all isDigit xs of
    True -> NOSI <$> parseNat' leadingZeroLegal xs
    _ -> return $ NOSS xs

parseExtension :: Char -> Parser [NumberOrString]
parseExtension c = fromMaybe [] <$> (optional $ do
  char c
  let leadingZeroLegal = if c == '-' then False else True
  sepBy1 (parseID leadingZeroLegal) (char '.'))

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- parseNat
  char '.'
  minor <- parseNat
  char '.'
  patch <- parseNat
  release <- parseExtension '-'
  meta <- parseExtension '+'
  eof
  return $ SemVer major minor patch release meta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Major" $ do
    it "negative integer" $ do
      let m = parseString parseSemVer mempty "-1.0.0-x.7.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "leading zero" $ do
      let m = parseString parseSemVer mempty "01.0.0-x.7.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

  describe "Minor" $ do
    it "negative integer" $ do
      let m = parseString parseSemVer mempty "1.-1.0-x.7.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "leading zero" $ do
      let m = parseString parseSemVer mempty "1.00.0-x.7.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

  describe "Patch" $ do
    it "negative integer" $ do
      let m = parseString parseSemVer mempty "1.0.-0-x.7.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "leading zero" $ do
      let m = parseString parseSemVer mempty "1.0.00-x.7.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

  describe "Release" $ do
    it "illegal character" $ do
      let m = parseString parseSemVer mempty "1.0.0-x=.7.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "empty identifier" $ do
      let m = parseString parseSemVer mempty "1.0.0-.7.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "leading zero" $ do
      let m = parseString parseSemVer mempty "1.0.0-x.07.z.92"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

  describe "Metadata" $ do
    it "illegal character" $ do
      let m = parseString parseSemVer mempty "1.0.0-x.7.z.92+exp=.sha.5114f85"
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

    it "empty identifier" $ do
      let m = parseString parseSemVer mempty "1.0.0-x.7.z.92+exp."
          r = maybeSuccess m
      print m
      r `shouldBe` Nothing

  describe "Success Examples" $ do
    it "success 1" $ do
      let m = parseString parseSemVer mempty "1.0.0-x.7.89.x-92+exp.-sha.05114f85"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0
        [NOSS "x", NOSI 7, NOSI 89, NOSS "x-92"]
        [NOSS "exp", NOSS "-sha", NOSS "05114f85"])

    it "success 2" $ do
      let m = parseString parseSemVer mempty "1.0.0-x.7.89.x-92"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0
        [NOSS "x", NOSI 7, NOSI 89, NOSS "x-92"]
        [])

    it "success 3" $ do
      let m = parseString parseSemVer mempty "1.0.0+exp.-sha.05114f85"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0
        []
        [NOSS "exp", NOSS "-sha", NOSS "05114f85"])

    it "success 4" $ do
      let m = parseString parseSemVer mempty "2.1.1"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 2 1 1 [] [])

    it "success 5" $ do
      let m = parseString parseSemVer mempty "1.0.0-alpha"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha"] [])

    it "success 6" $ do
      let m = parseString parseSemVer mempty "1.0.0-alpha.1"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha", NOSI 1] [])

    it "success 7" $ do
      let m = parseString parseSemVer mempty "1.0.0-0.3.7"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0 [NOSI 0, NOSI 3, NOSI 7] [])

    it "success 8" $ do
      let m = parseString parseSemVer mempty "1.0.0-alpha+001"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0 [NOSS "alpha"] [NOSI 1])

    it "success 9" $ do
      let m = parseString parseSemVer mempty "1.0.0+20130313144700"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0 [] [NOSI 20130313144700])

    it "success 10" $ do
      let m = parseString parseSemVer mempty "1.0.0-beta+exp.sha.5114f85"
          r = maybeSuccess m
      print m
      r `shouldBe` Just (SemVer 1 0 0
        [NOSS "beta"]
        [NOSS "exp", NOSS "sha", NOSS "5114f85"])

  describe "Precedence Tests" $ do
    it "precedence 1" $ do
      let a = parseString parseSemVer mempty "1.0.0-alpha"
          b = parseString parseSemVer mempty "1.0.0-alpha.1"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 2" $ do
      let a = parseString parseSemVer mempty "1.0.0-alpha.1"
          b = parseString parseSemVer mempty "1.0.0-alpha.beta"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 3" $ do
      let a = parseString parseSemVer mempty "1.0.0-alpha.beta"
          b = parseString parseSemVer mempty "1.0.0-beta"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 4" $ do
      let a = parseString parseSemVer mempty "1.0.0-beta"
          b = parseString parseSemVer mempty "1.0.0-beta.2"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 5" $ do
      let a = parseString parseSemVer mempty "1.0.0-beta.2"
          b = parseString parseSemVer mempty "1.0.0-beta.11"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 6" $ do
      let a = parseString parseSemVer mempty "1.0.0-beta.11"
          b = parseString parseSemVer mempty "1.0.0-rc.1"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 7" $ do
      let a = parseString parseSemVer mempty "1.0.0-rc.1"
          b = parseString parseSemVer mempty "1.0.0"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 8" $ do
      let a = parseString parseSemVer mempty "1.0.0-alpha+001"
          b = parseString parseSemVer mempty "1.0.1-1.rc.1+002"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 9" $ do
      let a = parseString parseSemVer mempty "1.0.0-abc+001"
          b = parseString parseSemVer mempty "2.0.0-1.rc.1+002"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 10" $ do
      let a = parseString parseSemVer mempty "2.0.0-abc+001"
          b = parseString parseSemVer mempty "2.1.0-1.rc.1+002"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 11" $ do
      let a = parseString parseSemVer mempty "2.1.0-abc+001"
          b = parseString parseSemVer mempty "2.1.1-1.rc.1+002"
          r = maybeSuccess a < maybeSuccess b
      r `shouldBe` True

    it "precedence 12" $ do
      let a = parseString parseSemVer mempty "1.0.0-abc+001"
          b = parseString parseSemVer mempty "1.0.0-abc+002"
          r = compare (maybeSuccess a) (maybeSuccess b)
      r `shouldBe` EQ
