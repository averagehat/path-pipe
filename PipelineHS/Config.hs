{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import GHC.Generics 
import Options.Generic ((<?>), ParseRecord)
import Data.Yaml
--import Data.Aeson
-- TODO: give everything lenses via (Aeson's?) makeLens?
data CommandArgs = CommandArgs {   
    r1 :: FilePath <?> "Forward"
  , r2 :: FilePath <?> "Reverse"
  , i1 :: FilePath <?> "Forward Index"
  , i2 :: FilePath <?> "Reverse Index"
  , outdir :: FilePath <?> "Output Directory"
  , config :: FilePath <?> "Path to config.yaml" }

deriving (Generic, Show)
instance ParseRecord CommandArgs

data Config = Config { 
                     , threads :: Int
                     , star :: STAROpts
                     , pricefilter :: PriceSeqFilterOpts
                     , cdhitdup :: CDHitOpts
                     , lzwfilter :: LZWOpts
                     , bowtie2    :: BowtieOpts
                     , gsnap     :: GSNAPOpts
                     , rapsearch :: RapSearchOpts
                     , ncbi :: Acc2TaxOpts
                     , assembler :: AssemblerOpts }

instance fromJSON Config
instance toJSON Config -- for generateing config.yaml.example. but requires all others to have toJSON as well.

data STAROpts = STAROpts { starDB :: FilePath } 
instance fromJSON STAROpts

data PriceSeqFilterOpts = PSFopts { calledPercent :: Int -- <= 100
                                  , hiqhQualPercent :: Int -- <=100
                                  , highQualMin :: Float } -- <= 1 
instance fromJSON PriceSeqFilterOpts

data CDHitOpts = CDHitOpts { maxSimilarity :: Percent } -- <= 1
instance fromJSON CDHitOpts

data LZWOpts = LZWOpts     { maxCompressionScore :: Percent } 
instance fromJSON LZWOpts

-- note multiple db accessors not allowed
data BowtieOpts = BowtieOpts { bowtieDB :: FilePath }
instance fromJSON BowtieOpts

data GSNAPOpts = GSNAPOpts   { gsnapDB :: FilePath }
instance fromJSON GSNAPOpts

data RapSearchOpts = RapSearchOpts { rapsearchDB :: FilePath } 
instance fromJSON RapSearchOpts

data Acc2TaxOpts = Acc2TaxOpts { nrDB :: FilePath, ntDB :: FilePath }
instance fromJSON Acc2TaxOpts

data AssemblerOpts = AssemblerOpts { foo :: Int } 
instance fromJSON AssemblerOpts
-- could have the index-building script output a yaml file??

 
main = do
  cfg <- (Y.decodeEither <$> readFile "config.yaml" :: Maybe Config)
  bimap  putStrLn putStrLn $ (bowtieDB . bowtie2) <$> cfg
  either putStrLn putStrLn $ (bowtieDB . bowtie2) <$> cfg
  -- is equivalent to the below
  case cfg of
    Left cfg' -> putStrLn $ show $ bowtieDB $ bowtie2 $ cfg
    Right err -> putStrLn err
  
-- have to get input1 and input2 from command args
-- have to get threads and gd from config file



