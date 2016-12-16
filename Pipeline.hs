
import Data.Char(ord,chr,toUpper)
import Data.String.Here
import Data.Word (Word8)
import System.Directory(doesFileExist)
import Text.Regex(subRegex,mkRegex)
import Control.Applicative
import Bio.Sequence.FastQ (readSangerQ, writeSangerQ)
import Bio.Core.Sequence (BioSeqQual, unSD, seqheader, seqdata, seqqual, unQD, unQual)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as BB 
-- LZW ('compress' on hackage import)

lowComplex :: BioSeqQual s => Double -> s -> Bool
lowComplex n s = compScore < n  where 
  compScore    = (ucSize - cSize) / ucSize
  cSize        = length' $ encode uncompressed
  ucSize       = length' uncompressed
  uncompressed = unSD . seqdata s
  length'      = foldr 0 (\(_,z) -> z + 1)

filterPair :: BioSeqQual s => (s -> Bool) -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
filterPair f in1 in2 o1 o2 = do
  r1 <- readSangerQ in1
  r2 <- readSangerQ in2
  let (r1', r2') = unzip $ filter pred $ zip r1 r2
  writeSangerQ o1 r1'
  writeSangerQ o2 r2' 
  where 
    pred (fwd, rev) = (f fwd) && (f rev)


["R1.lzw", "R2.lzw"] &%> \[out1, out2] -> do
  let [src1, src2] = [r1 -<.> "deduped", r2 -<.> "deduped"]
  need [src1, src2]
  liftIO $ filterPair (not . (lowComplex 0.45)) src1 src2 out1 out2 

need' x = do
  need [x]
  return $ x

"rapsearch.gi" %> \out -> do
  let src = need' "rapsearch.m8"
  rows  <- lines <$> readfile' src
  let rows' = dropwhile \(xs -> (head xs) == '@') rows
  let gids  = map (\x -> head $ splitOn "|" $ head splitOn "\t" x) rows'
  writeFile' out $ T.intercalate "\n" gids

"rapsearch.tsv" %> \out -> do
  let src = need' $ out -<.> "gi"
  acc2tax src out NTDB

data SeqType = NT | NR
acc2tax :: SeqType -> FilePath -> FilePath -> FilePath -> Action ()
acc2tax st fpin fpout db = cmd "acc2tax" "--gi" "-i" fpin "-o" fpout "--database" db "--entries" ENTS type
  where 
    ENTS = show 1114054124
    type = case st of -- also switch to get NR/NT db
      NT -> "--nucleotide"
      NR -> "--protein"
