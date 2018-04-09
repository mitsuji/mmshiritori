{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Control.Monad (foldM)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as LTB

import System.IO (stdin,Handle,hIsEOF)

import Data.Array.IO (IOArray(..),IOUArray(..))
import Data.Array.MArray (newArray,readArray,writeArray,getBounds)

import Data.Ord (Down(..))
import Data.List (sortOn)

import Control.Monad.RWS (liftIO,RWST(..),evalRWST,get,modify,tell)

import Data.Time (getCurrentTime,diffUTCTime)

-- [Memo]
-- 小さい文字が最後に来たら、大きい文字とみなす 'ぁ', 'ぃ', 'っ',...
-- 長音が最後に来たら、一つ前の文字を最後とみなす 'ー', '〜'
-- 'ん' で始まる/終わる -> 辞書にあるならOK
-- 'ゔ' で始まる/終わる -> Unicode にあるので有効
-- しりとりの最初は 'り' から?

-- 元データの処理
--   $ cat GetDic005_SJIS2.txt | iconv -f SHIFT_JISX0213 > dic-utf8-org.txt
--   その後いろいろ修正 -> dic-utf8.txt

-- :set -XOverloadedStrings

-- 0x3041 -- 'ぁ'
-- 0x3042 -- 'あ'
-- ...
-- 0x3093 -- 'ん'
-- 0x3094 -- 'ゔ'

type Word'    = T.Text -- 単語
type Kana     = T.Text -- かな(単語の読み)
type KanaCode = Int    -- かな番号 ['あ':0, 'ぃ':1, 'い':2, ... ,'を':80, 'ん':81, 'ゔ':82]
type Index    = Int 
type Count    = Int

type WordA       = IOArray  (KanaCode,KanaCode,Index) (Word',Kana) -- 単語帳 (先頭文字,末尾文字,index) -> (単語,かな)
type WordCountA  = IOUArray (KanaCode,KanaCode) Count              -- 単語カウンタ (先頭文字,末尾文字) -> 残数
type WordRanking = [(KanaCode,Count)]                              -- 単語残数ランキング (先頭文字,残数)の多い順リスト


main :: IO ()
main = do
  wordA      <- (newArray ((0,0,0),(kc 'ゔ',kc 'ゔ',800)) ("","")) :: IO WordA
  wordCountA <- (newArray ((0,0),(kc 'ゔ',kc 'ゔ')) 0) :: IO WordCountA
  headRanks  <- load wordA wordCountA  -- 単語帳と単語カウンタを読み込み,単語残数ランキングを集計する
  b <- getCurrentTime
--  LT.putStr =<< LTB.toLazyText <$> shiritori wordA wordCountA headRanks -- しりとりをする
  txt <- LTB.toLazyText <$> shiritori wordA wordCountA headRanks -- しりとりをする
  e <- getCurrentTime
  LT.putStr txt
  print $ diffUTCTime e b


shiritori :: WordA -> WordCountA -> WordRanking -> IO LTB.Builder
shiritori wordA wordCountA headRanks = snd <$> evalRWST (loop (kc 'り')) () headRanks
  where
    -- しりとりをする
    -- :: 先頭文字 -> RWST () LTB.Builder 残数ランキング IO ()
    loop :: KanaCode -> RWST () LTB.Builder WordRanking IO ()
    loop h = do
      m <- next h -- 次の単語を取得する
      case m of
        Just ((word,kana),h') -> do
          tell $ LTB.fromText word
          tell $ LTB.singleton '（'
          tell $ LTB.fromText kana
          tell $ LTB.singleton '）'
          tell $ LTB.singleton '\n'
          loop h'
        Nothing -> return ()
      
    -- 次の単語とその末尾文字を取得し、残数ランキングを更新する
    -- :: 先頭文字 -> RWST () LTB.Builder 残数ランキング IO (Maybe (単語,末尾文字))
    next :: KanaCode -> RWST () LTB.Builder WordRanking IO (Maybe ((Word',Kana),KanaCode))
    next h =
      -- 残数ランキングを get >>= 次の単語を取得 >>= 残数ランキングを modify >>= return
      get >>= (\hr -> liftIO $ f hr) >>= (\m -> (modify update) >> return m)
      where
        -- 次の単語の取得
        -- :: 残数ランキング -> IO (Maybe (単語,末尾文字))
        f :: WordRanking -> IO (Maybe ((Word',Kana),KanaCode))
        f ((l,_):hr') = do
          wc <- readArray wordCountA (h,l)
          if wc > 0 -- 最上ランクの単語が残っているかチェック
            then do
              txt <- readArray wordA (h,l,wc-1)   -- 単語帳から未使用の単語を取得
              writeArray wordCountA (h,l) (wc-1)  -- 単語カウンタを1個使用済にする
              return $ Just (txt,l)
            else
              if null hr'
                 then return Nothing  -- 残数ランキングがなくなったら終了
                 else f hr' -- 次のランクをチェック

        -- 取得した単語の先頭文字の残数を減らして残数ランキングを更新
        update :: WordRanking -> WordRanking
        update hr =
          let
            hr' = fmap (\(h',c) -> if h'==h then (h',c-1) else (h',c)) hr
          in sortOn (Down . snd) hr'



load :: WordA -> WordCountA -> IO WordRanking
load wordA wordCountA = do
  loop      -- 単語帳と単語カウンタを読み込む
  aggregate -- 単語残数ランキングを集計
  where

    -- 単語帳と単語カウンタを標準入力から読み込む
    loop :: IO ()
    loop = do
      eof <- hIsEOF stdin
      if eof
        then return ()
        else do
           line <- T.hGetLine stdin
           let word:kana:_ = T.splitOn "," line
           let (h,l) = (kc $ head' kana, kc $ last' kana)
           c <- readArray wordCountA (h,l)
           writeArray wordA (h,l,c) (word,kana)
           writeArray wordCountA (h,l) (c+1)
           loop

    -- 単語カウンタから単語残数ランキングを集計
    aggregate :: IO WordRanking
    aggregate  = do
      ((b,_),(e,_)) <- getBounds wordCountA
      sortOn (Down . snd) <$> mapM count [b..e]
      where
        count :: KanaCode -> IO (KanaCode,Count)
        count h = do
          ((_,b),(_,e)) <- getBounds wordCountA
          (\c -> (h, c)) <$> foldM add 0 [b..e]
          where
            add :: Count -> KanaCode -> IO Count
            add acc l = ((+)acc) <$> readArray wordCountA (h,l)



kc :: Char -> KanaCode
kc c = fromEnum c - fromEnum 'あ'

head' :: Kana-> Char
head' = T.head

last' :: Kana -> Char
last' = capit . f
  where
    f :: Kana -> Char
    f txt = case T.last txt of
      x | False
          || x == 'ー'
          || x == '〜'
          -> T.last $ T.init txt
      otherwise -> T.last txt

capit :: Char -> Char
capit = f
  where
    f :: Char -> Char
    f 'ぁ' = 'あ'
    f 'ぃ' = 'い'
    f 'ぅ' = 'う'
    f 'ぇ' = 'え'
    f 'ぉ' = 'お'
    f 'っ' = 'つ'
    f 'ゃ' = 'や'
    f 'ゅ' = 'ゆ'
    f 'ょ' = 'よ'
    f 'ゎ' = 'わ'
    f x  = x

