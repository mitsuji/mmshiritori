{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Control.Monad (foldM)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO (stdin,Handle,hIsEOF)

import qualified Data.Array.IO as IOA
import Data.Array.MArray (MArray(..),newArray,readArray,writeArray,getBounds)

import Data.Ord (Down(..))
import Data.List (sortOn)


-- [Memo]
-- 小さい文字が最後に来たら、大きい文字とみなす
-- 長音が最後に来たら、一つ前の文字を最後とみなす 'ー', '〜'
-- 'ん' でおわる -> 'ん'で始まる単語がないからOK?
-- 'ゔ' でおわる -> Unicode にあるので有効?
-- 副詞 とか 形容詞 が辞書にあるけど?
-- 重複した項目が辞書にあるけど?
-- しりとりの最初は 'り' から?

-- 元データの処理
--   $ cat GetDic005_SJIS2.txt | iconv -f SHIFT_JISX0213 > dic-utf8-org.txt
--   その後いろいろ修正 -> dic-utf8.txt

-- :set -XOverloadedStrings

-- 0x3041 -- 'ぁ'
-- 0x3042 -- 'あ'
-- 0x3093 -- 'ん'
-- 0x3094 -- 'ゔ'

type WordA          = IOA.IOArray  (Int,Int,Int) (T.Text,T.Text)  -- 単語の配列の配列(index:(先頭,終端,index))
type WordCountA     = IOA.IOUArray (Int,Int) Int                  -- 未使用単語数の配列(index:(先頭,終端))


main :: IO ()
main = do
  wordA      <- (newArray ((0,0,0),(kn 'ゔ',kn 'ゔ',800)) ("","")) :: IO WordA
  wordCountA <- (newArray ((0,0),(kn 'ゔ',kn 'ゔ')) 0) :: IO WordCountA
  headRanks  <- load wordA wordCountA  -- 単語を配列にロード,先頭文字毎のランキングを集計
  shiritori wordA wordCountA headRanks -- しりとりをする



shiritori :: WordA -> WordCountA -> [(Int,Int)] -> IO ()
shiritori wordA wordCountA hr = loop hr (kn 'り')
  where
    -- しりとりをする
    -- ランキング -> 先頭文字
    loop :: [(Int,Int)] -> Int -> IO ()
    loop hr h = do
      m <- get hr h  -- 次の単語の取得
      case m of
        Just ((word,kana),l,hr') -> do
          T.putStrLn $ word <> "（" <> kana <> "）"
          loop hr' l
        Nothing -> return () -- 終了

    -- 次の単語の取得とランキング更新
    -- ランキング -> 先頭文字 -> IO (Maybe (単語,終端文字,新ランキング))
    get :: [(Int,Int)] -> Int -> IO (Maybe ((T.Text,T.Text),Int,[(Int,Int)]))
    get hr h = f hr >>= (\m -> return $ (\(txt,l) -> (txt,l,(update' hr l))) <$> m)
      where
        -- 次の単語の取得
        -- 部分ランキング -> IO (Maybe (単語,終端文字))
        f :: [(Int,Int)] -> IO (Maybe ((T.Text,T.Text),Int))
        f ((l,_):hr') = do
          wc <- readArray wordCountA (h,l) -- 最上ランクの単語が残っているかチェック
          if wc > 0
            then do
              txt <- readArray wordA (h,l,wc-1)   -- 末尾の単語を取り出す
              writeArray wordCountA (h,l) (wc-1)  -- 1個使用済にする
              return $ Just (txt,l)
            else
              if null hr'
                 then return Nothing  -- ランキングが最後に達したら終わり
                 else f hr' -- 次のランクをチェック

        -- 次に使う先頭文字の残数を減らしてランキングを更新
        -- ランキング -> 次に使う先頭文字 -> 新ランキング
        update' :: [(Int,Int)] -> Int -> [(Int,Int)]
        update' hr h =
          let
            Just c = lookup h hr
            hr' = (h,c-1):(filter (\(h',_)-> h'/=h ) hr)
          in sortOn (Down . snd) hr'



load :: WordA -> WordCountA -> IO [(Int,Int)]
load wordA wordCountA = do
  loop      -- 単語を配列にロード
  aggregate -- 先頭毎のランキングを集計
  where
    
    -- 単語を配列にロード
    loop :: IO ()
    loop = do
      eof <- hIsEOF stdin
      if eof
        then return ()
        else do
           line <- T.hGetLine stdin
           let word:kana:_ = T.splitOn "," line
           let (h,l) = (kn $ head' kana, kn $ last' kana)
           c <- readArray wordCountA (h,l)
           writeArray wordA (h,l,c) (word,kana)
           writeArray wordCountA (h,l) (c+1)
           loop

    -- ランキングを集計
    aggregate :: IO [(Int,Int)]
    aggregate  = do
      ((b,_),(e,_)) <- getBounds wordCountA
      sortOn (Down . snd) <$> mapM headCount [b..e]
      where
        headCount :: Int -> IO (Int,Int)
        headCount h = do
          ((_,b),(_,e)) <- getBounds wordCountA
          count <- foldM add' 0 [b..e]
          return (h,count)
          where
            add' :: Int -> Int -> IO Int
            add' acc l = do
              c <- readArray wordCountA (h,l)
              return $ acc + c



kn :: Char -> Int
kn c = fromEnum c - fromEnum 'あ'

head' :: T.Text -> Char
head' = T.head

last' :: T.Text -> Char
last' = capit . f
  where
    f :: T.Text -> Char
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

