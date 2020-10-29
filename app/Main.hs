{-# LANGUAGE OverloadedStrings #-}
module Main where

import AlapGraphs

import qualified Data.Text.IO as T

main :: IO ()
main = do
  -- example phrase
  T.writeFile "phrase_tree.tex" $ tikzTree pitchNames $ runBuild phraseGraph
  T.writeFile "phrase_graph.tex" $ tikzGraph pitchNames $ runBuild phraseGraph
  T.writeFile "phrase_graph_alt.tex" $ tikzGraph pitchNames $ runBuild phraseGraph'
  -- double neighbor variants
  T.writeFile "double_a.tex" $ tikzGraph pitchNames $ runBuild doubleA
  T.writeFile "double_b.tex" $ tikzGraph pitchNames $ runBuild doubleB
  T.writeFile "double_c.tex" $ tikzGraph pitchNames $ runBuild doubleC
  -- main example
  T.writeFile "spine.tex" $ tikzGraph pitchNames $ runBuild spine
  T.writeFile "alap_graph.tex" $ tikzGraph pitchNames $ runBuild longGraph
  -- non-raga examples
  T.writeFile "heiland.tex" $ tikzGraph (pitchNames' diaNames) $ runBuild' diatonic id id heiland
  T.writeFile "moanin.tex" $ tikzGraph (pitchNames' bluesNames) $ runBuild' blues id id moanin
