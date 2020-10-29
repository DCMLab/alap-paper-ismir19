{-# LANGUAGE OverloadedStrings #-}
module AlapGraphs where

-- import Musicology.Types

import qualified Algebra.Graph.Class as G
import qualified Algebra.Graph.AdjacencyIntMap as GA

-- import Data.Char
import Data.List (find, intercalate)
import Data.Maybe (catMaybes)
import Control.Monad (when,unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (pass)

-- import Diagrams.Prelude as D hiding ((^-^), (^+^), start, end)
-- import qualified Diagrams.TwoD.Text
-- import Diagrams.Backend.PGF as PGF

import Text.LaTeX (LaTeX, math, raw, fromString, leftarrow, rightarrow)
import Text.LaTeX.Packages.AMSMath (varepsilon)
import qualified Text.LaTeX as Ltx
import qualified Text.LaTeX.Packages.Trees as Tr
import qualified Text.LaTeX.Packages.Trees.Qtree as QTr

import qualified Debug.Trace as DT

-- scale stuff
------------------------

-- separate up and down scales
-- each maps scale degrees to hierarchical level or "don't use"
data Scale = Scale
             { scMag  :: Int
             , scUp   :: [Maybe Int]
             , scDown :: [Maybe Int]
             }

multani = Scale 7
  [Just 0, Nothing, Just 2, Just 3, Just 1, Nothing, Just 2]
  [Just 0, Just 4 , Just 2, Just 3, Just 1, Just 4, Just 2]
multaniTrans x = x - 1
multaniTransInv = (1+)

diatonic = Scale 7
  [Just 0, Just 3, Just 2, Just 3, Just 1, Just 2, Just 3]
  [Just 0, Just 3, Just 2, Just 3, Just 1, Just 2, Just 3]

blues = Scale 6
  [Just 0, Just 2, Just 3, Just 4, Just 1, Just 2]
  [Just 0, Just 2, Just 3, Just 4, Just 1, Just 2]

isNB sc@(Scale mag up down) ref nb
  | ref == nb = False
  -- | ref < 0 || nb < 0 = isNB sc (ref+mag) (nb+mag)
  | nb > ref  = isNB' down ref nb
  | otherwise = isNB' up nb ref
  where isNB' sc lower upper = case levelOf nb of
                             Nothing -> False
                             Just l  -> all (moreStb l) between -- (DT.trace (show between) between)
          where levelOf i = sc !! (mod i mag)
                between = fmap levelOf [lower+1 .. upper-1]
                moreStb l Nothing = True
                moreStb l (Just x) = l < x

notesBetween sc@(Scale mag up down) first last
  | first == last = []
  | first > last = reverse $ fillers down last first
  | otherwise    = fillers up first last
  where fillers scale lower upper = btw
         where sdOf i = const i <$> scale !! (mod i mag)
               btw = catMaybes $ sdOf <$> [lower+1 .. upper-1]

-- derivation data structure
-----------------------

data Node p = MNote p | MStart | MEnd | MEpsilon
  deriving (Eq, Ord, Show)

data Rule = Init | DupL | DupR | LeftNB | RightNB | Pass | Fill | Split
  deriving (Eq, Ord, Show)

data MelodyGraph p = MelodyGraph
                     { mgGraph :: GA.AdjacencyIntMap
                     , mgTree :: Tr.Tree (Int,Int)
                     , mgNodeLabels :: (IM.IntMap (Node p))
                     , mgEdgeLabels :: (M.Map (Int,Int) Rule)
                     , mgDepth :: (IM.IntMap Int)
                     , mgSurface :: [Int]
                     , mgNextIndex :: Int
                     }
  -- deriving (Eq, Ord, Show)

start :: Int
start = 0

end :: Int
end = 1

type MelodyBuild p a = ReaderT (Scale,p->Int,Int->p) (State (MelodyGraph p)) a

topGraph :: MelodyGraph p
topGraph = MelodyGraph
  (GA.vertex start `G.connect` GA.vertex end)
  (Tr.Leaf (start,end))
  (IM.fromList [(start,MStart), (end,MEnd)])
  (M.fromList [((start,end), Init)])
  (IM.fromList [(start,0), (end, 0)])
  [start, end]
  2

topGraph' :: (Node p) -> (Node p) -> MelodyGraph p
topGraph' p1 p2 = MelodyGraph
  (GA.vertex start `G.connect` GA.vertex end)
  (Tr.Leaf (start,end))
  (IM.fromList [(start,p1), (end,p2)])
  (M.fromList [((start,end), Init)])
  (IM.fromList [(start,0), (end, 0)])
  [start, end]
  2

insertAfter :: Eq a => a -> [a] -> [a] -> [a]
insertAfter _ [] _ = []
insertAfter x (y:lst) as
  | x == y    = y : (as ++ lst)
  | otherwise = y : insertAfter x lst as

insertNodes :: Int -> Int -> [Node p] -> Rule -> MelodyBuild p [Int]
insertNodes _  _  []    _    = error "List of nodes must not be empty."
insertNodes n1 n2 nodes rule = do
  (MelodyGraph graph tree nlabels elabels depth surface newi) <- get
  let l = length nodes
      newis = [newi..newi+l-1]
      newd = (1 + max (depth IM.! n1) (depth IM.! n2))
      graph' = graph `G.overlay` (GA.path $ n1:newis++[n2])
      tree' = fillLeaf (n1,n2) newis tree
      nlabels' = foldl finsert nlabels (zip newis nodes) 
      elabels' = M.insert (n1,n2) rule elabels
      depth' = foldl finsert depth (zip newis (repeat newd))
      surface' = insertAfter n1 surface newis
  put $ MelodyGraph graph' tree' nlabels' elabels' depth' surface' (newi+l)
  return newis
  where finsert = flip $ uncurry IM.insert
        fillLeaf edge is (Tr.Node e children) = Tr.Node e (fillLeaf edge is <$> children)
        fillLeaf edge is (Tr.Leaf e)
          | edge == e = Tr.Node (Just e) (leaves n1 n2 is)
          | otherwise = Tr.Leaf e
        leaves n1 n2 is = Tr.Leaf <$> zip (n1:is) (is++[n2])

insertNode :: Int -> Int -> Node p -> Rule -> MelodyBuild p Int
insertNode n1 n2 node rule =
  head <$> insertNodes n1 n2 [node] rule

-- checking properties

getScale :: MelodyBuild p (Scale, p->Int, Int->p)
getScale = ask

checkNote :: (Show p) => (Node p) -> MelodyBuild p p
checkNote (MNote x) = return x
checkNote x         = error $ show x <> " is not a note"

getLabel :: Int -> MelodyBuild p (Node p)
getLabel i = do
  mg <- get
  return $ mgNodeLabels mg IM.! i

getNote i = do
  nd <- getLabel i
  checkNote nd

checkNB :: (Show p) => p -> p -> MelodyBuild p ()
checkNB ref nb = do
  (sc,trans,_) <- getScale
  let allGood = isNB sc (trans ref) (trans nb)
  unless allGood $ error $ show nb <> " is not a neighbor of " <> show ref

getFills :: (Show p) => p -> p -> MelodyBuild p [p]
getFills first last = do
  (sc,trans,transi) <- getScale
  return $ transi <$> notesBetween sc (trans first) (trans last)

-- graph operations
-------------------

initGraph :: p -> MelodyBuild p Int
initGraph p = do
  put topGraph
  insertNode start end (MNote p) Init

split :: Int -> Int -> MelodyBuild p Int
split n1 n2 = insertNode n1 n2 MEpsilon Split

dupLeft :: (Show p) => Int -> Int -> MelodyBuild p Int
dupLeft n1 n2 = do
  n <- getLabel n1
  checkNote n
  insertNode n1 n2 n DupL

dupRight :: (Show p) => Int -> Int -> MelodyBuild p Int
dupRight n1 n2 = do
  n <- getLabel n2
  checkNote n
  insertNode n1 n2 n DupR

rightNB :: (Show p) => Int -> Int -> p -> MelodyBuild p Int
rightNB n1 n2 nb = do
  ref <- getNote n1
  checkNB ref nb
  insertNode n1 n2 (MNote nb) RightNB

leftNB :: (Show p) => Int -> Int -> p -> MelodyBuild p Int
leftNB n1 n2 nb = do
  ref <- getNote n2
  checkNB ref nb
  insertNode n1 n2 (MNote nb) LeftNB

pass :: (Show p) => Int -> Int -> p -> MelodyBuild p Int
pass n1 n2 f = do
  from <- getNote n1
  to <- getNote n2
  checkNB f from
  checkNB to f
  insertNode n1 n2 (MNote f) Pass

fills :: (Show p) => Int -> Int -> MelodyBuild p [Int]
fills n1 n2 = do
  first <- getNote n1
  last  <- getNote n2
  fs <- getFills first last
  insertNodes n1 n2 (MNote <$> fs) Fill

-- drawing
----------

runBuild' :: Scale -> (p -> Int) -> (Int -> p) -> MelodyBuild p a -> MelodyGraph p
runBuild' sc trans transi b =
  snd $ runState (runReaderT b (sc,trans,transi)) topGraph

runBuild :: MelodyBuild Int a -> MelodyGraph Int
runBuild = runBuild' multani multaniTrans multaniTransInv

-- drawGraph :: MelodyGraph Int -> QDiagram b V2 Double Any

pitchNames :: Node Int -> LaTeX
pitchNames MStart = raw "\\rtimes"
pitchNames MEnd = raw "\\ltimes"
pitchNames MEpsilon = varepsilon
pitchNames (MNote i) = pitchAlts !! mi
                       <> fromString (show (mi+1))
                       <> octave (div (i-1) 7)
  where mi = mod (i-1) 7

octave o
  | o == 0 = mempty
  | o > 0  = raw $ T.replicate o "\'" -- "^" <> tshow o
  | o < 0  = raw $ T.replicate (abs o) "_\\prime" -- "_" <> tshow (abs o)

flat = raw "\\flat" :: LaTeX
sharp = raw "\\sharp" :: LaTeX
pitchAlts = [mempty, flat, flat, sharp, mempty, flat, mempty]

showRule :: Rule -> LaTeX
showRule Init    = "init"
showRule Split   = "split"
showRule DupL    = math leftarrow <> "dup"
showRule DupR    = "dup" <> math Ltx.to
showRule LeftNB  = "nb" <> math Ltx.to
showRule RightNB = math leftarrow <> "nb"
showRule Pass    = "pass"
showRule Fill    = "fill"

tshow :: Show a => a -> T.Text
tshow = T.pack . show

tikzGraph :: Eq a => (Node a -> LaTeX) -> MelodyGraph a -> T.Text
tikzGraph pitchNames (MelodyGraph graph _ nlabels elabels depth surface _) =
  mconcat (mkNode <$> (zip surface [0..])) <> mconcat (mkEdge <$> (GA.edgeList graph))
  where surfaceEdges =  zip surface (tail surface)
        mkNode (n, i) = "\\node[" <> style node <> "] (note" <> tshow n <> ") at (" <> tshow x
                        <> "," <> tshow y <> ") {" <> label <> "};\n"
          where x = i
                y = 0.5 * (fromIntegral $ negate $ depth IM.! n)
                node = nlabels IM.! n
                label = Ltx.render $ math $ pitchNames $ node
                style MEpsilon  = "epsilon node"
                style (MNote _) = "note node"
                style _         = "external node"
        -- mkEdge (n1,n2) = "\\draw (note" <> show n1 <> ") edge (note" <> show n2 <> ");\n"
        mkEdge (n1,n2) = "\\draw (note" <> tshow n1
                         <> ") edge[" <> eps <> "] node[operation] {"
                         <> op <> "} (note" <> tshow n2 <> ");\n"
          where op = case elabels M.!? (n1,n2) of
                       Just r  -> Ltx.render $ showRule r
                       Nothing -> ""
                l1  = nlabels IM.! n1
                l2  = nlabels IM.! n2
                styles :: [T.Text]
                styles = snd $ runWriter $ do
                  when (l1 == MEpsilon || l2 == MEpsilon) $ tell ["epsilon edge"]
                  when (l1 == MStart || l2 == MEnd) $ tell ["external edge"]
                  when ((n1,n2) `elem` surfaceEdges) $ tell ["surface edge"]
                eps = T.intercalate "," styles

toTree :: GA.AdjacencyIntMap -> Tr.Tree (Int,Int)
toTree graph = branch start end
  where vs = GA.vertexList graph
        isChild a b x = GA.hasEdge a x graph && GA.hasEdge x b graph
        branch a b = case find (isChild a b) vs of
                       Just x  -> Tr.Node (Just (a,b)) [branch a x, branch x b]
                       Nothing -> Tr.Leaf (a,b)

tikzTree :: (Node a -> LaTeX) -> MelodyGraph a -> T.Text
tikzTree pitchNames (MelodyGraph _ tree nlabels _ _ _ _) = Ltx.render ltx
  where ltx  = QTr.tree renderNode tree :: LaTeX
        note = pname . (nlabels IM.!)
        pname MEpsilon = ""
        pname a = pitchNames a
        renderNode (a,b) = math $ note a <> rightarrow <> note b

-- tikzGraph' :: MelodyGraph Int -> String
-- tikzGraph' = tikzGraph pitchNames

-- tikzTree' :: MelodyGraph Int -> String
-- tikzTree' = tikzTree pitchNames

putGraph = TIO.putStrLn . tikzGraph pitchNames . runBuild

putTree = TIO.putStrLn . tikzTree pitchNames . runBuild


pitchNames' :: [LaTeX] -> Node Int -> LaTeX
pitchNames' _  MStart    = raw "\\rtimes"
pitchNames' _  MEnd      = raw "\\ltimes"
pitchNames' _  MEpsilon  = varepsilon
pitchNames' ns (MNote i) = (ns !! mi) <> octave (div i 7)
  where mi = mod i $ length ns

diaNames :: [LaTeX]
diaNames = ["1", "2", "3", "4", "5", "6", "7"]

bluesNames :: [LaTeX]
bluesNames = ["1", flat <> "3", flat <> "4", flat <> "5", "5", flat <> "7"]

putGraph' ns sc = TIO.putStrLn . tikzGraph ns . runBuild' sc id id

putTree' ns sc = TIO.putStrLn . tikzTree ns . runBuild' sc id id

putDia = putGraph' (pitchNames' diaNames) diatonic
putBlues = putGraph' (pitchNames' bluesNames) blues


saveGraph build = do
  TIO.writeFile "test_graph.tex" $ tikzGraph pitchNames graph
  TIO.writeFile "test_tree.tex"  $ tikzTree  pitchNames graph
  where graph = runBuild build

saveLong = do
  TIO.writeFile "alap_graph.tex" $
    tikzGraph pitchNames $ runBuild longGraph
  
-- examples
-----------

simpleGraph :: MelodyBuild Int ()
simpleGraph = do
  top1 <- initGraph 1
  top5 <- leftNB start top1 5
  fills top5 top1 -- [4, 3, 2]
  return ()

phraseGraph :: MelodyBuild Int ()
phraseGraph = do
  top1 <- initGraph 1
  pre1 <- dupRight start top1
  leftNB start pre1 0
  prep3 <- leftNB pre1 top1 3
  pass prep3 top1 2
  return ()

phraseGraph' :: MelodyBuild Int ()
phraseGraph' = do
  top1 <- initGraph 1
  pre1 <- dupRight start top1
  cut <- split pre1 top1
  leftNB start pre1 0
  prep3 <- leftNB cut top1 3
  pass prep3 top1 2
  return ()

doubleA :: MelodyBuild Int ()
doubleA = do
  put $ topGraph' (MNote 5) (MNote 5)
  upper <- leftNB start end 7
  lower <- rightNB start upper 4
  pass upper end 6
  return ()

doubleB :: MelodyBuild Int ()
doubleB = do
  put $ topGraph' (MNote 5) (MNote 5)
  lower <- rightNB start end 4
  upper <- leftNB lower end 7
  pass upper end 6
  return ()

doubleC :: MelodyBuild Int ()
doubleC = do
  put $ topGraph' (MNote 5) (MNote 5)
  cut <- split start end
  lower <- rightNB start cut 4
  upper <- leftNB cut end 7
  pass upper end 6
  return ()


-- non-raga examples
--------------------

heiland :: MelodyBuild Int ()
heiland = do
  put $ topGraph' (MNote 0) (MNote 2)
  let top1 = start
      top3 = end
  middle <- split top1 top3
  end1 <- dupLeft top1 middle
  cut1 <- split top1 end1
  rightNB top1 cut1 (-1)
  nb3 <- leftNB cut1 end1 2
  pass2 <- pass nb3 end1 1
  passCutA <- split pass2 end1
  pass2' <- dupLeft pass2 passCutA
  rightNB pass2 pass2' 0
  nb1 <- leftNB middle top3 0
  mid5 <- leftNB nb1 top3 4
  mid3 <- rightNB nb1 mid5 2
  pass4 <- pass mid3 mid5 3
  passCutB <-split pass4 mid5
  pass4' <- dupLeft pass4 passCutB
  leftNB pass4 pass4' 2
  return ()

maman :: MelodyBuild Int ()
maman = do
  put $ topGraph' (MNote 0) (MNote 0)
  top5 <- leftNB start end 4
  snd5 <- dupLeft top5 end
  high6 <- leftNB top5 snd5 5
  fillers <- fills snd5 end
  return ()

moanin :: MelodyBuild Int ()
moanin = do
  put $ topGraph' (MNote 0) (MNote 0)
  mid1 <- dupLeft start end
  fst1 <- dupLeft start mid1
  fstCut <- split fst1 mid1
  leftNB start fst1 1
  rightNB fst1 fstCut (-2)
  leftNB fstCut mid1 (-1)
  nb3 <- rightNB mid1 end 1
  nb4 <- rightNB nb3 end 2
  nb4b <- rightNB nb4 end 3
  fills nb4b end
  return ()

spine :: MelodyBuild Int ()
spine = do
  first1 <- initGraph 1
  last1 <- dupLeft first1 end
  top8 <- leftNB first1 last1 8
  pass first1 top8 5
  pass top8 last1 5
  return ()

longGraph :: MelodyBuild Int ()
longGraph = do
  top1 <- initGraph 1
  --final1 <- dupLeft top1 end
  top8 <- rightNB top1 end 8
  asc5 <- pass top1 top8 5
  first1cut  <- split top1 asc5
  asc5a <- dupRight first1cut asc5
  asc5cut <- split asc5a asc5
  prelude start top1
  upTo5 first1cut asc5a asc5cut
  upTo8 asc5cut asc5 top8
  return ()

mkSpine' first1 last1 = do
  top8 <- leftNB first1 last1 8
  asc5a <- pass first1 top8 5
  desc5 <- pass top8 last1 5
  asc5b <- dupLeft asc5a top8
  asc5cut <- split asc5a asc5b
  top8a <- dupRight asc5b top8
  top8acut <- split top8a top8
  top8b <- dupLeft top8 desc5
  top8bcut <- split top8 top8b
  first1cut  <- split first1 asc5a
  upTo5 first1cut asc5a asc5cut
  upTo8 asc5cut asc5b top8a
  return ()

-- really long example
----------------------

exampleGraph :: MelodyBuild Int ()
exampleGraph = do
  top1 <- initGraph 1
  final1 <- dupLeft top1 end
  mkSpine top1 final1
  prelude start top1
  return ()

prelude start top1 = do
  dup1 <- dupRight start top1
  nb7  <- leftNB start dup1 0
  nb3  <- leftNB dup1 top1 3
  pass nb3 top1 2

mkSpine first1 last1 = do
  top8 <- leftNB first1 last1 8
  asc5a <- pass first1 top8 5
  desc5 <- pass top8 last1 5
  asc5b <- dupLeft asc5a top8
  asc5cut <- split asc5a asc5b
  top8a <- dupRight asc5b top8
  top8acut <- split top8a top8
  top8b <- dupLeft top8 desc5
  top8bcut <- split top8 top8b
  first1cut  <- split first1 asc5a
  -- elaborate
  upTo5 first1cut asc5a asc5cut
  upTo8 asc5cut asc5b top8a
  onTop8 top8acut top8 top8bcut
  descent top8bcut top8b desc5 last1

upTo5 start top5 end = do
  -- top5b <- dupLeft top5 end
  prep4 <- leftNB start top5 4 -- could also be pass?
  cut   <- split prep4 top5
  prep3 <- leftNB start prep4 3
  dup4  <- dupLeft prep4 cut
  leftNB prep4 dup4 5
  to41  <- rightNB dup4 cut 1
  -- to43  <- pass dup4 to41 3
  -- pass to43 to41 2
  fills dup4 to41
  up1   <- leftNB cut top5 1
  up3   <- pass up1 top5 3
  pass up3 top5 4
  down1 <- rightNB top5 end 1 -- top5dup, if generated above
  down3 <- pass top5 down1 3
  down4 <- pass top5 down3 4
  cutdown <- split top5 down4
  leftNB cutdown down4 3
  pass down3 down1 2

upTo8 start top5 top8 = do
  topCut <- split top5 top8
  -- around 5
  top5' <- dupLeft top5 topCut
  cut5 <- split top5 top5'
  top5end <- dupLeft top5' topCut
  -- cut5  <- split top5 top5'
  prep1 <- leftNB start top5 1
  leftNB start prep1 0
  pass3 <- pass prep1 top5 3
  pass pass3 top5 4
  -- down7 <- leftNB cut5 top5' 7
  nb3 <- leftNB cut5 top5' 3
  pass nb3 top5' 4
  down7 <- leftNB top5' top5end 7
  pass down7 top5end 6
  -- to3 <- rightNB top5' topCut 3
  -- pass top5' to3 4
  -- around 8
  up7 <- leftNB topCut top8 7
  up7p5 <- leftNB topCut up7 5
  up7p3 <- leftNB topCut up7p5 3
  pass up7p3 up7p5 4
  prep5 <- leftNB up7 top8 5
  prep5p3 <- leftNB up7 prep5 3
  fills up7 prep5p3 -- [6, 5, 4]
  -- pass up7 pass5 6
  -- pass pass5 prep5p3 4
  -- prep5p3' <- dupLeft prep5p3 prep5
  pass prep5p3 prep5 4
  
onTop8 start top8 end = do
  top8' <- dupLeft top8 end
  topCut <- split top8 top8'
  prep5 <- leftNB start top8 5
  pass prep5 top8 7
  prep10 <- leftNB topCut top8' 10
  pass prep10 top8' 9

descent start top8 top5 top1 = do
  prep5 <- leftNB start top8 5
  pass prep5 top8 7
  a5 <- dupRight top8 top5
  a5cut <- split a5 top5
  pass7 <- pass top8 a5 7
  pass pass7 a5 6
  b5 <- dupLeft top5 top1
  b5cut <- split top5 b5
  post3 <- rightNB top5 b5cut 3
  pass top5 post3 4
  post3' <- dupLeft post3 b5cut
  leftNB post3 post3' 1
  pass3 <- pass b5 top1 3
  pass3cut <- split b5 pass3
  pass3' <- dupRight pass3cut pass3
  nb4 <- leftNB pass3' pass3 4
  pass pass3 top1 2
