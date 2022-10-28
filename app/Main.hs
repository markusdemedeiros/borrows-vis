{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import System.Environment   
import System.Exit
import System.Directory
import System.Process
import Data.List
import Data.List.Split
import Data.Char

main :: IO ()
main = do
    args <- getArgs
    case args of 
        ["generate", file] -> do
            abs_path <- makeAbsolute file
            cache <- getXdgDirectory XdgData "borrows-vis"
            createDirectoryIfMissing True cache 
            putStrLn $ "[vis] generating facts for " ++ abs_path ++ " in " ++ cache
            removeDirectoryRecursive $ cache ++ "/mir_dump/"
            createDirectory $ cache ++ "/mir_dump/"
            await $ createProcess (proc "rustc" ["-Zdump_mir=main", "-Znll-facts", abs_path]){ cwd = Just cache } 
            putStrLn "[vis] running system-level polonius on rustc input facts"
            polonius_stdout <- readCreateProcess ((proc "polonius" ["-a", "naive", "-v", "--show-tuples", cache ++ "/nll-facts/main/"]) { cwd = Just cache }) ""  
            writeFile (cache ++ "/nll-facts/output_facts.facts") (clean_polonius_output polonius_stdout)
            putStrLn "[vis] done"
        ["vis"] -> do
            input_facts <- read_input
            output_facts <- read_output
            mir <- read_mir
            let all_facts = input_facts ++ output_facts
            -- putStrLn . show $ borrow_relations all_facts (20, Left 8)


            -- mapM_ print (filter ((== "cfg_edge") . head) input_facts)
            write_graph "./output/borrow_relations.dot" $ origin_relations_graph mir all_facts (happy_cfg mir)
            write_graph "./output/location_cfg.dot" $ location_cfg_graph mir (happy_cfg mir)
            write_graph "./output/happy_cfg.dot" . happy_cfg_graph . happy_cfg $ mir
            write_graph "./output/mir.dot" . mir_cfg_graph mir . happy_cfg $ mir
            write_graph "./output/raw_facts.dot" . polonius_facts_graph  mir all_facts . happy_cfg $ mir
            return ()
        _ -> do
            putStrLn "[vis] bad arguments"
            exitWith (ExitFailure (-1))
    where 
        await p = (p >>= (\(_, _, _, proc_h) -> waitForProcess proc_h))

        clean_polonius_output :: String -> String
        clean_polonius_output = intercalate "\n" .filter (\s -> (head s) /= '#') . drop 3 . lines

        input_fact_path :: IO String
        input_fact_path = do 
            cache <- getXdgDirectory XdgData "borrows-vis"
            return (cache ++ "/nll-facts/main/")

        read_input :: IO Facts
        read_input = do
            fact_dir <- input_fact_path
            all_files <- listDirectory fact_dir
            all_facts <- mapM read_in_fact all_files
            return $ concat all_facts

        read_in_fact :: String -> IO [[String]]
        read_in_fact fact_name = do
            fact_dir <- input_fact_path
            contents <- readFile $ fact_dir ++ fact_name
            return . fmap (name :) . fmap words . lines $ contents
            where 
                name = reverse . drop 6 . reverse $ fact_name

        read_output :: IO Facts
        read_output = do
            cache <- getXdgDirectory XdgData "borrows-vis"
            contents <- readFile (cache ++ "/nll-facts/output_facts.facts")
            return . fmap words . lines $ contents

        read_mir :: IO MirStatements 
        read_mir = do 
            cache <- getXdgDirectory XdgData "borrows-vis"
            files <- listDirectory $ cache ++ "/mir_dump/"
            contents <- readFile . ((cache ++ "/mir_dump/") ++) . check_mir_path . filter (".-------.nll.0.mir" `isSuffixOf`) $ files
            let blocks = drop 2 . splitOn [""] . fmap (dropWhile isSpace) . lines $ contents
            return . fmap (fmap (takeWhile (/= ';')) . tail . filter (not . (== "}")) . filter (not . (isPrefixOf) "//")) $ blocks 
            where 
                check_mir_path [a] = a
                check_mir_path input = error $ show input

        write_graph :: FilePath -> [String] -> IO()
        write_graph fname = writeFile fname . concat . fmap (++ "\n") 


type Facts = [[String]]
type MirStatements = [[String]] 


type BasicBlock = Int
type Terminator = String

-- Location -> MIR statement helper

terminator :: MirStatements -> BasicBlock -> Terminator
terminator st = last . (st !!) 

-- CFG without definitely unwinding paths (wip)
--  (cleaner in rust)
happy_path :: Terminator -> [BasicBlock]
happy_path "return" = []
happy_path "resume" = [] 
happy_path "unreachable" = [] 
happy_path s 
    | ("switchInt" `isPrefixOf` s) = fmap read_digit . tail . splitOn ": bb" . (!!1) . splitOn "-> [" $ s
    | (length spl_unc > 1) = [read_digit (spl_unc !! 1)]
    | (length spl_ret > 1) = [read_digit (spl_ret !! 1)]
    | (length spl_real > 1) = [read_digit (spl_real !! 1)]
    | otherwise = undefined
    where 
        spl_real = splitOn "[real: bb" s
        spl_ret = splitOn "[return: bb" s
        spl_unc = splitOn "-> bb" s

read_digit :: String -> Int
read_digit = read . takeWhile isDigit


-- helpers for cleaning raw input
read_raw_origin :: String -> Int 
read_raw_origin = read_digit . drop 4

read_raw_loan :: String -> Int 
read_raw_loan = read_digit . drop 3

read_raw_location :: String -> Location
read_raw_location s
    | ("\"Start(" `isPrefixOf` s) = (read_digit . drop 9 $ s, Left bb)
    | ("\"Mid(" `isPrefixOf` s) = (read_digit . drop 7 $ s, Right bb)
    | otherwise = error s
    where bb = read_digit . tail . dropWhile (/= '[') $ s

type BaseCFG = ([Int], [(Int, Int)])
happy_cfg :: MirStatements -> BaseCFG
happy_cfg mir = visit ([], []) [0]
    where
        visit :: ([Int], [(Int, Int)]) -> [BasicBlock] -> ([Int], [(Int, Int)])
        visit n [] = n
        visit (nodes, edges) (bb:bbs) 
            | bb `elem` nodes = visit (nodes, edges) bbs
            | otherwise = visit (next_nodes, next_edges) (bbs ++ nexts)
            where 
                nexts = happy_path . terminator mir $ bb
                next_nodes = bb : nodes
                next_edges = edges ++ (fmap (\s -> (bb, s)) nexts)
                next_todo = bbs ++ nexts


-- CFG graph, structure of basic blocks only
happy_cfg_graph :: BaseCFG -> Graphviz
happy_cfg_graph (nodes, edges) 
    = digraph "CFG" $ (map simple_node nodes) ++ (map simple_edge edges)


-- CFG graph with intermediate statements as records
mir_cfg_graph :: MirStatements -> BaseCFG -> Graphviz
mir_cfg_graph mir (nodes, edges)
    = digraph "CFG"
        $ (["rankdir=TB"] ++) 
        $ ((map (\s -> record_node s (mir !! s)) nodes) ++)
        $ map simple_edge edges

borrow_relations_graph :: MirStatements -> BaseCFG -> Graphviz
borrow_relations_graph mir cfg
    = digraph "CFG"
        $ (++) (map location_node nodes)
        $ map (uncurry (edge_location mir)) edges
    where 
        (nodes, edges) = location_cfg mir cfg


polonius_facts_graph :: MirStatements -> Facts -> BaseCFG -> Graphviz
polonius_facts_graph mir facts cfg
    = digraph "CFG"
        $ (["rankdir=TB"] ++) 
        $ ((map (\s -> location_record_node s (content s)) nodes) ++)
        $ map (uncurry (edge_location mir)) edges
    where
        (nodes, edges) = location_cfg mir cfg
        all_facts = nub . map (!!0) $ facts
        
        facts_at :: Location -> [[String]]
        facts_at s = filter ((== s) . read_raw_location . (!!1)) facts
        
        ocla s = intercalate ", " . map (\[_, _, o, l] -> "(" ++ o ++ ", " ++ l ++ ")") . filter ((== s) . read_raw_location . (!!1)) . get_fact "origin_contains_loan_at" $ facts

        issues s = intercalate ", " . map (\[_, o, l, _] -> "(" ++ o ++ ", " ++ l ++ ")") . filter ((== s) . read_raw_location . (!!3)) . get_fact "loan_issued_at" $ facts

        kills s = intercalate ", " . map (\[_, l, _] -> l) . filter ((== s) . read_raw_location . (!!2)) . get_fact "loan_killed_at" $ facts

        content :: Location -> [String]
        content s = ((label_location s) :) . map concat_tuple . filter ((/= []) . snd) $ [("origin_contains_loan_at: ", ocla s), ("loan_issued_at: ", issues s), ("loan_killed_at: ", kills s)]
        
        concat_tuple (a, b) = a ++ b




-- Location-level graph with subgraphs at each point (no BB's)
get_fact :: String -> [[String]] -> [[String]]
get_fact f = filter ((== f) . head) 


type Borrow = Int

-- Computes the induced relationships between origins (assume no negative information)
borrow_relations :: Facts -> Location -> [([Borrow], [Borrow])] 
borrow_relations facts l = induced_subset_graph origins
    where 
        ocla_facts = map (\f-> (read_raw_origin . (!!2) $ f, read_raw_loan . (!!3) $ f)) . filter ((l ==) . read_raw_location . (!!1)) . get_fact "origin_contains_loan_at" $ facts
        origins = map (map snd) . groupBy (\(a, _) -> (a ==) . fst) $ ocla_facts
        

induced_subset_graph :: (Eq a, Ord a) => [[a]] -> [([a], [a])]
induced_subset_graph = generate . nubBy seteq . map nub
    where 
        generate xs = [(a, b) | a <- xs, b <- xs, subset a b] 
        subset xs ys = foldl (flip $ (&&) . flip elem ys) True xs
        seteq xs ys = null (xs \\ ys) && null (ys \\ xs)

type Location = (Int, Either Int Int)
type LocationCFG = ([Location], [(Location, Location)])


-- BaseCFG does inter-block edges
-- Edge between every start and end for every mir statement
location_cfg :: MirStatements -> BaseCFG -> LocationCFG
location_cfg mir (bbs, bb_edges) = (nodes, edges)
    where 
        -- List of indicies of MIR statements within a bb (including terminator)
        bb_stmt_idxs bb =  [0..((length (mir!!bb)) - 1)]
        -- CFG edges within statement, grouped by bb
        statement_edge :: [[(Location, Location)]]
        statement_edge = map (\bb -> map (\ix -> ((bb, Left ix), (bb, Right ix))) (bb_stmt_idxs bb)) $ bbs
        -- CFG edge between statements, same BB
        bb_statement_edge :: [[(Location, Location)]]
        bb_statement_edge = map (\bb_edges -> map (\((_, l0), (l1, _)) -> (l0, l1)) $ zip bb_edges (drop 1 bb_edges)) statement_edge
        -- CFG edge between BB's
        bb_edge :: [(Location, Location)]
        bb_edge = map (\(bb_from, bb_to) -> ((bb_from, Right . pred . length . (mir!!) $ bb_from), (bb_to, Left 0))) bb_edges
        -- Combine
        edges = concat statement_edge ++ concat bb_statement_edge ++ bb_edge
        nodes = nub ((fmap fst edges) ++ (fmap snd edges))


location_cfg_graph :: MirStatements -> BaseCFG -> Graphviz
location_cfg_graph mir cfg 
    = digraph "CFG"
        $ (++) (map location_node nodes)
        $ map (uncurry (edge_location mir)) edges
    where 
        (nodes, edges) = location_cfg mir cfg

-- Computes OCLA facts, creates subgraph at each node for OCLA DAG
origin_relations_graph :: MirStatements -> Facts -> BaseCFG -> Graphviz
origin_relations_graph mir facts cfg
    = digraph "CFG"
        $ (++) (map (\n -> label_location_node n (show $ borrow_relations facts n))  nodes)
        $ map (uncurry (edge_location mir)) edges
    where 
        (nodes, edges) = location_cfg mir cfg



-- Graphviz helpers
type Graphviz = [String]

digraph :: String -> Graphviz -> Graphviz
digraph name = (["digraph " ++ name ++ " {"] ++) . (++ ["}"])

subgraph :: String -> Graphviz -> Graphviz
subgraph name = (["subgraph " ++ name ++ " {"] ++) . (++ ["}"])

simple_node :: Int -> String
simple_node s = (show s) ++ "[style=filled, fillcolor=lightgrey, label=" ++ (show s) ++ "]"

simple_edge :: (Int, Int) -> String
simple_edge (m, n) = (show m) ++ " -> " ++ (show n)


location_node :: Location -> String
location_node l = (label_location l) ++ "[style=filled, shape=rectangle, fillcolor=lightgrey, label=\"" ++ (display_location l) ++ "\"]"

label_location_node :: Location -> String -> String
label_location_node l s = (label_location l) ++ "[style=filled, shape=rectangle, fillcolor=lightgrey, label=\"" ++ (display_location l) ++ "\\n" ++ (escape_str s) ++ "\"]"



display_location :: Location -> String
display_location (bb, Left(n)) = "Start(bb" ++ (show bb) ++ "[" ++ (show n) ++ "])"
display_location (bb, Right(n)) = "Mid(bb" ++ (show bb) ++ "[" ++ (show n) ++ "])"

label_location :: Location -> String
label_location (bb, Left(n)) = "start_bb" ++ (show bb) ++ "_" ++ (show n)
label_location (bb, Right(n)) = "mid_bb" ++ (show bb) ++ "_" ++ (show n)

edge_location :: MirStatements -> Location -> Location -> String
edge_location mir l0@(_, Left(_))  l1
    = (label_location l0) ++ " -> " ++ (label_location l1) ++ "[label=\"" ++ (escape_str . mir_at mir $ l0) ++ "\"]" 
edge_location mir l0 l1 = (label_location l0) ++ " -> " ++ (label_location l1)

mir_at :: MirStatements -> Location -> String
mir_at mir (bb, Left(ix)) = (mir!!bb)!!ix
mir_at _ _ = undefined

record_node :: Int -> [String] -> String
record_node s dat = (show s) ++ "[shape=record, label=\"{" ++ (intercalate " | " . map escape_str $ dat) ++  "}\"]" 

location_record_node :: Location -> [String] -> String
location_record_node l dat = (label_location l) ++ "[shape=record, label=\"{" ++ (intercalate " | " . map escape_str $ dat) ++  "}\"]" 


escape_str [] = []
escape_str (d:ds)
    | d `elem` to_escape = '\\':d:(escape_str ds)
    | otherwise = d:(escape_str ds)
    where to_escape = "-<>{}\"" :: String




-- induced_subset_graph_strict :: (Eq a, Ord a) => [[a]] -> Digraph [a]
-- induced_subset_graph_strict = generate . nubBy seteq . map nub
--     where 
--         generate :: (Eq a, Ord a) => [[a]] -> Digraph [a]
--         generate [] = []
--         generate xss = (SDigraph content children):(generate nocomp)
--             where 
--                 current_root = longest xss 
--                 (subsets, nocomp) = partition (flip subset current_root) . delete current_root $ xss
--                 content = current_root \\ (foldl union [] subsets)
--                 children = generate subsets 
--          
--         longest xss = snd . maximum $ [(length xs, xs) | xs <- xss]
--         subset xs ys = foldl (flip $ (&&) . flip elem ys) True xs
--         seteq xs ys = null (xs \\ ys) && null (ys \\ xs)
