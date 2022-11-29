module Misc where

-- taken from assignment 6, did not ultimately use
type CFG nt t = ([nt], [t], [nt], [RewriteRule nt t])
--taken from assignment 6
type GenericCFG nt t  = ([nt], [t], nt, [RewriteRule nt t])

--taken from project ideas
type Configuration a b = ([a], [b])
--taken from project ideas
data StackSymbol nt = Plain nt | Barred nt deriving (Eq,Show)
--taken from assignment 6, with some modification to include D and N from assignment 7
data Cat = S | NP | VP | PP | V | P | D | N deriving (Show, Eq, Ord)

-- from assignment 6, defines a type of rule
data RewriteRule nt t = NTRule nt (nt, nt) | TRule nt t deriving (Show, Eq)


--completely based off of bottom CFG, with production rules for nonterminals copied exactly
--and terminals written with their Cat label and string/word
sample :: GenericCFG Cat String 
sample = ([D, NP, N, S, VP, V], ["the", "alligator", "ate", "the", "zebra"], S, 
                        [(NTRule VP (V,NP)), (NTRule S (NP, VP)), (NTRule NP (NP,PP)), (NTRule PP (P,NP)),
                         (NTRule VP (VP,PP)), (NTRule NP (D, N)), (TRule V "ate"), (TRule N "alligator"), (TRule N "zebra"), 
                         (TRule D "the")])

-- Copied from Week 6 Handout, with some modifications
week6CFG :: GenericCFG Cat String 
            -- Nonterminals, alphabet/terminals, initial nonterminals, production rules
week6CFG = ([VP,NP,PP,V,P], ["watches","spies","telescopes","with", "he"], VP, 
                        [(NTRule VP (V,NP)), (NTRule NP (NP,PP)), (NTRule PP (P,NP)),
                         (NTRule VP (VP,PP)),  (TRule NP "telescopes"),
                         (TRule VP "watches"), (TRule NP "watches"), (TRule P "with"), 
                         (TRule VP "spies"),   (TRule NP "spies"), (TRule V "watches"), 
                         (TRule NP "he"), (NTRule VP (VP, NP)), (NTRule S (NP, VP))])


-- starting configurations for each parser
leftCornerConfig :: Configuration (StackSymbol Cat) String
leftCornerConfig = ([Barred S], ["he", "watches","spies","with", "telescopes"])
--passes a plain S when it should be barred
badLCConfig :: Configuration (StackSymbol Cat) String
badLCConfig = ([Plain S], ["he", "watches","spies","with", "telescopes"])

bottomUpConfig :: Configuration Cat String
bottomUpConfig = ([], ["he", "watches", "spies", "with", "telescopes"])

topDownConfig :: Configuration Cat String
topDownConfig = ([S],["he", "watches", "spies", "with", "telescopes"])

-- sample cfg configurations

sampleBottomUp :: Configuration Cat String 
sampleBottomUp = ([], ["the", "alligator", "ate", "the", "zebra"])

sampleTopDown :: Configuration Cat String
sampleTopDown = ([S], ["the", "alligator", "ate", "the", "zebra"])
--starting config for top corner should be S barred, not plain
badLCConfig2 :: Configuration (StackSymbol Cat) String
badLCConfig2 = ([Plain S], ["the", "alligator", "ate", "the", "zebra"])




-- some filter functions to prevent ill-formed configurations from being processed


--data Maybe a = Nothing | Just a
--takes in a list of configurations of type maybe and return just the configurations 
filterMaybe :: [Maybe (Configuration nt t)] -> [Configuration nt t]
filterMaybe list = case list of 
    [] -> []
    --retrieves first config from list of configurations
    x:rest -> case x of 
        Nothing -> filterMaybe rest
        --returns a list of all the configurations while removing the 'Just' type from the Maybe definition
        Just a -> [a] ++ (filterMaybe rest)

--takes in an int and filters all configurations whose symbols list is greater than three times the length of the string
filterExcess :: Int -> [Configuration nt t] -> [Configuration nt t]
filterExcess bd configs = case configs of 
    [] -> []
    --isolates first configuration from list of configurations, and includes that configuration in 
    --the returned list if the number of symbols in the config is less than three times the length of the
    --string in that config
    x:rest -> case x of 
        (s, str) -> if length(s) <= bd then x:(filterExcess bd rest) else (filterExcess bd rest)

-- some functions to validate a step across different parsers

-- used in bottomUpParser to match word in config against every rule in CFG
isValidShift :: (Eq nt, Eq t) =>  RewriteRule nt t -> t -> Bool 
isValidShift rule symbol = case rule of 
    TRule a b -> if b == symbol then True else False
    NTRule c d -> False

isValidlcShift :: (Eq nt, Eq t) =>  RewriteRule nt t -> t -> Bool 
isValidlcShift rule symbol = case rule of 
    TRule a b -> if b == symbol then True else False
    NTRule c d -> False 

isValidReduce :: (Eq nt, Eq t) => RewriteRule nt t -> [nt] -> Bool 
isValidReduce rule s = case rule of 
    --returns False as reduce doesn't operate on terminals
    TRule a b -> False
    NTRule nt0 (nt1, nt2) -> case s of 
        [] -> False
        -- comparing first and last element from RHS of CFG rule with nonterminals from reduce
        e -> if (nt1 == (head s)) && (nt2 == (last s)) then True else False

-- left corner helper, operates on StackSymbols rather than regular nonterminals
plainSymbols :: [StackSymbol nt] -> [nt]
plainSymbols s = case s of 
            [] -> []
            (Plain sym):rest -> sym:(plainSymbols rest)
            (Barred sym):rest -> sym:(plainSymbols rest) 