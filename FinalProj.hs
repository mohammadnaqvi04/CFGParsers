module FinalProject where


import Misc

--function that takes in a list of lists of configurations and returns the size of the configuration with the maximum size
--interacts directly with the output given by the parsers which can return a list of lists of configurations depending on the ambiguity of the sentence
maxSize :: [[Configuration nt t]] -> Int
maxSize c = let helper c = case c of 
                                    [] -> [0]
                                    --calls maxHelper on first list of configs in list of lists of configurations and recursively calls helper on rest of list of lists, which in a vanilla case would be empty
                                    --constructs a list of the maximum configuration stack size in each list of configurations across entire list of lists of configurations (again, only matters in ambiguous cases)

                                    --ghci> 6:[7]
                                    --[6,7]
                                    x:rest -> (maxHelper x):(helper rest) in
    --returns the maximum configuration stack size in entire list of lists of configurations
    maximum (helper c)

--helper to maxSize which takes in a list of configurations and returns the maximum stack size out of all configurations
maxHelper:: [Configuration nt t] -> Int
maxHelper c = let helper c = case c of                                                         
                                [] -> [0]
                                --isolates first configuration which is 'x'
                                x:rest -> case x of 
                                    -- ghci> [6] ++ [7]
                                    -- [6,7]

                                    -- retrieves the stack size (# of symbols) of each configuration in the list and 
                                    -- adds it as an element into a new list

                                    -- list of configurations looks like [([symbols, string]), ([symbols, string]), ...]
                                     (symbols, str) -> [length(symbols)] ++ helper(rest) in 
    --returns the maximum configuration stack size in constructed list
    maximum (helper c)

-- a rule that returns the LHS of a production rule
getLHS :: RewriteRule nt t -> nt
getLHS rule = case rule of
    -- if the rule operates on a terminal as described in the CFG, return the string associated with it
    TRule x y -> x
    -- if the rule operates on a tuple of nonterminals as described in the CFG, return the nonterminal associated with it
    NTRule lhs (a, b) -> lhs


--retrieves all rules from a CFG and stores them in a list
rules :: GenericCFG nt t -> [RewriteRule nt t]
rules cfg = let (syms, terms, start, r) = cfg in r 



------------------------------------------------------------- Top-Down Parser -------------------------------------------------------------

--if there exists a terminal rule in the CFG where the first element of the 
--stack matches the LHS and the first element in the buffer matches the 
--RHS, then its a match, return (rest', rest), the rest of both lists as a config
match :: (Eq nt, Eq t) => RewriteRule nt t -> Configuration nt t -> Maybe (Configuration nt t)
match rule c = let (symbols, str) = c in case str of 
    [] -> Nothing
    x:rest -> case symbols of 
        [] -> Nothing
        x':rest' -> case rule of 
            NTRule nt0 (nt1, nt2) -> Nothing
            --if terminal and nonterminal match first elements in string and symbols,
            --then they match and cancel out so just return the rest of both lists
            TRule nt t -> if x == t && x' == nt then Just (rest', rest) else Nothing

--if there exists a nonterminal rule in the CFG where the LHS of the rule matches the 
--first element in the stack, then replace that element in the stack with the RHS of the rule
predict :: (Eq nt, Eq t) => RewriteRule nt t -> Configuration nt t -> Maybe (Configuration nt t)
predict rule c = let (symbols, str) = c in case symbols of 
    [] -> Nothing
    x:rest -> case rule of
        TRule a b -> Nothing
        --predict doesn't change string at all, if the first nonterminal in symbols
        --matches with the LHS of an NT rule in the CFG, then replace that first
        --nonterminal with both nonterminals from the RHS of the NT rule 
        NTRule nt0 (nt1, nt2) -> if nt0 == x then Just (nt1:nt2:rest, str) else Nothing 


topDownParser :: (Eq nt, Eq t) => [RewriteRule nt t] -> Configuration nt t -> [[Configuration nt t]]
topDownParser rules cn = let (symbols, str) = cn in 
    let bd = length str * 3 in case cn of
        --goal config
        ([], []) -> [[cn]]
        ([], b) -> []
        --concatenate all 
        (a, b) -> let lst = filterMaybe(map (\r -> match r cn) rules) ++ 
                                      filterMaybe(map (\r -> predict r cn) rules) in 

                                        --when predict returns the wrong config because it used the wrong rule, the resulting config,
                                        --when passed recursively to the parser, will return an empty list, as eventually,
                                        --the parser will call predict on a NT (like PP) and the symbols will consist of two NT's.
                                        --Then, when it then calls match on the first symbol in the symbols list, it can't operate on 
                                        --the config as its string will be empty, which will return a list of Nothings which, after being filtered by 
                                        --filterMaybe, will become an empty list and will return an empty list by pattern matching the second 
                                        --case under clst.
                                        
                                        --Thus an empty list would be returned by the first pattern match 
                                        --for clst for the path resulting from the inital faulty predict step.

                                        --clst filters out any configs where the number of symbols is greater than three times
                                        --the length of that config's string. This is to prevent infinite predict steps in a case
                                        --like ([VP,PP,PP],["ate","the","zebra"]) where VP will keep on getting replaced by
                                        --VP PP
                                        let clst = filterExcess bd lst in case clst of
                                            [] -> []
                                            --concat will then combine the empty list and the path of the valid config as 
                                            --if the faulty step never occurred
                                            e -> let paths = concat (map(\c -> topDownParser rules c) clst) in 
                                                --attaches sequence of configs in paths to first config, in vanilla there is only one path
                                                --reverse constructs list using prepend operator
                                                    map(\c -> cn:c) paths



------------------------------------------------------------- Bottom-Up Parser -------------------------------------------------------------

--returns the next configuration of type maybe after shift operation
shift :: (Eq nt, Eq t) => RewriteRule nt t-> Configuration nt t -> Maybe (Configuration nt t)
shift rule c = let (symbols, str) = c in case str of
    --checking RHS of config to see if its empty or contains words
    [] -> Nothing
    --if the first word of the string in the config can be operated on with a valid shift operation,
    --then isolate that element and return a tuple of the updated config where the symbols have been updated
    --and the rest of the string excluding the processed word is recursed on

    --                      [getLHS rule] returns the appropriate phrase of the word/word combination from the CFG rules
    x:rest -> if (isValidShift rule x) then Just ((symbols ++ [getLHS rule]), rest) else Nothing

--returns the next configuration of type maybe after reduce operation
reduce ::(Eq nt, Eq t) => RewriteRule nt t-> Configuration nt t -> Maybe (Configuration nt t)
reduce rule c = let (symbols, str) = c in case symbols of 
    [] -> Nothing
    x:[] -> Nothing
    --dropping everything but the last two symbols in the symbols list, then
    --calling isValidReduce on those last two symbols
    x:rest -> let i = (length symbols) - 2 in if (isValidReduce rule (drop i symbols)) then
        --if there exists a valid reduction of the two nonterminals in the CFG rules,
        --then isolate those two nonterminals, get the LHS of their rule, and replace the
        --two nonterminals with the singular nonterminal

           --load keeps everything but the last two symbols which are reduced into one and added back
        let load = take ((length symbols) - 2) symbols in Just (load ++ [getLHS rule], str) else Nothing


--saves config passed in then calls parser on config
bottomUpParser ::  (Eq nt, Eq t) => [RewriteRule nt t] -> Configuration nt t -> [[Configuration nt t]]
bottomUpParser rules cn = let (symbols, str) = cn in case cn of
    --checking the form of the configurations and pattern matching according to form

    --this is just when there is ONLY one NT
    ([nt], []) -> [[([nt], [])]]
    ([], []) -> []

    --processes config based on this: you know to use reduce when you check stack and
    --find a two symbol sequeunce that can be reduced to one NT based on CFG rule. Otherwise, you
    --continue to use shift to process terminals

    --The filterMaybe (map) combo for both shift and reduce filters out all configs of type Nothing.
    --Configurations are of type Nothing as when each rule in the CFG was passed with the configuration to shift or reduce, 
    --no rule in the CFG matched the current terminal(shift) or two nonterminals(reduce) except for one which 
    --returned the next config in the parsing schema (not of type Nothing)

    --the LHS of that rule will be used to update the stack

    --clist stores the next configuration
    --if it shifts when it's not supposed to shift, then it will be handled at concat
    (a, b) -> let clst = (filterMaybe (map (\r -> (shift r cn)) rules)) ++ (filterMaybe (map (\r -> (reduce r cn)) rules)) in case clst of
        [] -> []
        --recursively calls the parser on the next config stored in clst
        --paths diverge if clst ever > 1, otherwise there is only one path


        --when shift is called on a config that needs to be reduced, the resulting config,
        --when passed recursively to the parser, will return an empty list, as eventually,
        --the parser will be called on ([D,N,V,D,N],[]) and neither shift or reduce
        --can run on it, therefore all shifts and reduces will creates lists of Nothings
        --which, when filtered with filterMaybe, will both be empty. Thus an empty list would be returned
        --by the first pattern match for clst for the path resulting from the inital faulty shift step.
        
        --concat will then combine the empty list and the path of the valid reduced config as if the faulty shift step
        --never occurred
        e ->  let paths = concat (map (\c -> bottomUpParser rules c) clst) in
                --attaches sequence of configs in paths to first config, in vanilla there is only one path
                --reverse constructs list using prepend operator
                map(\c -> cn:c) paths



------------------------------------------------------------- Left-Corner Parser -------------------------------------------------------------


lcMatch :: (Eq nt, Eq t) => RewriteRule nt t -> Configuration (StackSymbol nt) t -> Maybe (Configuration (StackSymbol nt) t)
lcMatch rule c = let (symbols, str) = c in case str of 
    [] -> Nothing
    x:rest -> case symbols of 
        [] -> Nothing
        x':rest' -> case x' of 
            Plain s -> Nothing 
            Barred s -> case rule of 
                NTRule nt0 (nt1, nt2) -> Nothing
                --if the first symbol in symbols is barred and matches the current rule's LHS 
                --and the first word in the string matches the rule's RHS, cancel them out
                --and return the rest of both lists
                TRule nt t -> if x == t && s == nt then Just (rest', rest) else Nothing

lcShift :: (Eq nt, Eq t) => RewriteRule nt t -> Configuration (StackSymbol nt) t -> Maybe (Configuration (StackSymbol nt) t)
lcShift rule c = let (symbols, str) = c in case str of
    [] -> Nothing
    --retrieve the first word in string, check if the current rule is valid on that word,
    --then attach the LHS of the rule that returns true to the beginning of the symbols
    --list

    x:rest -> if (isValidShift rule x) then Just (([Plain (getLHS rule)] ++ symbols), rest) else Nothing


--lcPredict and lcConnect only operate on symbols rather than strings
lcPredict :: (Eq nt, Eq t) => RewriteRule nt t -> Configuration (StackSymbol nt) t -> Maybe (Configuration (StackSymbol nt) t)
lcPredict rule c = let (symbols, str) = c in case symbols of 
    [] -> Nothing
    --retrieves first symbol from symbols list
    x:rest -> case x of 
        Barred s -> Nothing
        Plain s -> case rule of 
                        --checks if rule is a nonterminal rule or a terminal rule
                        TRule a b -> Nothing
                        --if the first symbol in the symbols is equal to the first symbol on the RHS of the rule,
                        --return the rest of the RHS as barred, the LHS, and the rest of the symbols
                        NTRule nt0 (nt1, nt2) -> if s == nt1 then Just ((Barred nt2:Plain nt0:rest), str) else Nothing

lcConnect :: (Eq nt, Eq t) => RewriteRule nt t -> Configuration (StackSymbol nt) t -> Maybe (Configuration (StackSymbol nt) t)
lcConnect rule c = let (symbols, str) = c in case symbols of
    [] -> Nothing
    x:[] -> Nothing
    x:y:rest -> case x of 
        Barred s -> Nothing
        Plain s -> case y of
            Plain s' -> Nothing
            Barred s' -> case rule of 
                TRule a b -> Nothing
                --if the first symbol in the symbols list is equal to the first symbol on the RHS of the rule
                --and the second symbol in the symbols list is equal to the LHS, then attach the second symbol 
                --in the RHS (Barred) to the front of the symbols list
                NTRule nt0 (nt1, nt2) -> if s == nt1 && s' == nt0 then Just ((Barred nt2:rest), str) else Nothing


leftCornerParser :: (Eq nt, Eq t) => [RewriteRule nt t] -> Configuration (StackSymbol nt) t -> [[Configuration (StackSymbol nt) t]]
leftCornerParser rules cn = let (symbols, str) = cn in 
    let bd = length str * 3 in case cn of 
        ([], []) -> [[cn]]
        ([], b) -> []
        (a, b) -> let lst = filterMaybe(map (\r -> lcShift r cn) rules) ++ 
                                    filterMaybe(map (\r -> lcMatch r cn) rules) ++ 
                                    filterMaybe(map (\r -> lcPredict r cn) rules) ++ 
                                    filterMaybe(map(\r -> lcConnect r cn) rules)
                            in 
                    let clst = filterExcess bd lst in case clst of
                        [] -> []
                        e -> let paths = concat (map(\c -> leftCornerParser rules c) clst) in 
                                    map(\c -> cn:c) paths 
