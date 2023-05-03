# CFG Parsers in Haskell

This repository contains three context-free grammar (CFG) parsers implemented in Haskell: Top-Down, Bottom-Up, and Left-Corner. These parsers employ recursive algorithms to process sentences based on a given CFG.

## Parsers.hs

### Shared Functions
- `maxSize` and `maxHelper`: Compute the maximum stack size across all configurations in ambiguous cases.
- `getLHS`: Returns the left-hand side (LHS) of a production rule.
- `rules`: Retrieves all rules from a CFG and stores them in a list.

### Top-Down Parser
Top-Down parsing uses two operations: `match` and `predict`. `match` checks if the first symbol in the stack and the first word in the buffer match a terminal rule's LHS and RHS, respectively. `predict` replaces the first symbol in the stack with a nonterminal rule's RHS if they match. It keeps predicting until a match occurs or the stack is empty.

### Bottom-Up Parser
Bottom-Up parsing uses `shift` and `reduce`. `shift` moves a terminal symbol from the input buffer to the stack. If a sequence on the stack's top matches the RHS of any production, `reduce` replaces it with the corresponding LHS.

### Left-Corner Parser
Left-Corner parsing uses `lcMatch`, `lcShift`, `lcPredict`, and `lcConnect`. It's a hybrid parser that combines top-down prediction with bottom-up filtering. `lcMatch` and `lcShift` are similar to their counterparts in the top-down and bottom-up parsers. `lcPredict` predicts the next production, and `lcConnect` connects nonterminals to form valid productions.

## Usage
To use these parsers, import the `Parsers` module and provide a CFG and a sentence as input. The parsers will output a list of possible parse trees, or an empty list if the sentence is not accepted by the CFG.
