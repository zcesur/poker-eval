# poker-eval

This package is my solution to [Problem 54 - Project Euler](https://projecteuler.net/problem=54):

> The file, [poker.txt](https://projecteuler.net/project/resources/p054_poker.txt), contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.
>
> How many hands does Player 1 win?

## Prerequisites

- [stack](https://docs.haskellstack.org)

## Installation

```bash
git clone https://github.com/zcesur/poker-eval.git && \
cd poker-eval && \
stack build poker-eval
```

## Usage

As a standalone executable:

```bash
stack exec poker-eval-exe < poker.txt
```

```bash
stack exec poker-eval-exe << EOF
8C TS KC 9H 4S 7D 2S 5D 3S AC
5C AD 5D AC 9C 7C 5H 8D TD KS
3H 7H 6S KC JS QH TD JC 2D 8S
TH 8H 5C QS TC 9H 4D JC KS JS
7C 5H KC QH JD AS KH 4C AD 4S
EOF
```

As a library or in a GHCi session:

```console
foo@bar:~/poker-eval$ stack ghci

λ> eval <$> parse hand "" "8C QS QH 8S QD"
Right (FullHouse Queen Eight)

λ> eval <$> parse hand "" "TD KD QD 9D JD"
Right (StraightFlush King)

λ> uncurry compare <$> parse hands "" "8C QS QH 8S QD TD KD QD 9D JD"
Right LT
```
