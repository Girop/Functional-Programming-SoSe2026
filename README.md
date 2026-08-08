# Plain-Text Accounting - Functional Programming 2026

A minimal bookkeeping system written in Haskell.
Based on double-entry ledger file format, 
it supports 5 different queries and is able to present results either in the console
or save them as CSV format.


## Usage

The recommended way to use this tool, is to invoke it through cabal.
```
cabal run ledger -- [tool flags]
```

Available analyses:
- Expenses per month `-eM`
- Expenses per year `-eY`
- Accounts' balance over months `-bM`
- Accounts' balance over years `-bY`
- Accounts' balance now `-bN`


Available filters:
- Restrict to a specific time range `-t date1 date2`, only entries created between date1 and date2 will be considered.
Both dates must be specified in format `dd-mm-yyyy`.


Output formats:
- `--csv` produces results in a csv file.
- `--console` prints results to the console.
By default console is chosen, but chosing a single other format overrides it.
Multiple formats can be chosen in a single run.

## Ledger format

It supports ledger files in the following format:

```
...

02-01-2026 08:45:40         Salary
    Income:Salary           -2035.38
    Assets:Cash              2035.38

...
```

## Usage of AI
I used Anthropic's Claude.ai for code review.
