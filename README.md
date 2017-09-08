# pilya

Currently only the tokenizer is implemented.

## Usage

```
$ git clone https://github.com/yamnikov-oleg/pilya && cd pilya
$ stack build
$ stack exec pilya -- example.pilya
```

## Example of syntax

```
dim n !
read(n)

dim i %
for i as 0 to 10 do [
    n as (n * 2.0) - 1.0
]

write(n)
```

`%` means integer type, `!`  means real type, `$` means boolean type.
I know this is a bad syntax, but I didn't choose it.
