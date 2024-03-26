# ebok
Simple accounting helper.

## Persistence

Accounting data is kept as text files in a directory. The 'l' command
loads the highest-named file. When saving after updates, a new
higher-named file is written.

Reverting to a previous saved state is easy: Just remove higher-named
files as appropriate.

## Commands

Upon start the program enters an interactive loop. Commands are:

### l - load

Load accounting data.

### y YEAR       - set year

Set the year to be in focus. The year is shown in the prompt.
By default the current year is assumed.

### S            - summary

Print a summary for the year in focus.

### B            - print book

Print details for the year in focus.

### c MONTH DAY AMOUNT COMMENT... - book cost

Book a cost (the amount shall include 25% VAT).

### t MONTH DAY AMOUNT COMMENT... - book cost

Book a cost (the amount shall include 6% VAT, e.g. travel cost).

### e MONTH DAY AMOUNT COMMENT... - book earning

Book an earning. Use the invoice date, don't use the date when
payment happens. Specify the amount including VAT.

### a MONTH DAY SIGNED_AMOUNT COMMENT... - book accrual

The invoice for December Y1 work is split into a net earning and a
VAT part. The net earning shall be booked on the Y1 year,
and the VAT part shall be booked on the Y2 year.

To achieve this, book the earning and a negative accrual in year Y2,
and a positive accrual in Y1. The accrual shall amount to the net
earning of the invoice, thus the net earning gets "pushed backwards
in time" from Y2 to Y1.

### s - save

Save accounting data to a new file. The new high-named file is the
previous file with new events appended.

### h - help

Show built-in help.
