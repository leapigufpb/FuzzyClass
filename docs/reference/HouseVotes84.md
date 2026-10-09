# United States Congressional Voting Records 1984

This data set includes votes for each of the U.S. House of
Representatives Congressmen on the 16 key votes identified by the CQA.
The CQA lists nine different types of votes: voted for, paired for, and
announced for (these three simplified to yea), voted against, paired
against, and announced against (these three simplified to nay), voted
present, voted present to avoid conflict of interest, and did not vote
or otherwise make a position known (these three simplified to an unknown
disposition).

## Usage

``` r
data(HouseVotes84)
```

## Format

A data frame with 435 observations on 17 variables:

|     |                                                 |
|-----|-------------------------------------------------|
| 1   | Class Name: 2 (democrat, republican)            |
| 2   | handicapped-infants: 2 (y,n)                    |
| 3   | water-project-cost-sharing: 2 (y,n)             |
| 4   | adoption-of-the-budget-resolution: 2 (y,n)      |
| 5   | physician-fee-freeze: 2 (y,n)                   |
| 6   | el-salvador-aid: 2 (y,n)                        |
| 7   | religious-groups-in-schools: 2 (y,n)            |
| 8   | anti-satellite-test-ban: 2 (y,n)                |
| 9   | aid-to-nicaraguan-contras: 2 (y,n)              |
| 10  | mx-missile: 2 (y,n)                             |
| 11  | immigration: 2 (y,n)                            |
| 12  | synfuels-corporation-cutback: 2 (y,n)           |
| 13  | education-spending: 2 (y,n)                     |
| 14  | superfund-right-to-sue: 2 (y,n)                 |
| 15  | crime: 2 (y,n)                                  |
| 16  | duty-free-exports: 2 (y,n)                      |
| 17  | export-administration-act-south-africa: 2 (y,n) |

## Source

- Source: Congressional Quarterly Almanac, 98th Congress, 2nd session
  1984, Volume XL: Congressional Quarterly Inc., ington, D.C., 1985

- Donor: Jeff Schlimmer (Jeffrey.Schlimmer@a.gp.cs.cmu.edu)

These data have been taken from the UCI Repository Of Machine Learning
Databases at

- <https://archive.ics.uci.edu/datasets>

- <https://archive.ics.uci.edu/datasets>

and were converted to R format by Friedrich Leisch.

## References

Newman, D.J. & Hettich, S. & Blake, C.L. & Merz, C.J. (1998). UCI
Repository of machine learning databases
\[https://archive.ics.uci.edu/datasets\]. Irvine, CA: University of
California, Department of Information and Computer Science.

## Examples

``` r
data(HouseVotes84)
summary(HouseVotes84)
#>         Class       V1        V2        V3        V4        V5        V6     
#>  democrat  :267   n  :236   n  :192   n  :171   n  :247   n  :208   n  :152  
#>  republican:168   y  :187   y  :195   y  :253   y  :177   y  :212   y  :272  
#>                   NAs: 12   NAs: 48   NAs: 11   NAs: 11   NAs: 15   NAs: 11  
#>    V7        V8        V9       V10       V11       V12       V13     
#>  n  :182   n  :178   n  :206   n  :212   n  :264   n  :233   n  :201  
#>  y  :239   y  :242   y  :207   y  :216   y  :150   y  :171   y  :209  
#>  NAs: 14   NAs: 15   NAs: 22   NAs:  7   NAs: 21   NAs: 31   NAs: 25  
#>   V14       V15       V16     
#>  n  :170   n  :233   n  : 62  
#>  y  :248   y  :174   y  :269  
#>  NAs: 17   NAs: 28   NAs:104  
```
