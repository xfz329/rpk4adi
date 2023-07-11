# rpk4adi

## Project Information

This project is the R-implement version of [pk4adi](https://github.com/xfz329/pk4adi). Please refer the doc [here](https://github.com/xfz329/pk4adi/blob/main/README.md) for more information.

The package's name rpk4adi is short for "R-implement PK for anesthetic depth indicators". The PK (Prediction probability) was first proposed by [Dr. Warren D. Smith](https://www.csus.edu/faculty/s/smithwd/) in the paper [Measuring the Performance of Anesthetic Depth Indicators](https://pubs.asahq.org/anesthesiology/article/84/1/38/35261/Measuring-the-Performance-of-Anesthetic-Depth) in 1996. Dr. Warren D. Smith and his team provide a tool to calculate PK written using the MS Excel macro language.

Our team provide a reimplementation of the PK tools developed using the R language with easy-to-use APIs in this package. The project is fully open source on [github](https://github.com/xfz329/rpk4adi). The latest released version could be found [here](https://github.com/xfz329/rpk4adi/releases). 

A GUI version of pk4adi called pk4adi_gui is also under development. This project is also open source on [github](https://github.com/xfz329/pk4adi_gui).

Please feel free to contact us (silencejiang@zju.edu.cn). Any kind of feedback is welcome. You could report any bugs or issues when using pk4adi on github [project](https://github.com/xfz329/rpk4adi/issues).

## Changelogs

Please refer the [changelog.md](https://github.com/xfz329/rpk4adi/blob/main/CHANGELOG.md) for details.

## Requirements

### Packages

```
data.table >= 1.10
stats
```

## Install

To install rpk4adi, run the following in the command prompt.
```
install.packages('pk4adi')
```

## APIs

1. calculate_pk
```r
calculate_pk <- function(x_in, y_in)

@title Compute the pk value to Measure the Performance of Anesthetic Depth Indicators.

@param x_in a vector, the indicator.
@param y_in a vector, the state.

@return a list containing all the matrices and variables during the calculation.
    The value list$type is "PK", which indicated the list is return-value of the function calculate_pk().
    The type of list$basic is also a list, which contains the most important results of the function.
    The type of list$matrices is also a list, which contains all the matrices during the calculation.
    The type of list$details is also a list, which contains all the intermediate variables during the calculation.
```

2. compare_pks()
```r
compare_pks <- function(pk1, pk2)

@title Compare two answers of the pk values.

@description Both of the two input have to be the output of the function calculate_pk().

@param pk1 a list, the output of the function calculate_pk().
@param pk2 a list, the output of the function calculate_pk().

@return a list containing all the variables during the calculation.
    The value list$type is "PKC", which indicated the list is return-value of the function compare_pk().
    The type of list$group is also a list, which contains the normal distribution test results for the group variables.
    The type of list$pair is also a list, which contains the t distribution test results for the pair variables.
    The type of list$details is also a list, which contains all the intermediate variables during the calculation.
```

## Examples

The best way to use this package is to use R scripts.

### 1. calculate PK

```r
x1 <- c(0, 0, 0, 0, 0, 0)
y1 <- c(1, 1, 1, 1, 1, 2)
ans <- calculate_pk(x1, y1)

## show the most important results.
print(ans$basic)

x2 <- c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6)
y2 <- c(1, 1, 1, 1, 1, 2, 1, 1, 3, 3, 2, 2, 2, 2, 2, 1, 3, 3, 3, 3, 3, 3, 3, 3)
ans2 <- calculate_pk(x2, y2)

## show the full results.
print(ans)
```
You will get the following output.
```r
$PK
[1] 0.5

$SE0
[1] 0

$SE1
[1] 0

$jack_ok
[1] FALSE

$PKj
[1] NaN

$SEj
[1] NaN

$PK
[1] 0.5

$SE0
[1] 0

$SE1
[1] 0

$jack_ok
[1] FALSE

$PKj
[1] NaN

$SEj
[1] NaN

```

### 2. compare results of PK

```r
x1 <- c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6)
y1 <- c(1, 1, 1, 1, 1, 2, 1, 1, 3, 3, 2, 2, 2, 2, 2, 1, 3, 3, 3, 3, 3, 3, 3, 3)

pk1 <- calculate_pk(x_in = x1, y_in = y1)

x2 <- c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6)
y2 <- c(1, 1, 2, 1, 1, 2, 1, 2, 3, 3, 2, 2, 1, 2, 2, 2, 3, 3, 3, 3, 2, 3, 3, 2)

pk2 <- calculate_pk(x_in = x2, y_in = y2)

ans <- compare_pks(pk1, pk2)
print(ans$group)
print(ans$pair)
```
You will get the following output.
```r
$PKD
[1] 0.06757172

$SED
[1] 0.1010385

$ZD
[1] 0.6687717

$ZP
[1] 0.5036411

$ZJ
[1] "P > 0.05"

$DF
[1] 23

$PKDJ
[1] 0.02971846

$SEDJ
[1] 0.06558182

$TD
[1] 0.4531508

$TP
[1] 0.3273431

$TJ
[1] "P > 0.05"
```

### 3. more details
You could get the all the matrices and variables in the returned lists of the function calculate_pk() and compare_pks().
Then just get the value with the key of the lists!

# Development

## Contribute

Please feel free to contact us (silencejiang@zju.edu.cn). Any kind of feedback is welcome and appreciated.
- Check out the wiki for development info (coming soon!).
- Fork us from @xfz329's [main](https://github.com/xfz329/rpk4adi) and star us.
- Report an issue or a bug with data [here](https://github.com/xfz329/rpk4adi/issues).
- Any other free discussion [here](https://github.com/xfz329/rpk4adi/discussions).

## References
1. [Measuring the Performance of Anesthetic Depth Indicators](https://pubs.asahq.org/anesthesiology/article/84/1/38/35261/Measuring-the-Performance-of-Anesthetic-Depth)
2. [A measure of association for assessing prediction accuracy that is a generalization of non-parametric ROC area](https://onlinelibrary.wiley.com/doi/10.1002/(SICI)1097-0258(19960615)15:11%3C1199::AID-SIM218%3E3.0.CO;2-Y)
3. [Excel 4.0 Macro Functions Reference - My Online Training Hub](https://d13ot9o61jdzpp.cloudfront.net/files/Excel%204.0%20Macro%20Functions%20Reference.pdf)
