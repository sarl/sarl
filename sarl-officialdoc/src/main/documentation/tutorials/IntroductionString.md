# Introduction to Strings with SARL

[:Outline:]

> **_Note:_** If you don't know how to solve an problem, or what is the function to be used, you could search on Internet for the answer using the API of the Java programming language. Indeed, since SARL is fully compatible with the Java API, you could use all the types or functions that are defined in this Java API.

## Exercise 1

Write a SARL program to calculate the length of a string, providing on the command line.

> [Solution](IntroductionStringAnswers.md#exercise-1)


## Exercise 2

Write a SARL program to count the number of characters (character frequency) in a string.

* Sample String: `google.com`
* Expected Result: `{'g': 2, 'o': 3, 'l': 1, 'e': 1, '.': 1, 'c': 1, 'm': 1}`

> [Solution](IntroductionStringAnswers.md#exercise-2)


## Exercise 3

Write a SARL program to get a string made of the first 2 and last 2 characters of a given string (from the command line). If the string length is less than 2, return the empty string instead.

* Sample String: `w3resource`
* Expected Result: `w3ce`
* Sample String: `w3`
* Expected Result: `w3w3`
* Sample String: ` w`
* Expected Result: Empty String

> [Solution](IntroductionStringAnswers.md#exercise-3)


## Exercise 4

Write a SARL program to get a string from a given string (from the command line) where all occurrences of its first char have been changed to `*`, except the first char itself.

* Sample String: `restart`
* Expected Result: `resta*t`

> [Solution](IntroductionStringAnswers.md#exercise-4)


## Exercise 5

Write a SARL program to get a single string from two given strings (from the command line), separated by a space and swap the first two characters of each string.

* Sample String: `abc`, `xyz`
* Expected Result: `xyc abz`

> [Solution](IntroductionStringAnswers.md#exercise-5)


## Exercise 6

Write a SARL program to add `ing` at the end of a given string (length should be at least 3). If the given string already ends with `ing`, add `ly` instead. If the string length of the given string is less than 3, leave it unchanged.

* Sample String: `abc`
* Expected Result: `abcing`
* Sample String: `string`
* Expected Result: `stringly`

> [Solution](IntroductionStringAnswers.md#exercise-6)


## Exercise 7

Write a SARL program to find the first occurrence of the substrings `not` and `poor` in a given string. If `not` follows `poor`, replace the whole `not`...`poor` substring with `good`. Return the resulting string.

* Sample String: `The lyrics is not that poor!` and `The lyrics is poor!`
* Expected Result: `The lyrics is good!` and `The lyrics is poor!`

> [Solution](IntroductionStringAnswers.md#exercise-7)


## Exercise 8

Write a SARL function that takes a list of words and return the longest word and the length of the longest one.

> [Solution](IntroductionStringAnswers.md#exercise-8)


## Exercise 9

Write a SARL program to remove the nth index character from a nonempty string.

> [Solution](IntroductionStringAnswers.md#exercise-9)


## Exercise 10

Write a SARL program to change a given string to a newly string where the first and last chars have been exchanged.

> [Solution](IntroductionStringAnswers.md#exercise-10)


## Exercise 11

Write a SARL program to remove characters that have odd index values in a given string.

> [Solution](IntroductionStringAnswers.md#exercise-11)


## Exercise 12

Write a SARL program to count the occurrences of each word in a given sentence.

> [Solution](IntroductionStringAnswers.md#exercise-12)


## Exercise 13

Write a SARL script that takes input from the command line and displays that input back in upper and lower cases.

> [Solution](IntroductionStringAnswers.md#exercise-13)


## Exercise 14

Write a SARL program that accepts a comma-separated sequence of words as command line input and prints the distinct words in sorted form (alphanumerically).

* Sample Words: `red, white, black, red, green, black`
* Expected Result: `black`, `green`, `red`, `white`, `red`

> [Solution](IntroductionStringAnswers.md#exercise-14)


## Exercise 15

Write a SARL function to create an HTML string with tags around the word(s).
Sample function and result:

* `['i', 'SARL']` gives `<i>SARL</i>`
* `['b', 'SARL Tutorial']` gives `<b>SARL Tutorial</b>`

> [Solution](IntroductionStringAnswers.md#exercise-15)


## Exercise 16

Write a SARL function to insert a string in the middle of a string.
Sample function and result :

* arguments `('[[]]', 'SARL')` gives `[[SARL]]`
* arguments `('{{}}', 'PHP')` gives `{{PHP}}`

> [Solution](IntroductionStringAnswers.md#exercise-16)


## Exercise 17

Write a SARL function to get a string made of 4 copies of the last two characters of a specified string (length must be at least 2).
Sample function and result :

* `SARL` gives `RLRLRL`
* `Exercises` -> `eseseses`

> [Solution](IntroductionStringAnswers.md#exercise-17)


## Exercise 18

Write a SARL function to get a string made of the first three characters of a specified string. If the length of the string is less than 3, return the original string.
Sample function and result :

* `ipy` gives `ipy`
* `SARL` gives `SAR`

> [Solution](IntroductionStringAnswers.md#exercise-18)


## Exercise 19

Write a SARL function to reverse a string if its length is a multiple of 4.

> [Solution](IntroductionStringAnswers.md#exercise-19)


## Exercise 20

Write a SARL function to convert a given string to all uppercase if it contains at least 2 uppercase characters in the first 4 characters.

> [Solution](IntroductionStringAnswers.md#exercise-20)


## Exercise 21

Write a SARL program to sort a string lexicographically.

> [Solution](IntroductionStringAnswers.md#exercise-21)


## Exercise 22

Write a SARL program to remove a newline in a string.

> [Solution](IntroductionStringAnswers.md#exercise-22)


## Exercise 23

Write a SARL program to check whether a string starts with specified characters.

> [Solution](IntroductionStringAnswers.md#exercise-23)


## Exercise 24

Write a SARL program to create a Caesar encryption.

> **_Note:_** In cryptography, a Caesar cipher, also known as Caesar's cipher, the shift cipher, Caesar's code or Caesar shift, is one of the simplest and most widely known encryption techniques. It is a type of substitution cipher in which each letter in the plaintext is replaced by a letter some fixed number of positions down the alphabet. For example, with a left shift of 3, D would be replaced by A, E would become B, and so on. The method is named after Julius Caesar, who used it in his private correspondence.

> [Solution](IntroductionStringAnswers.md#exercise-24)


## Exercise 25

Write a SARL program to remove existing indentation from all of the lines in a given text.

> [Solution](IntroductionStringAnswers.md#exercise-25)


## Exercise 26

Write a SARL program to add prefix text to all of the lines in a string.

> [Solution](IntroductionStringAnswers.md#exercise-26)


## Exercise 27

Write a SARL program to print the real numbers up to 2 decimal places.

> [Solution](IntroductionStringAnswers.md#exercise-27)


## Exercise 28

Write a SARL program to print the real numbers up to 2 decimal places with a sign.

> [Solution](IntroductionStringAnswers.md#exercise-28)


## Exercise 29

Write a SARL program to print the real positive and negative numbers with no decimal places.

> [Solution](IntroductionStringAnswers.md#exercise-29)


## Exercise 30

Write a SARL program to print the integers with zeros to the left of the specified width.

> [Solution](IntroductionStringAnswers.md#exercise-30)


## Exercise 31

Write a SARL program to print the integers with '*' to the right of the specified width.

> [Solution](IntroductionStringAnswers.md#exercise-31)


## Exercise 32

Write a SARL program to count occurrences of a substring in a string, both provided on the command line.

> [Solution](IntroductionStringAnswers.md#exercise-32)


## Exercise 33

Write a SARL program to reverse a string that is provided on the command line.

> [Solution](IntroductionStringAnswers.md#exercise-33)


## Exercise 34

Write a SARL program to reverse words in a string that is provided on the command line.

> [Solution](IntroductionStringAnswers.md#exercise-34)


## Exercise 35

Write a SARL program to print the square and cube symbols in the area of a rectangle and the volume of a cylinder, using the string formatting tool of the API.

* Input variables:

```text
area = 1256.66
volume = 1254.725
decimals = 2
```

* Sample output:

```text
The area of the rectangle is 1256.66cm2
The volume of the cylinder is 1254.725cm3
```

> [Solution](IntroductionStringAnswers.md#exercise-35)


## Exercise 36

Write a SARL program to print the index of a character in a string.

* Sample string: `w3resource`
* Expected output:

```text
Current character w position at 0
Current character 3 position at 1
Current character r position at 2
....
Current character c position at 8
Current character e position at 9
```

> [Solution](IntroductionStringAnswers.md#exercise-36)


## Exercise 37

Write a SARL program to check whether a string contains all letters of the alphabet.

> [Solution](IntroductionStringAnswers.md#exercise-37)


## Exercise 38

Write a SARL program to convert a given string into a list of words.

* Input: `The quick brown fox jumps over the lazy dog.`
* Sample Output:

```text
['The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog.']
```

> [Solution](IntroductionStringAnswers.md#exercise-38)


## Exercise 39

Write a SARL program to lowercase the first n characters in a string.

> [Solution](IntroductionStringAnswers.md#exercise-39)


## Exercise 40

Write a SARL program to swap commas and dots in a string.

* Sample string: `32.054,23`
* Expected Output: `32,054.23`

> [Solution](IntroductionStringAnswers.md#exercise-40)


## Exercise 41

Write a SARL program to count and display vowels in text.

> [Solution](IntroductionStringAnswers.md#exercise-41)


## Exercise 42

Write a SARL program to split a string on the last occurrence of the delimiter.

> [Solution](IntroductionStringAnswers.md#exercise-42)


## Exercise 43

Write a SARL program to find the first non-repeating character in a given string.

> [Solution](IntroductionStringAnswers.md#exercise-43)


## Exercise 44

Write a SARL program to print all permutations with a given repetition number of characters of a given string.

> [Solution](IntroductionStringAnswers.md#exercise-44)


## Exercise 45

Write a SARL program to find the first repeated character in a given string.

> [Solution](IntroductionStringAnswers.md#exercise-45)


## Exercise 46

Write a SARL program to find the first repeated character in a given string where the index of the first occurrence is smallest.

> [Solution](IntroductionStringAnswers.md#exercise-46)


## Exercise 47

Write a SARL program to find the first repeated word in a given string.

> [Solution](IntroductionStringAnswers.md#exercise-47)


## Exercise 48

Write a SARL program to find the second most repeated word in a given string.

> [Solution](IntroductionStringAnswers.md#exercise-48)


## Exercise 49

Write a SARL program to remove spaces from a given string.

> [Solution](IntroductionStringAnswers.md#exercise-49)


## Exercise 50

Write a SARL program to find all the common characters in lexicographical order from two given lower case strings. If there are no similar letters print `No common characters`.

> [Solution](IntroductionStringAnswers.md#exercise-50)


## Exercise 51

Write a SARL program to make two given strings (lower case, may or may not be of the same length) anagrams without removing any characters from any of the strings.

> [Solution](IntroductionStringAnswers.md#exercise-51)


## Exercise 52

Write a SARL program to remove all consecutive duplicates of a given string.

> [Solution](IntroductionStringAnswers.md#exercise-52)


## Exercise 53

Write a SARL program to find the longest common sub-string from two given strings.

> [Solution](IntroductionStringAnswers.md#exercise-53)


## Exercise 54

Write a SARL program to count Uppercase, Lowercase, special characters and numeric values in a given string.

> [Solution](IntroductionStringAnswers.md#exercise-54)


## Exercise 55

Write a SARL program to wrap a given string into a paragraph with a given width.

* Input a string: `The quick brown fox.`
* Input the width of the paragraph: `10`
* Result:

```text
The quick
brown fox.
```

> [Solution](IntroductionStringAnswers.md#exercise-55)


## Exercise 56

Write a SARL program to swap cases in a given string.

* Input: `SARL eXERiCISES`
* Output: `sarl ExerIcises`

> [Solution](IntroductionStringAnswers.md#exercise-56)


## Exercise 57

Write a SARL program to check whether a given string contains a capital letter, a lower case letter, a number and a minimum length.

> [Solution](IntroductionStringAnswers.md#exercise-57)


## Exercise 58

Write a SARL program to convert a given heterogeneous list of scalars into a string.
Sample Output:

* Original list: `['Red', 100, -50, 'green', 'w,3,r', 12.12, false]`
* Convert the heterogeneous list of scalars into a string: `Red,100,-50,green,w,3,r,12.12,false`

> [Solution](IntroductionStringAnswers.md#exercise-58)


## Exercise 59

Write a SARL program to extract numbers from a given string.

* Original string: `red 12 black 45 green`
* Extract numbers from the said string: `[12, 45]`

> [Solution](IntroductionStringAnswers.md#exercise-59)


## Exercise 60

Write a SARL program to replace each character of a word of length five and more with a hash character (`#`).

* Original string: `Count the lowercase letters in the said list of words:`
* Replace words (length five or more) with hash characters in the said string: `##### the ######### ####### in the said list of ######`

> [Solution](IntroductionStringAnswers.md#exercise-60)



[:Include:](../legal.inc)
