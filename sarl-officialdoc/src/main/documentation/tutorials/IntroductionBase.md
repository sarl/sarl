# SARL Basic

[:Outline:]

> **_Note:_** If you don't know how to solve an problem, or what is the function to be used, you could search on Internet for the answer using the API of the Java programming language. Indeed, since SARL is fully compatible with the Java API, you could use all the types or functions that are defined in this Java API.

## Exercise 1

Write a SARL program to print the following string in a specific format (see the output).

* Sample String: `Twinkle, twinkle, little star, How I wonder what you are! Up above the world so high, Like a diamond in the sky. Twinkle, twinkle, little star, How I wonder what you are`
* Output:

```text
Twinkle, twinkle, little star,
	How I wonder what you are! 
		Up above the world so high,   		
		Like a diamond in the sky. 
Twinkle, twinkle, little star, 
	How I wonder what you are
```

> [Solution](IntroductionBaseAnswers.md#exercise-1)

## Exercise 2

Write a SARL program to display the current date and time.

* Sample Output :

```
Current date and time:
2014-07-05 14:34:14
```

> [Solution](IntroductionBaseAnswers.md#exercise-2)

## Exercise 3

Write a SARL program that calculates the area of a circle based on the radius entered by the user from the command line when your are launching the agent.

* Sample Output:

```
r = 1.1
Area = 3.8013271108436504
```

> [Solution](IntroductionBaseAnswers.md#exercise-3)

## Exercise 4

Write a SARL program that accepts the user's first and last name as inputs from the command line, and prints them in reverse order with a space between them.

> [Solution](IntroductionBaseAnswers.md#exercise-4)


## Exercise 5

Write a SARL program that accepts a sequence of comma-separated numbers from the command line and generates a list of those numbers.

* Sample data: `3, 5, 7, 23`
* Output:

```
List : ['3', ' 5', ' 7', ' 23']
```

> [Solution](IntroductionBaseAnswers.md#exercise-5)


## Exercise 6

Write a SARL program that accepts a filename from the command line, and prints the extension of the file.

* Sample filename: `abc.sarl`
* Output: `sarl`

> [Solution](IntroductionBaseAnswers.md#exercise-6)


## Exercise 7

Write a SARL program to display the first and last colors from the following list.

```
color_list = ["Red","Green","White" ,"Black"]
```

> [Solution](IntroductionBaseAnswers.md#exercise-7)


## Exercise 8

Write a SARL program that accepts an integer (n) on the command line and computes the value of n+nn+nnn.

* Sample value of n is 5
* Expected Result : 615

> [Solution](IntroductionBaseAnswers.md#exercise-8)


## Exercise 9

Write a SARL program that prints the calendar for a given month and year.

* Note: Use 'GregorianCalendar' java type.

> [Solution](IntroductionBaseAnswers.md#exercise-9)


## Exercise 10

Write a SARL program to calculate the number of days between two dates.

* Sample dates: `(2014, 7, 2)`, `(2014, 7, 11)`
* Expected output : 9 days

> [Solution](IntroductionBaseAnswers.md#exercise-10)


## Exercise 11

Write a SARL program to get the volume of a sphere with radius six.

> [Solution](IntroductionBaseAnswers.md#exercise-11)


## Exercise 12

Write a SARL program to calculate the difference between a given number and 17. If the number is greater than 17, return twice the absolute difference.

> [Solution](IntroductionBaseAnswers.md#exercise-12)


## Exercise 13

Write a SARL program to test whether a number is within 100 of 1000 or 2000.

> [Solution](IntroductionBaseAnswers.md#exercise-13)


## Exercise 14

Write a SARL program to calculate the sum of three given numbers, that are provided on the command line. If the values are equal, return three times their sum.

> [Solution](IntroductionBaseAnswers.md#exercise-14)


## Exercise 15

Write a SARL program to get a newly-generated string from a given string where `Is` has been added to the front. Return the string unchanged if the given string already begins with `Is`.

> [Solution](IntroductionBaseAnswers.md#exercise-15)


## Exercise 16

Write a SARL program that returns a string that is n (non-negative integer) copies of a given string (from the command line).

> [Solution](IntroductionBaseAnswers.md#exercise-16)


## Exercise 17

Write a SARL program that determines whether a given number (accepted from the command line) is even or odd, and prints an appropriate message to the user.

> [Solution](IntroductionBaseAnswers.md#exercise-17)


## Exercise 18

Write a SARL program to count the number 4 in a given list, provided on the command line.

> [Solution](IntroductionBaseAnswers.md#exercise-18)


## Exercise 19

Write a SARL program to get n (non-negative integer) copies of the first 2 characters of a given string (from the command line). Return n copies of the whole string if the length is less than 2.

> [Solution](IntroductionBaseAnswers.md#exercise-19)


## Exercise 20

Write a SARL program to test whether a passed letter from the command line is a vowel or not.

> [Solution](IntroductionBaseAnswers.md#exercise-20)


## Exercise 21

Write a SARL program that checks whether a specified value is contained within a group of values.

* Test Data:

```
3 -> [1, 5, 8, 3] : True
-1 -> [1, 5, 8, 3] : False
```

> [Solution](IntroductionBaseAnswers.md#exercise-21)


## Exercise 22

Write a SARL program that concatenates all elements in a list into a string and returns it.

> [Solution](IntroductionBaseAnswers.md#exercise-22)


## Exercise 23

Write a SARL program to print all even numbers from a given list of numbers in the same order and stop printing any after 237 in the sequence.

* Sample numbers list:

```
numbers = [    
    386, 462, 47, 418, 907, 344, 236, 375, 823, 566, 597, 978, 328, 615, 953, 345, 
    399, 162, 758, 219, 918, 237, 412, 566, 826, 248, 866, 950, 626, 949, 687, 217, 
    815, 67, 104, 58, 512, 24, 892, 894, 767, 553, 81, 379, 843, 831, 445, 742, 717, 
    958,743, 527
    ]
```

> [Solution](IntroductionBaseAnswers.md#exercise-23)


## Exercise 24

Write a SARL program that prints out all colors from `color_list_1` that are not present in `color_list_2`.

* Test Data:

```
color_list_1 = set(["White", "Black", "Red"])
color_list_2 = set(["Red", "Green"])
```

* Expected Output:

```
{'Black', 'White'}
```

> [Solution](IntroductionBaseAnswers.md#exercise-24)


## Exercise 25

Write a SARL program that will accept the base and height of a triangle and compute its area.

> [Solution](IntroductionBaseAnswers.md#exercise-25)


## Exercise 26

Write a SARL program that computes the greatest common divisor (GCD) of two positive integers.

> [Solution](IntroductionBaseAnswers.md#exercise-26)


## Exercise 27

Write a SARL program to find the least common multiple (LCM) of two positive integers.

> [Solution](IntroductionBaseAnswers.md#exercise-27)


## Exercise 28

Write a SARL program to sum three given integers. However, if two values are equal, the sum will be zero.

> [Solution](IntroductionBaseAnswers.md#exercise-28)


## Exercise 29

Write a SARL program to sum two given integers. However, if the sum is between 15 and 20 it will return 20.

> [Solution](IntroductionBaseAnswers.md#exercise-29)


## Exercise 30

Write a SARL program that returns `true` if the two given integer values are equal or their sum or difference is 5.

> [Solution](IntroductionBaseAnswers.md#exercise-30)


## Exercise 31

Write a SARL program to add two objects if both objects are integers.

> [Solution](IntroductionBaseAnswers.md#exercise-31)


## Exercise 32

Write a SARL program to solve `(x + y) * (x + y)`.

* Test Data: `x = 4, y = 3`
* Expected Output: `(4 + 3) ^ 2 = 49`

> [Solution](IntroductionBaseAnswers.md#exercise-32)


## Exercise 33

Write a SARL program to calculate the distance between the points (x1, y1) and (x2, y2).

> [Solution](IntroductionBaseAnswers.md#exercise-33)


## Exercise 34

Write a SARL program to check whether a file exists, when the name of the file is provided from the command line. Use the Java API.

> [Solution](IntroductionBaseAnswers.md#exercise-34)


## Exercise 35

Write a SARL program to parse a string to float or integer.

> [Solution](IntroductionBaseAnswers.md#exercise-35)


## Exercise 36

Write a SARL program to list all files in a directory.

> [Solution](IntroductionBaseAnswers.md#exercise-36)


## Exercise 37

Write a SARL program to print a message provided from the command line without a newline or space.

> [Solution](IntroductionBaseAnswers.md#exercise-37)


## Exercise 38

Write a SARL program to print to STDERR (standard error output of the process).

> [Solution](IntroductionBaseAnswers.md#exercise-38)


## Exercise 39

Write a SARL program to access environment variables.

> [Solution](IntroductionBaseAnswers.md#exercise-39)


## Exercise 40

Write a SARL program to get the current username.

> [Solution](IntroductionBaseAnswers.md#exercise-40)


## Exercise 41

Write a SARL program to sum the first n positive integers.

> [Solution](IntroductionBaseAnswers.md#exercise-41)


## Exercise 42

Write a SARL program to convert height (in feet and inches) to centimeters, when the value is provided from the command line.

> [Solution](IntroductionBaseAnswers.md#exercise-42)


## Exercise 43

Write a SARL program to calculate the hypotenuse of a right angled triangle.

> [Solution](IntroductionBaseAnswers.md#exercise-43)


## Exercise 44

Write a SARL program to convert all units of time into seconds.

> [Solution](IntroductionBaseAnswers.md#exercise-44)


## Exercise 45

Write a SARL program to get an absolute file path from a name that is provided on the command line.

> [Solution](IntroductionBaseAnswers.md#exercise-45)


## Exercise 46

Write a SARL program that converts seconds into days, hours, minutes, and seconds.

> [Solution](IntroductionBaseAnswers.md#exercise-46)


## Exercise 47

Write a SARL program to calculate the body mass index.

> [Solution](IntroductionBaseAnswers.md#exercise-47)


## Exercise 48

Write a SARL program to convert pressure in kilopascals to pounds per square inch, a millimeter of mercury (mmHg) and atmosphere pressure.

> [Solution](IntroductionBaseAnswers.md#exercise-48)


## Exercise 49

Write a SARL program to calculate sum of digits, from a string of characters, of a number.

> [Solution](IntroductionBaseAnswers.md#exercise-49)


## Exercise 50

Write a SARL program to sort three integers without using conditional statements and loops.

> [Solution](IntroductionBaseAnswers.md#exercise-50)


## Exercise 51

Write a SARL program to concatenate N strings.

> [Solution](IntroductionBaseAnswers.md#exercise-51)


## Exercise 52

Write a SARL program to calculate the sum of all items of a container (array, list, set).

> [Solution](IntroductionBaseAnswers.md#exercise-52)


## Exercise 53

Write a SARL program to test whether all numbers in a list, provided on the command line, are greater than a certain number, provided also on command line.

> [Solution](IntroductionBaseAnswers.md#exercise-53)


## Exercise 54

Write a SARL program to count the number of occurrences of a specific character in a string.

> [Solution](IntroductionBaseAnswers.md#exercise-54)


## Exercise 55

Write a SARL program to check whether a file path, provided on the command line, is a file or a directory.

> [Solution](IntroductionBaseAnswers.md#exercise-55)


## Exercise 56

Write a SARL program to swap two variables.

> [Solution](IntroductionBaseAnswers.md#exercise-56)


## Exercise 57

Write a SARL program to check whether a string is numeric.

> [Solution](IntroductionBaseAnswers.md#exercise-57)


## Exercise 58

Write a SARL program to prove that two string variables of the same value point to the same memory location.

> [Solution](IntroductionBaseAnswers.md#exercise-58)


## Exercise 59

Write a SARL program to determine the largest and smallest integers, longs, and floats.

> [Solution](IntroductionBaseAnswers.md#exercise-59)



[:Include:](../legal.inc)
