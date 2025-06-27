# Introduction and exercises to the general SARL syntax

<script>
function exerciseToggle(id) {
    var x = document.getElementById('exercise' + id.toString());
    if (x.style.display === "none") {
        x.style.display = "block";
    } else {
        x.style.display = "none";
    }
}
</script>

> **_Note:_** If you don't know how to solve an problem, or what is the function to be used, you could search on Internet for the answer using the API of the Java programming language. Indeed, since SARL is fully compatible with the Java API, you could use all the types or functions that are defined in this Java API.

## Exercise 1

* Write a SARL program to print the following string in a specific format (see the output).
* Expected output:

<pre><code>
Twinkle, twinkle, little star,
	How I wonder what you are! 
		Up above the world so high,   		
		Like a diamond in the sky. 
Twinkle, twinkle, little star, 
	How I wonder what you are
</code></pre>

<a href="javascript:exerciseToggle(1)">Answer</a>
<div id="exercise1" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			println("Twinkle, twinkle, little star,\n\tHow I wonder what you are!\n\t\tUp above the world so high,\n\t\tLike a diamond in the sky.\n\tTwinkle, twinkle, little star,\ntHow I wonder what you are")
		}
	}
[:End:]
</div>

## Exercise 2

* Write a SARL program to display the current date and time.
* Expected output :

```text
Current date and time:
2014-07-05 14:34:14
```

<a href="javascript:exerciseToggle(2)">Answer</a>
<div id="exercise2" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import java.util.Date
	agent Solution {
		on Initialize {
			var dt = new Date
			println(dt)
		}
	}
[:End:]
</div>

## Exercise 3

* Write a SARL program that calculates the area of a circle based on the radius entered by the user from the command line when your are launching the agent.
* Expected output:

```text
r = 1.1;
Area = 3.8013271108436504
```

<a href="javascript:exerciseToggle(3)">Answer</a>
<div id="exercise3" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var r = occurrence.parameters.get(0) as Double
			var area = Math::PI * r**2
			println("r = " + r)
			println("; Area = " + area)
		}
	}
[:End:]
</div>

## Exercise 4

* Write a SARL program that accepts the user's first and last name as inputs from the command line, and prints them in reverse order with a space between them.

<a href="javascript:exerciseToggle(4)">Answer</a>
<div id="exercise4" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent A {
		on Initialize {
			var firstname = occurrence.parameters.get(0) as String
			var lastname = occurrence.parameters.get(1) as String
			println(lastname + " " + firstname)
		}
	}
[:End:]
</div>

## Exercise 5

* Write a SARL program that accepts a sequence of comma-separated numbers from the command line and generates a list of those numbers.
* Example of input string: `3, 5, 7, 23`
* Expected output:

```text
List : ['3', ' 5', ' 7', ' 23']
```

<a href="javascript:exerciseToggle(5)">Answer</a>
<div id="exercise5" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent A {
		on Initialize {
			var numberListString = occurrence.parameters.get(0) as String
			var numberArray = numberListString.split("\\s*,\\s*")
			var numberList = newArrayList(numberArray)
			println("List : " + numberList)
		}
	}
[:End:]
</div>


## Exercise 6

* Write a SARL program that accepts a filename from the command line, and prints the extension of the file.
* Input filename: `abc.sarl`
* Expected output: `sarl`

<a href="javascript:exerciseToggle(6)">Answer</a>
<div id="exercise6" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var filename = occurrence.parameters.get(0) as String
			var idx = filename.indexOf('.')
			if (idx >= 0) {
				println(filename.substring(idx + 1))
			}
		}
	}
[:End:]
</div>

## Exercise 7

* Write a SARL program to display the first and last colors from the following list.
* Example of inut list: `color_list = ["Red","Green","White" ,"Black"]`

<a href="javascript:exerciseToggle(7)">Answer</a>
<div id="exercise7" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var color_list = #["Red", "Green", "White", "Black"]
			println(color_list.get(0))
			println(color_list.get(color_list.length - 1))
		}
	}
[:End:]
</div>

## Exercise 8

* Write a SARL program that accepts an integer `n` on the command line and computes the value of n+n*n+n*n*n.
* For example, input value for n is `5`
* Expected result : `615`

<a href="javascript:exerciseToggle(8)">Answer</a>
<div id="exercise8" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var n = occurrence.parameters.get(0) as Integer
			var value = n + n**2 + n**3
			println("Result : " + value)
		}
	}
[:End:]
</div>


## Exercise 9

* Write a SARL program that prints the calendar for a given month and year.

> _**Note:**_ Use 'GregorianCalendar' type from the Java API.

<a href="javascript:exerciseToggle(9)">Answer</a>
<div id="exercise9" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import java.util.GregorianCalendar
	agent Solution {
		on Initialize {
			var month = occurrence.parameters.get(0) as Integer
			var year = occurrence.parameters.get(1) as Integer
			var cal = new GregorianCalendar(year, month, 1)
			println(cal)
		}
	}
[:End:]
</div>

## Exercise 10

* Write a SARL program to calculate the number of days between two dates.
* Examples of input dates: `(2014, 7, 2)`, `(2014, 7, 11)`
* Expected output : 9 days

<a href="javascript:exerciseToggle(10)">Answer</a>
<div id="exercise10" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import java.time.LocalDate
	import java.time.temporal.ChronoUnit
	agent Solution {
		on Initialize {
			var day0 = occurrence.parameters.get(0) as Integer
			var month0 = occurrence.parameters.get(1) as Integer
			var year0 = occurrence.parameters.get(2) as Integer
			
			var day1 = occurrence.parameters.get(3) as Integer
			var month1 = occurrence.parameters.get(4) as Integer
			var year1 = occurrence.parameters.get(5) as Integer
			
			var cal0 = LocalDate::of(year0, month0, day0)
			var cal1 = LocalDate::of(year1, month1, day1)
			
			var days = ChronoUnit::DAYS.between(cal1, cal0)
			println(days + " days")
		}
	}
[:End:]
</div>

## Exercise 11

* Write a SARL program to get the volume of a sphere with radius six.

<a href="javascript:exerciseToggle(11)">Answer</a>
<div id="exercise11" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var volume = 4/3 * Math::PI * 6**3;
			println(volume)
		}
	}
[:End:]
</div>

## Exercise 12

* Write a SARL program to calculate the difference between a given number and 17. If the number is greater than 17, return twice the absolute difference.

<a href="javascript:exerciseToggle(12)">Answer</a>
<div id="exercise12" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var number = occurrence.parameters.get(0) as Integer
			var diff = number - 17
			if (diff < 0) {
				diff = 2 * -diff
			}
			println(diff)
		}
	}
[:End:]
</div>


## Exercise 13

* Write a SARL program to test whether a number is within a distance 100 to 1000 or 2000.

<a href="javascript:exerciseToggle(13)">Answer</a>
<div id="exercise13" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.api.core.Initialize
	import static java.lang.Math.*
	agent Solution {
		on Initialize {
			var n = occurrence.parameters.get(0) as Integer
			var result = ((abs(1000 - n) <= 100) || (abs(2000 - n) <= 100))
			println(result)
		}
	}
[:End:]
</div>

## Exercise 14

* Write a SARL program to calculate the sum of three given numbers, that are provided on the command line. If the values are equal, return three times their sum.

<a href="javascript:exerciseToggle(14)">Answer</a>
<div id="exercise14" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var number0 = occurrence.parameters.get(0) as Integer
			var number1 = occurrence.parameters.get(1) as Integer
			var number2 = occurrence.parameters.get(2) as Integer
			
			var sum = number0 + number1 + number2
			if (number0 == number1 && number1 == number2) {
				sum *= 3
			}
			println(sum)
		}
	}
[:End:]
</div>

## Exercise 15

* Write a SARL program to get a newly-generated string from a given string where `Is` has been added to the front. Return the string unchanged if the given string already begins with `Is`.

<a href="javascript:exerciseToggle(15)">Answer</a>
<div id="exercise15" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var str = occurrence.parameters.get(0) as String
			if (!str.startsWith("Is ")) {
				str = "Is " + str
			}
			println(str)
		}
	}
[:End:]
</div>

## Exercise 16

* Write a SARL program that returns a string that is n (non-negative integer) copies of a given string (from the command line).

<a href="javascript:exerciseToggle(16)">Answer</a>
<div id="exercise16" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var str = occurrence.parameters.get(0) as String
			var n = occurrence.parameters.get(1) as Integer
			var result = ""
			for (i : 1..n) {
				result += str
			}
			println(result)
		}
	}
[:End:]
</div>

## Exercise 17

* Write a SARL program that determines whether a given number (accepted from the command line) is even or odd, and prints an appropriate message to the user.

<a href="javascript:exerciseToggle(17)">Answer</a>
<div id="exercise17" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var n = occurrence.parameters.get(0) as Integer
			if ((n%2) == 0) {
				println("Even number: " + n)
			} else {
				println("Odd number: " + n)
			}
		}
	}
[:End:]
</div>

## Exercise 18

* Write a SARL program to count the number 4 in a given list, provided on the command line.

<a href="javascript:exerciseToggle(18)">Answer</a>
<div id="exercise18" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var n = 0
			for (arg : occurrence.parameters) {
				try {
					var nb = arg as Integer
					if (nb == 4) {
						n++
					}
				} catch (ex : Throwable) {
				}
			}
			println(n)
		}
	}
[:End:]
</div>

## Exercise 19

* Write a SARL program to get n (non-negative integer) copies of the first 2 characters of a given string (from the command line). Return n copies of the whole string if the length is less than 2.

<a href="javascript:exerciseToggle(19)">Answer</a>
<div id="exercise19" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var input = occurrence.parameters.get(0) as String
			var n = occurrence.parameters.get(1) as Integer
			var token : String
			if (input.length < 2) {
			    token = input
			} else {
			    token = input.substring(0, 2)
			}
			var result = ""
			for (i : 1..n) {
			    result += input
			}
			println(result)
		}
	}
[:End:]
</div>

## Exercise 20

* Write a SARL program to test whether a passed letter from the command line is a vowel or not.

<a href="javascript:exerciseToggle(20)">Answer</a>
<div id="exercise20" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		val vowels = newArrayList('a', 'e', 'i', 'o', 'u', 'y')
		
		on Initialize {
			var str = occurrence.parameters.get(0) as String
			var letter = (str.charAt(0) as Character).toLowerCase
			if (vowels.contains(letter)) {
				println("The letter is a vowel")
			} else {
				println("The letter is not a vowel")
			}
		}
	}
[:End:]
</div>

## Exercise 21

* Write a SARL program that checks whether a specified value is contained within a group of values.
* Examples:

```text
3 -> [1, 5, 8, 3] : True
-1 -> [1, 5, 8, 3] : False
```

<a href="javascript:exerciseToggle(21)">Answer</a>
<div id="exercise21" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		val group = newArrayList(1, 5, 8, 3)
		
		on Initialize {
			var value = occurrence.parameters.get(0) as Integer
			println(value + " -> " + group + " : " + group.contains(value))
		}
	}
[:End:]
</div>

## Exercise 22

* Write a SARL program that concatenates all elements in a list into a string and returns it.

<a href="javascript:exerciseToggle(22)">Answer</a>
<div id="exercise22" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var list = <String>newArrayList
			for (arg : occurrence.parameters) {
				var str = arg as String
				list += str
			}
			println(list)
		}
	}
[:End:]
</div>

## Exercise 23

* Write a SARL program to print all even numbers from a given list of numbers in the same order and stop printing any after 237 in the sequence.
* Sample numbers list:

```text
numbers = [    
    386, 462, 47, 418, 907, 344, 236, 375, 823, 566, 597, 978, 328, 615, 953, 345, 
    399, 162, 758, 219, 918, 237, 412, 566, 826, 248, 866, 950, 626, 949, 687, 217, 
    815, 67, 104, 58, 512, 24, 892, 894, 767, 553, 81, 379, 843, 831, 445, 742, 717, 
    958,743, 527
    ]
```

<a href="javascript:exerciseToggle(23)">Answer</a>
<div id="exercise23" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		var numbers : int[] = #[    
		    386, 462, 47, 418, 907, 344, 236, 375, 823, 566, 597, 978, 328, 615, 953, 345, 
		    399, 162, 758, 219, 918, 237, 412, 566, 826, 248, 866, 950, 626, 949, 687, 217, 
		    815, 67, 104, 58, 512, 24, 892, 894, 767, 553, 81, 379, 843, 831, 445, 742, 717, 
		    958,743, 527
		]
		on Initialize {
			var i = 0
			while (i < numbers.length && i < 237) {
				println(numbers.get(i))
				i++
			}
		}
	}
[:End:]
</div>

## Exercise 24

* Write a SARL program that prints out all colors from `color_list_1` that are not present in `color_list_2`.
* Test data:

```text
color_list_1 = set(["White", "Black", "Red"])
color_list_2 = set(["Red", "Green"])
```

* Expected output:

```text
{'Black', 'White'}
```

<a href="javascript:exerciseToggle(24)">Answer</a>
<div id="exercise24" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		var color_list_1 = newHashSet(#["White", "Black", "Red"])
		var color_list_2 = newHashSet(#["Red", "Green"])
		on Initialize {
			for (col : color_list_1) {
				if (!color_list_2.contains(col)) {
					println(col)
				}
			}
		}
	}
[:End:]
</div>

## Exercise 25

* Write a SARL program that will accept the base and height of a triangle and compute its area.

<a href="javascript:exerciseToggle(25)">Answer</a>
<div id="exercise25" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var base = occurrence.parameters.get(0) as Double
			var height = occurrence.parameters.get(1) as Double
			var area = (base * height) / 2
			println("Area = " + area)
		}
	}
[:End:]
</div>

## Exercise 26

* Write a SARL program that computes the greatest common divisor (GCD) of two positive integers.

<a href="javascript:exerciseToggle(26)">Answer</a>
<div id="exercise26" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var number1 = occurrence.parameters.get(0) as Double
			var number2 = occurrence.parameters.get(1) as Double
			var gcd = gcd(number1, number2)
			println("GCD(" + number1 + ", "+ number2 + ") = " + gcd)
		}
		def gcd(a : double, b : double) : double {
			var da = a
			var db = b
			while (da != db) {
				if (da > db) {
					da = da - db
				} else {
					db = db - da
				}
			}
			return da
		}
	}
[:End:]
</div>

## Exercise 27

* Write a SARL program to find the least common multiple (LCM) of two positive integers.

<a href="javascript:exerciseToggle(27)">Answer</a>
<div id="exercise27" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import static java.lang.Math.*
	agent Solution {
		on Initialize {
			var number1 = occurrence.parameters.get(0) as Double
			var number2 = occurrence.parameters.get(1) as Double
			var lcm = lcm(number1, number2)
			println("LCM(" + number1 + ", "+ number2 + ") = " + lcm)
		}
		def lcm(a : double, b : double) : double {
			abs(a * b) / gcd(a, b)
		}
		def gcd(a : double, b : double) : double {
			var da = a
			var db = b
			while (da != db) {
				if (da > db) {
					da = da - db
				} else {
					db = db - da
				}
			}
			return da
		}
	}
[:End:]
</div>

## Exercise 28

* Write a SARL program to sum three given integers. However, if two values are equal, the sum will be zero.

<a href="javascript:exerciseToggle(28)">Answer</a>
<div id="exercise28" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var number1 = occurrence.parameters.get(0) as Integer
			var number2 = occurrence.parameters.get(1) as Integer
			var number3 = occurrence.parameters.get(2) as Integer
			var sum = 0
			if (number1 != number2 && number2 != number3 && number1 != number3) {
				sum = number1 + number2 + number3
			}
			println(sum);
		}
	}
[:End:]
</div>

## Exercise 29

* Write a SARL program to sum two given integers. However, if the sum is between 15 and 20 it will return 20.

<a href="javascript:exerciseToggle(29)">Answer</a>
<div id="exercise29" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var number1 = occurrence.parameters.get(0) as Integer
			var number2 = occurrence.parameters.get(1) as Integer
			var sum = number1 + number2
			if ((15..20).contains(sum)) {
				sum = 20
			}
			println(sum);
		}
	}
[:End:]
</div>

## Exercise 30

* Write a SARL program that returns `true` if the two given integer values are equal or their sum or difference is 5.

<a href="javascript:exerciseToggle(30)">Answer</a>
<div id="exercise30" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var number1 = occurrence.parameters.get(0) as Integer
			var number2 = occurrence.parameters.get(1) as Integer
			var result = program(number1, number2)
			println(result);
		}
		def program(number1 : int, number2 : int) : boolean {
			number1 == number2 || number1 + number2 == 5
		}
	}
[:End:]
</div>

## Exercise 31

* Write a SARL program to add two objects if both objects are integers.

<a href="javascript:exerciseToggle(31)">Answer</a>
<div id="exercise31" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var number1 = occurrence.parameters.get(0)
			var number2 = occurrence.parameters.get(1)
			var result = sum(number1, number2)
			println(result);
		}
		def sum(number1 : Object, number2 : Object) : int {
			if (number1 instanceof Number) {
				if (number2 instanceof Number) {
					return number1.intValue + number2.intValue
				}
			}
			return 0
		}
	}
[:End:]
</div>

## Exercise 32

* Write a SARL program to solve `(x + y) * (x + y)`.
* Test Data: `x = 4, y = 3`
* Expected Output: `(4 + 3) ^ 2 = 49`

<a href="javascript:exerciseToggle(32)">Answer</a>
<div id="exercise32" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var x = occurrence.parameters.get(0) as Double
			var y = occurrence.parameters.get(1) as Double
			var result = solve(x, y)
			println(result);
		}
		def solve(x : double, y : double) : double {
			(x + y) ** 2
		}
	}
[:End:]
</div>

## Exercise 33

* Write a SARL program to calculate the distance between the points (x1, y1) and (x2, y2).

<a href="javascript:exerciseToggle(33)">Answer</a>
<div id="exercise33" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var x1 = occurrence.parameters.get(0) as Double
			var y1 = occurrence.parameters.get(1) as Double
			var x2 = occurrence.parameters.get(2) as Double
			var y2 = occurrence.parameters.get(3) as Double
			var result = distance(x1, y1, x2, y2)
			println(result);
		}
		def distance(x1 : double, y1 : double, x2 : double, y2 : double) : double {
			Math::sqrt((x2 - x1)**2 + (y2 - y1)**2)
		}
	}
[:End:]
</div>

## Exercise 34

* Write a SARL program to check whether a file exists, when the name of the file is provided from the command line. Use the Java API.

<a href="javascript:exerciseToggle(34)">Answer</a>
<div id="exercise34" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import java.io.File
	agent Solution {
		on Initialize {
			var filename = occurrence.parameters.get(0) as String
			var file = new File(filename)
			var isExist = file.exists
			println(isExist);
		}
	}
[:End:]
</div>

## Exercise 35

* Write a SARL program to parse a string to float or integer.

<a href="javascript:exerciseToggle(35)">Answer</a>
<div id="exercise35" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var value = occurrence.parameters.get(0) as String
			try {
				var floatNumber = value as Double
				println("Double value = " + floatNumber)
			} catch (ex : Throwable) {
				var intNumber = value as Long
				println("Long value = " + intNumber)
			}
		}
	}
[:End:]
</div>

## Exercise 36

* Write a SARL program to list all files in a directory.

<a href="javascript:exerciseToggle(36)">Answer</a>
<div id="exercise36" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import java.io.File
	agent Solution {
		on Initialize {
			var folderName = occurrence.parameters.get(0) as String
			var folder = new File(folderName)
			var list = folder.listFiles
			println(list)
		}
	}
[:End:]
</div>

## Exercise 37

* Write a SARL program to print a message provided from the command line without a newline or space.

<a href="javascript:exerciseToggle(37)">Answer</a>
<div id="exercise37" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var value = occurrence.parameters.get(0) as String
			value = value.replaceAll("[\n ]+", "")
			println(value)
		}
	}
[:End:]
</div>

## Exercise 38

* Write a SARL program to print to STDERR (standard error output of the process).

<a href="javascript:exerciseToggle(38)">Answer</a>
<div id="exercise38" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import io.sarl.api.core.Logging
	agent Solution {
		uses Logging
		on Initialize {
			var value = occurrence.parameters.get(0) as String
			error(value)
		}
	}
[:End:]
</div>


## Exercise 39

* Write a SARL program to access environment variables.

<a href="javascript:exerciseToggle(39)">Answer</a>
<div id="exercise39" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var variableName = occurrence.parameters.get(0) as String
			println(System::getProperty(variableName))
		}
	}
[:End:]
</div>

## Exercise 40

* Write a SARL program to get the current username.

<a href="javascript:exerciseToggle(40)">Answer</a>
<div id="exercise40" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			println(System::getProperty("user.name"))
		}
	}
[:End:]
</div>

## Exercise 41

* Write a SARL program to sum the first n positive integers.

<a href="javascript:exerciseToggle(41)">Answer</a>
<div id="exercise41" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var n = occurrence.parameters.get(0) as Integer
			var sum = 0
			for (i : 1..n) {
				sum += i
			}
			println(sum)
		}
	}
[:End:]
</div>

## Exercise 42

* Write a SARL program to convert height (in feet and inches) to centimeters, when the value is provided from the command line.

<a href="javascript:exerciseToggle(42)">Answer</a>
<div id="exercise42" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var height = occurrence.parameters.get(0) as Double
			height = 2.54 * height
			println(height)
		}
	}
[:End:]
</div>

## Exercise 43

* Write a SARL program to calculate the hypotenuse of a right angled triangle.

<a href="javascript:exerciseToggle(43)">Answer</a>
<div id="exercise43" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var side1 = occurrence.parameters.get(0) as Double
			var side2 = occurrence.parameters.get(1) as Double
			var hypotenuse = Math::sqrt(side1**2 + side2**2)
			println(hypotenuse)
		}
	}
[:End:]
</div>

## Exercise 44

* Write a SARL program to convert all units of time into seconds.

<a href="javascript:exerciseToggle(44)">Answer</a>
<div id="exercise44" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import static java.util.concurrent.TimeUnit.*
	agent Solution {
		on Initialize {
			var days = occurrence.parameters.get(0) as Long
			var hours = occurrence.parameters.get(1) as Long
			var minutes = occurrence.parameters.get(1) as Long
			var seconds = occurrence.parameters.get(1) as Long
			var total = DAYS.toSeconds(days) + HOURS.toSeconds(hours) + MINUTES.toSeconds(hours) + seconds
			println(total)
		}
	}
[:End:]
</div>

## Exercise 45

* Write a SARL program to get an absolute file path from a name that is provided on the command line.

<a href="javascript:exerciseToggle(45)">Answer</a>
<div id="exercise45" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import java.io.File
	agent Solution {
		on Initialize {
			var name = occurrence.parameters.get(0) as String
			var file = new File(name)
			var absoluteFilename = file.absolutePath
			println(absoluteFilename)
		}
	}
[:End:]
</div>

## Exercise 46

* Write a SARL program that converts seconds into days, hours, minutes, and seconds.

<a href="javascript:exerciseToggle(46)">Answer</a>
<div id="exercise46" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var seconds = occurrence.parameters.get(0) as Integer
			var days = (seconds / (3600 * 24)) as int
			seconds = seconds - days * 3600 * 24
			var hours = (seconds / 3600) as int
			seconds = seconds - hours * 3600
			var minutes = (seconds / 60) as int
			seconds = seconds - minutes * 60
			println("Days = " + days)
			println("Hours = " + hours)
			println("Minutes = " + minutes)
			println("Seconds = " + seconds)
		}
	}
[:End:]
</div>

## Exercise 47

* Write a SARL program to calculate the body mass index.

<a href="javascript:exerciseToggle(47)">Answer</a>
<div id="exercise47" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var weight = occurrence.parameters.get(0) as Double
			var height = occurrence.parameters.get(1) as Double
			var bmi = weight / height**2
			println(bmi)
		}
	}
[:End:]
</div>

## Exercise 48

* Write a SARL program to convert pressure in kilopascals to pounds per square inch, a millimeter of mercury (mmHg) and atmosphere pressure.

<a href="javascript:exerciseToggle(48)">Answer</a>
<div id="exercise48" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var kilopascals = occurrence.parameters.get(0) as Double
			var psl = kilopascals * 0.145038
			println("Pounds per square inch = " + psl + " Psl")
			var mmhg = kilopascals * 7.50062
			println("Millimeters of mercury = " + mmhg + " mmHg")
			var atmosphere = kilopascals / 101.325
			println("Atmosphere pressure = " + atmosphere + " atmosphere")
		}
	}
[:End:]
</div>

## Exercise 49

* Write a SARL program to calculate sum of digits, from a string of characters, of a number.

<a href="javascript:exerciseToggle(49)">Answer</a>
<div id="exercise49" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var value = occurrence.parameters.get(0) as String
			var sum = 0
			for (c : value.toCharArray) {
				if (c >= '0' && c <= '9') {
					sum += (c - '0') as int
				}
			}
			println(sum)
		}
	}
[:End:]
</div>

## Exercise 50

* Write a SARL program to sort three integers without using conditional statements and loops.

<a href="javascript:exerciseToggle(50)">Answer</a>
<div id="exercise50" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import static java.lang.Math.*
	agent Solution {
		on Initialize {
			var x = occurrence.parameters.get(0) as Integer
			var y = occurrence.parameters.get(1) as Integer
			var z = occurrence.parameters.get(2) as Integer
			var a1 = min(x, min(y, z))
			var a3 = max(x, max(y, z))
			var a2 = (x + y + z) - a1 - a3
			println("Numbers in sorted order: (" + a1 + ", " + a2 + ", "  + a3 + ")")
		}
	}
[:End:]
</div>

## Exercise 51

* Write a SARL program to concatenate N strings.

<a href="javascript:exerciseToggle(51)">Answer</a>
<div id="exercise51" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var all = ""
			for (p : occurrence.parameters) {
				all += p as String
			}
			println(all)
		}
	}
[:End:]
</div>

## Exercise 52

* Write a SARL program to calculate the sum of all items of a container (array, list, set).

<a href="javascript:exerciseToggle(52)">Answer</a>
<div id="exercise52" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import java.util.stream.Collectors
	import static extension java.util.Arrays.*
	agent Solution {
		on Initialize {
			var s = sum(occurrence.parameters.asList.stream
				.filter[it instanceof Number]
				.map[it as Number].collect(Collectors.toList))
			println(s)
		}
		def sum(container : Iterable<? extends Number>) : double {
			var s = 0.0
			var iter = container.iterator
			while (iter.hasNext) {
				var elt = iter.next
				s += elt as double
			}
			return s
		}
	}
[:End:]
</div>

## Exercise 53

* Write a SARL program to test whether all numbers in a list, provided on the command line, are greater than a certain number, provided also on command line.

<a href="javascript:exerciseToggle(53)">Answer</a>
<div id="exercise53" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import static extension java.util.Arrays.*
	agent Solution {
		on Initialize {
			var input = occurrence.parameters.asList.stream
				.filter[it instanceof Number]
				.map[it as Number]
			var iter = input.iterator
			var reference = iter.next as double
			var output = newArrayList
			while (iter.hasNext) {
				var n = iter.next as double
				if (n > reference) {
					output += n
				}
			}
			println(output)
		}
	}
[:End:]
</div>

## Exercise 54

* Write a SARL program to count the number of occurrences of a specific character in a string.

<a href="javascript:exerciseToggle(54)">Answer</a>
<div id="exercise54" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var c = occurrence.parameters.get(0) as Character
			var value = occurrence.parameters.get(1) as String
			var count = value.chars.filter[it == c].count
			println(count)
		}
	}
[:End:]
</div>

## Exercise 55

* Write a SARL program to check whether a file path, provided on the command line, is a file or a directory.

<a href="javascript:exerciseToggle(55)">Answer</a>
<div id="exercise55" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	import java.io.File
	agent Solution {
		on Initialize {
			var filename = occurrence.parameters.get(0) as String
			var file = new File(filename)
			println(file.isFile || file.isDirectory)
		}
	}
[:End:]
</div>

## Exercise 56

* Write a SARL program to swap two variables.

<a href="javascript:exerciseToggle(56)">Answer</a>
<div id="exercise56" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var var1 = occurrence.parameters.get(0)
			var var2 = occurrence.parameters.get(1)
			var tmp = var1
			var1 = var2
			var2 = tmp
			println("var1 = " + var1)
			println("var2 = " + var2)
		}
	}
[:End:]
</div>

## Exercise 57

* Write a SARL program to check whether a string is numeric.

<a href="javascript:exerciseToggle(57)">Answer</a>
<div id="exercise57" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var value = occurrence.parameters.get(0) as String
			println(isNumeric(value))
		}
		def isNumeric(value : String) : boolean {
			try {
				Double::parseDouble(value)
				return true
			} catch (ex : Throwable) {
				return false
			}
		}
	}
[:End:]
</div>

## Exercise 58

* Write a SARL program to prove that two string variables of the same value point to the same memory location.

<a href="javascript:exerciseToggle(58)">Answer</a>
<div id="exercise58" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			var value1 = occurrence.parameters.get(0) as String
			var value2 = occurrence.parameters.get(2) as String
			println(isSameAddressInMemory(value1, value2))
		}
		def isSameAddressInMemory(value1 : String, value2 : String) : boolean {
			value1 === value2
		}
	}
[:End:]
</div>

## Exercise 59

* Write a SARL program to determine the largest and smallest integers, longs, and floats.

<a href="javascript:exerciseToggle(59)">Answer</a>
<div id="exercise59" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:OnHtml]
	import io.sarl.api.core.Initialize
	agent Solution {
		on Initialize {
			println("Min integer = " + Integer::MIN_VALUE)
			println("Max integer = " + Integer::MAX_VALUE)
			println("Min long = " + Long::MIN_VALUE)
			println("Max long = " + Long::MAX_VALUE)
			println("Min float = " + Float::MIN_VALUE)
			println("Max float = " + Float::MAX_VALUE)
		}
	}
[:End:]
</div>

[:Include:](../../includes/legal.inc)
