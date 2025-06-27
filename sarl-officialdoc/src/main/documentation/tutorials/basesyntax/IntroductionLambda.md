# Introduction to Lambda Expressions with SARL

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

* Write a SARL program to create a lambda function that adds 15 to a given number passed in as an argument, also create a lambda function that multiplies argument x with argument y and prints the result.
* Sample Output:

```text
25
48
```

<a href="javascript:exerciseToggle(1)">Answer</a>
<div id="exercise1" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {
		def firstLambda_version1 {
			var lambda : (int) => int
			lambda = [x : int | x + 15]
		}
		
		def firstLambda_version2 {
			var lambda : (int) => int
			lambda = [x | x + 15]
		}

		def firstLambda_version3 {
			var lambda : (int) => int
			lambda = [it + 15]
		}

		def secondLambda_verison1 {
			var lambda : (int, int) => void
			lambda = [x : int, y : int | println(x * y)]
		}

		def secondLambda_verison2 {
			var lambda : (int, int) => void
			lambda = [x, y | println(x * y)]
		}
	}
[:End:]
</div>

## Exercise 2

* Write a SARL program to create a function that takes one argument, and that argument will be multiplied with an unknown given number.
* Sample Output:

```text
Double the number of 15 = 30
Triple the number of 15 = 45
Quadruple the number of 15 = 60
Quintuple the number 15 = 75
```

<a href="javascript:exerciseToggle(2)">Answer</a>
<div id="exercise2" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution1 {
		static def computeLambda(n : int) : (int) => int {
			[x | x * n]
		}
		static def main {
			var result = computeLambda(2)
			println("Double the number of 15 =" + result.apply(15))
			result = computeLambda(3)
			println("Triple the number of 15 =" + result.apply(15))
			result = computeLambda(4)
			println("Quadruple the number of 15 =" + result.apply(15))
			result = computeLambda(5)
			println("Quintuple the number of 15 =" + result.apply(15))
		}
	}
[:End:]
</div>

Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution2 {
		static def computeLambda(n : int) : (int) => int {
			[it * n]
		}
		static def main {
			var result = computeLambda(2)
			println("Double the number of 15 =" + result.apply(15))
			result = computeLambda(3)
			println("Triple the number of 15 =" + result.apply(15))
			result = computeLambda(4)
			println("Quadruple the number of 15 =" + result.apply(15))
			result = computeLambda(5)
			println("Quintuple the number of 15 =" + result.apply(15))
		}
	}
[:End:]
</div>

## Exercise 3

* Write a SARL program to sort a list of tuples using Lambda.
* Original list of tuples: `[('English', 88), ('Science', 90), ('Maths', 97), ('Social sciences', 82)]`
* Sorting the list of tuples: `[('Social sciences', 82), ('English', 88), ('Science', 90), ('Maths', 97)]`

<a href="javascript:exerciseToggle(3)">Answer</a>
<div id="exercise3" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static var marks = newArrayList(#[
			#['English', 88],
			#['Science', 90],
			#['Maths', 97],
			#['Social sciences', 82]
		])

		static def main {
			println("Original list of tuples: " + marks)
			marks.sort [a, b | (a.get(1) as Integer) <=> (b.get(1) as Integer)]
			println("Sorting the List of tuples: " + marks)
		}

	}
[:End:]
</div>

## Exercise 4

* Write a SARL program to sort a list of dictionaries using Lambda.
* Original list of maps: `[{'make': 'Nokia', 'model': 216, 'color': 'Black'}, {'make': 'Mi Max', 'model': '2', 'color': 'Gold'}, {'make': 'Samsung', 'model': 7, 'color': 'Blue'}]`
* Sorting the list of maps: `[{'make': 'Nokia', 'model': 216, 'color': 'Black'}, {'make': 'Samsung', 'model': 7, 'color': 'Blue'}, {'make': 'Mi Max', 'model': '2', 'color': 'Gold'}]`

<a href="javascript:exerciseToggle(4)">Answer</a>
<div id="exercise4" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static var maps = newArrayList(#[
			#{'make' -> 'Nokia', 'model' -> 216, 'color' -> 'Black'},
			#{'make' -> 'Mi Max', 'model' -> 2, 'color' -> 'Gold'},
			#{'make' -> 'Samsung', 'model' -> 7, 'color' -> 'Blue'}
		])

		static def main {
			println("Original list of maps: " + maps)
			maps.sort [a, b | (b.get("model") as Integer) <=> (a.get("model") as Integer)]
			println("Sorting the list of maps: " + maps)
		}

	}
[:End:]
</div>

## Exercise 5

* Write a SARL program to filter a list of integers using Lambda.
* Original list of integers: `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
* Even numbers from the said list: `[2, 4, 6, 8, 10]`
* Odd numbers from the said list: `[1, 3, 5, 7, 9]`

<a href="javascript:exerciseToggle(5)">Answer</a>
<div id="exercise5" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.stream.Collectors
	class Solution1 {

		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		static def main {
			println("Original list of integers: " + original)

			var evenNumbers = original.stream.filter[(it % 2) == 0].collect(Collectors::toList)
			println("Even numbers from the said list: " + evenNumbers)

			var oddNumbers = original.stream.filter[(it % 2) != 0].collect(Collectors::toList)
			println("Odd numbers from the said list: " + oddNumbers)
		}

	}
[:End:]
Answer #2 is:
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution2 {

		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		static def main {
			println("Original list of integers: " + original)

			var evenNumbers = original.filter[(it % 2) == 0].toList
			println("Even numbers from the said list: " + evenNumbers)

			var oddNumbers = original.filter[(it % 2) != 0].toList
			println("Odd numbers from the said list: " + oddNumbers)
		}

	}
[:End:]
</div>

## Exercise 6

* Write a SARL program to square and cube every number in a given list of integers using Lambda.
* Original list of integers: `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
* Square every number of the said list: `[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]`
* Cube every number of the said list: `[1, 8, 27, 64, 125, 216, 343, 512, 729, 1000]`

<a href="javascript:exerciseToggle(6)">Answer</a>
<div id="exercise6" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.stream.Collectors
	class Solution1 {

		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		static def main {
			println("Original list of integers: " + original)

			var squareNumbers = original.stream.map[it.doubleValue ** 2].collect(Collectors::toList)
			println("Square every number of the said list: " + squareNumbers)

			var cubeNumbers = original.stream.map[it.doubleValue ** 3].collect(Collectors::toList)
			println("Cube every number of the said list: " + cubeNumbers)
		}

	}
[:End:]
Answer #2 is:
[:Success:]
	class Solution2 {

		static var original = #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		static def main {
			println("Original list of integers: " + original)

			var squareNumbers = original.map[it.doubleValue ** 2].toList
			println("Square every number of the said list: " + squareNumbers)

			var cubeNumbers = original.map[it.doubleValue ** 3].toList
			println("Cube every number of the said list: " + cubeNumbers)
		}

	}
[:End:]
</div>

## Exercise 7

* Write a SARL program to find if a given string starts with a given character using Lambda.

<a href="javascript:exerciseToggle(7)">Answer</a>
<div id="exercise7" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static def main {
			var starts_with = [x : String | x.startsWith('P')]
			println(starts_with.apply('Python'))
			println(starts_with.apply('Java'))
		}

	}
[:End:]
</div>

## Exercise 8

* Write a SARL program to extract year, month, date and time using Lambda.
* Sample Output:

```text
2020-01-15 09:03:32.744178
2020
1
15
09:03:32.744178
```

<a href="javascript:exerciseToggle(8)">Answer</a>
<div id="exercise8" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.Calendar
	class Solution {

		static def main {
			var now = Calendar::instance
			println(now)

			var year : (Calendar) => int = [it.get(Calendar::YEAR)]
			var month : (Calendar) => int = [it.get(Calendar::MONTH)]
			var day : (Calendar) => int = [it.get(Calendar::DAY_OF_MONTH)]
			var time : (Calendar) => String = [it.get(Calendar::HOUR) + ":" + it.get(Calendar::MINUTE) + ":" + it.get(Calendar::SECOND) + "." + it.get(Calendar::MILLISECOND)]

			println(year.apply(now))
			print(month.apply(now))
			print(day.apply(now))
			print(time.apply(now))
		}

	}
[:End:]
</div>

## Exercise 9

* Write a SARL program to check whether a given string is a number or not using Lambda.
* Sample Output:

```text
26587 : true
4.2365 : true
-12547 : false
00 : true
Z001 : false
001 : true
-16.4 : true
-24587.11 : true
```

<a href="javascript:exerciseToggle(9)">Answer</a>
<div id="exercise9" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static def dotest(n : String) {
			var is_num = [
				try {
					Double.parseDouble(it.toString)
					return true
				} catch (ex : Throwable) {
					return false
				}
			]
			println(n + " : " + is_num.apply(n))
		}

		static def main {
			dotest("26587")
			dotest("4.2365")
			dotest("-12547")
			dotest("00")
			dotest("Z001")
			dotest("001")
			dotest("-16.4")
			dotest("-24587.11")
		}

	}
[:End:]
</div>

## Exercise 10

* Write a SARL program to create Fibonacci series up to n using Lambda.
* Fibonacci series upto 2: `[0, 1]`
* Fibonacci series upto 5: `[0, 1, 1, 2, 3]`
* Fibonacci series upto 6: `[0, 1, 1, 2, 3, 5]`
* Fibonacci series upto 9: `[0, 1, 1, 2, 3, 5, 8, 13, 21]`

<a href="javascript:exerciseToggle(10)">Answer</a>
<div id="exercise10" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {

		static def fibonacci(n : int) : List<Integer> {
			(0..n).map[#[it]].reduce[accumulator, current |
				var nl : List<Integer> = newArrayList(accumulator)
				var v = accumulator.get(accumulator.size - 1).intValue + accumulator.get(accumulator.size - 2).intValue
				nl += v
				return nl
			]
		}

		static def main {
			println(fibonacci(2))
			println(fibonacci(5))
			println(fibonacci(6))
			println(fibonacci(9))
		}

	}
[:End:]
</div>


## Exercise 11

* Write a SARL program to find the intersection of two given arrays using Lambda.
* Original arrays:

```text
[1, 2, 3, 5, 7, 8, 9, 10]
[1, 2, 4, 8, 9]
```

* Intersection of the said arrays: `[1, 2, 8, 9]`

<a href="javascript:exerciseToggle(11)">Answer</a>
<div id="exercise11" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static var content1 = #[1, 2, 3, 5, 7, 8, 9, 10]
		static var content2 = #[1, 2, 4, 8, 9]

		static def main {
			var result = content2.filter[content1.contains(it)].toList 
			println(result)
		}

	}
[:End:]
</div>

## Exercise 12

* Write a SARL program to rearrange positive and negative numbers in a given array using Lambda.
* Original arrays: `[-1, 2, -3, 5, 7, 8, 9, -10]`
* Rearrange positive and negative numbers of the said array: `[2, 5, 7, 8, 9, -10, -3, -1]`

<a href="javascript:exerciseToggle(12)">Answer</a>
<div id="exercise12" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import static extension java.lang.Math.signum
	class Solution {

		static var content : Integer[] = #[-1, 2, -3, 5, 7, 8, 9, -10]

		static def main {
			var result = newArrayList(content)
			result.sort[a, b |
				if (a.signum == b.signum) {
					return a <=> b
				} else if (a < 0) {
					return 1
				} else {
					return 1
				}
			] 
			println(result)
		}

	}
[:End:]
</div>

## Exercise 13

* Write a SARL program to count the even and odd numbers in a given array of integers using Lambda.
* Original arrays: `[1, 2, 3, 5, 7, 8, 9, 10]`
* Number of even numbers in the above array: `3`
* Number of odd numbers in the above array: `5`

<a href="javascript:exerciseToggle(13)">Answer</a>
<div id="exercise13" style="display:none;">
Two answers are possible. Answer #1 is:
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution1 {

		static var content = #[1, 2, 3, 5, 7, 8, 9, 10]

		static def main {
			var ecount = content.filter[(it%2) == 0].size
			var ocount = content.filter[(it%2) != 0].size
			println("Original arrays: " + content)
			println("Number of even numbers in the above array: " + ecount)
			println("Number of odd numbers in the above array: " + ocount)
		}

	}
[:End:]
Answer #2 is:
[:Success:]
	class Solution2 {

		static var content = #[1, 2, 3, 5, 7, 8, 9, 10]

		static def main {
			var ecount = content.stream.filter[(it%2) == 0].count
			var ocount = content.stream.filter[(it%2) != 0].count
			println("Original arrays: " + content)
			println("Number of even numbers in the above array: " + ecount)
			println("Number of odd numbers in the above array: " + ocount)
		}

	}
[:End:]
</div>

## Exercise 14

* Write a SARL program to filter a given list to determine if the values in the list have a length of 6 using Lambda.
* Sample Output:

```text
Monday
Friday
Sunday
```

<a href="javascript:exerciseToggle(14)">Answer</a>
<div id="exercise14" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static var weekdays = #['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']

		static def main {
			var result = weekdays.filter[it.length == 6].toList
			println(result)
		}

	}
[:End:]
</div>

## Exercise 15

* Write a SARL program to add two given lists using map and lambda.
* Original list:

```text
[1, 2, 3]
[4, 5, 6]
```

* Result: after adding two list `[5, 7, 9]`

<a href="javascript:exerciseToggle(15)">Answer</a>
<div id="exercise15" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {

		static var content1 : List<Integer> = #[1, 2, 3]
		static var content2 : List<Integer> = #[4, 5, 6]

		static def main {
			var result : List<Integer>
			if (content1.size > content2.size) {
				var iter = content1.iterator
				result = content2.map[it.intValue + iter.next.intValue]
			} else {
				var iter = content2.iterator
				result = content1.map[it.intValue + iter.next.intValue]
			}
			println(result)
		}

	}
[:End:]
</div>

## Exercise 16

* Write a SARL program to find the second lowest total marks of any student(s) from the given names and marks of each student using lists and lambda. Input the number of students, the names and grades of each student.
* Names and Grades of all students: `[['S ROY', 1.0], ['B BOSE', 3.0], ['N KAR', 2.0], ['C DUTTA', 1.0], ['G GHOSH', 1.0]]`

```text
Second lowest grade: 2.0
Names: N KAR
```

<a href="javascript:exerciseToggle(16)">Answer</a>
<div id="exercise16" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {

		static var marks : List<List<Object>> = #[
			#['S ROY', 1.0],
			#['B BOSE', 3.0],
			#['N KAR', 2.0],
			#['C DUTTA', 1.0],
			#['G GHOSH', 1.0]
		]

		static def main {
			var result = marks
				.sortWith [a, b | (b.get(1) as Double) <=> (a.get(1) as Double)]
			var firstMark = result.get(0).get(1) as Double
			var secondMark : Double
			var studentsNames = newArrayList
			for (student : result) {
				var m = student.get(1) as Double
				if (secondMark === null) {
					if (m != firstMark) {
						secondMark = m 
						studentsNames += student.get(0) as String
					}
				} else if (m == secondMark) {
					studentsNames += student.get(0) as String
				} else {
					break;					
				}
			}
			println("Second lowest grade: " + secondMark)
			println("Names: " + studentsNames)
		}

	}
[:End:]
</div>


## Exercise 17

* Write a SARL program to find numbers divisible by nineteen or thirteen from a list of numbers using Lambda.
* Orginal list: `[19, 65, 57, 39, 152, 639, 121, 44, 90, 190]`
* Numbers of the above list divisible by nineteen or thirteen: `[19, 65, 57, 39, 152, 190]`

<a href="javascript:exerciseToggle(17)">Answer</a>
<div id="exercise17" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static var original = #[19, 65, 57, 39, 152, 639, 121, 44, 90, 190]

		static def main {
			var result = original.filter [(it % 19) == 0 || (it % 13) == 0].toList
			println("Orginal list: " + original)
			println("Numbers of the above list divisible by nineteen or thirteen: " + result)
		}

	}
[:End:]
</div>


## Exercise 18

* Write a SARL program to find palindromes in a given list of strings using Lambda.
* Orginal list of strings: `['php', 'w3r', 'SARL', 'abcd', 'Java', 'aaa']`
* List of palindromes: `['php', 'aaa']`

<a href="javascript:exerciseToggle(18)">Answer</a>
<div id="exercise18" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static var original = #['php', 'w3r', 'SARL', 'abcd', 'Java', 'aaa']

		static def main {
			var result = original.filter[it.palindrome].toList
			println("List of palindromes: " + result)
		}
	
		static def palindrome(value : String) : boolean {
			var i = 0
			var j = value.length - 1
			while (i < j) {
				if (value.charAt(i) != value.charAt(j)) {
					return false
				}
			}
			return true
		}

	}
[:End:]
</div>

## Exercise 19

* Write a SARL program to find all anagrams of a string in a given list of strings using Lambda.
* Original list of strings: `['bcda', 'abce', 'cbda', 'cbea', 'adcb']`
* Anagrams of 'abcd' in the above string: `['bcda', 'cbda', 'adcb']`

<a href="javascript:exerciseToggle(19)">Answer</a>
<div id="exercise19" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.Map
	class Solution {

		static var original = #['bcda', 'abce', 'cbda', 'cbea', 'adcb']

		static def main {
			var search_for = "abcd".counter
			var result = original.filter[
				(search_for == it.counter)
			].toList 
			println("List of palindromes: " + result)
		}

		static def counter(value : String) : Map<Character, Integer> {
			var counter = newHashMap
			for (c : value.bytes) {
				var n = counter.getOrDefault(c as char, 0)
				n++
				counter.put(c as char, n) 
			}
			return counter
		}
	}
[:End:]
</div>

## Exercise 20

* Write a SARL program to find the numbers in a given string and store them in a list. Afterward, display the numbers that are longer than the length of the list in sorted form. Use the lambda function to solve the problem.
* Original string: `sdf 23 safs8 5 sdfsd8 sdfs 56 21sfs 20 5`
* Numbers in sorted form: `20 23 56`

<a href="javascript:exerciseToggle(20)">Answer</a>
<div id="exercise20" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static var original = "sdf 23 safs8 5 sdfsd8 sdfs 56 21sfs 20 5"

		static def main {
			var words = original.split("\\s+")
			var numbers = words.filter[isInteger].map[it as Integer].sort.toList
			println("Numbers in sorted form: " + numbers)
			var length = words.size
			var result = numbers.filter[it > length].toList
			println(result.join(' '))
   		}

		static def isInteger(value : String) : boolean {
			try {
				Long::parseLong(value)
				return true
			} catch (ex : Throwable) {
				return false
			}
		}
	}
[:End:]
</div>

## Exercise 21

* Write a SARL program that multiplies each number in a list with a given number using lambda functions. Print the results.
* Original list: `[2, 4, 6, 9, 11]`
* Given number: `2`
* Result: `4 8 12 18 22`

<a href="javascript:exerciseToggle(21)">Answer</a>
<div id="exercise21" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {

		static var original = #[2, 4, 6, 9, 11]

		static def main {
			var search_for = 2
			var result = original.map[it * search_for].toList
			println(result)			
   		}
	}
[:End:]
</div>

## Exercise 22

* Write a SARL program that sums the length of a list of names after removing those that start with lowercase letters. Use the lambda function.
* Result: `16`

<a href="javascript:exerciseToggle(22)">Answer</a>
<div id="exercise22" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import static extension java.lang.Character.isUpperCase
	class Solution {

		static var original = #['sally', 'Dylan', 'rebecca', 'Diana', 'Joanne', 'keith']

		static def main {
			var result = original.filter[it.charAt(0).isUpperCase].map[it.length]
				.reduce[accumulator, current | accumulator + current]
			println(result)
   		}
	}
[:End:]
</div>

## Exercise 23

* Write a SARL program to calculate the sum of the positive and negative numbers of a given list of numbers using the lambda function.
* Original list: `[2, 4, -6, -9, 11, -12, 14, -5, 17]`
* Sum of the positive numbers: `-32`
* Sum of the negative numbers: `48`

<a href="javascript:exerciseToggle(23)">Answer</a>
<div id="exercise23" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {

		static var original = #[2, 4, -6, -9, 11, -12, 14, -5, 17]

		static def main {
			var positive = original.filter[it >= 0].sum
			var negative = original.filter[it < 0].sum
			println("Sum of the positive numbers: " + positive)
			println("Sum of the negative numbers: " + negative)
   		}
 
 		static def sum(value : Iterable<Integer>) : int {
 			var sum = 0
 			for (c : value) {
 				sum += c
 			}
 			return sum
 		}
	}
[:End:]
</div>


## Exercise 24

* Write a SARL program to find numbers within a given range where every number is divisible by every digit it contains.
* Input Range: `1..22`
* Sample Output: `[1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 15, 22]`

<a href="javascript:exerciseToggle(24)">Answer</a>
<div id="exercise24" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {
		static def divisible_by_digits(start_num : int, end_num : int) : List<Integer> {
			(start_num..end_num).filter[n |
				((n as String).bytes).forall[digit | (n%(digit as int)) == 0]
			].toList
		}

		static def main {
			println(divisible_by_digits(1,22))
   		}
	}
[:End:]
</div>

## Exercise 25

* Write a SARL program to create the next bigger number by rearranging the digits of a given number.
* Original number: `12`
* Next bigger number: `21`
* Original number: `10`
* Next bigger number: `null`
* Original number: `201`
* Next bigger number: `210`
* Original number: `102`
* Next bigger number: `120`
* Original number: `445`
* Next bigger number: `454`

<a href="javascript:exerciseToggle(25)">Answer</a>
<div id="exercise25" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	class Solution {
		static def rearrange_bigger(n : int) : Integer {
		    var nums = (n as String).bytes.map[it as Integer]
		    for (i : (nums.size-2)..0) {
		        if (nums.get(i) < nums.get(i+1)) {
		            var z = nums.subList(i, nums.size)
		            var y = z.filter[it > z.get(0)].min
		            z -= y
		            z = z.sort
		            var new_nums = (nums.subList(0, i) + #[y] + z)
		            return (new_nums.join("")) as Integer
				}
			}
		    return null
		}

		static def dotest(n : int) {
			println("Original number: " + n)
			println("Next bigger number: " + rearrange_bigger(n))
		}

		static def main {
			dotest(12)
			dotest(10)
			dotest(201)
			dotest(102)
			dotest(445)
   		}
	}
[:End:]
</div>

## Exercise 26

* Write a SARL program to find a list with maximum and minimum length using lambda.
* Original list: `[[0], [1, 3], [5, 7], [9, 11], [13, 15, 17]]`
* List with maximum length of lists: `(3, [13, 15, 17])`
* List with minimum length of lists: `(1, [0])`

<a href="javascript:exerciseToggle(26)">Answer</a>
<div id="exercise26" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {
		static var original = #[#[0], #[1, 3], #[5, 7], #[9, 11], #[13, 15, 17]]
	
		static def max_length_list(input_list : List<List<Integer>>) : List<Object> {
		    var max_length = input_list.map[it.size].max 
		    var max_list = input_list.filter[it.size == max_length].toList    
		    return #[max_length, max_list]
		}
    
		static def min_length_list(input_list : List<List<Integer>>) : List<Object> {
		    var min_length = input_list.map[it.size].min
		    var min_list = input_list.filter[it.size == min_length].toList    
		    return #[min_length, min_list]
		}
      
		static def main {
			println("Original list: " + original)
			println("List with maximum length of lists: " + max_length_list(original))
			println("List with minimum length of lists: " + min_length_list(original))
   		}
	}
[:End:]
</div>

## Exercise 27

* Write a SARL program to sort each sublist of strings in a given list of lists using lambda.
* Original list: `[['green', 'orange'], ['black', 'white'], ['white', 'black', 'orange']]`
* After sorting each sublist of the said list of lists: `[['green', 'orange'], ['black', 'white'], ['black', 'orange', 'white']]`

<a href="javascript:exerciseToggle(27)">Answer</a>
<div id="exercise27" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {

		static var original = #[
			#['green', 'orange'],
			#['black', 'white'],
			#['white', 'black', 'orange']
		]
	
		static def main {
			var result = original.map[it.sort]
			println(result)
   		}
	}
[:End:]
</div>

## Exercise 28

* Write a SARL program to sort a given list of lists by length and value using lambda.
* Original list: `[[2], [0], [1, 3], [0, 7], [9, 11], [13, 15, 17]]`
* Sort the list of lists by length and value: `[[0], [2], [0, 7], [1, 3], [9, 11], [13, 15, 17]]`

<a href="javascript:exerciseToggle(28)">Answer</a>
<div id="exercise28" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {

		static var original = #[
			#[2], #[0], #[1, 3], #[0, 7], #[9, 11], #[13, 15, 17]
		]
	
		static def main {
			var result = original.map[it.sort].sortWith[a, b | a <=> b] 
			println(result)
   		}

		static def operator_spaceship(a : List<Integer>, b : List<Integer>) : int {
			var cmp = a.size <=> b.size
			if (cmp != 0) {
				return cmp
			}
			var i0 = a.iterator
			var i1 = b.iterator
			while (i0.hasNext && i1.hasNext) {
				var va = i0.next
				var vb = i1.next
				cmp = va <=> vb
				if (cmp != 0) {
					return cmp
				}
			}
			return 0
		}
	}
[:End:]
</div>

## Exercise 29

* Write a SARL program to find the maximum value in a given heterogeneous list using lambda.
* Original list: `['SARL', 3, 2, 4, 5, 'version']`
* Maximum values in the said list using lambda: `5`

<a href="javascript:exerciseToggle(29)">Answer</a>
<div id="exercise29" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {

		static var original = #['SARL', 3, 2, 4, 5, 'version']
	
		static def main {
			var result = original.filter[it instanceof Number].map[(it as Number).longValue].max
			println(result)
   		}
	}
[:End:]
</div>

## Exercise 30

* Write a SARL program to sort a given matrix in ascending order according to the sum of its rows using lambda.
* Original Matrix: `[[1, 2, 3], [2, 4, 5], [1, 1, 1]]`
* Sort the said matrix in ascending order according to the sum of its rows: `[[1, 1, 1], [1, 2, 3], [2, 4, 5]]`
* Original Matrix: `[[1, 2, 3], [-2, 4, -5], [1, -1, 1]]`
* Sort the said matrix in ascending order according to the sum of its rows: `[[-2, 4, -5], [1, -1, 1], [1, 2, 3]]`

<a href="javascript:exerciseToggle(30)">Answer</a>
<div id="exercise30" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.lambdaexercises
	[:OnHtml]
	import java.util.List
	class Solution {

		static var original1 = #[
			#[1, 2, 3], #[2, 4, 5], #[1, 1, 1]
		]
	
		static var original2 = #[
			#[1, 2, 3], #[-2, 4, -5], #[1, -1, 1]
		]

		static def sort_matrix(m : List<List<Integer>>) : List<List<Integer>> {
			m.sortWith[a, b | a.sum <=> b.sum]
		}

		static def sum(m : List<Integer>) : int {
			m.reduce[accumulator, current | accumulator + current]
		}

		static def main {
			println(sort_matrix(original1))
			println(sort_matrix(original2))
   		}
	}
[:End:]
</div>

[:Include:](../../includes/legal.inc)
