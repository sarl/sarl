# SARL Basic - Answers

[:Outline:]

## Exercise 1

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent Solution {
		on Initialize {
			println("Twinkle, twinkle, little star,\n\tHow I wonder what you are!\n\t\tUp above the world so high,\n\t\tLike a diamond in the sky.\n\tTwinkle, twinkle, little star,\ntHow I wonder what you are")
		}
	}
[:End:]

## Exercise 2

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import java.util.Date
	agent Solution {
		on Initialize {
			var dt = new Date
			println(dt)
		}
	}
[:End:]

## Exercise 3

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var r = occurrence.parameters.get(0) as Double
			var area = Math::PI * r**2
			println("r = " + r)
			println("Area = " + area)
		}
	}
[:End:]

## Exercise 4

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var firstname = occurrence.parameters.get(0) as String
			var lastname = occurrence.parameters.get(1) as String
			println(lastname + " " + firstname)
		}
	}
[:End:]

## Exercise 5

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var numberListString = occurrence.parameters.get(0) as String
			var numberArray = numberListString.split("\\s*,\\s*")
			var numberList = newArrayList(numberArray)
			println("List : " + numberList)
		}
	}
[:End:]

## Exercise 6

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var filename = occurrence.parameters.get(0) as String
			var idx = filename.indexOf('.')
			if (idx >= 0) {
				println(filename.substring(idx + 1))
			}
		}
	}
[:End:]

## Exercise 7

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var color_list = #["Red", "Green", "White", "Black"]
			println(color_list.get(0))
			println(color_list.get(color_list.length - 1))
		}
	}
[:End:]

## Exercise 8

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var n = occurrence.parameters.get(0) as Integer
			var value = n + n**2 + n**3
			println("Result : " + value)
		}
	}
[:End:]

## Exercise 9

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import java.util.GregorianCalendar
	agent A {
		on Initialize {
			var month = occurrence.parameters.get(0) as Integer
			var year = occurrence.parameters.get(1) as Integer
			var cal = new GregorianCalendar(year, month, 1)
			println(cal)
		}
	}
[:End:]

## Exercise 10

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import java.time.LocalDate
	import java.time.temporal.ChronoUnit
	agent A {
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

## Exercise 11

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var volume = 4/3 * Math::PI * 6**3;
			println(volume)
		}
	}
[:End:]

## Exercise 12

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 13

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import static java.lang.Math.*
	agent A {
		on Initialize {
			var n = occurrence.parameters.get(0) as Integer
			var result = ((abs(1000 - n) <= 100) || (abs(2000 - n) <= 100))
			println(result)
		}
	}
[:End:]

## Exercise 14

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 15

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var str = occurrence.parameters.get(0) as String
			if (!str.startsWith("Is ")) {
				str = "Is " + str
			}
			println(str)
		}
	}
[:End:]

## Exercise 16

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 17

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 18

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 19

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 20

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 21

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		val group = newArrayList(1, 5, 8, 3)
		
		on Initialize {
			var value = occurrence.parameters.get(0) as Integer
			println(value + " -> " + group + " : " + group.contains(value))
		}
	}
[:End:]

## Exercise 22

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 23

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 24

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 25

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var base = occurrence.parameters.get(0) as Double
			var height = occurrence.parameters.get(1) as Double
			var area = (base * height) / 2
			println("Area = " + area)
		}
	}
[:End:]

## Exercise 26

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 27

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import static java.lang.Math.*
	agent A {
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

## Exercise 28

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 29

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 30

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 31

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 32

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 33

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 34

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import java.io.File
	agent A {
		on Initialize {
			var filename = occurrence.parameters.get(0) as String
			var file = new File(filename)
			var isExist = file.exists
			println(isExist);
		}
	}
[:End:]

## Exercise 35

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 36

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import java.io.File
	agent A {
		on Initialize {
			var folderName = occurrence.parameters.get(0) as String
			var folder = new File(folderName)
			var list = folder.listFiles
			println(list)
		}
	}
[:End:]


## Exercise 37

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var value = occurrence.parameters.get(0) as String
			value = value.replaceAll("[\n ]+", "")
			println(value)
		}
	}
[:End:]

## Exercise 38

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import io.sarl.core.Logging
	agent A {
		uses Logging
		on Initialize {
			var value = occurrence.parameters.get(0) as String
			error(value)
		}
	}
[:End:]

## Exercise 39

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var variableName = occurrence.parameters.get(0) as String
			println(System::getProperty(variableName))
		}
	}
[:End:]

## Exercise 40

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			println(System::getProperty("user.name"))
		}
	}
[:End:]

## Exercise 41

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 42

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var height = occurrence.parameters.get(0) as Double
			height = 2.54 * height
			println(height)
		}
	}
[:End:]

## Exercise 43

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var side1 = occurrence.parameters.get(0) as Double
			var side2 = occurrence.parameters.get(1) as Double
			var hypotenuse = Math::sqrt(side1**2 + side2**2)
			println(hypotenuse)
		}
	}
[:End:]

## Exercise 44

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import static java.util.concurrent.TimeUnit.*
	agent A {
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

## Exercise 45

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import java.io.File
	agent A {
		on Initialize {
			var name = occurrence.parameters.get(0) as String
			var file = new File(name)
			var absoluteFilename = file.absolutePath
			println(absoluteFilename)
		}
	}
[:End:]

## Exercise 46

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 47

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var weight = occurrence.parameters.get(0) as Double
			var height = occurrence.parameters.get(1) as Double
			var bmi = weight / height**2
			println(bmi)
		}
	}
[:End:]

## Exercise 48

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 49

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 50

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import static java.lang.Math.*
	agent A {
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

## Exercise 51

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var all = ""
			for (p : occurrence.parameters) {
				all += p as String
			}
			println(all)
		}
	}
[:End:]

## Exercise 52

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import java.util.stream.Collectors
	import static extension java.util.Arrays.*
	agent A {
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

## Exercise 53

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import static extension java.util.Arrays.*
	agent A {
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

## Exercise 54

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
		on Initialize {
			var c = occurrence.parameters.get(0) as Character
			var value = occurrence.parameters.get(1) as String
			var count = value.chars.filter[it == c].count
			println(count)
		}
	}
[:End:]

## Exercise 55

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	import java.io.File
	agent A {
		on Initialize {
			var filename = occurrence.parameters.get(0) as String
			var file = new File(filename)
			println(file.isFile || file.isDirectory)
		}
	}
[:End:]

## Exercise 56

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 57

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 58

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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

## Exercise 59

[:Success:]
	package io.sarl.docs.tutorials.baseexercises
	[:On]
	import io.sarl.core.Initialize
	agent A {
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


[:Include:](../legal.inc)
