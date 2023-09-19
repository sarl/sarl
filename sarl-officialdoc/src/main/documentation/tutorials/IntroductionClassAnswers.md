# Introduction to Object-Oriented Classes with SARL - Answers

[:Outline:]

## Exercise 1

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	class MyClass {
	}
	class Solution {
		def main(args : String*) {
			println(typeof(MyClass).^package.name)
		}
	}
[:End:]

## Exercise 2

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	class MyClass {
	}
	class Solution {
		def main(args : String*) {
			var obj = new MyClass
			println(obj.class.^package.name)
		}
	}
[:End:]

## Exercise 3

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	import static java.lang.Math.abs
	class Solution {
		def main(args : String*) {
			println(abs(-155))
		}
	}
[:End:]

## Exercise 4

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	class Student {
		var age : int
		var name : String

		def getAge : int {
			this.age
		}
		def setAgent(age : int) {
			this.age = age
		}

		def getName : String {
			this.name
		}
		def setName(name : String) {
			this.name = name
		}
	}
[:End:]


## Exercise 5

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	import org.eclipse.xtend.lib.annotations.Accessors
	class Student {
		@Accessors
		var age : int
		@Accessors
		var name : String
	}
[:End:]

## Exercise 6

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	class Student {
	}
	class Mark {
	}
	class Solution {
		def main(args : String*) {
			var a = new Student
			var b = new Mark
			println("a is Student class = " + (a instanceof Student))
			println("a is Mark class = " + (a instanceof Mark))
			println("a is Object class = " + (a instanceof Object))
			println("b is Student class = " + (b instanceof Student))
			println("b is Mark class = " + (b instanceof Mark))
			println("b is Object class = " + (b instanceof Object))
		}
	}
[:End:]

## Exercise 7

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	import org.eclipse.xtend.lib.annotations.Accessors
	class Student {
		@Accessors
		var age : int
		@Accessors
		var name : String
	}
	class Solution {
		def main(args : String*) {
			var student1 = new Student
			student1.setAge(15)
			student1.setName("First name")

			var student2 = new Student
			student2.age = 18
			student2.name = "Second name"
			
			println("Student1 age = " + student1.getAge)
			println("Student1 name = " + student1.getName)

			println("Student2 age = " + student2.age)
			println("Student2 name = " + student2.name)
		}
	}
[:End:]

## Exercise 8

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	import java.util.List
	import org.eclipse.xtend.lib.annotations.Accessors
	class Student {
		@Accessors
		var age : int
		@Accessors
		var name : String
		@Accessors
		val marks : List<Float> = newArrayList
	}
	class Main {
		def main(args : String*) {
			var s = new Student
			println(s.means)
		}
		def means(student : Student) : float {
			var mean = 0f
			if (!student.marks.empty) {
				for (mark : student.marks) {
					mean += mark
				}
				mean /= student.marks.size
			}
			return mean
		}
	}
[:End:]

## Exercise 9

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	class IntRomanConverter {
		static val coefs = #[1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1 ]
		static val symbols = #[ "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I" ]
		
		static def convert(num : int) : String {
			var acc = num
			var roman = ""
			var i = 0
			while  (acc > 0) {
				for (x : 0..<(acc / coefs.get(i))) {
					roman += symbols.get(i)
					acc -= coefs.get(i)
				}
				i++
			}
			return roman
		}
	}
	class Main {
		def main(args : String*) {
			println(IntRomanConverter::convert(1))
			println(IntRomanConverter::convert(1994))
		}
	}
[:End:]

## Exercise 10

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	class RomanIntConverter {
		static val coefs = #{'I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000}
		
		static def convert(roman : String) : int {
			var num = 0
			for (i : 0..<roman.length) {
				if (i > 0 && coefs.get(roman.charAt(i)) > coefs.get(roman.charAt(i - 1))) {
					num += coefs.get(roman.charAt(i)) - 2 * coefs.get(roman.charAt(i - 1))
				} else {
					num += coefs.get(roman.charAt(i))
				}
			}
    		return num
   		}
	}
	class Main {
		def main(args : String*) {
			println(RomanIntConverter::convert("I"))
			println(RomanIntConverter::convert("MCMXCIV"))
		}
	}
[:End:]

## Exercise 11

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	import org.eclipse.xtend.lib.annotations.Accessors
	class Vector {
		@Accessors
		var x : double
		@Accessors
		var y : double

		new (x : double, y : double) {
			this.x = x
			this.y = y
		}

		new {
			x = 0
			y = 0
		}
		
		def add(v : Vector) : Vector {
			new Vector(this.x + v.x, this.y + v.y)
		}

		def toString : String {
			"(x=" + x + ", y=" + y + ")"
		}
	}
	class Main {
		def main(args : String*) {
			var a = new Vector(124, 45)
			var b = new Vector(-456, 78)
			var c = a.add(b)
			println(c)
			var d = b.add(a)
			println(d)
		}
	}
[:End:]

## Exercise 12

[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:On]
	import org.eclipse.xtend.lib.annotations.Accessors
	class Vector {
		@Accessors
		var x : double
		@Accessors
		var y : double

		new (x : double, y : double) {
			this.x = x
			this.y = y
		}

		new {
			x = 0
			y = 0
		}
		
		def operator_plus(v : Vector) : Vector {
			new Vector(this.x + v.x, this.y + v.y)
		}

		def toString : String {
			"(x=" + x + ", y=" + y + ")"
		}
	}
	class Main {
		def main(args : String*) {
			var a = new Vector(124, 45)
			var b = new Vector(-456, 78)
			var c = a + b
			println(c)
			var d = b + a
			println(d)
		}
	}
[:End:]



[:Include:](../legal.inc)
