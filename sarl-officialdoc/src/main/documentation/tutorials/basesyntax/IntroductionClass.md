# Introduction to Object-Oriented Programming with SARL

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

* Write a SARL program to create a class and display the package (or namespace) of that class.

<a href="javascript:exerciseToggle(1)">Answer</a>
<div id="exercise1" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
	class MyClass {
	}
	class Solution {
		static def main {
			println(typeof(MyClass).^package.name)
		}
	}
[:End:]
</div>

## Exercise 2

* Write a SARL program to create an instance of a specified class and display the package (or namespace) of the said instance.

<a href="javascript:exerciseToggle(2)">Answer</a>
<div id="exercise2" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
	class MyClass {
	}
	class Solution {
		static def main {
			var obj = new MyClass
			println(obj.class.^package.name)
		}
	}
[:End:]
</div>

## Exercise 3

* Write a SARL program that imports the `abs()` function from the `java.lang.Math` class using the `import` statement, and finds the absolute value of `-155`.

<a href="javascript:exerciseToggle(3)">Answer</a>
<div id="exercise3" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
	import static java.lang.Math.abs
	class Solution {
		static def main {
			println(abs(-155))
		}
	}
[:End:]
</div>

## Exercise 4

* Define a class that is repsenting a student, with her/her name and age as attributes, and the associated getter/setter functions.

<a href="javascript:exerciseToggle(4)">Answer</a>
<div id="exercise4" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
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
</div>

## Exercise 5

* Define a class that is repsenting a student, with her/her name and age as attributes, *without* implementing the getter/setter functions.
* The getter/setter functions must be automatically generatd by the SARL compiler.

<a href="javascript:exerciseToggle(5)">Answer</a>
<div id="exercise5" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
	import org.eclipse.xtend.lib.annotations.Accessors
	class Student {
		@Accessors
		var age : int
		@Accessors
		var name : String
	}
[:End:]
</div>

## Exercise 6

* Write a SARL program to create two empty classes, `Student` and `Marks`. Now create some instances and check whether they are instances of the said classes or not. Also, check whether the said classes are subclasses of the built-in object class or not.

<a href="javascript:exerciseToggle(6)">Answer</a>
<div id="exercise6" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
	class Student {
	}
	class Mark {
	}
	class Solution {
		static def main {
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
</div>

## Exercise 7

* Write a SARL class named Student with two instances student1, student2 and assign values to the instances' attributes. Print all the attributes of the student1, student2 instances with their values.

<a href="javascript:exerciseToggle(7)">Answer</a>
<div id="exercise7" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
	import org.eclipse.xtend.lib.annotations.Accessors
	class Student {
		@Accessors
		var age : int
		@Accessors
		var name : String
	}
	class Solution {
		static def main {
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
</div>

## Exercise 8

* Define a class that is representing a student, with her/her name, age and marks as attributes, and the associated getter/setter functions.
* Define a second class named `Main` that is creating an instance of the student class.
* How to simulate the addition of the method `s.means()`, that is computing the mean of the marks of the student `s` **without changing** the code of the student class?

<a href="javascript:exerciseToggle(8)">Answer</a>
<div id="exercise8" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
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
		static def main : void {
			var s = new Student
			println(s.means)
		}
		static def means(student : Student) : float {
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
</div>

## Exercise 9

* Write a SARL class to convert an integer to a Roman numeral.

<a href="javascript:exerciseToggle(9)">Answer</a>
<div id="exercise9" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
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
		static def main {
			println(IntRomanConverter::convert(1))
			println(IntRomanConverter::convert(1994))
		}
	}
[:End:]
</div>

## Exercise 10

* Write a SARL class to convert a Roman numeral to an integer.

<a href="javascript:exerciseToggle(10)">Answer</a>
<div id="exercise10" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
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
		static def main {
			println(RomanIntConverter::convert("I"))
			println(RomanIntConverter::convert("MCMXCIV"))
		}
	}
[:End:]
</div>

## Exercise 11

* Write a SARL class, named `Vector` that is representing a 2D vector of values `(x, y)`. Implements the function that enables to sum two vectors, named `add()`.
* Input Vectors: `(x=124, y=45)` and `(x=-456, y=78)`
* Output Vector: `(x=-332, y=123)`

<a href="javascript:exerciseToggle(11)">Answer</a>
<div id="exercise11" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
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
		static def main {
			var a = new Vector(124, 45)
			var b = new Vector(-456, 78)
			var c = a.add(b)
			println(c)
			var d = b.add(a)
			println(d)
		}
	}
[:End:]
</div>

## Exercise 12

* Update the class, named `Vector`, with the definition of the operation `+` that sums two vectors. Use this operator to sum two vectors instead of calling the `add()` function.
* Input Vectors: `(x=124, y=45)` and `(x=-456, y=78)`
* Output Vector: `(x=-332, y=123)`

<a href="javascript:exerciseToggle(12)">Answer</a>
<div id="exercise12" style="display:none;">
[:Success:]
	package io.sarl.docs.tutorials.classexercises
	[:OnHtml]
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
		static def main {
			var a = new Vector(124, 45)
			var b = new Vector(-456, 78)
			var c = a + b
			println(c)
			var d = b + a
			println(d)
		}
	}
[:End:]
</div>

[:Include:](../../includes/legal.inc)
