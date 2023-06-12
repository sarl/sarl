# Time Capacity

[:Outline:]

The built-in capacity [:timecap:] provides tools for obtaining the current time from the run-time platform.

Time definition is application-dependent and platform-dependent. In other words,
the time values replied by this capacity depends on the run-time environment:
it may be the operating system time, or a simulator time.

<!--- Test that all the documented functions are defined in the capacity, and no function is missed to be
      documented --> 
[:Fact:]{typeof(io.sarl.core.[:timecap](Time)).shouldHaveMethods(
	"[:fctgettime](getTime)(java.util.concurrent.TimeUnit) : double",
	"getTime : double",
	"[:fctgetostimefactor](getOSTimeFactor) : double",
	"[:fcttoostime](toOSTime)(double) : double",
	"[:fctfromostime](fromOSTime)(double) : double",
	"[:fcttoosduration](toOSDuration)(double) : double",
	"[:fctfromosduration](fromOSDuration)(double) : double")
}


## Get the Current Time

For obtaining the current time, the [:fctgettime:] function is provides by the [:timecap:] capacity:

[:Success:]
	package io.sarl.docs.reference.bic
	import java.util.concurrent.TimeUnit
	interface Tmp {
	[:On]
		def [:fctgettime!](timeUnit : TimeUnit = null) : double
	[:Off]
	}
[:End:]


The timeUnit parameter will enable you to specify the unit of the replied
value (hours, seconds, milliseconds, etc.). If it is not provided,
the values will be expressed in seconds.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Time
	import io.sarl.core.Logging
	import java.util.concurrent.TimeUnit
	[:On]
	agent A {
		uses [:timecap!]
		def myaction {
			var ct = [:fctgettime!]
			var ct2 = [:fctgettime!](null)
			var ct3 = [:fctgettime!](TimeUnit::HOURS)
		}
	}
[:End:]


## Convertion from Operating System Time to the SARL Run-time Time

At least, there is two different time scales:

* Operating System (OS) Time: it is the time of the operating system, and accessible from objects provided by the Java virtual machine for example.
* SARL Run-time Time: it is the time that is considered by the SARL run-time environment, and accessible from the `Time` capacity.


The function [:fctgetostimefactor:] provides the multiplication factor between these two time scales.
The functions [:fcttoostime:] and [:fctfromostime:] do the convertion of a time between these two scales.
The functions [:fcttoosduration:] and [:fctfromosduration:] do the convertion of a duration between these two scales.


### Get the Factor for Mapping to Operating-System Time

For the time replied by the [:fctgettime:] function may be not the operating-system time, e.g. a simulation time.
In order to map the time replied by the [:fctgettime:] function and the operating-system time, the
[:fctgetostimefactor:] function replies a factor that could be applied so that:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Time
	agent Tmp {
		uses Time
		def action {
			var
	[:On]
			operatingSystemTime = [:fctgetostimefactor!] * [:fctgettime!]
	[:Off]
		}
	}
[:End:]


The provided function is:

[:Success:]
	package io.sarl.docs.reference.bic
	import java.util.concurrent.TimeUnit
	interface Tmp {
	[:On]
		def [:fctgetostimefactor!] : double
	[:Off]
	}
[:End:]


### Convertion of a Time

For converting a time, you have access to two functions:

* [:fcttoostime:]: converts a OS time to its equivalent time into the SARL run-time environment. 
* [:fctfromostime:] converts a SARL run-time time to its equivalent into the OS.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Time
	agent Tmp {
		uses Time
		def action {
			var operatingSystemTime = 1.0
			var sreTime = 1.0
	[:On]
			// Convertion from OS to SRE
			sreTime = fromOSTime(operatingSystemTime)
			// Convertion from SRE to OS
			operatingSystemTime = toOSTime(sreTime)
	[:Off]
		}
	}
[:End:]


### Convertion of a Duration

For converting a duration, you have access to two functions:

* [:fcttoosduration:]: converts a OS duration to its equivalent time into the SARL run-time environment. 
* [:fctfromosduration:] converts a SARL run-time duration to its equivalent into the OS.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Time
	agent Tmp {
		uses Time
		def action {
			var operatingSystemTime = 1.0
			var sreTime = 1.0
	[:On]
			// Convertion from OS to SRE
			sreTime = fromOSDuration(operatingSystemTime)
			// Convertion from SRE to OS
			operatingSystemTime = toOSDuration(sreTime)
	[:Off]
		}
	}
[:End:]


[:Include:](../../legal.inc)
