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
			"[:fctgetostimefactor](getOSTimeFactor) : double")
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


## Get the Factor for Mapping to Operating-System Time

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



[:Include:](../../legal.inc)
