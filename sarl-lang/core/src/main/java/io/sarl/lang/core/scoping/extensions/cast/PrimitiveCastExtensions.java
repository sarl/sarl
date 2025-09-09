/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.core.scoping.extensions.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static functions related to the casting of primitives that are not numbers.
 *
 * @author $Author: sgalland$
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.9
 */
public final class PrimitiveCastExtensions {

	private PrimitiveCastExtensions() {
		//
	}

	/** Convert the given value to {@code boolean}.
	 *
	 * <p>See {@link Integer#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code boolean} type.
	 */
	@Pure
	@Inline(value = "$2.parseBoolean(($1).toString())", imported = Boolean.class)
	public static boolean booleanValue(CharSequence value) {
		return value != null && Boolean.parseBoolean(value.toString());
	}

	/** Convert an {@code Object} to a primitive {@code boolean}.
	 *
	 * <p>If the given object is a boolean, the boolean value is replied.
	 * Otherwise, the given value is converted to String and then to boolean.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code boolean} type.
	 * @since 0.13
	 */
	@Pure
	public static boolean booleanValue(Object value) {
		if (value instanceof Boolean bvalue) {
			return bvalue.booleanValue();
		}
		if (value != null) {
			return booleanValue(value.toString());
		}
		return false;
	}

	/** Convert the given value to {@code AtomicBoolean} into its {@code boolean value}.
	 *
	 * <p>See {@link Integer#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code boolean} type.
	 * @since 0.12
	 */
	@Pure
	@Inline(value = "($1).get()", imported = Boolean.class)
	public static boolean booleanValue(AtomicBoolean value) {
		return value != null && value.get();
	}

	/** Convert the given value to {@code String}.
	 *
	 * @param value a value of {@code boolean} type.
	 * @return the equivalent value to {@code value} of {@code String} type.
	 */
	@Pure
	@Inline(value = "$2.toString($1)", imported = Boolean.class)
	public static String toString(boolean value) {
		return Boolean.toString(value);
	}

	/** Convert the given value to {@code String}.
	 *
	 * @param value a value of {@code char} type.
	 * @return the equivalent value to {@code value} of {@code String} type.
	 */
	@Pure
	@Inline(value = "$2.toString($1)", imported = Character.class)
	public static String toString(char value) {
		return Character.toString(value);
	}

	/** Decodes a {@code CharSequence} into a {@code byte}.
	 *
	 * <p>In opposite to the functions of {@link Byte}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Byte#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code byte} type.
	 * @since 0.9
	 * @see Byte#decode(String)
	 */
	@Pure
	public static byte byteValue(CharSequence value) {
		try {
			return Byte.decode(value.toString()).byteValue();
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
	}

	/** Convert an {@code Object} to a primitive {@code byte}.
	 *
	 * <p>If the given object is a number, the byte value is replied.
	 * Otherwise, the given value is converted to String and then to byte.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code byte} type.
	 * @since 0.13
	 */
	@Pure
	public static byte byteValue(Object value) {
		if (value instanceof Number cvalue) {
			return cvalue.byteValue();
		}
		if (value != null) {
			return byteValue(value.toString());
		}
		return 0;
	}

	/** Convert an {@code Object} to a {@code Byte}.
	 *
	 * <p>If the given object is a number, the byte value is replied.
	 * Otherwise, the given value is converted to String and then to byte.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code Byte} type.
	 * @since 0.13
	 */
	@Pure
	public static Byte toByte(Object value) {
		if (value instanceof Byte cvalue) {
			return cvalue;
		}
		final byte bvalue;
		if (value instanceof Number cvalue) {
			bvalue = cvalue.byteValue();
		} else if (value != null) {
			return Byte.valueOf(toByte(value.toString()));
		} else {
			bvalue = 0;
		}
		return Byte.valueOf(bvalue);
	}

	/** Decodes a {@code CharSequence} into a {@code Byte}.
	 *
	 * <p>In opposite to the functions of {@link Byte}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Byte#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code Byte} type.
	 * @since 0.12
	 * @see Byte#decode(String)
	 */
	@Pure
	public static byte toByte(CharSequence value) {
		try {
			return Byte.decode(value.toString()).byteValue();
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
	}

	/** Decodes a {@code CharSequence} into a {@code short}.
	 *
	 * <p>In opposite to the functions of {@link Short}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Short#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code short} type.
	 * @since 0.9
	 * @see Short#decode(String)
	 */
	@Pure
	public static short shortValue(CharSequence value) {
		try {
			return Short.decode(value.toString()).shortValue();
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
	}

	/** Convert an {@code Object} to a primitive {@code short}.
	 *
	 * <p>If the given object is a number, the short value is replied.
	 * Otherwise, the given value is converted to String and then to short.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code short} type.
	 * @since 0.13
	 */
	@Pure
	public static short shortValue(Object value) {
		if (value instanceof Number cvalue) {
			return cvalue.shortValue();
		}
		if (value != null) {
			return shortValue(value.toString());
		}
		return 0;
	}

	/** Convert an {@code Object} to a {@code Short}.
	 *
	 * <p>If the given object is a number, the short value is replied.
	 * Otherwise, the given value is converted to String and then to short.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code Short} type.
	 * @since 0.13
	 */
	@Pure
	public static Short toShort(Object value) {
		if (value instanceof Short cvalue) {
			return cvalue;
		}
		final short svalue;
		if (value instanceof Number cvalue) {
			svalue = cvalue.shortValue();
		} else if (value != null) {
			return toShort(value.toString());
		} else {
			svalue = 0;
		}
		return Short.valueOf(svalue);
	}
	
	/** Decodes a {@code CharSequence} into a {@code Short}.
	 *
	 * <p>In opposite to the functions of {@link Short}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Short#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code Short} type.
	 * @since 0.12
	 * @see Short#decode(String)
	 */
	@Pure
	public static Short toShort(CharSequence value) {
		try {
			return Short.decode(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return Short.valueOf((short) 0);
	}

	/** Decodes a {@code CharSequence} into an {@code int}.
	 *
	 * <p>In opposite to the functions of {@link Integer}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Integer#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code int} type.
	 * @since 0.9
	 * @see Integer#decode(String)
	 */
	@Pure
	public static int intValue(CharSequence value) {
		try {
			return Integer.decode(value.toString()).intValue();
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
	}

	/** Convert an {@code Object} to a primitive {@code int}.
	 *
	 * <p>If the given object is a number, the int value is replied.
	 * Otherwise, the given value is converted to String and then to int.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code int} type.
	 * @since 0.13
	 */
	@Pure
	public static int intValue(Object value) {
		if (value instanceof Number cvalue) {
			return cvalue.intValue();
		}
		if (value != null) {
			return intValue(value.toString());
		}
		return 0;
	}

	/** Convert an {@code Object} to a {@code Integer}.
	 *
	 * <p>If the given object is a number, the int value is replied.
	 * Otherwise, the given value is converted to String and then to int.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code Integer} type.
	 * @since 0.13
	 */
	@Pure
	public static Integer toInteger(Object value) {
		if (value instanceof Integer cvalue) {
			return cvalue;
		}
		final int ivalue;
		if (value instanceof Number cvalue) {
			ivalue = cvalue.intValue();
		} else if (value != null) {
			return toInteger(value.toString());
		} else {
			ivalue = 0;
		}
		return Integer.valueOf(ivalue);
	}

	/** Decodes a {@code CharSequence} into an {@code Integer}.
	 *
	 * <p>In opposite to the functions of {@link Integer}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Integer#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code Integer} type.
	 * @since 0.12
	 * @see Integer#decode(String)
	 */
	@Pure
	public static Integer toInteger(CharSequence value) {
		try {
			return Integer.decode(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return Integer.valueOf(0);
	}

	/** Decodes a {@code CharSequence} into a {@code char}.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code char} type.
	 * @since 0.12
	 */
	@Pure
	public static char charValue(CharSequence value) {
		try {
			return value.charAt(0);
		} catch (Throwable exception) {
			//
		}
		return '\0';
	}

	/** Convert an {@code Object} to a primitive {@code char}.
	 *
	 * <p>If the given object is a character, the char value is replied.
	 * Otherwise, the given value is converted to String and then to char.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code char} type.
	 * @since 0.13
	 */
	@Pure
	public static char charValue(Object value) {
		if (value instanceof Character cvalue) {
			return cvalue.charValue();
		}
		if (value != null) {
			return charValue(value.toString());
		}
		return 0;
	}

	/** Convert an {@code Object} to a {@code Character}.
	 *
	 * <p>If the given object is a character, the value is replied.
	 * If the given object is a number, the int value converted to char is replied.
	 * Otherwise, the given value is converted to String and then to char.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code Character} type.
	 * @since 0.13
	 */
	@Pure
	public static Character toCharacter(Object value) {
		if (value instanceof Character cvalue) {
			return cvalue;
		}
		final char cvalue;
		if (value instanceof Number nvalue) {
			cvalue = (char) nvalue.intValue();
		} else if (value != null) {
			return toCharacter(value.toString());
		} else {
			cvalue = 0;
		}
		return Character.valueOf(cvalue);
	}

	/** Decodes a {@code CharSequence} into a {@code Character}.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code Character} type.
	 * @since 0.12
	 */
	@Pure
	public static Character toCharacter(CharSequence value) {
		try {
			return Character.valueOf(value.charAt(0));
		} catch (Throwable exception) {
			//
		}
		return Character.valueOf('\0');
	}

	/** Decodes a {@code CharSequence} into a {@code long}.
	 *
	 * <p>In opposite to the functions of {@link Long}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Long#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code long} type.
	 * @since 0.9
	 * @see Long#decode(String)
	 */
	@Pure
	public static long longValue(CharSequence value) {
		try {
			return Long.decode(value.toString()).longValue();
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
	}

	/** Convert an {@code Object} to a primitive {@code long}.
	 *
	 * <p>If the given object is a number, the long value is replied.
	 * Otherwise, the given value is converted to String and then to long.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code long} type.
	 * @since 0.13
	 */
	@Pure
	public static long longValue(Object value) {
		if (value instanceof Number cvalue) {
			return cvalue.longValue();
		}
		if (value != null) {
			return longValue(value.toString());
		}
		return 0;
	}

	/** Convert an {@code Object} to a {@code Long}.
	 *
	 * <p>If the given object is a number, the long value is replied.
	 * Otherwise, the given value is converted to String and then to long.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code Long} type.
	 * @since 0.13
	 */
	@Pure
	public static Long toLong(Object value) {
		if (value instanceof Long cvalue) {
			return cvalue;
		}
		final long lvalue;
		if (value instanceof Number cvalue) {
			lvalue = cvalue.longValue();
		} else if (value != null) {
			return toLong(value.toString());
		} else {
			lvalue = 0;
		}
		return Long.valueOf(lvalue);
	}

	/** Decodes a {@code CharSequence} into a {@code Long}.
	 *
	 * <p>In opposite to the functions of {@link Long}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Long#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code Long} type.
	 * @since 0.9
	 * @see Long#decode(String)
	 */
	@Pure
	public static Long toLong(CharSequence value) {
		try {
			return Long.decode(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return Long.valueOf(0l);
	}

	/** Decodes a {@code CharSequence} into a {@code float}.
	 *
	 * <p>In opposite to the functions of {@link Float}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Float#valueOf(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code float} type.
	 * @since 0.9
	 * @see Float#valueOf(String)
	 */
	@Pure
	public static float floatValue(CharSequence value) {
		try {
			return Float.parseFloat(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
	}

	/** Convert an {@code Object} to a primitive {@code float}.
	 *
	 * <p>If the given object is a number, the float value is replied.
	 * Otherwise, the given value is converted to String and then to float.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code float} type.
	 * @since 0.13
	 */
	@Pure
	public static float floatValue(Object value) {
		if (value instanceof Number cvalue) {
			return cvalue.floatValue();
		}
		if (value != null) {
			return floatValue(value.toString());
		}
		return 0f;
	}

	/** Convert an {@code Object} to a {@code Float}.
	 *
	 * <p>If the given object is a number, the float value is replied.
	 * Otherwise, the given value is converted to String and then to float.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code Float} type.
	 * @since 0.13
	 */
	@Pure
	public static Float toFloat(Object value) {
		if (value instanceof Float cvalue) {
			return cvalue;
		}
		final float fvalue;
		if (value instanceof Number cvalue) {
			fvalue = cvalue.floatValue();
		} else if (value != null) {
			return toFloat(value.toString());
		} else {
			fvalue = 0;
		}
		return Float.valueOf(fvalue);
	}

	/** Decodes a {@code CharSequence} into a {@code Float}.
	 *
	 * <p>In opposite to the functions of {@link Float}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Float#valueOf(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code Float} type.
	 * @since 0.9
	 * @see Float#valueOf(String)
	 */
	@Pure
	public static Float toFloat(CharSequence value) {
		try {
			return Float.valueOf(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return Float.valueOf(0f);
	}

	/** Decodes a {@code CharSequence} into a {@code double}.
	 *
	 * <p>In opposite to the functions of {@link Double}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Double#valueOf(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code double} type.
	 * @since 0.9
	 * @see Double#valueOf(String)
	 */
	@Pure
	public static double doubleValue(CharSequence value) {
		try {
			return Double.parseDouble(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
	}

	/** Convert an {@code Object} to a primitive {@code double}.
	 *
	 * <p>If the given object is a number, the double value is replied.
	 * Otherwise, the given value is converted to String and then to double.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code double} type.
	 * @since 0.13
	 */
	@Pure
	public static double doubleValue(Object value) {
		if (value instanceof Number cvalue) {
			return cvalue.doubleValue();
		}
		if (value != null) {
			return doubleValue(value.toString());
		}
		return 0.;
	}

	/** Convert an {@code Object} to a {@code Double}.
	 *
	 * <p>If the given object is a number, the double value is replied.
	 * Otherwise, the given value is converted to String and then to double.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code Double} type.
	 * @since 0.13
	 */
	@Pure
	public static Double toDouble(Object value) {
		if (value instanceof Double cvalue) {
			return cvalue;
		}
		final double dvalue;
		if (value instanceof Number cvalue) {
			dvalue = cvalue.doubleValue();
		} else if (value != null) {
			return toDouble(value.toString());
		} else {
			dvalue = 0;
		}
		return Double.valueOf(dvalue);
	}

	/** Decodes a {@code CharSequence} into a {@code Double}.
	 *
	 * <p>In opposite to the functions of {@link Double}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link Double#valueOf(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code Double} type.
	 * @since 0.12
	 * @see Double#valueOf(String)
	 */
	@Pure
	public static Double toDouble(CharSequence value) {
		try {
			return Double.valueOf(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return Double.valueOf(0.0);
	}

	/** Decodes a {@code CharSequence} into a {@code AtomicBoolean}.
	 *
	 *
	 * <p>See {@link Integer#decode(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code AtomicBoolean} type.
	 * @since 0.12
	 * @see #booleanValue(CharSequence)
	 */
	@Pure
	@Inline(value = "new $2($3.booleanValue($1))", imported = {AtomicBoolean.class, PrimitiveCastExtensions.class})
	public static AtomicBoolean toAtomicBoolean(CharSequence value) {
		return new AtomicBoolean(booleanValue(value));
	}

	/** Convert an {@code Object} to an atomic {@code boolean}.
	 *
	 * <p>If the given object is a boolean, the boolean value is replied.
	 * Otherwise, the given value is converted to String and then to boolean.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code boolean} type.
	 * @since 0.13
	 */
	@Pure
	public static AtomicBoolean toAtomicBoolean(Object value) {
		if (value instanceof AtomicBoolean cvalue) {
			return cvalue;
		}
		return new AtomicBoolean(booleanValue(value));
	}

	/** Convert the given boolean to its equivalent {@code AtomicBoolean}.
	 *
	 *
	 * @param value a value to convert.
	 * @return the equivalent value to {@code value} of {@code AtomicBoolean} type.
	 * @since 0.12
	 */
	@Pure
	@Inline(value = "new $2($1)", imported = {AtomicBoolean.class})
	public static AtomicBoolean toAtomicBoolean(boolean value) {
		return new AtomicBoolean(value);
	}

	/** Convert an {@code Object} to an atomic {@code integer}.
	 *
	 * <p>If the given object is a number, the int value is replied.
	 * Otherwise, the given value is converted to String and then to int.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code int} type.
	 * @since 0.13
	 */
	@Pure
	public static AtomicInteger toAtomicInteger(Object value) {
		if (value instanceof AtomicInteger cvalue) {
			return cvalue;
		}
		return new AtomicInteger(intValue(value));
	}

	/** Decodes a {@code CharSequence} into a {@code AtomicInteger}.
	 *
	 * <p>In opposite to the functions of {@link Integer}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link #intValue(CharSequence)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code AtomicInteger} type.
	 * @since 0.9
	 * @see #intValue(CharSequence)
	 */
	@Pure
	@Inline(value = "new $2($3.intValue($1))", imported = {AtomicInteger.class, PrimitiveCastExtensions.class})
	public static AtomicInteger toAtomicInteger(CharSequence value) {
		return new AtomicInteger(intValue(value));
	}

	/** Convert an {@code Object} to an atomic {@code long}.
	 *
	 * <p>If the given object is a number, the long value is replied.
	 * Otherwise, the given value is converted to String and then to long.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code long} type.
	 * @since 0.13
	 */
	@Pure
	public static AtomicLong toAtomicLong(Object value) {
		if (value instanceof AtomicLong cvalue) {
			return cvalue;
		}
		return new AtomicLong(longValue(value));
	}

	/** Decodes a {@code CharSequence} into a {@code AtomicLong}.
	 *
	 * <p>In opposite to the functions of {@link Long}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link #longValue(CharSequence)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code AtomicLong} type.
	 * @since 0.9
	 * @see #longValue(CharSequence)
	 */
	@Pure
	@Inline(value = "new $2($3.longValue($1))", imported = {AtomicLong.class, PrimitiveCastExtensions.class})
	public static AtomicLong toAtomicLong(CharSequence value) {
		return new AtomicLong(longValue(value));
	}

	/** Convert an {@code Object} to an atomic {@code double}.
	 *
	 * <p>If the given object is a number, the double value is replied.
	 * Otherwise, the given value is converted to String and then to double.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code double} type.
	 * @since 0.13
	 */
	@Pure
	public static AtomicDouble toAtomicDouble(Object value) {
		if (value instanceof AtomicDouble cvalue) {
			return cvalue;
		}
		return new AtomicDouble(doubleValue(value));
	}

	/** Decodes a {@code CharSequence} into a {@code AtomicDouble}.
	 *
	 * <p>In opposite to the functions of {@link Double}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link #doubleValue(CharSequence)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code AtomicDouble} type.
	 * @since 0.9
	 * @see #doubleValue(CharSequence)
	 */
	@Pure
	@Inline(value = "new $2($3.doubleValue($1))", imported = {AtomicDouble.class, PrimitiveCastExtensions.class})
	public static AtomicDouble toAtomicDouble(CharSequence value) {
		return new AtomicDouble(doubleValue(value));
	}

	private static boolean startsWith(CharSequence value, String prefix, int index) {
		if (value == null) {
			return prefix == null;
		}
		final var prefixLength = prefix.length();
		if (value.length() == 0) {
			return prefixLength == 0;
		}
		final var eoffset = index + prefixLength;
		if (eoffset > value.length()) {
			return false;
		}
		var idx0 = index;
		for (var idx1 = 0; idx1 < prefixLength; ++idx1, ++idx0) {
			if (value.charAt(idx0) != prefix.charAt(idx1)) {
				return false;
			}
		}
        return true;
	}

	/** Convert an {@code Object} to a big integer.
	 *
	 * <p>If the given object is a number, the long value is replied.
	 * Otherwise, the given value is converted to String and then to long.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code BigInteger} type.
	 * @since 0.13
	 */
	@Pure
	public static BigInteger toBigInteger(Object value) {
		if (value instanceof BigInteger cvalue) {
			return cvalue;
		}
		if (value instanceof BigDecimal cvalue) {
			return cvalue.toBigInteger();
		}
		final String svalue;
		if (value != null) {
			if (value instanceof Number cvalue) {
				svalue = Long.toString(cvalue.longValue());
			} else {
				svalue = value.toString();
			}
		} else {
			svalue = "0"; //$NON-NLS-1$
		}
		return new BigInteger(svalue);
	}

	/** Decodes a {@code CharSequence} into a {@code BigInteger}.
	 *
	 * <p>In opposite to the functions of {@link Integer}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link BigInteger#BigInteger(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code BigInteger} type.
	 * @since 0.9
	 * @see #intValue(CharSequence)
	 */
	@Pure
	public static BigInteger toBigInteger(CharSequence value) {
		try {
			var negative = false;
			var index = 0;
			final var firstChar = value.charAt(0);

			// Handle sign, if present
			if (firstChar == '-') {
				negative = true;
				++index;
			} else if (firstChar == '+') {
				++index;
			}

			// Handle radix specifier, if present
			var radix = 10;
			if (startsWith(value, "0x", index) || startsWith(value, "0X", index)) { //$NON-NLS-1$ //$NON-NLS-2$
				index += 2;
				radix = 16;
			} else if (startsWith(value, "#", index)) { //$NON-NLS-1$
				++index;
				radix = 16;
			} else if (startsWith(value, "0", index) && value.length() > 1 + index) { //$NON-NLS-1$
				++index;
				radix = 8;
			}
			final CharSequence endValue;
			if (index > 0) {
				endValue = value.subSequence(index, value.length());
			} else {
				endValue = value;
			}
			final var number = new BigInteger(endValue.toString(), radix);
			if (negative) {
				return number.negate();
			}
			return number;
		} catch (Throwable exception) {
			// Silent error
		}
		return BigInteger.valueOf(0);
	}

	/** Convert an {@code Object} to a big decimal.
	 *
	 * <p>If the given object is a number, the double value is replied.
	 * Otherwise, the given value is converted to String and then to double.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code BigDecimal} type.
	 * @since 0.13
	 */
	@Pure
	public static BigDecimal toBigDecimal(Object value) {
		if (value instanceof BigDecimal cvalue) {
			return cvalue;
		}
		if (value != null) {
			return new BigDecimal(value.toString());
		}
		return new BigDecimal(0.);
	}

	/** Decodes a {@code CharSequence} into a {@code BigDecimal}.
	 *
	 * <p>In opposite to the functions of {@link Double}, this function is
	 * null-safe and does not generate a {@link NumberFormatException}.
	 * If the given string cannot by parsed, {@code 0} is replied.
	 *
	 * <p>See {@link BigDecimal#BigDecimal(String)} for details on the accepted formats
	 * for the input string of characters.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code BigInteger} type.
	 * @since 0.9
	 * @see #intValue(CharSequence)
	 */
	@Pure
	public static BigDecimal toBigDecimal(CharSequence value) {
		try {
			return new BigDecimal(value.toString());
		} catch (Throwable exception) {
			// Silent error
		}
		return BigDecimal.valueOf(0.);
	}

	/** Convert an {@code Object} to a {@code UUID}.
	 *
	 * <p>If the given object is a identifier, the value is replied.
	 * Otherwise, the given value is converted to String and then to identifier.
	 *
	 * @param value a value of {@code Object} type.
	 * @return the equivalent value to {@code value} of {@code UUID} type.
	 * @since 0.13
	 */
	@Pure
	public static UUID toUUID(Object value) {
		if (value instanceof UUID cvalue) {
			return cvalue;
		}
		if (value != null) {
			return toUUID(value.toString());
		}
		return null;
	}

	/** Decodes a {@code CharSequence} into a {@code UUID}.
	 *
	 * <p>In opposite to the functions of {@link UUID}, this function is
	 * null-safe and does not generate an exception.
	 * If the given string cannot by parsed, {@code null} is replied.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code UUID} type, or {@code null}
	 *     if a UUID cannot be parsed from the string of characters.
	 * @since 0.12
	 */
	@Pure
	public static UUID toUUID(CharSequence value) {
		try {
			return UUID.fromString(value.toString());
		} catch (Throwable exception) {
			// Silent error
		}
		return null;
	}

}
