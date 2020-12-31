/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.scoping.extensions.cast;

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
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
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
			return Byte.decode(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
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
			return Byte.decode(value.toString());
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
			return Short.decode(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
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
		return 0;
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
			return Integer.decode(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
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
		return 0;
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

	/** Decodes a {@code CharSequence} into a {@code Character}.
	 *
	 * @param value a value of {@code CharSequence} type.
	 * @return the equivalent value to {@code value} of {@code Character} type.
	 * @since 0.12
	 */
	@Pure
	public static Character toCharacter(CharSequence value) {
		try {
			return value.charAt(0);
		} catch (Throwable exception) {
			//
		}
		return '\0';
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
			return Long.decode(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0;
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
		return 0L;
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
			return Float.parseFloat(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0f;
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
			return Double.parseDouble(value.toString());
		} catch (Throwable exception) {
			// Silent exception.
		}
		return 0.0;
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
		final int prefixLength = prefix.length();
		if (value.length() == 0) {
			return prefixLength == 0;
		}
		final int eoffset = index + prefixLength;
		if (eoffset > value.length()) {
			return false;
		}
		int idx0 = index;
		for (int idx1 = 0; idx1 < prefixLength; ++idx1, ++idx0) {
			if (value.charAt(idx0) != prefix.charAt(idx1)) {
				return false;
			}
		}
        return true;
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
	@SuppressWarnings("checkstyle:magicnumber")
	public static BigInteger toBigInteger(CharSequence value) {
		try {
			boolean negative = false;
			int index = 0;
			final char firstChar = value.charAt(0);

			// Handle sign, if present
			if (firstChar == '-') {
				negative = true;
				++index;
			} else if (firstChar == '+') {
				++index;
			}

			// Handle radix specifier, if present
			int radix = 10;
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
			final BigInteger number = new BigInteger(endValue.toString(), radix);
			if (negative) {
				return number.negate();
			}
			return number;
		} catch (Throwable exception) {
			// Silent error
		}
		return BigInteger.valueOf(0);
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
