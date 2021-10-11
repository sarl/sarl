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
package io.sarl.tests.api.tools;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Strings;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.eclipse.xtext.util.DiffUtil;
import org.eclipse.xtext.xbase.lib.Pure;

/** Set of utility classes that provide additional assertion functions.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public final class TestUtils {

	/** Precision of the floating point number epsilon-tests.
	 */
	public static final int DEFAULT_DECIMAL_COUNT = 6;

	private TestUtils() {
		//
	}

	/** Merge two arrays.
	 *
	 * @param operand1 the first array.
	 * @param operand2 the second array.
	 * @return the merge.
	 */
	@Pure
	public static String[] merge(String[] operand1, String[] operand2) {
		if (operand1 == null) {
			if (operand2 == null) {
				return new String[0];
			}
			return operand2;
		}
		if (operand2 == null) {
			return operand1;
		}
		String[] tab = new String[operand1.length + operand2.length];
		System.arraycopy(
				operand1, 0,
				tab, 0,
				operand1.length);
		System.arraycopy(
				operand2, 0,
				tab, operand1.length,
				operand2.length);
		return tab;
	}

	/** Helper for setting a field, even if it is not visible.
	 *
	 * @param instance the object.
	 * @param fieldType the type of the field.
	 * @param fieldName the name of the field.
	 * @param fieldValue the field value.
	 */
	public static <T> void setField(Object instance, Class<T> fieldType,
			String fieldName, T fieldValue) {
		Field field = null;
		Class<?> type = instance.getClass();
		while (type != null) {
			try {
				field = type.getDeclaredField(fieldName);
				assertEquals(fieldType, field.getType());
				field.setAccessible(true);
				field.set(instance, fieldValue);
				return;
			} catch (Throwable exception) {
				type = type.getSuperclass();
			}
		}
		throw new NoSuchFieldError(fieldName);
	}

	/** Replies if the runtime environment is Eclipse.
	 *
	 * @return <code>true</code> if the runtime environment is Eclipse.
	 */
	@Pure
	public static boolean isEclipseRuntimeEnvironment() {
		final String cmd = System.getProperty("sun.java.command", "");
		// Assuming that the Maven launcher is providing an absolute path to the launcher.
		return cmd != null
				&& (cmd.startsWith("org.eclipse.equinox.launcher.Main")
						|| cmd.startsWith("org.eclipse.jdt.internal.junit."));
	}

	/** Change the log level at the global level.
	 *
	 * @param level the new level.
	 */
	public static void setGlobalLogLevel(Level level) {
		LogManager.getRootLogger().setLevel(level);
	}


	/** Replies the stack trace of the caller of this function.
	 *
	 * @return the stack trace.
	 */
	@Pure
	public static StackTraceElement[] getStackTrace() {
		try {
			throw new Exception();
		} catch (Throwable e) {
			List<StackTraceElement> types = new ArrayList<>();
			StackTraceElement[] elements = e.getStackTrace();
			for (int i = 1; i < elements.length; ++i) {
				types.add(elements[i]);
			}
			StackTraceElement[] array = new StackTraceElement[types.size()];
			types.toArray(array);
			return array;
		}
	}

	/** Replies the OS-dependent line separator.
	 *
	 * @return the line separator from the {@code "line.separator"} property, or {@code "\n"}.
	 * @since 0.5
	 */
	@Pure
	public static String getLineSeparator() {
		final String nl = System.getProperty("line.separator");
		if (Strings.isNullOrEmpty(nl)) {
			throw new Error("NO LINE SEPARATOR DEFINED");
			//return "\n";
		}
		return nl;
	}

	/** Helper for writing a multi-line string in unit tests, which supports the
	 * OS-dependent line separator.
	 *
	 * @param lines the lines in the string.
	 * @return the complete multi-line string.
	 */
	@Pure
	public static String multilineString(Object... lines) {
		final StringBuilder buffer = new StringBuilder();
		final String nl = getLineSeparator();
		boolean first = true;
		for (final Object obj : lines) {
			if (first) {
				first = false;
			} else {
				buffer.append(nl);
			}
			if (obj != null) {
				if (obj instanceof CharSequence) {
					buffer.append((CharSequence) obj);
				} else {
					buffer.append(obj.toString());
				}
			}
		}
		return buffer.toString();
	}

	/** Replies if two values are equals at epsilon.
	 *
	 * @param v1 the first value.
	 * @param v2 the second value.
	 * @param isNaNEqual indicates if the NaN value is equals to itself.
	 * @return <code>true</code> or <code>false</code>
	 */
	@Pure
	public static boolean isEpsilonEquals(double v1, double v2, boolean isNaNEqual) {
		if (v1 == v2) {
			return true;
		}
		final boolean nanA = Double.isNaN(v1);
		final boolean nanB = Double.isNaN(v2);
		if (nanA || nanB) {
			if (isNaNEqual) {
				return nanA == nanB;
			}
			return false;
		}
		if (!Double.isInfinite(v1) && !Double.isInfinite(v1)
				&& !Double.isNaN(v1) && !Double.isNaN(v2)) {
			return isEpsilonEquals(new BigDecimal(v1), new BigDecimal(v2), DEFAULT_DECIMAL_COUNT / 2);
		}
		return false;
	}

	/** Replies if two values are equals at epsilon.
	 *
	 * @param v1 the first value.
	 * @param v2 the second value.
	 * @param precision is the number of decimal digits to test.
	 * @return <code>true</code> or <code>false</code>
	 */
	@Pure
	public static boolean isEpsilonEquals(BigDecimal v1, BigDecimal v2, int precision) {
		final BigDecimal ma = v1.movePointRight(precision);
		final BigDecimal mb = v2.movePointRight(precision);
		BigDecimal aa = ma.setScale(0, RoundingMode.HALF_UP);
		BigDecimal bb = mb.setScale(0, RoundingMode.HALF_UP);
		if (aa.compareTo(bb) == 0) {
			return true;
		}
		aa = ma.setScale(0, RoundingMode.DOWN);
		bb = mb.setScale(0, RoundingMode.DOWN);
		return aa.compareTo(bb) == 0;
	}

	/** 
	 * Replies result at the given index of the run of the agent.
	 * 
	 * @param source - the source of the data.
	 * @param type - the type of the result.
	 * @param index - the index of the result.
	 * @return the value; or {@code null} if no result.
	 */
	@Pure
	public static <T> T elementAt(List<?> source, Class<T> type, int index) {
		final Object element = source.get(index);
		if (element == null || type.isInstance(element)) {
			return type.cast(element);
		}
		return null;
	}

	/** Replies the simple name of the given type name
	 * 
	 * @param typeName the fully qualified name of a type.
	 */
	@Pure
	public static String simpleTypeName(String typeName) {
		final int index1 = typeName.lastIndexOf("$");
		final int index2 = typeName.lastIndexOf(".");
		final int index = Math.max(index1, index2);
		if (index >= 0) {
			return typeName.substring(index + 1);
		}
		return typeName;
	}

	/** Replies a string describing the differences between two strings.
	 *
	 * @param a the first string.
	 * @param b the second string
	 * @return the description of the difference.
	 * @since 0.12
	 */
	@Pure
	public static String differences(String a, String b) {
		final String diff = DiffUtil.diff(a,  b);
		return diff;
	}

}
