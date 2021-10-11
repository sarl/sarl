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

package io.sarl.maven.docs.testing;

import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URL;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.Iterables;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.ReflectionUtil;
import org.eclipse.jdt.core.Flags;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XBinaryOperation;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCollectionLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XSetLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.lib.Pair;
import org.osgi.framework.Version;

import io.sarl.lang.util.SarlUtils;

/** Should functions for the documentation facts.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings({"checkstyle:methodname"})
public final class ShouldExtensions {

	private static final int HEX_RADIX = 16;

	private ShouldExtensions() {
		//
	}

	/** Ensure that the string has the format of a date.
	 *
	 * @param actual the string to parse.
	 * @param dateFormat the expected format of the date, as
	 *     described in {@link SimpleDateFormat}. If {@code null}, the
	 *     default date format is considered.
	 * @return the validation status
	 */
	public static boolean shouldBeDate(String actual, String dateFormat) {
		if (actual == null || actual.isEmpty()) {
			return false;
		}
		try {
			final DateFormat format;
			if (dateFormat == null || dateFormat.isEmpty()) {
				format = DateFormat.getDateInstance();
			} else {
				format = new SimpleDateFormat(dateFormat);
			}
			return format.parse(actual) != null;
		} catch (Throwable exception)  {
			//
		}
		return false;
	}

	/** Ensure that the string has the format of a date.
	 *
	 * @param actual the string to parse.
	 * @return the validation status
	 */
	public static boolean shouldBeDate(String actual) {
		return shouldBeDate(actual, null);
	}

	/** Ensure that the string has the format of a number.
	 *
	 * @param actual the string to parse.
	 * @param numberFormat the expected format of the number, as
	 *     described in {@link DecimalFormat}. If {@code null}, the
	 *     default date format is considered.
	 * @return the validation status
	 */
	public static boolean shouldBeNumber(String actual, String numberFormat) {
		if (actual == null || actual.isEmpty()) {
			return false;
		}
		try {
			final NumberFormat format;
			if (numberFormat == null || numberFormat.isEmpty()) {
				format = NumberFormat.getNumberInstance();
			} else {
				format = new DecimalFormat(numberFormat);
			}
			return format.parse(actual) != null;
		} catch (Throwable exception)  {
			//
		}
		return false;
	}

	/** Ensure that the string has the format of a number.
	 *
	 * @param actual the string to parse.
	 * @return the validation status
	 */
	public static boolean shouldBeNumber(String actual) {
		return shouldBeNumber(actual, null);
	}

	/** Ensure that the given string is a valid Maven version number.
	 *
	 * @param actual the string to test.
	 * @param allowSnapshot indicates if the <code>-SNAPSHOT</code> postfix
	 *     is considered as valid.
	 * @return the validation status.
	 */
	public static boolean shouldBeMavenVersion(String actual, boolean allowSnapshot) {
		if (actual == null) {
			return false;
		}
		final StringBuilder pattern = new StringBuilder("^"); //$NON-NLS-1$
		pattern.append("[0-9a-zA-Z_-]+(\\.[0-9a-zA-Z_-]+)*"); //$NON-NLS-1$
		if (allowSnapshot) {
			pattern.append("(?:"); //$NON-NLS-1$
			pattern.append(Matcher.quoteReplacement("-SNAPSHOT")); //$NON-NLS-1$
			pattern.append(")?"); //$NON-NLS-1$
		}
		pattern.append("$"); //$NON-NLS-1$
		return Pattern.matches(pattern.toString(), actual);
	}

	/** Ensure that the given string is a valid Maven version number, including snapshot.
	 *
	 * @param actual the string to test.
	 * @return the validation status.
	 */
	public static boolean shouldBeMavenVersion(String actual) {
		return shouldBeMavenVersion(actual, true);
	}

	private static Version parseJavaVersion(String version, Version defaultVersion) {
		try {
			final Pattern pattern = Pattern.compile("^([0-9]+)(?:\\.([0-9]+)(?:\\.([0-9]+))?)?"); //$NON-NLS-1$
			final Matcher matcher = pattern.matcher(version);
			if (matcher.find()) {
				int minor = 0;
				String group = matcher.group(2);
				if (group != null && !group.isEmpty()) {
					try {
						minor = Integer.parseInt(group);
					} catch (Exception exception) {
						//
					}
				}
				int micro = 0;
				group = matcher.group(3);
				if (group != null && !group.isEmpty()) {
					try {
						micro = Integer.parseInt(group);
					} catch (Exception exception) {
						//
					}
				}
				final int major = Integer.parseInt(matcher.group(1));
				return new Version(major, minor, micro);
			}
			if (version != null && !version.isEmpty()) {
				final Version v = Version.valueOf(version);
				if (v != null) {
					return v;
				}
			}
		} catch (Exception exception) {
			//
		}
		return defaultVersion;
	}

	/** Ensure that the version of the current Java specification is in
	 * the range given by the minVersion (inclusive) and maxVersion (exclusive).
	 * If the maxVersion is not given or is not a properly formated version
	 * number, then all versions after the minVersion are valid.
	 *
	 * @param minVersion the minimal version.
	 * @param maxVersion the maximal version.
	 * @return the validation status.
	 */
	public static boolean shouldBeJavaRange(String minVersion, String maxVersion) {
		final Version jreV = parseJavaVersion(System.getProperty("java.version"), null); //$NON-NLS-1$
		if (jreV != null && minVersion != null) {
			final Version minV = parseJavaVersion(minVersion, null);
			if (minV != null) {
				Version maxV = null;
				if (maxVersion != null) {
					maxV = parseJavaVersion(maxVersion, null);
				}
				if (maxV == null) {
					return jreV.compareTo(minV) >= 0;
				}
				return jreV.compareTo(minV) >= 0 && jreV.compareTo(maxV) < 0;
			}
		}
		return false;
	}

	/** Ensure that the version of the current Java specification is at
	 * least the one given by minVersion.
	 *
	 * @param minVersion the minimal version.
	 * @return the validation status.
	 */
	public static boolean shouldBeAtLeastJava(String minVersion) {
		return shouldBeJavaRange(minVersion, null);
	}

	/** Ensure that the iterator replies the expected values in the given order.
	 *
	 * @param actual the iterator to test.
	 * @param expected the expected values.
	 * @return the validation status
	 */
	public static boolean shouldIterate(Iterator<?> actual, Object expected) {
		return shouldIterate(actual, expected, true);
	}

	/** Ensure that the iterator replies the expected values in the given order.
	 *
	 * @param actual the iterator to test.
	 * @param expected the expected values.
	 * @param significantOrder indicates if the order of the elements is significant.
	 * @return the validation status
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	public static boolean shouldIterate(Iterator<?> actual, Object expected, boolean significantOrder) {
		Object obj;
		final Iterator<?> it;
		if (expected instanceof Iterable<?>) {
			it = ((Iterable<?>) expected).iterator();
		} else if (expected instanceof Array) {
			final Array array = (Array) expected;
			it = new ArrayIterator(array);
		} else if (expected instanceof Map<?, ?>) {
			it = ((Map<?, ?>) expected).entrySet().iterator();
		} else {
			it = Collections.singleton(expected).iterator();
		}

		if (significantOrder) {
			// Significant order
			Object eObj;
			while (actual.hasNext()) {
				obj = actual.next();
				if (!it.hasNext()) {
					return false;
				}
				eObj = it.next();
				if (obj instanceof XExpression) {
					if (!shouldBeLiteral((XExpression) obj, eObj)) {
						return false;
					}
				} else if (!Objects.equals(obj, eObj)) {
					return false;
				}
			}
			return !it.hasNext();
		}

		// Unsignificant order
		final List<Object> expectedElements = new LinkedList<>();
		while (it.hasNext()) {
			expectedElements.add(it.next());
		}
		boolean found;
		while (actual.hasNext()) {
			obj = actual.next();
			final Iterator<Object> i = expectedElements.iterator();
			found = false;
			while (!found && i.hasNext()) {
				final Object eObj = i.next();
				if (obj instanceof XExpression) {
					if (shouldBeLiteral((XExpression) obj, eObj)) {
						i.remove();
						found = true;
					}
				} else if (obj instanceof JvmIdentifiableElement
						&& Objects.equals(((JvmIdentifiableElement) obj).getQualifiedName(), eObj)) {
					i.remove();
					found = true;
				} else if (obj instanceof JvmTypeReference
						&& Objects.equals(((JvmTypeReference) obj).getQualifiedName(), eObj)) {
					i.remove();
					found = true;
				} else if (Objects.equals(obj, eObj)) {
					i.remove();
					found = true;
				}
			}
			if (!found) {
				return false;
			}
		}
		return expectedElements.isEmpty();
	}

	/** Ensure that the string has the format of an URL to the SARL API.
	 *
	 * @param actual the string to parse.
	 * @param allowedAPIhostname is a list of API base hostname to consider as valid.
	 *     If not given, only "www.sarl.io" is allowed.
	 * @return the validation status
	 */
	public static boolean shouldBeApiURL(String actual, String allowedAPIhostname) {
		if (actual == null || actual.isEmpty()) {
			return false;
		}
		try {
			final URL u = FileSystem.convertStringToURL(actual, true);
			if (u == null) {
				return false;
			}
			String[] validHostnames = {"www.sarl.io"}; //$NON-NLS-1$
			if (allowedAPIhostname != null && !allowedAPIhostname.isEmpty()) {
				validHostnames = allowedAPIhostname.split("[ \t]*[,;][ \t]*"); //$NON-NLS-1$
			}
			final List<String> hosts = Arrays.asList(validHostnames);
			if (!hosts.contains(u.getHost())
					|| !u.getQuery().endsWith(".html") //$NON-NLS-1$
					|| !u.getPath().endsWith("index.html")) { //$NON-NLS-1$
				return false;
			}
			return true;
		} catch (Throwable exception)  {
			//
		}
		return false;
	}

	private static Number cleanNumber(String stringRepresentation) {
		if (stringRepresentation == null) {
			return null;
		}
		if (stringRepresentation.startsWith("0x") || stringRepresentation.startsWith("0X")) { //$NON-NLS-1$//$NON-NLS-2$
			return new BigInteger(stringRepresentation.substring(2), HEX_RADIX);
		}
		String literal = stringRepresentation.replace("_", ""); //$NON-NLS-1$//$NON-NLS-2$
		literal = literal.toLowerCase().replaceFirst("l|f|d|(bi)|(bd)$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		return new BigDecimal(literal);
	}

	/** Ensure that the given string literal is equal to the given value.
	 *
	 * @param actual the string literal to test.
	 * @param expected the expected value.
	 * @return the validation status
	 */
	public static boolean shouldBe(XStringLiteral actual, Object expected) {
		if (actual == null) {
			return false;
		}
		final String string = (expected == null) ? null : expected.toString();
		return Objects.equals(string, actual.getValue());
	}

	/** Ensure that the given boolean literal is equal to the given value.
	 *
	 * @param actual the boolean literal to test.
	 * @param expected the expected value.
	 * @return the validation status
	 */
	public static boolean shouldBe(XBooleanLiteral actual, Object expected) {
		if (actual == null) {
			return false;
		}
		final Boolean b;
		if (expected instanceof Boolean) {
			b = (Boolean) expected;
		} else {
			try {
				b =  Boolean.parseBoolean(expected.toString());
			} catch (Throwable exception) {
				return false;
			}
		}
		return b.booleanValue() == actual.isIsTrue();
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual the number literal to test.
	 * @param expected the expected value.
	 * @return the validation status
	 */
	public static boolean shouldBe(XNumberLiteral actual, Object expected) {
		if (actual == null) {
			return false;
		}
		final Number number;
		if (expected instanceof Number) {
			number = (Number) expected;
		} else {
			try {
				number = NumberFormat.getInstance().parse(expected.toString());
			} catch (Throwable exception) {
				return false;
			}
		}
		final Number anumber = cleanNumber(actual.getValue());
		return number.doubleValue() == anumber.doubleValue();
	}

	/** Ensure that the given type literal is equal to the given type.
	 *
	 * @param actual the type literal to test.
	 * @param expected the name of the expected type.
	 * @return the validation status
	 */
	public static boolean shouldBe(XTypeLiteral actual, Object expected) {
		if (actual == null) {
			return false;
		}
		final String fqn;
		if (expected instanceof Class) {
			fqn = ((Class<?>) expected).getName();
		} else {
			fqn = expected.toString();
		}
		return actual.getType() != null
				&& Objects.equals(fqn, actual.getType().getQualifiedName());
	}

	/** Ensure that the given type literal is equal to the given list.
	 *
	 * @param actual the type literal to test.
	 * @param expected the name of the expected type.
	 * @return the validation status
	 */
	public static boolean shouldBe(XCollectionLiteral actual, Object expected) {
		if (actual == null || actual.getElements() == null) {
			return false;
		}
		return shouldIterate(
				actual.getElements().iterator(),
				expected,
				!(actual instanceof XSetLiteral));
	}

	/** Ensure that the given type literal is equal to the given type.
	 *
	 * @param actual the type literal to test.
	 * @param expected the name of the expected type.
	 * @return the validation status
	 */
	@SuppressWarnings({"checkstyle:returncount", "checkstyle:npathcomplexity"})
	public static boolean shouldBeLiteral(XExpression actual, Object expected) {
		if (actual instanceof XNumberLiteral) {
			return shouldBe((XNumberLiteral) actual, expected);
		}
		if (actual instanceof XBooleanLiteral) {
			return shouldBe((XBooleanLiteral) actual, expected);
		}
		if (actual instanceof XStringLiteral) {
			return shouldBe((XStringLiteral) actual, expected);
		}
		if (actual instanceof XTypeLiteral) {
			return shouldBe((XTypeLiteral) actual, expected);
		}
		if (actual instanceof XNullLiteral) {
			return Objects.equals("null", expected); //$NON-NLS-1$
		}
		if (actual instanceof XCollectionLiteral) {
			return shouldBe((XCollectionLiteral) actual, expected);
		}
		if (actual instanceof XBinaryOperation) {
			final XBinaryOperation op = (XBinaryOperation) actual;
			if ("operator_mappedTo".equals(op.getFeature().getSimpleName())) { //$NON-NLS-1$
				final Object key;
				final Object value;
				if (expected instanceof Pair<?, ?>) {
					key = ((Pair<?, ?>) expected).getKey();
					value = ((Pair<?, ?>) expected).getValue();
				} else if (expected instanceof Entry<?, ?>) {
					key = ((Entry<?, ?>) expected).getKey();
					value = ((Entry<?, ?>) expected).getValue();
				} else {
					return false;
				}
				return shouldBeLiteral(op.getLeftOperand(), key)
						&& shouldBeLiteral(op.getRightOperand(), value);
			}
		}
		return false;
	}

	/** Ensure that the given type has the given deprecated method.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID(TYPE, TYPE...)</li>
	 * <li>ID : TYPE</li>
	 * <li>ID(TYPE, TYPE...) : TYPE</li>
	 * </ul>
	 *
	 * @param type the type to check.
	 * @param name the name and prototype, e.g. <code>fct(java.lang.String):int</code>.
	 * @return the method.
	 */
	public static Method shouldHaveDeprecatedMethod(Class<?> type, String name) {
		return shouldHaveMethod(type, name, true);
	}

	/** Ensure that the given type has the given method.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID(TYPE, TYPE...)</li>
	 * <li>ID : TYPE</li>
	 * <li>ID(TYPE, TYPE...) : TYPE</li>
	 * </ul>
	 *
	 * @param type the type to check.
	 * @param name the name and prototype, e.g. <code>fct(java.lang.String):int</code>.
	 * @return the method.
	 */
	public static Method shouldHaveMethod(Class<?> type, String name) {
		return shouldHaveMethod(type, name, false);
	}

	/** Ensure that the given type has the given method.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID(TYPE, TYPE...)</li>
	 * <li>ID : TYPE</li>
	 * <li>ID(TYPE, TYPE...) : TYPE</li>
	 * </ul>
	 *
	 * @param type the type to check.
	 * @param name the name and prototype, e.g. <code>fct(java.lang.String):int</code>.
	 * @param deprecated indicates if the field must be deprecated.
	 * @return the method.
	 */
	@SuppressWarnings({"rawtypes", "checkstyle:npathcomplexity"})
	protected static Method shouldHaveMethod(Class<?> type, String name, boolean deprecated) {
		final Pattern pattern = Pattern.compile(
				"^([_a-zA-Z0-9]+)\\s*" //$NON-NLS-1$
				+ "(?:\\(\\s*([_a-zA-Z0-9.\\$]+(?:\\[\\])?\\s*" //$NON-NLS-1$
				+ "(?:,\\s*[_a-zA-Z0-9.\\$]+(?:\\[\\])?\\s*)*)\\))?" //$NON-NLS-1$
				+ "(?:\\s*:\\s*([_a-zA-Z0-9.\\$]+(?:\\[\\])?))?$"); //$NON-NLS-1$
		final Matcher matcher = pattern.matcher(name);
		if (matcher.matches()) {
			String paramText;
			try {
				paramText = matcher.group(2).trim();
			} catch (Throwable exception) {
				paramText = ""; //$NON-NLS-1$
			}
			String returnText;
			try {
				returnText = matcher.group(3).trim();
			} catch (Throwable exception) {
				returnText = ""; //$NON-NLS-1$
			}
			final String[] params;
			if (paramText.isEmpty()) {
				params = new String[0];
			} else {
				params = paramText.split("\\s*,\\s*"); //$NON-NLS-1$
			}
			final Class[] types = new Class[params.length];
			for (int i = 0; i < params.length; ++i) {
				if (params[i].endsWith("[]")) { //$NON-NLS-1$
					Class<?> paramType;
					final String typeName = params[i].substring(0, params[i].length() - 2);
					try {
						paramType = ReflectionUtil.forName(typeName);
					} catch (ClassNotFoundException e) {
						throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_2, typeName));
					}
					final Object tmpArray = Array.newInstance(paramType, 0);
					types[i] = tmpArray.getClass();
				} else {
					try {
						types[i] = ReflectionUtil.forName(params[i]);
					} catch (ClassNotFoundException e) {
						throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_2, params[i]));
					}
				}
			}
			final String fctName = matcher.group(1);
			Method method;
			try {
				method = type.getDeclaredMethod(fctName, types);
			} catch (NoSuchMethodException | SecurityException e) {
				method = null;
			}
			if (method == null) {
				throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_3, name));
			}
			final boolean validReturnType;
			if (returnText == null || returnText.isEmpty()) {
				validReturnType = void.class.equals(method.getReturnType())
						|| Void.class.equals(method.getReturnType());
			} else {
				final Class<?> rtype;
				try {
					rtype = ReflectionUtil.forName(returnText);
				} catch (ClassNotFoundException exception) {
					throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_2, returnText));
				}
				validReturnType = rtype.equals(method.getReturnType());
			}
			if (validReturnType) {
				final Deprecated deprecatedAnnotation = method.getAnnotation(Deprecated.class);
				if ((deprecated && deprecatedAnnotation != null) || (!deprecated && deprecatedAnnotation == null)) {
					return method;
				}
				throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_4, name));
			}
			throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_5, name, method.getReturnType().getName()));
		}
		throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_6, name, pattern));
	}

	/** Ensure that the given type has the given methods.
	 * The format of the prototypes may be: <ul>
	 * <li>ID</li>
	 * <li>ID(TYPE, TYPE...)</li>
	 * <li>ID : TYPE</li>
	 * <li>ID(TYPE, TYPE...) : TYPE</li>
	 * </ul>
	 * The methods with hidden names according to SARL are not considered.
	 *
	 * @param type the type to check.
	 * @param prototypes the prototypes, e.g. <code>fct(java.lang.String):int</code>.
	 * @return the validation status.
	 */
	public static boolean shouldHaveMethods(Class<?> type, String... prototypes) {
		return shouldHaveMethods(type,false, prototypes);
	}
	
	/** Ensure that the given type has the given methods.
	 * The format of the prototypes may be: <ul>
	 * <li>ID</li>
	 * <li>ID(TYPE, TYPE...)</li>
	 * <li>ID : TYPE</li>
	 * <li>ID(TYPE, TYPE...) : TYPE</li>
	 * </ul>
	 *
	 * @param type the type to check.
	 * @param considerHiddenNames indicates if the methods with hidden names are considered.
	 * @param prototypes the prototypes, e.g. <code>fct(java.lang.String):int</code>.
	 * @return the validation status.
	 * @since 0.12
	 */
	public static boolean shouldHaveMethods(Class<?> type, boolean considerHiddenNames, String... prototypes) {
		List<Method> methods = new ArrayList<>();
		for (final Method method : type.getDeclaredMethods()) {
			if (Flags.isPublic(method.getModifiers())) {
				final Deprecated deprecatedAnnotation = method.getAnnotation(Deprecated.class);
				if (deprecatedAnnotation == null &&
						(considerHiddenNames || !shouldBeHiddenName(method.getName()))) {
					methods.add(method);
				}
			}
		}
		for (final String prototype : prototypes) {
			final Method method = shouldHaveMethod(type, prototype, false);
			if (method != null) {
				methods.remove(method);
			} else {
				throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_0,
						prototypes, Iterables.toString(methods)));
			}
		}
		if (!methods.isEmpty()) {
			throw new ShouldException(MessageFormat.format(Messages.ShouldExtensions_1,
					type.getName(), Iterables.toString(methods)));
		}
		return true;
	}

	/** Ensure that the given string is an hidden name according to the SARL specification.
	 *
	 * @param string the text to validate.
	 * @return {@code true} if the given string is an hidden name.
	 * @since 0.12
	 */
	public static boolean shouldBeHiddenName(String string) {
		return SarlUtils.isHiddenMember(string);
	}

	/** Ensure that the given type has the given type has the given deprecated field.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID : TYPE</li>
	 * </ul>
	 *
	 * @param type the type to check.
	 * @param name the name and prototype, e.g. <code>x:int</code>.
	 * @return the field.
	 */
	public static Field shouldHaveDeprecatedField(Class<?> type, String name) {
		return shouldHaveField(type, name, true);
	}

	/** Ensure that the given type has the given type has the given field.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID : TYPE</li>
	 * </ul>
	 *
	 * @param type the type to check.
	 * @param name the name and prototype, e.g. <code>x:int</code>.
	 * @return the field.
	 */
	public static Field shouldHaveField(Class<?> type, String name) {
		return shouldHaveField(type, name, false);
	}

	/** Ensure that the given type has the given type has the given method.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID : TYPE</li>
	 * </ul>
	 *
	 * @param type the type to check.
	 * @param name the name and prototype, e.g. <code>x:int</code>.
	 * @param deprecated indicates if the field must be deprecated.
	 * @return the field.
	 */
	protected static Field shouldHaveField(Class<?> type, String name, boolean deprecated) {
		try {
			final Pattern pattern = Pattern.compile(
					"^([_a-zA-Z0-9]+)\\s*" //$NON-NLS-1$
					+ "(?:\\s*:\\s*([_a-zA-Z0-9.\\$]+(?:\\[\\])?))?$"); //$NON-NLS-1$
			final Matcher matcher = pattern.matcher(name);
			if (matcher.matches()) {
				final String fieldName = matcher.group(1);
				String fieldType;
				try {
					fieldType = matcher.group(2).trim();
				} catch (Throwable exception) {
					fieldType = ""; //$NON-NLS-1$
				}
				final Field field = type.getDeclaredField(fieldName);
				if (field == null) {
					return null;
				}
				final boolean validType;
				if (fieldType != null && !fieldType.isEmpty()) {
					Class<?> rtype;
					if (fieldType.endsWith("[]")) { //$NON-NLS-1$
						final Class<?> paramType = ReflectionUtil.forName(fieldType.substring(0, fieldType.length() - 2));
						final Object tmpArray = Array.newInstance(paramType, 0);
						rtype = tmpArray.getClass();
					} else {
						rtype = ReflectionUtil.forName(fieldType);
					}
					validType = rtype.equals(field.getType());
				} else {
					validType = true;
				}
				if (validType) {
					final Deprecated deprecatedAnnotation = field.getAnnotation(Deprecated.class);
					if ((deprecated && deprecatedAnnotation != null) || (!deprecated && deprecatedAnnotation == null)) {
						return field;
					}
				}
			}
		} catch (Throwable e) {
			//
		}
		return null;
	}

	/** Ensure that the given type extends specific types.
	 *
	 * @param type the type to check.
	 * @param expectedTypes the qualified names of the expected types, separated by comas.
	 * @return the validation status.
	 */
	public static boolean shouldExtend(Class<?> type, String expectedTypes) {
		if (type == null) {
			return false;
		}
		try {
			final Class<?> st = type.getSuperclass();
			final List<Class<?>> types = new LinkedList<>();
			if (st == null || Object.class.equals(st)) {
				if (type.isInterface()) {
					types.addAll(Arrays.asList(type.getInterfaces()));
				}
			} else {
				types.add(st);
			}
			//
			if (expectedTypes == null || expectedTypes.isEmpty()) {
				return types.isEmpty();
			}
			for (final String expectedType : expectedTypes.split("\\s*,\\s*")) { //$NON-NLS-1$
				final Class<?> et = ReflectionUtil.forName(expectedType);
				if (!types.remove(et)) {
					return false;
				}
			}
			return types.isEmpty();
		} catch (Throwable e) {
			//
		}
		return false;
	}

	/** Ensure that the given type has the number of members.
	 *
	 * @param type the type to check.
	 * @param expectedNbOfElements the expected number of elements.
	 * @return the validation status.
	 */
	public static boolean shouldHaveNbMembers(Class<?> type, int expectedNbOfElements) {
		if (type == null) {
			return false;
		}
		try {
			final int nb = type.getDeclaredConstructors().length
					+ type.getDeclaredFields().length
					+ type.getDeclaredMethods().length
					+ type.getDeclaredAnnotations().length
					+ type.getDeclaredClasses().length;
			return nb == expectedNbOfElements;
		} catch (Throwable e) {
			//
		}
		return false;
	}

	/** Ensure that the given map contains the elements.
	 *
	 * @param <K> type of the keys.
	 * @param <V> type of the values.
	 * @param map the map to check.
	 * @param reference the expected elements in the map.
	 * @return the validation status.
	 */
	public static <K, V> boolean shouldBe(Map<K, V> map, Map<? super K, ? super V> reference) {
		if (map == null) {
			return false;
		}
		if (reference == null || reference.isEmpty()) {
			return map.isEmpty();
		}
		for (final Entry<? super K, ? super V> entry : reference.entrySet()) {
			if (!map.containsKey(entry.getKey())) {
				return false;
			}
			final V currentValue = map.get(entry.getKey());
			if (!Objects.equals(currentValue, entry.getValue())) {
				return false;
			}
		}
		return map.size() == reference.size();
	}

	/** Ensure that the given URL is a property file with the given property name.
	 *
	 * @param propertyFile the name of the property file.
	 * @param propertyName the name of the property name.
	 * @return the validation status.
	 */
	public static boolean shouldHaveProperty(URL propertyFile, String propertyName) {
		try {
			final Properties props = new Properties();
			try (InputStream is = propertyFile.openStream()) {
				props.load(is);
				final String value = props.getProperty(propertyName, null);
				return value != null;
			}
		} catch (Throwable exception) {
			//
		}
		return false;
	}

	/** Ensure that the given URL is a property file with the given property.
	 *
	 * @param propertyFile the name of the property file.
	 * @param property the property.
	 * @return the validation status.
	 */
	public static boolean shouldHaveProperty(URL propertyFile, Pair<String, String> property) {
		if (propertyFile != null && property != null) {
			try {
				final Properties props = new Properties();
				try (InputStream is = propertyFile.openStream()) {
					props.load(is);
					final String value = props.getProperty(property.getKey(), null);
					return Objects.equals(value, property.getValue());
				}
			} catch (Throwable exception) {
				//
			}
		}
		return false;
	}

	/** Iterator on array.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ArrayIterator implements Iterator<Object> {

		private final Array array;

		private int index;

		private Object obj;

		/** Construct the iterator on array.
		 *
		 * @param array the array to iterate on.
		 */
		ArrayIterator(Array array) {
			this.array = array;
			searchNext();
		}

		private void searchNext() {
			try {
				this.obj = Array.get(this.array, this.index);
				++this.index;
			} catch (Throwable exception) {
				this.obj = null;
			}
		}

		@Override
		public boolean hasNext() {
			return this.obj != null;
		}

		@Override
		public Object next() {
			if (this.obj == null) {
				throw new NoSuchElementException();
			}
			final Object object = this.obj;
			searchNext();
			return object;
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}

	/** Major exception in a should function.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ShouldException extends RuntimeException {

		private static final long serialVersionUID = 3129485320879065188L;

		/** Constructor.
		 * 
		 * @param message the message.
		 */
		public ShouldException(String message) {
			super(message);
		}

	}
	
}
