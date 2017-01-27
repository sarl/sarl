/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.docs.utils;

import static org.jnario.lib.Assert.assertTrue;

import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URL;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
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
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.io.Files;

import org.arakhne.afc.vmutil.ClassLoaderFinder;
import org.arakhne.afc.vmutil.ClasspathUtil;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.ReflectionUtil;
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


/** Helper for tests.
 * This class should disappear when the Jnario API will provide
 * the similar features: <a href="https://github.com/sebastianbenz/Jnario/pull/142">issue #142</a>.
 *
 * <p>FIXME: https://github.com/sebastianbenz/Jnario/pull/142
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({"checkstyle:methodname"})
public final class SpecificationTools {

	/** Indicates if the network-based tests are mandatory, i.e. tests with network connections
	 * must be run and successful.
	 */
	public static final boolean MANDATORY_NETWORK_TESTS = false;

	private static final int HEX_RADIX = 16;

	private SpecificationTools() {
		//
	}

	/** Print the given message if the debug flag is set.
	 * The debug flags are the system property <code>jnario.debug</code>,
	 * and the environment variable <code>JNARIO_DEBUG</code>.
	 *
	 * @param message - the message to print.
	 */
	@SuppressWarnings("checkstyle:regexp")
	public static void printDebug(Object... message) {
		if (Boolean.getBoolean("jnario.debug") //$NON-NLS-1$
				|| Boolean.parseBoolean(System.getenv("JNARIO_DEBUG"))) { //$NON-NLS-1$
			for (final Object m : message) {
				if (m != null) {
					System.err.print(m.toString());
				}
			}
			System.err.println();
		}
	}

	/** Ensure that the given caller specification has a valid link
	 * to another Jnario specification with the given name.
	 *
	 * @param url - the url to check.
	 * @param source - the object that is containing the URL.
	 * @return the validation result.
	 */
	public static boolean should_beAccessibleFrom(String url, Object source) {
		return isJnarioLink(url, source) || isResourceLink(url, source);
	}

	/**
	 * Replies the URL of a resource.
	 *
	 * <p>You may use Unix-like syntax to write the resource path, ie.
	 * you may use slashes to separate filenames.
	 *
	 * <p>The name of <var>packagename</var> is translated into a resource
	 * path (by replacing the dots by slashes) and the given path
	 * is append to. For example, the two following codes are equivalent:<pre><code>
	 * Resources.getResources(Package.getPackage("org.arakhne.afc"), "/a/b/c/d.png");
	 * Resources.getResources("org/arakhne/afc/a/b/c/d.png");
	 * </code></pre>
	 *
	 * <p>If the <var>classLoader</var> parameter is <code>null</code>,
	 * the class loader replied by {@link ClassLoaderFinder} is used.
	 * If this last is <code>null</code>, the class loader of
	 * the Resources class is used.
	 *
	 * <p>Copied from <a href="https://github.com/gallandarakhneorg/afc/blob/master/core/vmutils/src/main/java/org/arakhne/afc/vmutil/Resources.java">AFC</a>.
	 *
	 * @param classLoader is the research scope. If <code>null</code>,
	 *     the class loader replied by {@link ClassLoaderFinder} is used.
	 * @param packagename is the package in which the resource should be located.
	 * @param path is the relative path of the resource in the package.
	 * @return the url of the resource or <code>null</code> if the resource was
	 *     not found in class paths.
	 */
	private static URL getResource(ClassLoader classLoader, Package packagename, String path) {
		if (packagename == null || path == null) {
			return null;
		}
		final StringBuilder buffer = new StringBuilder();
		buffer.append(packagename.getName().replaceAll(
				Pattern.quote("."), //$NON-NLS-1$
				java.util.regex.Matcher.quoteReplacement("/"))); //$NON-NLS-1$
		if (!path.startsWith("/")) { //$NON-NLS-1$
			buffer.append("/"); //$NON-NLS-1$
		}
		buffer.append(path);
		return getResource(packagename.getClass().getClassLoader(), buffer.toString());
	}

	/**
	 * Copied from <a href="https://github.com/gallandarakhneorg/afc/blob/master/core/vmutils/src/main/java/org/arakhne/afc/vmutil/StandardJREResourceWrapper.java">AFC</a>.
	 *
	 * @param classLoader - the class loader to use for loading the specified resource
	 * @param path - the path to the resource you're looking for
	 * @return the full URL of the corresponding resource
	 */
	private static URL getResource(ClassLoader classLoader, String path) {
		if (path == null) {
			return null;
		}
		String resourcePath = path;
		if (path.startsWith("/")) { //$NON-NLS-1$
			resourcePath = path.substring(1);
		}

		final ClassLoader loader = (classLoader == null) ? SpecificationTools.class.getClassLoader() : classLoader;
		assert loader != null;

		URL url = loader.getResource(resourcePath);

		if (url == null) {
			// Try to find in ./resources sub directory
			url = loader.getResource("resources/" + resourcePath); //$NON-NLS-1$
		}
		return url;
	}

	private static boolean isResourceLink(String url, Object source) {
		if (source == null || url == null) {
			return false;
		}
		// Check if it is a URL of a file path
		try {
			URL fileURL;
			try {
				fileURL = new URL(url);
			} catch (Throwable exception) {
				fileURL = new URL("file:" + url); //$NON-NLS-1$
			}
			if ("file".equalsIgnoreCase(fileURL.getProtocol())) { //$NON-NLS-1$
				// Get local resource
				final URL u = getResource(
						SpecificationTools.class.getClassLoader(),
						source.getClass().getPackage(),
						fileURL.getPath());
				if (u != null) {
					return true;
				}
			}
		} catch (Throwable exception) {
			//
		}
		return false;
	}

	@SuppressWarnings({"checkstyle:magicnumber", "checkstyle:npathcomplexity"})
	private static boolean isJnarioLink(String url, Object source) {
		if (source == null || url == null) {
			return false;
		}
		//
		try {
			String ref = url;
			if (ref.startsWith("#")) { //$NON-NLS-1$
				ref = "./" + source.getClass().getSimpleName() + ".html" + ref; //$NON-NLS-1$ //$NON-NLS-2$
			}

			String[] fragments = null;
			if (ref.contains(".html#")) { //$NON-NLS-1$
				final String[] parts = ref.split(java.util.regex.Matcher.quoteReplacement(".html#")); //$NON-NLS-1$
				assertEquals(
						String.format("Invalid link format: %s", ref), //$NON-NLS-1$
						new Integer(2), new Integer(parts.length));
				fragments = parts[1].split(java.util.regex.Matcher.quoteReplacement("_") + "+"); //$NON-NLS-1$ //$NON-NLS-2$
				final StringBuilder b = new StringBuilder();
				for (final String s : fragments) {
					if (s.length() > 1) {
						b.append(s.substring(0, 1).toUpperCase() + s.substring(1));
					} else {
						b.append(s.toUpperCase());
					}
				}
				if (parts[0].endsWith("Spec")) { //$NON-NLS-1$
					ref = parts[0].substring(0, parts[0].length() - 4)
							+ b.toString() + "Spec.html"; //$NON-NLS-1$
				} else {
					ref = parts[0] + b.toString() + "Spec.html"; //$NON-NLS-1$
				}
			}
			//
			if (!isJnarioSpec(source.getClass(), ref)) {

				// The specification could be a function of the Java class.
				if (fragments != null) {
					final StringBuilder operationName = new StringBuilder();
					for (final String fragment : fragments) {
						if (operationName.length() > 0) {
							operationName.append(fragment.substring(0, 1).toUpperCase() + fragment.substring(1).toLowerCase());
						} else {
							operationName.append(fragment.toLowerCase());
						}
					}
					final String operationNameStr = "_" + operationName.toString(); //$NON-NLS-1$
					try {
						source.getClass().getMethod(operationNameStr);
						return true;
					} catch (Throwable exception) {
						// Failure
					}
				}

				return false;
			}
			return true;
		} catch (Throwable exception) {
			return false;
		}
	}

	@SuppressWarnings("checkstyle:magicnumber")
	private static boolean isJnarioSpec(Class<?> callingSpecification, String reference) {
		String url = reference;
		//
		assertTrue(
				String.format("\"%s\" must end with \".html\".", //$NON-NLS-1$
				url.toString(), ".html"), //$NON-NLS-1$
				url.endsWith(".html")); //$NON-NLS-1$
		//
		url = url.substring(0, url.length() - 5);
		final File caller = new File(callingSpecification.getName().replaceAll(
				"\\.", File.separator)).getParentFile(); //$NON-NLS-1$
		final File resolved = new File(caller, url.replaceAll("\\/", File.separator)); //$NON-NLS-1$
		String resolvedPath = Files.simplifyPath(resolved.getPath());
		resolvedPath = resolvedPath.replaceAll(java.util.regex.Matcher.quoteReplacement(File.separator), "."); //$NON-NLS-1$
		try {
			ReflectionUtil.forName(resolvedPath);
			return true;
		} catch (Throwable exception) {
			return false;
		}
	}

	/** Ensure that the iterator replies the expected values in the given order.
	 *
	 * @param actual - the iterator to test.
	 * @param expected - the expected values.
	 * @return the validation status
	 */
	public static boolean should_iterate(Iterator<?> actual, Object expected) {
		return should_iterate(actual, expected, true);
	}

	/** Ensure that the iterator replies the expected values in the given order.
	 *
	 * @param actual - the iterator to test.
	 * @param expected - the expected values.
	 * @param significantOrder - indicates if the order of the elements is significant.
	 * @return the validation status
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	public static boolean should_iterate(Iterator<?> actual, Object expected, boolean significantOrder) {
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
					if (!should_beLiteral((XExpression) obj, eObj)) {
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
					if (should_beLiteral((XExpression) obj, eObj)) {
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

	/** Ensure that the string has the format of a date.
	 *
	 * @param actual - the string to parse.
	 * @param dateFormat - the expected format of the date, as
	 *     described in {@link SimpleDateFormat}. If <code>null</code>, the
	 *     default date format is considered.
	 * @return the validation status
	 */
	public static boolean should_beDate(String actual, String dateFormat) {
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

	private static boolean isValidURL(URL url, String requiredSchemes) {
		if (requiredSchemes != null && !requiredSchemes.isEmpty()) {
			boolean mustHaveScheme = false;
			for (final String s : requiredSchemes.trim().split("\\s*,\\s*")) { //$NON-NLS-1$
				if (!s.isEmpty()) {
					if (s.startsWith("!")) { //$NON-NLS-1$
						if (s.substring(1).equalsIgnoreCase(url.getProtocol())) {
							return false;
						}
					} else {
						mustHaveScheme = true;
						if (s.equalsIgnoreCase(url.getProtocol())) {
							return true;
						}
					}
				}
			}
			return !mustHaveScheme;
		}
		return true;
	}

	/** Ensure that the string has the format of an URL.
	 *
	 * @param actual - the string to parse.
	 * @param requiredSchemes - is a list of schemes that are supported.
	 *     If a scheme is prefix with the <code>!</code> character (without space),
	 *     then the scheme is not allowed.
	 *     If not given, all the schemes are allowed.
	 * @return the validation status
	 */
	public static boolean should_beURL(String actual, String requiredSchemes) {
		if (actual == null || actual.isEmpty()) {
			return false;
		}
		try {
			final URL u = FileSystem.convertStringToURL(actual, true);
			if (u == null) {
				return false;
			}
			if (isValidURL(u, requiredSchemes)) {
				try (InputStream is = u.openStream()) {
					is.read();
				} catch (Throwable exception) {
					if (MANDATORY_NETWORK_TESTS) {
						return false;
					}
					Logger.getLogger(SpecificationTools.class.getName()).warning("Unable to connect to: " //$NON-NLS-1$
							+ u);
				}
				return true;
			}
		} catch (Throwable exception)  {
			//
		}
		return false;
	}

	/** Ensure that the string has the format of an URL to the SARL API.
	 *
	 * @param actual - the string to parse.
	 * @param allowedAPIhostname - is a list of API base hostname to consider as valid.
	 *     If not given, only "www.sarl.io" is allowed.
	 * @return the validation status
	 */
	public static boolean should_beApiURL(String actual, String allowedAPIhostname) {
		if (actual == null || actual.isEmpty()) {
			return false;
		}
		try {
			final URL u = FileSystem.convertStringToURL(actual, true);
			if (u == null) {
				return false;
			}
			String[] validHostnames = {"www.sarl.io"}; //$NON-NLS-1$
			if (allowedAPIhostname != null && allowedAPIhostname.isEmpty()) {
				validHostnames = allowedAPIhostname.split("[ \t]*[,;][ \t]*"); //$NON-NLS-1$
			}
			final List<String> hosts = Arrays.asList(validHostnames);
			if (!hosts.contains(u.getHost())
				|| !u.getQuery().endsWith(".html") //$NON-NLS-1$
				|| !u.getPath().endsWith("index.html")) { //$NON-NLS-1$
				return false;
			}
			try (InputStream is = u.openStream()) {
				is.read();
			} catch (Throwable exception) {
				if (MANDATORY_NETWORK_TESTS) {
					return false;
				}
				Logger.getLogger(SpecificationTools.class.getName()).warning("Unable to connect to: " //$NON-NLS-1$
						+ u);
			}
			return true;
		} catch (Throwable exception)  {
			//
		}
		return false;
	}

	/** Ensure that the string has the format of a number.
	 *
	 * @param actual - the string to parse.
	 * @param numberFormat - the expected format of the number, as
	 *     described in {@link DecimalFormat}. If <code>null</code>, the
	 *     default date format is considered.
	 * @return the validation status
	 */
	public static boolean should_beNumber(String actual, String numberFormat) {
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

	/** Assert that two objects are equal.
	 *
	 * @param message - the error message.
	 * @param expected - the expected value.
	 * @param actual - the value to test.
	 */
	public static void assertEquals(String message, Object expected, Object actual) {
		assertTrue(message, Objects.equals(expected, actual));
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
	 * @param actual - the string literal to test.
	 * @param expected - the expected value.
	 * @return the validation status
	 */
	public static boolean _should_be(XStringLiteral actual, Object expected) {
		if (actual == null) {
			return false;
		}
		final String string = (expected == null) ? null : expected.toString();
		return Objects.equals(string, actual.getValue());
	}

	/** Ensure that the given boolean literal is equal to the given value.
	 *
	 * @param actual - the boolean literal to test.
	 * @param expected - the expected value.
	 * @return the validation status
	 */
	public static boolean _should_be(XBooleanLiteral actual, Object expected) {
		if (actual == null) {
			return false;
		}
		final Boolean b;
		if (expected instanceof Boolean) {
			b = (Boolean) expected;
		} else {
			try {
				b =  new Boolean(expected.toString());
			} catch (Throwable exception) {
				return false;
			}
		}
		return b.booleanValue() == actual.isIsTrue();
	}

	/** Ensure that the given number literal is equal to the given value.
	 *
	 * @param actual - the number literal to test.
	 * @param expected - the expected value.
	 * @return the validation status
	 */
	public static boolean _should_be(XNumberLiteral actual, Object expected) {
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
	 * @param actual - the type literal to test.
	 * @param expected - the name of the expected type.
	 * @return the validation status
	 */
	public static boolean _should_be(XTypeLiteral actual, Object expected) {
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
	 * @param actual - the type literal to test.
	 * @param expected - the name of the expected type.
	 * @return the validation status
	 */
	public static boolean _should_be(XCollectionLiteral actual, Object expected) {
		if (actual == null || actual.getElements() == null) {
			return false;
		}
		return should_iterate(
				actual.getElements().iterator(),
				expected,
				!(actual instanceof XSetLiteral));
	}

	/** Ensure that the given type literal is equal to the given type.
	 *
	 * @param actual - the type literal to test.
	 * @param expected - the name of the expected type.
	 * @return the validation status
	 */
	@SuppressWarnings({"checkstyle:returncount", "checkstyle:npathcomplexity"})
	public static boolean should_beLiteral(XExpression actual, Object expected) {
		if (actual instanceof XNumberLiteral) {
			return _should_be((XNumberLiteral) actual, expected);
		}
		if (actual instanceof XBooleanLiteral) {
			return _should_be((XBooleanLiteral) actual, expected);
		}
		if (actual instanceof XStringLiteral) {
			return _should_be((XStringLiteral) actual, expected);
		}
		if (actual instanceof XTypeLiteral) {
			return _should_be((XTypeLiteral) actual, expected);
		}
		if (actual instanceof XNullLiteral) {
			return Objects.equals("null", expected); //$NON-NLS-1$
		}
		if (actual instanceof XCollectionLiteral) {
			return _should_be((XCollectionLiteral) actual, expected);
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
				return should_beLiteral(op.getLeftOperand(), key)
						&& should_beLiteral(op.getRightOperand(), value);
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
	 * @param type - the type to check.
	 * @param name - the name and prototype, e.g. <code>fct(java.lang.String):int</code>.
	 * @return the validation status.
	 */
	public static boolean should_haveDeprecatedMethod(Class<?> type, String name) {
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
	 * @param type - the type to check.
	 * @param name - the name and prototype, e.g. <code>fct(java.lang.String):int</code>.
	 * @return the validation status.
	 */
	public static boolean should_haveMethod(Class<?> type, String name) {
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
	 * @param type - the type to check.
	 * @param name - the name and prototype, e.g. <code>fct(java.lang.String):int</code>.
	 * @param deprecated indicates if the field must be deprecated.
	 * @return the validation status.
	 */
	@SuppressWarnings({"rawtypes", "checkstyle:npathcomplexity"})
	protected static boolean shouldHaveMethod(Class<?> type, String name, boolean deprecated) {
		try {
			final Pattern pattern = Pattern.compile(
					"^([_a-zA-Z0-9]+)\\s*" //$NON-NLS-1$
					+ "(?:\\(\\s*([_a-zA-Z0-9.]+\\s*" //$NON-NLS-1$
					+ "(?:,\\s*[_a-zA-Z0-9.]+\\s*)*)\\))?" //$NON-NLS-1$
					+ "(?:\\s*:\\s*([_a-zA-Z0-9.]+))?$"); //$NON-NLS-1$
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
					types[i] = ReflectionUtil.forName(params[i]);
				}
				final String fctName = matcher.group(1);
				final Method method = type.getDeclaredMethod(fctName, types);
				if (method == null) {
					return false;
				}
				final boolean validReturnType;
				if (returnText == null || returnText.isEmpty()) {
					validReturnType = void.class.equals(method.getReturnType())
							|| Void.class.equals(method.getReturnType());
				} else {
					final Class<?> rtype = ReflectionUtil.forName(returnText);
					validReturnType = rtype.equals(method.getReturnType());
				}
				if (validReturnType) {
					final Deprecated deprecatedAnnotation = method.getAnnotation(Deprecated.class);
					return (deprecated && deprecatedAnnotation != null) || (!deprecated && deprecatedAnnotation == null);
				}
			}
		} catch (Throwable e) {
			//
		}
		return false;
	}

	/** Ensure that the given type has the given type has the given deprecated field.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID : TYPE</li>
	 * </ul>
	 *
	 * @param type - the type to check.
	 * @param name - the name and prototype, e.g. <code>x:int</code>.
	 * @return the validation status.
	 */
	public static boolean should_haveDeprecatedField(Class<?> type, String name) {
		return shouldHaveField(type, name, true);
	}

	/** Ensure that the given type has the given type has the given field.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID : TYPE</li>
	 * </ul>
	 *
	 * @param type - the type to check.
	 * @param name - the name and prototype, e.g. <code>x:int</code>.
	 * @return the validation status.
	 */
	public static boolean should_haveField(Class<?> type, String name) {
		return shouldHaveField(type, name, false);
	}

	/** Ensure that the given type has the given type has the given method.
	 * The format of the name may be: <ul>
	 * <li>ID</li>
	 * <li>ID : TYPE</li>
	 * </ul>
	 *
	 * @param type - the type to check.
	 * @param name - the name and prototype, e.g. <code>x:int</code>.
	 * @param deprecated indicates if the field must be deprecated.
	 * @return the validation status.
	 */
	protected static boolean shouldHaveField(Class<?> type, String name, boolean deprecated) {
		try {
			final Pattern pattern = Pattern.compile(
					"^([_a-zA-Z0-9]+)\\s*" //$NON-NLS-1$
					+ "(?:\\s*:\\s*([_a-zA-Z0-9.]+))?$"); //$NON-NLS-1$
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
					return false;
				}
				final boolean validType;
				if (fieldType != null && !fieldType.isEmpty()) {
					final Class<?> rtype = ReflectionUtil.forName(fieldType);
					validType = rtype.equals(field.getType());
				} else {
					validType = true;
				}
				if (validType) {
					final Deprecated deprecatedAnnotation = field.getAnnotation(Deprecated.class);
					return (deprecated && deprecatedAnnotation != null) || (!deprecated && deprecatedAnnotation == null);
				}
			}
		} catch (Throwable e) {
			//
		}
		return false;
	}

	/** Ensure that the given type extends specific types.
	 *
	 * @param type - the type to check.
	 * @param expectedTypes - the qualified names of the expected types, separated by comas.
	 * @return the validation status.
	 */
	public static boolean should_extend(Class<?> type, String expectedTypes) {
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

	/** Ensure that the given string is a valid Maven version number.
	 *
	 * @param actual - the string to test.
	 * @param allowSnapshot - indicates if the <code>-SNAPSHOT</code> postfix
	 *     is considered as valid.
	 * @return the validation status.
	 */
	public static boolean should_beMavenVersion(String actual, boolean allowSnapshot) {
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
	 * @param minVersion - the minimal version.
	 * @param maxVersion - the maximal version.
	 * @return the validation status.
	 */
	public static boolean should_beJavaRange(String minVersion, String maxVersion) {
		final Version jreV = parseJavaVersion(System.getProperty("java.version"), null); //$NON-NLS-1$
		printDebug("Current Java version: ", jreV); //$NON-NLS-1$
		if (jreV != null && minVersion != null) {
			final Version minV = parseJavaVersion(minVersion, null);
			printDebug("Min version:", minV); //$NON-NLS-1$
			if (minV != null) {
				Version maxV = null;
				if (maxVersion != null) {
					maxV = parseJavaVersion(maxVersion, null);
				}
				if (maxV == null) {
					printDebug("Max version: none"); //$NON-NLS-1$
					printDebug(minV, "<=", jreV); //$NON-NLS-1$
					return jreV.compareTo(minV) >= 0;
				}
				printDebug("Max version:", maxV); //$NON-NLS-1$
				printDebug(minV, "<=", jreV, "<", maxV); //$NON-NLS-1$//$NON-NLS-2$
				return jreV.compareTo(minV) >= 0 && jreV.compareTo(maxV) < 0;
			}
		}
		return false;
	}

	/** Ensure that the given type has the number of members.
	 *
	 * @param type - the type to check.
	 * @param expectedNbOfElements - the expected number of elements.
	 * @return the validation status.
	 */
	public static boolean should_haveNbMembers(Class<?> type, int expectedNbOfElements) {
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
	 * @param map - the map to check.
	 * @param reference - the expected elements in the map.
	 * @return the validation status.
	 */
	public static <K, V> boolean should_be(Map<K, V> map, Map<? super K, ? super V> reference) {
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
	 * @param propertyFile - the name of the property file.
	 * @param propertyName - the name of the property name.
	 * @return the validation status.
	 */
	public static boolean should_haveProperty(URL propertyFile, String propertyName) {
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
	 * @param propertyFile - the name of the property file.
	 * @param property - the property.
	 * @return the validation status.
	 */
	public static boolean should_haveProperty(URL propertyFile, Pair<String, String> property) {
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

	/** Replies the URL of the bundle's file.
	 *
	 * @param bundleName - the name of the bundle, i.e. the name of the jar file.
	 * @param filename - the name of the file.
	 * @return the URL, or <code>null</code>.
	 */
	public static URL getBundlePropertyURL(String bundleName, String filename) {
		try {
			final Iterator<URL> urls = ClasspathUtil.getClasspath();
			URL url;
			while (urls.hasNext()) {
				url = urls.next();
				final String resourceName = FileSystem.basename(url);
				if (resourceName != null && resourceName.startsWith(bundleName + "-")) { //$NON-NLS-1$
					return FileSystem.toJarURL(url, filename);
				}
			}
		} catch (Throwable exception) {
			//
		}
		return null;
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
		 * @param array - the array to iterate on.
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

}
