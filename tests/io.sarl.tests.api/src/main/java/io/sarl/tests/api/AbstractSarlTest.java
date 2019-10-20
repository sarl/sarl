/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
package io.sarl.tests.api;

import static com.google.common.collect.Iterables.any;
import static com.google.common.collect.Iterables.filter;

import java.io.InputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.inject.Provider;

import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.collect.Collections2;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import com.google.inject.Injector;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.XtextRunner;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.AssumptionViolatedException;
import org.junit.ComparisonFailure;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.model.Statement;
import org.mockito.ArgumentMatcher;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.internal.matchers.InstanceOf;
import org.mockito.internal.util.Primitives;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Version;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;

/** Abstract class that is providing useful tools for unit tests.
 *
 * This class provides assertion functions, clear any property
 * related to Sarl, and reset the attributes of the unit test that
 * are marked <code>@Mock</code>, <code>@InjectMocks</code> or
 * <code>@Nullable</code>.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@RunWith(XtextRunner.class)
@InjectWith(ExtendedSARLInjectorProvider.class)
public abstract class AbstractSarlTest extends Assert {

	/** URL of the Maven central repository.
	 */
	public static final String MAVEN_CENTRAL_REPOSITORY_URL = "http://repo1.maven.org/maven2/io/sarl/lang/io.sarl.lang.core/0.2.0/io.sarl.lang.core-0.2.0.pom";

	/** Timeout for connecting to the Maven central server (in milliseconds).
	 */
	public static final int MAVEN_CENTRAL_TIMEOUT = 15000;

	/** Precision of the floating point number epsilon-tests.
	 */
	public static final int DEFAULT_DECIMAL_COUNT = 6;

	@Inject
	protected ReflectExtensions reflect;

	@Inject
	private Injector injector;

	@Inject
	private ValidationTestHelper validationHelper;

	@Inject
	private ParseHelper<SarlScript> parser;

	@Inject
	private CompilationTestHelper compiler;

	@Inject
	private Provider<SarlJvmModelAssociations> associations;

	/** Replies the injector.
	 *
	 * @return the injector.
	 * @since 0.10
	 */
	protected Injector getInjector() {
		return this.injector;
	}
	
	/** Temporary fixing a bug in the class loading of Mockito 2.
	 * 
	 * @param type the type to mock.
	 * @return the mocked instance.
	 * @see http://stackoverflow.com/questions/37702952/classnotfoundexception-with-mockito-2-in-osgi
	 */
	public static <T> T mock(Class<T> type) {
		if (type == null) {
			return null;
		}
		final ClassLoader loader = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(Mockito.class.getClassLoader());
		try {
			return Mockito.mock(type);
		} finally {
			Thread.currentThread().setContextClassLoader(loader);
		}
	}

	/** Temporary fixing a bug in the class loading of Mockito 2.
	 * 
	 * @param instance the instance to spy.
	 * @return the spied instance.
	 * @see http://stackoverflow.com/questions/37702952/classnotfoundexception-with-mockito-2-in-osgi
	 */
	public static <T> T spy(T instance) {
		if (instance == null) {
			return null;
		}
		final ClassLoader loader = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(Mockito.class.getClassLoader());
		try {
			return Mockito.spy(instance);
		} finally {
			Thread.currentThread().setContextClassLoader(loader);
		}
	}

	/** Replies if the runtime environment is Eclipse.
	 *
	 * @return <code>true</code> if the runtime environment is Eclipse.
	 */
	protected static boolean isEclipseRuntimeEnvironment() {
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
	protected static void setGlobalLogLevel(Level level) {
		LogManager.getRootLogger().setLevel(level);
	}

	/** This rule permits to clean automatically the fields
	 * at the end of the test.
	 */
	@Rule
	public TestWatcher rootSarlWatchter = new TestWatcher() {
		private int getMockableFeatures() {
			int features = 0;
			Class<?> type = AbstractSarlTest.this.getClass();
			while (type != null && !AbstractSarlTest.class.equals(type)) {
				if (type.getAnnotation(ManualMocking.class) != null) {
					return 0;
				}
				for (Field field : type.getDeclaredFields()) {
					if (field.getAnnotation(Mock.class) != null) {
						features |= 0x1;
					} else if (field.getAnnotation(InjectMocks.class) != null) {
						features |= 0x2;
					}
				}
				type = type.getSuperclass();
			}
			return features;
		}
		@Override
		protected void starting(Description description) {
			// Check if the minimal version of Java is used for running the tests.
			final JavaVersion cVersion = JavaVersion.fromQualifier(System.getProperty("java.specification.version"));
			if (cVersion == null) {
				throw new Error("You must use JDK " + SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT + " or higher for running the tests.");
			}
			final JavaVersion mVersion = JavaVersion.fromQualifier(SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
			if (mVersion == null || !cVersion.isAtLeast(mVersion)) {
				throw new Error("You must use JDK " + SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT + " or higher for running the tests.");
			}
			final JavaVersion xVersion = JavaVersion.fromQualifier(SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
			// If null the max version that is specified into the SARL configuration is not yey supported by Xtext enumeration
			if (xVersion != null && cVersion.isAtLeast(xVersion)) {
				throw new Error("You must use JDK strictly below " + SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT + " for running the tests.");
			}
			//
			final int mockFeaturing = getMockableFeatures();
			if (mockFeaturing != 0) {
				MockitoAnnotations.initMocks(AbstractSarlTest.this);
			}
		}

		@Override
		public Statement apply(Statement base, Description description) {
			setGlobalLogLevel(Level.WARN);
			// This test is working only in Eclipse, or Maven/Tycho.
			TestScope scope = description.getAnnotation(TestScope.class);
			if (scope == null) {
				Class<?> enclosingType = description.getTestClass();
				while (scope == null && enclosingType != null) {
					scope = enclosingType.getAnnotation(TestScope.class);
					enclosingType = enclosingType.getEnclosingClass();
				}
			}
			if (scope != null) {
				if (!scope.tycho() && !scope.eclipse()) {
					throw new AssumptionViolatedException("not running on the current framework");
				} else if (scope.tycho() || scope.eclipse()) {
					boolean isEclipse = isEclipseRuntimeEnvironment();
					if (scope.tycho()) {
						Assume.assumeFalse(isEclipse);
					} else {
						Assume.assumeTrue(isEclipse);
					}
				}
				if (scope.needmavencentral()) {
					boolean canAccessNetwork = true;
					try {
						URL central = new URL(MAVEN_CENTRAL_REPOSITORY_URL);
						URLConnection connection = central.openConnection();
						connection.setConnectTimeout(MAVEN_CENTRAL_TIMEOUT);
						try (InputStream is = connection.getInputStream()) {
							byte[] buffer = new byte[128];
							int length = is.read(buffer);
							while (length > 0) {
								length = is.read(buffer);
							}
						}
					} catch (Exception exception) {
						canAccessNetwork = false;
					}
					Assume.assumeTrue(canAccessNetwork);
				}
			}
			//
			if (isIgnorable(base, description)) {
				throw new AssumptionViolatedException("This test is dynamically ignored.");
			}
			//
			return super.apply(base, description);
		}

		private boolean isNullable(Field field) {
			if (field.getAnnotation(Mock.class) != null
					|| field.getAnnotation(InjectMocks.class) != null) {
				return true;
			}
			for (Annotation annotation : field.getAnnotations()) {
				if ("Nullable".equals(annotation.annotationType().getSimpleName())
						|| "NonNullByDefault".equals(annotation.annotationType().getSimpleName())) {
					return true;
				}
			}
			return false;
		}

		@Override
		protected void finished(Description description) {
			// Clear the references to the mock objects or the injected objects
			Class<?> type = AbstractSarlTest.this.getClass();
			while (type != null && !Object.class.equals(type)) {
				for (Field field : type.getDeclaredFields()) {
					if (isNullable(field) && (field.getModifiers() & (Modifier.FINAL | Modifier.STATIC)) == 0) {
						boolean isAcc = field.isAccessible();
						try {
							field.setAccessible(true);
							field.set(AbstractSarlTest.this, null);
						} catch (Exception e) {
							throw new Error(e);
						} finally {
							field.setAccessible(isAcc);
						}
					}
				}
				type = type.getSuperclass();
			}
		}

	};

	/** Replies if the test could be ignored.
	 *
	 * @param base the base statement.
	 * @param description the test description.
	 * @return {@code true} if the test should be ignored.
	 */
	protected boolean isIgnorable(Statement base, Description description) {
		return false;
	}

	@Override
	protected void finalize() throws Throwable {
		this.injector = null;
		this.reflect = null;
		this.validationHelper = null;
		this.parser = null;
		this.associations = null;
		this.rootSarlWatchter = null;
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
				boolean acc = field.isAccessible();
				try {
					field.setAccessible(true);
					field.set(instance, fieldValue);
					return;
				} finally {
					field.setAccessible(acc);
				}
			} catch (Throwable exception) {
				type = type.getSuperclass();
			}
		}
		throw new NoSuchFieldError(fieldName);
	}

	/** Assert the values are equal.
	 *
	 * @param expected the expected value.
	 * @param actual the actual value.
	 * @param epsilon the precision.
	 */
	public static void assertEpsilonEquals(double expected, double actual) {
		if (!isEpsilonEquals(expected, actual, true)) {
			throw new ComparisonFailure("Values are not equal.", Double.toString(expected), Double.toString(actual));
		}
	}

	/** Replies if two values are equals at espilon.
	 *
	 * @param v1 the first value.
	 * @param v2 the second value.
	 * @param isNaNEqual indicates if the NaN value is equals to itself.
	 * @return <code>true</code> or <code>false</code>
	 */
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

	/** Replies if two values are equals at espilon.
	 *
	 * @param v1 the first value.
	 * @param v2 the second value.
	 * @param precision is the number of decimal digits to test.
	 * @return <code>true</code> or <code>false</code>
	 */
	public static boolean isEpsilonEquals(BigDecimal v1, BigDecimal v2, int precision) {
		final BigDecimal ma = v1.movePointRight(precision);
		final BigDecimal mb = v2.movePointRight(precision);
		BigDecimal aa = ma.setScale(0, BigDecimal.ROUND_HALF_UP);
		BigDecimal bb = mb.setScale(0, BigDecimal.ROUND_HALF_UP);
		if (aa.compareTo(bb) == 0) {
			return true;
		}
		aa = ma.setScale(0, BigDecimal.ROUND_DOWN);
		bb = mb.setScale(0, BigDecimal.ROUND_DOWN);
		return aa.compareTo(bb) == 0;
	}

	/**
	 * Test if the given exception has a cause of the given type.
	 *
	 * If the given exception has no cause, it is the cause.
	 *
	 * @param <T> - the type of the expected cause.
	 * @param expected the type of the expected cause.
	 * @param actual the exception to test.
	 * @return the cause.
	 */
	public static <T extends Throwable> T assertCause(Class<T> expected, Throwable actual) {
		Throwable cause = actual;
		while (cause != null && cause.getCause() != null && cause.getCause() != cause) {
			cause = cause.getCause();
		}
		if (cause == null) {
			cause = actual;
		}
		assertTrue("Unexpected type of exception's cause. Expected: " + expected.getName() + ". Actual: "
				+ cause.getClass().getName(), expected.isInstance(cause));
		return expected.cast(cause);
	}

	/** Test if the objects are equal.
	 *
	 * @param message the message.
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertEquals(String message, Object expected, Object actual) {
		if (!Objects.equal(expected, actual)) {
			String s1 = java.util.Objects.toString(expected);
			String s2 = java.util.Objects.toString(actual);
			throw new ComparisonFailure(message, s1, s2);
		}
	}

	/** Test if the objects are equal.
	 *
	 * @param message the message.
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertEquals(Object expected, Object actual) {
		assertEquals(null, expected, actual);
	}

	/** Check if the given value is <code>null</code> or empty.
	 *
	 * @param actual
	 */
	public static void assertNullOrEmpty(Iterable<?> actual) {
		if (actual != null) {
			assertFalse("Not null nor empty", actual.iterator().hasNext());
		}
	}

	/** Check if the given value is <code>null</code> or empty.
	 *
	 * @param actual
	 */
	public static void assertNullOrEmpty(String actual) {
		if (!Strings.isNullOrEmpty(actual)) {
			fail("Not null nor empty. Actual value: " + actual);
		}
	}

	/** Check if the given value is not <code>null</code> nor empty.
	 *
	 * @param actual
	 */
	public static void assertNotNullOrEmpty(String actual) {
		if (Strings.isNullOrEmpty(actual)) {
			fail("Null or empty.");
		}
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertContains(Iterable<?> actual, Object... expected) {
		assertContainsCollection(actual, Arrays.asList(expected));
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertContainsCollection(Iterable<?> actual, Iterable<?> expected) {
		assertNotNull(actual);
		Collection<Object> la = new ArrayList<>();
		Iterables.addAll(la, actual);
		Collection<Object> le = new ArrayList<>();
		Iterables.addAll(le, expected);

		final SortedSet<String> unexpectedElements = new TreeSet<>();

		Iterator<?> it1 = la.iterator();
		while (it1.hasNext()) {
			Object ac = it1.next();
			it1.remove();
			if (ac != null && !le.remove(ac)) {
				unexpectedElements.add(ac.toString());
			}
		}

		if (!unexpectedElements.isEmpty()) {
			fail("Unexpected elements:\n" + unexpectedElements.toString() + "\nActual elements are:\n" +
					Iterables.toString(actual) + "\nExpected elements are:\n" + Iterables.toString(expected));
		} else if (!le.isEmpty()) {
			fail("Expecting the following elements:\n" + le.toString() + "\nActual elements are:\n" +
					Iterables.toString(actual) + "\nExpected elements are:\n" + Iterables.toString(expected));
		}
	}

	/** Test if the actual collection/iterable contains at least all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertPartlyContains(Iterable<?> actual, Object... expected) {
		assertPartlyContainsCollection(actual, Arrays.asList(expected));
	}

	/** Test if the actual collection/iterable contains at least all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertPartlyContainsCollection(Iterable<?> actual, Iterable<?> expected) {
		assertNotNull(actual);
		Collection<Object> la = new ArrayList<>();
		Iterables.addAll(la, actual);
		Collection<Object> le = new ArrayList<>();
		Iterables.addAll(le, expected);

		Iterator<?> it1 = la.iterator();
		while (it1.hasNext()) {
			Object ac = it1.next();
			it1.remove();
			le.remove(ac);
		}

		if (!le.isEmpty()) {
			fail("Expecting the following elements:\n" + le.toString() + "\nbut was:\n" +
					Iterables.toString(actual));
		}
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertContainsStrings(Iterable<?> actual, String... expected) {
		assertContainsStringCollection(actual, Arrays.asList(expected));
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertContainsStringCollection(Iterable<?> actual, Iterable<String> expected) {
		assertNotNull(actual);
		Collection<Object> la = new ArrayList<>();
		Iterables.addAll(la, actual);
		Collection<String> le = new ArrayList<>();
		Iterables.addAll(le, expected);

		Iterator<?> it1 = la.iterator();
		while (it1.hasNext()) {
			Object ac = it1.next();
			it1.remove();
			if (!le.remove(ac.toString())) {
				fail("Unexpecting element: " + ac);
				return;
			}
		}

		if (!le.isEmpty()) {
			fail("Expecting the following elements:\n" + le.toString() + "\nbut was:\n" +
					Iterables.toString(actual));
		}
	}

	/** Assert if the value is the string representation of
	 * the boolean vlaue <code>true</code>.
	 *
	 * @param actual the value.
	 */
	public static void assertTrueStr(String actual) {
		assertTrueStr(null, actual);
	}

	/** Assert if the value is the string representation of
	 * the boolean vlaue <code>true</code>.
	 *
	 * @param message the error message.
	 * @param actual the value.
	 */
	public static void assertTrueStr(String message, String actual) {
		assertEquals(message, Boolean.TRUE.toString(), actual);
	}

	/** Assert if the value is the string representation of
	 * the boolean vlaue <code>false</code>.
	 *
	 * @param actual the value.
	 */
	public static void assertFalseStr(String actual) {
		assertFalseStr(null, actual);
	}

	/** Assert if the value is the string representation of
	 * the boolean vlaue <code>false</code>.
	 *
	 * @param message the error message.
	 * @param actual the value.
	 */
	public static void assertFalseStr(String message, String actual) {
		assertEquals(message, Boolean.FALSE.toString(), actual);
	}

	/** Assert if the system property with the given name has
	 * the boolean value <code>true</code>.
	 *
	 * The property must be defined
	 *
	 * @param name the name of the property.
	 */
	public static void assertTrueProperty(String name) {
		String v = System.getProperty(name);
		if (Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is not defined.");
		}
		assertTrueStr("The property '" + name + "' is expected to be true.", v);
	}

	/** Assert if the system property with the given name has
	 * the boolean value <code>false</code>.
	 *
	 * @param name the name of the property.
	 */
	public static void assertFalseProperty(String name) {
		String v = System.getProperty(name);
		if (Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is not defined.");
		}
		assertFalseStr("The property '" + name + "' is expected to be true.", v);
	}

	/** Assert if the system property with the given name has
	 * the boolean value <code>false</code>.
	 *
	 * @param name the name of the property.
	 */
	public static void assertNullProperty(String name) {
		String v = System.getProperty(name);
		if (!Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is expected to be undefined; but is has the value: " + v);
		}
	}

	/** Assert if the system property with the given name has
	 * the given value.
	 *
	 * @param name the name of the property.
	 * @param value the value of the property.
	 */
	public static void assertProperty(String name, String value) {
		if (Strings.isNullOrEmpty(value)) {
			assertNullProperty(name);
		} else {
			String v = System.getProperty(name);
			if (Strings.isNullOrEmpty(v)) {
				fail("The property '" + name + "' is expected to be defined." + v);
			}
			assertEquals(value, v);
		}
	}

	/** Assert that the given value is stricty positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyPositive(int actual) {
		if (actual <= 0) {
			fail("Expecting a strictly positive number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty negative.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyNegative(int actual) {
		if (actual >= 0) {
			fail("Expecting a strictly negative number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertPositiveOrZero(int actual) {
		if (actual < 0) {
			fail("Expecting a positive or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is negative or zero.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNegativeOrZero(int actual) {
		if (actual > 0) {
			fail("Expecting a negative or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyPositive(float actual) {
		if (actual <= 0f) {
			fail("Expecting a strictly positive number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty negative.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyNegative(float actual) {
		if (actual >= 0f) {
			fail("Expecting a strictly negative number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertPositiveOrZero(float actual) {
		if (actual < 0f) {
			fail("Expecting a positive or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is negative or zero.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNegativeOrZero(float actual) {
		if (actual > 0f) {
			fail("Expecting a negative or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyPositive(double actual) {
		if (actual <= 0.) {
			fail("Expecting a strictly positive number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty negative.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyNegative(double actual) {
		if (actual >= 0.) {
			fail("Expecting a strictly negative number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertPositiveOrZero(double actual) {
		if (actual < 0.) {
			fail("Expecting a positive or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is negative or zero.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNegativeOrZero(double actual) {
		if (actual > 0.) {
			fail("Expecting a negative or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is equal to zero.
	 *
	 * @param actual the value to test.
	 */
	public static void assertZero(int actual) {
		assertZero(null, actual);
	}

	/** Assert that the given value is equal to zero.
	 *
	 * @param message the error message.
	 * @param actual the value to test.
	 */
	public static void assertZero(String message, int actual) {
		if (actual != 0) {
			String msg;
			if (!Strings.isNullOrEmpty(message)) {
				msg = message + ". "; //$NON-NLS-1$
			} else {
				msg = ""; //$NON-NLS-1$
			}
			fail(msg + "Expecting a zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is NaN.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNaN(float actual) {
		assertNaN(null, actual);
	}

	/** Assert that the given value is NaN.
	 *
	 * @param message the error message.
	 * @param actual the value to test.
	 */
	public static void assertNaN(String message, float actual) {
		if (!Float.isNaN(actual)) {
			String msg;
			if (!Strings.isNullOrEmpty(message)) {
				msg = message + ". "; //$NON-NLS-1$
			} else {
				msg = ""; //$NON-NLS-1$
			}
			fail(msg + "Expecting NaN, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is NaN.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNaN(double actual) {
		assertNaN(null, actual);
	}

	/** Assert that the given value is NaN.
	 *
	 * @param message the error message.
	 * @param actual the value to test.
	 */
	public static void assertNaN(String message, double actual) {
		if (!Double.isNaN(actual)) {
			String msg;
			if (!Strings.isNullOrEmpty(message)) {
				msg = message + ". "; //$NON-NLS-1$
			} else {
				msg = ""; //$NON-NLS-1$
			}
			fail(msg + "Expecting NaN, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the two given arrays contain the same values even
	 * they are not in the same order.
	 *
	 * @param expected the expected values.
	 * @param actual the actual values.
	 */
	public static <T> void assertArraySimilar(T[] expected, T[] actual) {
		List<T> expectedValues = new ArrayList<>(Arrays.asList(expected));
		List<T> actualValues = new ArrayList<>(Arrays.asList(actual));
		Iterator<T> iterator = expectedValues.iterator();
		while (iterator.hasNext()) {
			T v = iterator.next();
			iterator.remove();
			if (!actualValues.remove(v)) {
				fail("Expecting a value in the array: " + v); //$NON-NLS-1$
			}
		}
		if (!expectedValues.isEmpty()) {
			fail("Expecting a value(s) in the array: " + expectedValues); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is inside the array..
	 *
	 * @param expected the array.
	 * @param actual the value.
	 */
	public static <T> void assertArrayContains(T[] expected, T actual) {
		for (T value : expected) {
			if (Objects.equal(actual, value)) {
				return;
			}
		}
		fail("Unable to find the value in the array, value: " + actual //$NON-NLS-1$
				+ "\narray: " + Arrays.toString(expected)); //$NON-NLS-1$
	}

	/** Replies the OS-dependent line separator.
	 *
	 * @return the line separator from the {@code "line.separator"} property, or {@code "\n"}.
	 * @since 0.5
	 */
	public static String getLineSeparator() {
		final String nl = System.getProperty("line.separator");
		if (Strings.isNullOrEmpty(nl)) {
			return "\n";
		}
		return nl;
	}

	/** Helper for writting a multiline string in unit tests, which supports the
	 * OS-dependent line separator.
	 *
	 * @param lines the lines in the string.
	 * @return the complete multiline string.
	 */
	public static String multilineString(Object... lines) {
		return Joiner.on(getLineSeparator()).join(lines);
	}

	/** Assert that the given iterable object replies the expected identifiers.
	 *
	 * The order of the identifier is significant.
	 *
	 * @param actualReferences the actual elements.
	 * @param expectedIdentifiers the expected elements.
	 * @see JvmTypeReference#getIdentifier()
	 */
	public static void assertTypeReferenceIdentifiers(Iterable<? extends JvmTypeReference> actualReferences, String... expectedIdentifiers) {
		int i = 0;
		for (JvmTypeReference reference : actualReferences) {
			assertTypeReferenceIdentifier(reference, expectedIdentifiers[i]);
			++i;
		}
		if (i < expectedIdentifiers.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedIdentifiers)
			+ "Actual: " + Iterables.toString(actualReferences));
		}
	}

	/** Assert the the given type reference has the given identifier.
	 *
	 * @param actualReference the actual type reference.
	 * @param expectedIdentifier the expected identifier.
	 * @see JvmTypeReference#getIdentifier()
	 */
	public static void assertTypeReferenceIdentifier(JvmTypeReference actualReference, String expectedIdentifier) {
		if (actualReference == null) {
			assertEquals("void", expectedIdentifier);
			return;
		}
		assertEquals("Unexpected type reference", expectedIdentifier, actualReference.getIdentifier());
	}

	/** Assert that the given actual formal parameters have the expected names.
	 *
	 * The order of the parameters and the expected names is significant.
	 *
	 * @param actualFormalParameters the list of the formal parameters.
	 * @param expectedParameterNames the expected names for the formal parameters.
	 */
	public static void assertParameterNames(Iterable<? extends XtendParameter> actualFormalParameters, String... expectedParameterNames) {
		int i = 0;
		for (XtendParameter parameter : actualFormalParameters) {
			assertEquals("Unexpected parameter: " + parameter + ". Expected: " + expectedParameterNames[i],
					parameter.getName(), expectedParameterNames[i]);
			++i;
		}
		if (i < expectedParameterNames.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedParameterNames)
			+ "Actual: " + Iterables.toString(actualFormalParameters));
		}
	}

	/** Assert that the given actual formal parameters have the expected types.
	 *
	 * The order of the parameters and the expected types is significant.
	 *
	 * @param actualFormalParameters the list of the formal parameters.
	 * @param expectedParameterTypes the expected types for the formal parameters.
	 */
	public static void assertParameterTypes(Iterable<? extends XtendParameter> actualFormalParameters, String... expectedParameterTypes) {
		int i = 0;
		for (XtendParameter parameter : actualFormalParameters) {
			assertTypeReferenceIdentifier(
					parameter.getParameterType(), expectedParameterTypes[i]);
			++i;
		}
		if (i < expectedParameterTypes.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedParameterTypes)
			+ "Actual: " + Iterables.toString(actualFormalParameters));
		}
	}

	/** Assert that the given actual formal parameters have the expected default values.
	 *
	 * The order of the parameters and the expected types is significant.
	 *
	 * The parameter <code>expectedDefaultValues</code> is a sequence of pairs, where
	 * the first element is the type of the default value and the second element is
	 * the representation of the default value.
	 * If the first element of the pair is null (meaning no default value), then
	 * the second element must be missed.
	 *
	 * @param actualFormalParameters the list of the formal parameters.
	 * @param expectedDefaultValues the expected default values.
	 */
	public static void assertParameterDefaultValues(Iterable<? extends XtendParameter> actualFormalParameters, Object... expectedDefaultValues) {
		int i = 0;
		for (XtendParameter parameter : actualFormalParameters) {
			if (expectedDefaultValues[i] == null) {
				if (parameter instanceof SarlFormalParameter) {
					assertNull("No default value expected", ((SarlFormalParameter) parameter).getDefaultValue());
				}
			} else {
				assertTrue(parameter instanceof SarlFormalParameter);
				assertTrue("The #" + i + " in expectedDefaultValues is not a Class", expectedDefaultValues[i] instanceof Class);
				Class type = (Class) expectedDefaultValues[i];
				assertTrue("Unexpected type for the default value.", type.isInstance(((SarlFormalParameter) parameter).getDefaultValue()));
				if (XNumberLiteral.class.isAssignableFrom(type)) {
					++i;
					assertEquals(expectedDefaultValues[i], ((XNumberLiteral) ((SarlFormalParameter) parameter).getDefaultValue()).getValue());
				} else if (XStringLiteral.class.isAssignableFrom(type)) {
					++i;
					assertEquals(expectedDefaultValues[i], ((XStringLiteral) ((SarlFormalParameter) parameter).getDefaultValue()).getValue());
				} else if (XNullLiteral.class.isAssignableFrom(type)) {
					//
				} else {
					throw new RuntimeException("Unsupported type of literal for this assertion function");
				}
			}
			++i;
		}
		if (i < expectedDefaultValues.length) {
			fail("Not enough default values. Expected: " + Arrays.toString(expectedDefaultValues)
			+ "Actual: " + Iterables.toString(actualFormalParameters));
		}
	}

	/** Assert that the last parameter in the given actual formal parameters is variadic.
	 *
	 * @param actualFormalParameters
	 */
	public static void assertParameterVarArg(Iterable<? extends XtendParameter> actualFormalParameters) {
		Iterator<? extends XtendParameter> iterator = actualFormalParameters.iterator();
		XtendParameter lastParam = null;
		while (iterator.hasNext()) {
			lastParam = iterator.next();
		}
		if (lastParam == null || !lastParam.isVarArg()) {
			fail("The last parameter is expected to be a variadic parameter.");
		}
	}

	/** Assert that the last parameter in the given actual formal parameters is not variadic.
	 *
	 * @param actualFormalParameters
	 */
	public static void assertNoParameterVarArg(Iterable<? extends XtendParameter> actualFormalParameters) {
		Iterator<? extends XtendParameter> iterator = actualFormalParameters.iterator();
		XtendParameter lastParam = null;
		while (iterator.hasNext()) {
			lastParam = iterator.next();
		}
		if (lastParam != null && lastParam.isVarArg()) {
			fail("The last parameter is expected to be not a variadic parameter.");
		}
	}

	/** Assert the actual XExpression is of the given type and initialized with the given literal.
	 *
	 * @param actualExpression the expression to test.
	 * @param expectedType the expected type of expression.
	 * @param expectedValue the expected value.
	 */
	public static void assertXExpression(XExpression actualExpression, Class<? extends XExpression> expectedType, String expectedValue) {
		assertTrue("Expecting type of expression: " + expectedType.getName(),
				expectedType.isInstance(actualExpression));
		if (XNumberLiteral.class.isAssignableFrom(expectedType)) {
			assertEquals("Invalid value.", expectedValue, ((XNumberLiteral) actualExpression).getValue());
		}
		else if (XStringLiteral.class.isAssignableFrom(expectedType)) {
			assertEquals("Invalid value.", expectedValue, ((XStringLiteral) actualExpression).getValue());
		}
		else if (XNullLiteral.class.isAssignableFrom(expectedType)) {
			//
		}
	}

	/** Assert the actual object is a not-null instance of the given type.
	 *
	 * @param actualExpression the expected type.
	 * @param expectedType the instance.
	 */
	public static void assertInstanceOf(Class<?> expected, Object actual) {
		assertInstanceOf(null, expected, actual);
	}

	/** Assert the actual object is a not-null instance of the given type.
	 *
	 * @param message the error message.
	 * @param actualExpression the expected type.
	 * @param expectedType the instance.
	 */
	public static void assertInstanceOf(String message, Class<?> expected, Object actual) {
		String m = message;
		if (m == null) {
			m = "Unexpected object type.";
		}
		if (actual == null) {
			fail(m);
		} else if (!expected.isInstance(actual)) {
			throw new ComparisonFailure(
					m,
					expected.getName(),
					actual.getClass().getName());
		}
	}

	/** Create an instance of agent
	 */
	protected SarlAgent agent(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlAgent) decls.get(decls.size() - 1);
	}

	/** Create an instance of agent
	 */
	protected SarlAgent agent(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlAgent) decls.get(decls.size() - 1);
	}

	/** Create an instance of capacity.
	 */
	protected SarlCapacity capacity(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlCapacity) decls.get(decls.size() - 1);
	}

	/** Create an instance of capacity.
	 */
	protected SarlCapacity capacity(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlCapacity) decls.get(decls.size() - 1);
	}

	/** Create an instance of event.
	 */
	protected SarlEvent event(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlEvent) decls.get(decls.size() - 1);
	}

	/** Create an instance of event.
	 */
	protected SarlEvent event(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlEvent) decls.get(decls.size() - 1);
	}

	/** Create an instance of skill.
	 */
	protected SarlSkill skill(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlSkill) decls.get(decls.size() - 1);
	}

	/** Create an instance of skill.
	 */
	protected SarlSkill skill(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlSkill) decls.get(decls.size() - 1);
	}

	/** Create an instance of behavior.
	 */
	protected SarlBehavior behavior(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlBehavior) decls.get(decls.size() - 1);
	}

	/** Create an instance of behavior.
	 */
	protected SarlBehavior behavior(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlBehavior) decls.get(decls.size() - 1);
	}

	/** Create an instance of class.
	 */
	protected SarlClass clazz(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlClass) decls.get(decls.size() - 1);
	}

	/** Create an instance of class.
	 */
	protected SarlClass clazz(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlClass) decls.get(decls.size() - 1);
	}

	/** Create a SARL script.
	 */
	protected SarlScript file(String string) throws Exception {
		return file(string, false);
	}

	/** Replies the parse helper.
	 *
	 * @return the parse helper.
	 * @since 0.7
	 */
	protected ParseHelper<SarlScript> getParseHelper() {
		return this.parser;
	}

	/** Replies the compile helper.
	 *
	 * @return the compile helper.
	 * @since 0.9
	 */
	protected CompilationTestHelper getCompileHelper() {
		return this.compiler;
	}

	/** Create an instance of class.
	 */
	protected SarlScript file(String string, boolean validate) throws Exception {
		return file(string, null, validate);
	}

	/** Create an instance of class.
	 * @since 0.9
	 */
	protected SarlScript file(String string, ResourceSet resourceSet, boolean validate) throws Exception {
		SarlScript script;
		if (resourceSet == null) {
			script = getParseHelper().parse(string);
		} else {
			script = getParseHelper().parse(string, resourceSet);
		}
		if (validate) {
			Resource resource = script.eResource();
			ResourceSet resourceSet0 = resource.getResourceSet();
			if (resourceSet0 instanceof XtextResourceSet) {
				((XtextResourceSet) resourceSet0).setClasspathURIContext(getClass());
			}
			assertEquals(resource.getErrors().toString(), 0, resource.getErrors().size());
			Collection<Issue> issues = Collections2.filter(issues(resource), new Predicate<Issue>() {
				@Override
				public boolean apply(Issue input) {
					return input.getSeverity() == Severity.ERROR;
				}
			});
			assertTrue("Resource contained errors : " + issues.toString(), issues.isEmpty());
		}
		return script;
	}

	/** Validate the given file and reply the issues.
	 */
	protected List<Issue> issues(SarlScript file) {
		return issues(file.eResource());
	}

	/** Validate the given resource and reply the issues.
	 */
	protected List<Issue> issues(Resource resource) {
		return this.validationHelper.validate(resource);
	}

	/** Validate the given file and reply the validator.
	 */
	protected Validator validate(SarlScript file) {
		return validate(file.eResource());
	}

	/** Validate the given resource and reply the validator.
	 */
	protected Validator validate(Resource resource) {
		Validator validator = new XtextValidator(resource, this.validationHelper);
		this.injector.injectMembers(validator);
		return validator;
	}

	/** Create an instance of annotation type.
	 */
	protected SarlAnnotationType annotationType(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlAnnotationType) decls.get(decls.size() - 1);
	}

	/** Create an instance of annotation type.
	 */
	protected SarlAnnotationType annotationType(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlAnnotationType) decls.get(decls.size() - 1);
	}

	/** Create an instance of interface.
	 */
	protected SarlInterface interfaze(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlInterface) decls.get(decls.size() - 1);
	}

	/** Create an instance of interface.
	 */
	protected SarlInterface interfaze(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlInterface) decls.get(decls.size() - 1);
	}

	/** Create an instance of enumeration.
	 */
	protected SarlEnumeration enumeration(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlEnumeration) decls.get(decls.size() - 1);
	}

	/** Create an instance of enumeration.
	 */
	protected SarlEnumeration enumeration(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlEnumeration) decls.get(decls.size() - 1);
	}

	/** Create an instance of function.
	 */
	protected SarlAction function(String string, String... prefix) throws Exception {
		SarlClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "class Foo { " + string + "}");
		return (SarlAction) clazz.getMembers().get(0);
	}

	/** Create an instance of function.
	 */
	protected SarlAction function(String string, boolean validate, String... prefix) throws Exception {
		SarlClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "class Foo { " + string + "}");
		return (SarlAction) clazz.getMembers().get(0);
	}

	/** Create an instance of JVM function.
	 */
	protected JvmOperation jvmOperation(String string, String... prefix) throws Exception {
		SarlAction action = function(string, prefix);
		return (JvmOperation) this.associations.get().getPrimaryJvmElement(action);
	}

	/** Create an instance of JVM function.
	 */
	protected JvmOperation jvmOperation(String string, boolean validate, String... prefix) throws Exception {
		SarlAction action = function(string, validate, prefix);
		return (JvmOperation) this.associations.get().getPrimaryJvmElement(action);
	}

	/** Create an instance of function signature.
	 */
	protected SarlAction functionSignature(String string, String... prefix) throws Exception {
		SarlInterface interfaze = interfaze(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "interface Foo { " + string + "}");
		return (SarlAction) interfaze.getMembers().get(0);
	}

	/** Create an instance of function signature.
	 */
	protected SarlAction functionSignature(String string, boolean validate, String... prefix) throws Exception {
		SarlInterface interfaze = interfaze(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "interface Foo { " + string + "}", validate);
		return (SarlAction) interfaze.getMembers().get(0);
	}

	/** Create an instance of JVM function.
	 */
	protected JvmOperation jvmOperationSignature(String string, String... prefix) throws Exception {
		SarlAction action = functionSignature(string, prefix);
		return (JvmOperation) this.associations.get().getPrimaryJvmElement(action);
	}

	/** Create an instance of JVM function.
	 */
	protected JvmOperation jvmOperationSignature(String string, boolean validate, String... prefix) throws Exception {
		SarlAction action = functionSignature(string, validate, prefix);
		return (JvmOperation) this.associations.get().getPrimaryJvmElement(action);
	}

	/** Create an instance of constructor.
	 */
	protected SarlConstructor constructor(String string, String... prefix) throws Exception {
		SarlClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "class Foo { " + string + "}");
		return (SarlConstructor) clazz.getMembers().get(0);
	}

	/** Create an instance of constructor.
	 */
	protected SarlConstructor constructor(String string, boolean validate, String... prefix) throws Exception {
		SarlClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "class Foo { " + string + "}", validate);
		return (SarlConstructor) clazz.getMembers().get(0);
	}

	/** Create an instance of JVM constructor.
	 */
	protected JvmConstructor jvmConstructor(String string, String... prefix) throws Exception {
		SarlConstructor constructor = constructor(string, prefix);
		return (JvmConstructor) this.associations.get().getPrimaryJvmElement(constructor);
	}

	/** Create an instance of JVM constructor.
	 */
	protected JvmConstructor jvmConstructor(String string, boolean validate, String... prefix) throws Exception {
		SarlConstructor constructor = constructor(string, validate, prefix);
		return (JvmConstructor) this.associations.get().getPrimaryJvmElement(constructor);
	}

	/** Create an instance of field.
	 */
	protected SarlField field(String string, String... prefix) throws Exception {
		SarlClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "class Foo { " + string + "}");
		return (SarlField) clazz.getMembers().get(0);
	}

	/** Create an instance of field.
	 */
	protected SarlField field(String string, boolean validate, String... prefix) throws Exception {
		SarlClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "class Foo { " + string + "}", validate);
		return (SarlField) clazz.getMembers().get(0);
	}

	/** Create an instance of behavior unit.
	 */
	protected SarlBehaviorUnit behaviorUnit(String string, String... prefix) throws Exception {
		SarlAgent agent = agent(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "agent Foo { " + string + "}");
		return (SarlBehaviorUnit) agent.getMembers().get(0);
	}

	/** Create an instance of behavior unit.
	 */
	protected SarlBehaviorUnit behaviorUnit(String string, boolean validate, String... prefix) throws Exception {
		SarlAgent agent = agent(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "agent Foo { " + string + "}", validate);
		return (SarlBehaviorUnit) agent.getMembers().get(0);
	}

	/** Create a type reference with the SARL parser.
	 */
	protected JvmTypeReference getType(String typeName, String... prefix) throws Exception {
		SarlAgent agent = agent(
				IterableExtensions.join(Arrays.asList(prefix), getLineSeparator())
				+ getLineSeparator() + "agent Foo { var fooAttr : " + typeName + " }");
		return ((SarlField) agent.getMembers().get(0)).getType();
	}

	/** Merge two arrays.
	 *
	 * @param operand1 the first array.
	 * @param operand2 the second array.
	 * @return the merge.
	 */
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

	/** Replies the stack trace of the caller of this function.
	 *
	 * @return the stack trace.
	 */
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

	protected static void assertOsgiVersionEquals(Version expected, Version actual) {
		if (Objects.equal(expected, actual)) {
			return;
		}
		if (expected == null) {
			fail("Version not null");
		}
		if (actual == null) {
			fail("Unexpected null value");
		}
		if (expected.getMajor() == actual.getMajor()
				&& expected.getMinor() == actual.getMinor()
				&& expected.getMicro() == actual.getMicro()) {
			if (!Strings.isNullOrEmpty(expected.getQualifier())) {
				final String expectedQualifier = expected.getQualifier();
				if ("qualifier".equals(expectedQualifier)) {
					if (!Strings.isNullOrEmpty(actual.getQualifier())) {
						return;
					}
				}
				if (Objects.equal(expected, actual.getQualifier())) {
					return;
				}
			} else {
				return;
			}
		}
		throw new ComparisonFailure("Not same versions", expected.toString(), actual.toString());
	}

	/** Mockito matcher that matches {@code null} or an instance of the given type.
	 *
	 * @param type the expected type.
	 * @return the default value for the given type.
	 */
	public static <T> T anyInstanceOrNull(Class<T> type) {
		final ArgumentMatcher matcher = new InstanceOf.VarArgAware(type);
		return ArgumentMatchers.argThat((it) -> {
			return it == null || matcher.matches(it);
		});
	}

	/** Replies the string representation of issues.
	 *
	 * @param model the parsed model.
	 * @param issues the issues.
	 * @param result the result.
	 * @return {@code result}.
	 */
	public static StringBuilder getIssuesAsString(EObject model, Iterable<Issue> issues, StringBuilder result) {
		for(Issue issue : issues) {
			URI uri = issue.getUriToProblem();
			result.append(issue.getSeverity());
			result.append(" (");
			result.append(issue.getCode());
			result.append(") '");
			result.append(issue.getMessage());
			result.append("'");
			if (uri != null) {
				EObject eObject = model.eResource().getResourceSet().getEObject(uri, true);
				result.append(" on ");
				result.append(eObject.eClass().getName());
			}
			result.append("\n");
		}
		return result;
	}

	/** Replies if the given issue has the given parts within its message.
	 *
	 * @param issue the issue to test.
	 * @param messageParts the parts of the error message to search for.
	 * @return {@code true} if all parts are found.
	 */
	public static boolean isIssueMessage(Issue issue, String... messageParts) {
		for (String messagePart : messageParts) {
			if (!issue.getMessage().toLowerCase().contains(messagePart.toLowerCase())) {
				return false;
			}
		}
		return true;
	}

	/** Assert that the given issue is inside the list of issues.
	 *
	 * @param issues the list of issues.
	 * @param severity the expected severity.
	 * @param model the issued model.
	 * @param objectType the type of the object.
	 * @param code the issue code.
	 * @param messageParts the parts of the issue message that are expected.
	 */
	public static void assertIssue(List<Issue> issues, Severity severity, EObject model,
			EClass objectType, String code, String... messageParts) {
		Iterator<Issue> iterator = issues.iterator();
		while (iterator.hasNext()) {
			Issue issue = iterator.next();
			if (Objects.equal(issue.getCode(), code) && issue.getSeverity() == severity) {
				EObject object = model.eResource().getResourceSet().getEObject(issue.getUriToProblem(), true);
				if (objectType.isInstance(object)) {
					if (isIssueMessage(issue, messageParts)) {
						iterator.remove();
						return;
					}
				}
			}
		}
		StringBuilder message = new StringBuilder("Expected ");
		message.append(severity);
		message.append(" '");
		message.append(code);
		message.append("' on ");
		message.append(objectType.getName());
		message.append(" but got\n");
		getIssuesAsString(model, issues, message);
		fail(message.toString());
	}

	/** Assert that the given object was compiled with at least one error.
	 *
	 * @param source the compiled object from which errors should be retrieved.
	 * @param codes the list of the error codes to be ignored.
	 * @since 0.7
	 */
	public void assertAnyError(EObject source, String... codes) {
		final List<String> codeSet = Arrays.asList(codes);
		final List<Issue> validate = this.validationHelper.validate(source);
		if (!any(validate, input -> Severity.ERROR == input.getSeverity() && !codeSet.contains(input.getCode()))) {
			fail("Expected an error, but got nothing");
		}
	}

	/** Assert that the given object was compiled without any error except the ones with the specified codes.
	 *
	 * @param source the compiled object from which errors should be retrieved.
	 * @param codes the list of the error codes to be ignored.
	 * @since 0.7
	 */
	public void assertNoErrorsExcept(EObject source, String... codes) {
		final List<String> codeSet = Arrays.asList(codes);
		final List<Issue> validate = this.validationHelper.validate(source);
		final Predicate<Issue> pred = input -> Severity.ERROR == input.getSeverity() && !codeSet.contains(input.getCode());
		if (any(validate, pred)) {
			fail("Expected no error, found: " + filter(validate, pred));
		}
	}

	/** Assert that the given warning is inside the list of issues.
	 *
	 * @param issues the list of issues.
	 * @param severity the expected severity.
	 * @param model the issued model.
	 * @param objectType the type of the object.
	 * @param code the issue code.
	 * @param messageParts the parts of the issue message that are expected.
	 */
	public static void assertWarning(List<Issue> issues, EObject model, EClass objectType, String code,
			String... messageParts) {
		assertIssue(issues, Severity.WARNING, model, objectType, code, messageParts);
	}

	/**
	 * Assert there is no more issue in the list.
	 *
	 * @param issues the list of issues.
	 * @param model the checked model.
	 */
	public static void assertNoMoreIssues(List<Issue> issues, EObject model) {
		if (!issues.isEmpty()) {
			StringBuilder message = new StringBuilder("Expecting no issue but got\n");
			getIssuesAsString(model, issues, message);
			fail(message.toString());
		}
	}

	/** Validation helper on a specific resource.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public interface Validator {

		List<Issue> getIssues();

		Validator assertNoIssues();

		Validator assertNoErrors();

		Validator assertNoError(String issuecode);

		Validator assertNoErrors(EClass objectType, String code, String... messageParts);

		Validator assertNoErrors(String code);

		Validator assertNoIssues(EClass objectType);

		Validator assertNoIssue(EClass objectType, String issuecode);

		Validator assertError(EClass objectType, String code, String... messageParts);

		Validator assertIssue(EClass objectType, String code, Severity severity, String... messageParts);

		Validator assertNoIssues(EClass objectType, String code, Severity severity, String... messageParts);

		Validator assertWarning(EClass objectType, String code, String... messageParts);

		Validator assertNoWarnings(EClass objectType, String code, String... messageParts);

	}

	/** Wrapper for the validation helper on a specific resource.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	static class XtextValidator implements Validator {

		private Resource resource;

		private ValidationTestHelper testHelper;

		/** Constructor.
		 *
		 * @param resource the resource to validate.
		 * @param testHelper the validator.
		 */
		XtextValidator(Resource resource, ValidationTestHelper testHelper) {
			this.resource = resource;
			this.testHelper = testHelper;
		}

		@Override
		protected void finalize() throws Throwable {
			this.resource = null;
			this.testHelper = null;
		}

		public List<Issue> getIssues() {
			return this.testHelper.validate(this.resource);
		}

		public Validator assertNoIssues() {
			this.testHelper.assertNoIssues(this.resource);
			return this;
		}

		public Validator assertNoErrors() {
			this.testHelper.assertNoErrors(this.resource);
			return this;
		}

		public Validator assertNoError(String issuecode) {
			this.testHelper.assertNoError(this.resource, issuecode);
			return this;
		}

		public Validator assertNoErrors(EClass objectType, String code, String... messageParts) {
			this.testHelper.assertNoErrors(this.resource, objectType, code, messageParts);
			return this;
		}

		public Validator assertNoErrors(String code) {
			this.testHelper.assertNoErrors(this.resource, code);
			return this;
		}

		public Validator assertNoIssues(EClass objectType) {
			this.testHelper.assertNoIssues(this.resource, objectType);
			return this;
		}

		public Validator assertNoIssue(EClass objectType, String issuecode) {
			this.testHelper.assertNoIssue(this.resource, objectType, issuecode);
			return this;
		}

		public Validator assertError(EClass objectType, String code, String... messageParts) {
			this.testHelper.assertError(this.resource, objectType, code, messageParts);
			return this;
		}

		public Validator assertIssue(EClass objectType, String code, Severity severity, String... messageParts) {
			this.testHelper.assertIssue(this.resource, objectType, code, severity, messageParts);
			return this;
		}

		public Validator assertNoIssues(EClass objectType, String code, Severity severity, String... messageParts) {
			this.testHelper.assertNoIssues(this.resource, objectType, code, severity, messageParts);
			return this;
		}

		public Validator assertWarning(EClass objectType, String code, String... messageParts) {
			this.testHelper.assertWarning(this.resource, objectType, code, messageParts);
			return this;
		}

		public Validator assertNoWarnings(EClass objectType, String code, String... messageParts) {
			this.testHelper.assertNoWarnings(this.resource, objectType, code, messageParts);
			return this;
		}

	}

	/**
	 * Extended utility class for reflection.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ReflectExtensions {

		/**
		 * Retrieves the value of the given accessible static field of the given type.
		 * 
		 * @param receiverType the type of the container of the field, not <code>null</code>
		 * @param fieldName the field's name, not <code>null</code>
		 * @return the value of the field
		 * 
		 * @throws NoSuchFieldException see {@link Class#getField(String)}
		 * @throws SecurityException see {@link Class#getField(String)}
		 * @throws IllegalAccessException see {@link Field#get(Object)}
		 * @throws IllegalArgumentException see {@link Field#get(Object)}
		 */
		public <T> T getStatic(Class<?> receiverType, String fieldName) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
			Field f = getDeclaredField(receiverType, fieldName);
			if (!f.isAccessible()) {
				f.setAccessible(true);
			}
			return (T) f.get(null);
		}

		/**
		 * Set the value of the given accessible static field of the given type.
		 * 
		 * @param receiverType the type of the container of the field, not <code>null</code>
		 * @param fieldName the field's name, not <code>null</code>
		 * @return the value of the field
		 * 
		 * @throws NoSuchFieldException see {@link Class#getField(String)}
		 * @throws SecurityException see {@link Class#getField(String)}
		 * @throws IllegalAccessException see {@link Field#get(Object)}
		 * @throws IllegalArgumentException see {@link Field#get(Object)}
		 */
		public <T> void setStatic(Class<?> receiverType, String fieldName, Object value) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
			Field f = getDeclaredField(receiverType, fieldName);
			if (!f.isAccessible()) {
				f.setAccessible(true);
			}
			f.set(null, value);
		}

		/**
		 * Set the value of the given accessible field of the given instance.
		 * 
		 * @param instance the container of the field, not <code>null</code>
		 * @param fieldName the field's name, not <code>null</code>
		 * @return the value of the field
		 */
		public <T> void set(Object instance, String fieldName, Object value) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
			Class<?> type = instance.getClass();
			while (type != null) {
				try {
					Field f = getDeclaredField(type, fieldName);
					if (!f.isAccessible()) {
						f.setAccessible(true);
					}
					f.set(instance, value);
					return;
				} catch (NoSuchFieldException exception) {
					//
				}
				type = type.getSuperclass();
			}
			throw new NoSuchFieldException(fieldName);
		}

		/**
		 * Replies the value of the given accessible field of the given instance.
		 * 
		 * @param instance the container of the field, not <code>null</code>
		 * @param fieldName the field's name, not <code>null</code>
		 * @return the value of the field
		 */
		public <T> T get(Object instance, String fieldName) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
			Class<?> type = instance.getClass();
			while (type != null) {
				try {
					Field f = getDeclaredField(type, fieldName);
					if (!f.isAccessible()) {
						f.setAccessible(true);
					}
					return (T) f.get(instance);
				} catch (NoSuchFieldException exception) {
					//
				}
				type = type.getSuperclass();
			}
			throw new NoSuchFieldException(fieldName);
		}

		/**
		 * Invokes the first accessible constructor defined on the receiver's class with
		 * a parameter list compatible to the given arguments.
		 *
		 * @param <T> the type of the object to create.
		 * @param type type of the object to create.
		 * @param args the arguments for the method invocation
		 * @return the result of the constructor invocation.
		 * @throws InvocationTargetException 
		 * @throws IllegalArgumentException 
		 * @throws IllegalAccessException 
		 * @throws InstantiationException 
		 * @throws SecurityException 
		 * @throws NoSuchMethodException 
		 */
		public <T> T newInstance(Class<T> type, Object... args) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
			final Object[] arguments = args == null ? new Object[]{null} : args;
			Constructor<?> compatible = null;
			for (Constructor<?> candidate : type.getDeclaredConstructors()) {
				if (candidate != null && isCompatible(candidate, arguments)) {
					if (compatible != null) {
						throw new IllegalStateException(
								"Ambiguous constructor to invoke. Both " //$NON-NLS-1$
								+ compatible + " and  " + candidate + " would be compatible choices."); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-2
					}
					compatible = candidate;
				}
			}
			if (compatible != null) {
				if (!compatible.isAccessible()) {
					compatible.setAccessible(true);
				}
				return type.cast(compatible.newInstance(arguments));
			}
			// not found provoke constructor not found exception
			Class<?>[] paramTypes = new Class<?>[arguments.length];
			for (int i = 0; i< arguments.length ; i++) {
				paramTypes[i] = arguments[i] == null ? Object.class : arguments[i].getClass();
			}
			Constructor<T> cons = type.getConstructor(paramTypes);
			return cons.newInstance(args);
		}

		protected static boolean isCompatible(Constructor<?> candidate, Object... args) {
			if (candidate.getParameterTypes().length != args.length)
				return false;
			for (int i = 0; i< candidate.getParameterTypes().length; i++) {
				Object param = args[i];
				Class<?> class1 = candidate.getParameterTypes()[i];
				if (class1.isPrimitive()) {
					class1 = wrapperTypeFor(class1);
				}
				if (param != null && !class1.isInstance(param))
					return false;
			}
			return true;
		}

		protected static Class<?> wrapperTypeFor(Class<?> primitive) {
			assert primitive != null;
			if (primitive == Boolean.TYPE) return Boolean.class;
			if (primitive == Byte.TYPE) return Byte.class;
			if (primitive == Character.TYPE) return Character.class;
			if (primitive == Short.TYPE) return Short.class;
			if (primitive == Integer.TYPE) return Integer.class;
			if (primitive == Long.TYPE) return Long.class;
			if (primitive == Float.TYPE) return Float.class;
			if (primitive == Double.TYPE) return Double.class;
			if (primitive == Void.TYPE) return Void.class;
			throw new IllegalArgumentException(primitive+ " is not a primitive"); //$NON-NLS-1$
		}

		protected Field getDeclaredField(Class<?> clazz, String name) throws NoSuchFieldException {
			NoSuchFieldException initialException = null;
			do {
				try {
					Field f = clazz.getDeclaredField(name);
					return f;
				} catch(NoSuchFieldException noSuchField) {
					if (initialException == null) {
						initialException = noSuchField;
					}
				}
			} while((clazz = clazz.getSuperclass()) != null);
			throw initialException;
		}

		/**
		 * Invokes the first accessible constructor defined on the receiver's class with
		 * a parameter list compatible to the given arguments.
		 *
		 * @param <T> the type of the object to create.
		 * @param type type of the object to create.
		 * @param args the arguments for the method invocation
		 * @return the result of the constructor invocation.
		 * @throws InvocationTargetException 
		 * @throws IllegalArgumentException 
		 * @throws IllegalAccessException 
		 * @throws InstantiationException 
		 * @throws SecurityException 
		 * @throws NoSuchMethodException 
		 * @throws ClassNotFoundException 
		 */
		public <T> T newInstance(String type, Object... args)
				throws InstantiationException, IllegalAccessException, IllegalArgumentException,
				InvocationTargetException, NoSuchMethodException, SecurityException, ClassNotFoundException {
			final Class<?> t = forName(type);
			return (T) newInstance(t, args);
		}

		/**
		 * Find the given type.
		 *
		 * @param name the name of the type. 
		 * @return the class.
		 * @throws ClassNotFoundException 
		 */
		public Class<?> forName(String name) throws ClassNotFoundException {
			try {
				return Class.forName(name);
			} catch (Exception exception) {
				//
			}
			BundleContext context = TestPluginActivator.context;
			if (context != null) {
				for (Bundle b : context.getBundles()) {
					try {
						return b.loadClass(name);
					} catch (ClassNotFoundException e) {
						// No problem, this bundle doesn't have the class
					}
				}
			}
			throw new ClassNotFoundException(name);
		}

		/**
		 * Invokes the first accessible method defined on the receiver'c class with the given name and
		 * a parameter list compatible to the given arguments.
		 * 
		 * @param receiver the method call receiver, not <code>null</code>
		 * @param methodName the method name, not <code>null</code>
		 * @return the result of the method invocation. <code>null</code> if the method was of type void.
		 */
		public Object invoke(Object receiver, String methodName) throws SecurityException, IllegalArgumentException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
			assert receiver != null;
			assert methodName != null;

			Class<? extends Object> clazz = receiver.getClass();
			Method compatible = null;
			do {
				for (Method candidate : clazz.getDeclaredMethods()) {
					if (candidate != null && !candidate.isBridge() && Objects.equal(methodName, candidate.getName())
							&& candidate.getParameterCount() == 0) {
						if (compatible != null) 
							throw new IllegalStateException("Ambiguous methods to invoke. Both "+compatible+" and  "+candidate+" would be compatible choices.");
						compatible = candidate;
					}
				}
			} while(compatible == null && (clazz = clazz.getSuperclass()) != null);
			if (compatible != null) {
				if (!compatible.isAccessible())
					compatible.setAccessible(true);
				return compatible.invoke(receiver);
			}
			// not found provoke method not found exception
			Method method = receiver.getClass().getMethod(methodName);
			return method.invoke(receiver);
		}

		/**
		 * Invokes the first accessible method defined on the receiver'c class with the given name and
		 * a parameter list compatible to the given arguments.
		 * 
		 * @param receiver the method call receiver, not <code>null</code>
		 * @param methodName the method name, not <code>null</code>
		 * @return the result of the method invocation. <code>null</code> if the method was of type void.
		 */
		public Object invoke(Object receiver, String methodName, Object... args) throws Exception, IllegalArgumentException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
			assert receiver != null;
			assert methodName != null;

			if (args == null) {
				args = new Object[] {null};
			}

			Class<? extends Object> clazz = receiver.getClass();
			Method compatible = null;
			do {
				for (Method candidate : clazz.getDeclaredMethods()) {
					if (candidate != null && !candidate.isBridge() && Objects.equal(methodName, candidate.getName())
							&& isValidArgs(candidate.isVarArgs(), args, candidate.getParameterTypes())) {
						if (compatible != null) 
							throw new IllegalStateException("Ambiguous methods to invoke. Both "+compatible+" and  "+candidate+" would be compatible choices.");
						compatible = candidate;
					}
				}
			} while(compatible == null && (clazz = clazz.getSuperclass()) != null);
			if (compatible != null) {
				if (!compatible.isAccessible())
					compatible.setAccessible(true);
				if (compatible.isVarArgs()) {
					Object[] newArgs = new Object[compatible.getParameterCount()];
					for (int i = 0; i < compatible.getParameterCount() - 1; ++i) {
						newArgs[i] = args[i];
					}
					Class<?> componentType = compatible.getParameterTypes()[compatible.getParameterCount() - 1].getComponentType();
					int varArgsLength = args.length - compatible.getParameterCount() + 1;
					Object varArgs = Array.newInstance(componentType, varArgsLength);
					for (int i = 0; i < varArgsLength; ++i) {
						Array.set(varArgs, i, args[i + compatible.getParameterCount() - 1]);
					}
					newArgs[compatible.getParameterCount() - 1] = varArgs;
					return compatible.invoke(compatible.getDeclaringClass().cast(receiver), (Object[]) newArgs);
				}
				return compatible.invoke(compatible.getDeclaringClass().cast(receiver), (Object[]) args);
			}
			// not found provoke method not found exception
			Method method = receiver.getClass().getMethod(methodName);
			return method.invoke(receiver);
		}

		private static boolean isValidArgs(boolean varargs, Object[] args, Class<?>[] params) {
			for (int i = 0; i < args.length; ++i) {
				if (i >= params.length) {
					return false;
				}
				if (args[i] == null) {
					if (params[i].isPrimitive()) {
						return false;
					}
				} else if ((!(params[i].isInstance(args[i]))) && varargs && i == params.length - 1) {
					Class<?> componentType = params[i].getComponentType();

					Class<?>[] newParams = new Class[args.length - params.length + 1];
					for (int j = 0; j < newParams.length; ++j) {
						newParams[j] = componentType;
					}

					Object[] newArgs = new Object[newParams.length];
					for (int j = 0; j < newArgs.length; ++j, ++i) {
						newArgs[j] = args[i];
					}

					return isValidArgs(false, newArgs, newParams);
				} else if (!(params[i].isInstance(args[i]))) {
					if (Primitives.isPrimitiveOrWrapper(params[i])) {
						if (!Objects.equal(
								Primitives.primitiveTypeOf(params[i]),
								Primitives.primitiveTypeOf(args[i].getClass()))) {
							return false;
						}
					} else {
						return false;
					}
				}
			}
			return true;
		}

	}

	/** Assert all the elements within the given collection are different.
	 *
	 * @param collection the collection to test.
	 */
	public static void assertAllDifferents(List<?> collection) {
		final int len = collection.size();
		final int penulvian = collection.size() - 1;
		for (int i = 0; i < penulvian; ++i) {
			final Object obj1 = collection.get(i);
			for (int j = i + 1; j < len; ++j) {
				final Object obj2 = collection.get(j);
				if (Objects.equal(obj1, obj2)) {
					fail("Same objects at positions " + i + " and " + j);
				}
			}
		}
	}

	/** Start a time out on the operation.
	 *
	 * @param enable programmatic flag for enabling the time out.
	 * @return the time out manager.
	 * @since 0.10
	 */
	protected TimeOutHandler startTimeOut(boolean enable) {
		final TimeOutHandler handler = new TimeOutHandler();
		if (enable) {
			handler.start();
		}
		return handler;
	}

	/** Start a time out on the operation.
	 *
	 * @return the time out manager.
	 * @since 0.9
	 */
	protected TimeOutHandler startTimeOut() {
		return startTimeOut(true);
	}

	/** An object for managing the time out of operations.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	protected static class TimeOutHandler {

		private static final int TIME_OUT = 10000;
		
		private Thread thread;

		private Thread threadToBreak;

		private final AtomicBoolean continueLoop = new AtomicBoolean(true);
		
		private final AtomicBoolean timeout = new AtomicBoolean();

		/** Constructor.
		 */
		TimeOutHandler() {
			//
		}

		/** Start the time out process.
		 */
		void start() {
			this.threadToBreak = Thread.currentThread();
			this.thread = new Thread() {
				@Override
				public void run() {
					final long endTime = System.currentTimeMillis() + TIME_OUT;
					while (TimeOutHandler.this.continueLoop.get()
							&& System.currentTimeMillis() <= endTime) {
						Thread.yield();
					}
					if (TimeOutHandler.this.continueLoop.get()) {
						TimeOutHandler.this.timeout.set(true);
						TimeOutHandler.this.stop();
					}
				}
			};
			this.thread.setDaemon(true);
			this.thread.setName("Test TimeOut Manager");
			this.thread.start();
		}

		/** Stop the time out process.
		 */
		public synchronized void stop() {
			this.continueLoop.set(false);
			if (this.thread != null) {
				this.thread = null;
				if (this.threadToBreak != null) {
					try {
						this.threadToBreak.stop();
					} catch (ThreadDeath exception) {
						if (this.timeout.get()) {
							throw new RuntimeException(new TimeoutException());
						}
					} finally {
						this.threadToBreak = null;
					}
				}
			}
		}

	}
}
