/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.google.common.base.Predicate;
import com.google.common.base.Throwables;
import com.google.common.collect.Collections2;
import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.testing.IInjectorProvider;
import org.eclipse.xtext.testing.IRegistryConfigurator;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.internal.InjectorProviders;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Assert;
import org.junit.internal.builders.AllDefaultPossibilitiesBuilder;
import org.junit.internal.builders.AnnotatedBuilder;
import org.junit.internal.builders.JUnit4Builder;
import org.junit.internal.builders.NullBuilder;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runner.Runner;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.Suite;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.RunnerBuilder;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** A Junit suite for running tests within massive compilation process. 
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@InjectWith(ExtendedSARLInjectorProvider.class)
public class MassiveCompilationSuite extends Suite {

	private final List<Runner> runners;

	@Inject
	private ParseHelper<SarlScript> parser;

	@Inject
	private CompilationTestHelper compiler;

	@Inject
	private ValidationTestHelper validator;


	/**
	 * @param testedType the tested type.
	 * @throws InitializationError in case of error.
	 */
	public MassiveCompilationSuite(Class<?> testedType) throws InitializationError {
		super(testedType, Collections.emptyList());
		//
		this.runners = new ArrayList<>();
		final IInjectorProvider injectorProvider = InjectorProviders.getOrCreateInjectorProvider(getTestClass());
		assert injectorProvider != null;
		try {
			if (injectorProvider instanceof IRegistryConfigurator) {
				final IRegistryConfigurator registryConfigurator = (IRegistryConfigurator) injectorProvider;
				registryConfigurator.setupRegistry();
			}
			final Injector injector = injectorProvider.getInjector();
			assert injector != null;
			injector.injectMembers(this);
			//
			final Context context = new Context(injector, this.parser, this.validator);
			Class<?> type = testedType;
			boolean hasMassiveMethod = false;
			try {
				while (type != null && !Object.class.equals(type)) {
					for (final Method meth : type.getDeclaredMethods()) {
						if (meth.isAnnotationPresent(CompilationTest.class)) {
							if (!Modifier.isStatic(meth.getModifiers())) {
								throw new InitializationError("Function must be declared with the static modifier."); //$NON-NLS-1$
							}
							if (meth.getParameterCount() != 1
									|| !meth.getParameters()[0].getType().isAssignableFrom(Context.class)) {
								throw new InitializationError("Function must have one formal parameter of type " + Context.class.getSimpleName()); //$NON-NLS-1$
							}
							hasMassiveMethod = true;
							context.setCurrentMethod(meth);
							try {
								meth.invoke(null, context);
							} catch (Exception exception) {
								Throwable source = Throwables.getRootCause(exception);
								if (source instanceof CompilationStop) {
									throw new CompilationStop();
								}
								this.runners.add(new MassiveCompilationErrorRunner(testedType, formatFunctionName(meth.getName()), source));
							}
						}
					}
					type = type.getSuperclass();
				}
				final ResourceSet rs = context.getResourceSet();
				if (!hasMassiveMethod || rs == null) {
					throw new InitializationError("Unable to find a method that is contributing to the massive compilation process."); //$NON-NLS-1$
				}
				//
				assert this.compiler != null;
				try {
					this.compiler.compile(rs, it -> {
						for (final Entry<String, String> entry : context.getExpectedResults().entrySet()) {
							final String actual = it.getGeneratedCode(entry.getKey());
							try {
								final int index1 = entry.getKey().lastIndexOf('.');
								final int index0 = entry.getKey().lastIndexOf('.', index1 - 1) + 1;
								final String functionName = entry.getKey().substring(index0, index1);
								this.runners.add(new MassiveCompilationRunner(testedType, formatFunctionName(functionName), entry.getValue(), actual));
							} catch (InitializationError e) {
								throw new RuntimeException(e);
							}
						}
					});
				} catch (RuntimeException exception) {
					if (exception.getCause() instanceof InitializationError) {
						throw (InitializationError) exception.getCause();
					}
					throw exception;
				}
			} catch (CompilationStop exception) {
				//
			}
		} finally {
			if (injectorProvider instanceof IRegistryConfigurator) {
				final IRegistryConfigurator registryConfigurator = (IRegistryConfigurator) injectorProvider;
				registryConfigurator.restoreRegistry();
			}
		}
		//
		final AllDefaultPossibilitiesBuilder builder = new AllDefaultPossibilitiesBuilder() {
			@Override
			protected RunnerBuilder suiteMethodBuilder() {
		        return new NullBuilder();
		    }
			@Override
			protected AnnotatedBuilder annotatedBuilder() {
				// Avoid infinite loop by running the tests with the current suite.
				return new AnnotatedBuilder(this) {
					@SuppressWarnings("synthetic-access")
					@Override
					public Runner runnerForClass(Class<?> testClass) throws Exception {
						Class<?> currentTestClass = testClass.getSuperclass();
						while (currentTestClass != null) {
							final Class<? extends Runner> runner = findRunner(currentTestClass);
							if (runner != null) {
								try {
									return buildRunner(runner, testClass);
								} catch (Exception exception) {
									return null;
								}
							}
							if (currentTestClass.isMemberClass() && !Modifier.isStatic(currentTestClass.getModifiers())) {
								currentTestClass = currentTestClass.getEnclosingClass();
							} else {
								currentTestClass = null;
							}
						}

						return null;
					}
				};
			}

			@Override
			protected JUnit4Builder junit4Builder() {
				return new JUnit4Builder() {
					@Override
					public Runner runnerForClass(Class<?> testClass) throws Throwable {
						try {
							return new BlockJUnit4ClassRunner(testClass);
						} catch (Exception exception) {
							return null;
						}
					}
				};
			}

			private Class<? extends Runner> findRunner(Class<?> base) {
				Class<?> type = base;
				while (type != null) {
					RunWith annotation = base.getDeclaredAnnotation(RunWith.class);
					if (annotation != null) {
						final Class<? extends Runner> runner = annotation.value();
						// Avoid infinite loop by running the tests with the current suite.
						if (runner != null && !MassiveCompilationSuite.class.isAssignableFrom(runner)) {
							return runner;
						}
					}
					type = type.getSuperclass();
				}
				return null;
			}
		};
		Runner runner = builder.safeRunnerForClass(testedType);
		if (runner != null) {
			this.runners.add(0, runner);
		}
	}

	private static String formatFunctionName(String baseName) {
		return baseName + "(" + Context.class.getSimpleName() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	protected List<Runner> getChildren() {
		return this.runners;
	}

	/** Context for massive compilation test suite.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	public static class Context {

		private final Injector injector;

		private final ParseHelper<SarlScript> parser;

		private final ValidationTestHelper validator;

		private Method currentMethod;

		private ResourceSet resourceSet;

		private final Map<String, String> expectedJava = new TreeMap<>();

		/** Constructor.
		 *
		 * @param injector the injector.
		 * @param parser the SARL parser.
		 * @param validator the validator.
		 */
		Context(Injector injector, ParseHelper<SarlScript> parser, ValidationTestHelper validator) {
			assert injector != null;
			assert parser != null;
			assert validator != null;
			this.injector = injector;
			this.parser = parser;
			this.validator = validator;
		}

		/** Replies the expected java results.
		 *
		 * @return the expected results.
		 */
		public synchronized Map<String, String> getExpectedResults() {
			return this.expectedJava;
		}

		/** Replies the resource set.
		 *
		 * @return the resource set.
		 */
		public synchronized ResourceSet getResourceSet() {
			return this.resourceSet;
		}

		/** Replies the current method.
		 *
		 * @return the current method.
		 */
		public Method getCurrentMethod() {
			return this.currentMethod;
		}

		/** Set the current method.
		 *
		 * @param method the current method.
		 */
		void setCurrentMethod(Method method) {
			this.currentMethod = method;
		}

		/** Replies the name of the current method.
		 *
		 * @return the name of the current method.
		 */
		public String getCurrentMethodName() {
			return this.currentMethod.getName();
		}

		/** Stop massive compilation.
		 */
		@SuppressWarnings("static-method")
		public void doNothing() {
			throw new CompilationStop();
		}

		/** Assert that a single type is correctly generated.
		 * Correctly means that the expected Java code is generated and equals to the expected code.
		 *
		 * @param sarlExpression the SARL expression to compile, without the package definition.
		 * @param javaExpression the expected Java expression, without the package definition.
		 * @throws Exception in case of error.
		 */
		public void compileTo(String sarlExpression, String javaExpression) throws Exception {
			final String packageName = "io.sarl.lang.core.tests." + this.currentMethod.getName(); //$NON-NLS-1$
			final String inputCode = "package " + packageName + "\n" + sarlExpression; //$NON-NLS-1$ //$NON-NLS-2$
			final SarlScript script = file(inputCode, false, true);
			final String qualifiedName = packageName + "." + script.getXtendTypes().get(script.getXtendTypes().size() - 1).getName(); //$NON-NLS-1$
			final String expectedJava = "package " + packageName + ";\n\n" + javaExpression; //$NON-NLS-1$ //$NON-NLS-2$
			this.expectedJava.put(qualifiedName, expectedJava);
		}

		/** Assert that a single type is correctly generated.
		 * Correctly means that the expected Java code is generated and equals to the expected code.
		 *
		 * @param sarlExpressions the SARL expressions to compile, with the package definitions.
		 * @param typeName the name of the java type to verify.
		 * @param javaExpression the expected Java expression, with the package definition.
		 * @throws Exception in case of error.
		 */
		public void compileTo(String[] sarlExpressions, String typeName, String javaExpression) throws Exception {
			SarlScript script = null;
			for (final String sarlExpression : sarlExpressions) {
				if (this.resourceSet == null) {
					script = this.parser.parse(sarlExpression);
					this.resourceSet = script.eResource().getResourceSet();
				} else {
					script = this.parser.parse(sarlExpression, this.resourceSet);
				}
			}
			Assert.assertNotNull(script);
			this.expectedJava.put(typeName, javaExpression);
		}

		/** Assert that a single type is not correctly generated due to invalid implementation of the Xbase API.
		 * In future version of this class, this function may disappear.
		 *
		 * @param sarlExpression the SARL expression to compile, without the package definition.
		 * @throws Exception in case of error.
		 */
		public void compileToUnexpectedCastError(String sarlExpression) throws Exception {
			final String packageName = "io.sarl.lang.core.tests." + this.currentMethod.getName(); //$NON-NLS-1$
			final String inputCode = "package " + packageName + "\n" + sarlExpression; //$NON-NLS-1$ //$NON-NLS-2$
			validate(file(inputCode, true, true).eResource()).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_CAST);
		}

		private SarlScript file(String code, boolean validate, boolean updateResourceSet) throws Exception {
			SarlScript script;
			if (this.resourceSet == null) {
				script = this.parser.parse(code);
				if (updateResourceSet) {
					this.resourceSet = script.eResource().getResourceSet();
				}
			} else {
				script = this.parser.parse(code, this.resourceSet);
			}
			if (this.resourceSet instanceof XtextResourceSet) {
				((XtextResourceSet) this.resourceSet).setClasspathURIContext(getClass());
			}
			Resource resource = script.eResource();
			Assert.assertEquals(resource.getErrors().toString(), 0, resource.getErrors().size());
			if (validate) {
				Collection<Issue> issues = Collections2.filter(this.validator.validate(resource), new Predicate<Issue>() {
					@Override
					public boolean apply(Issue input) {
						return input.getSeverity() == Severity.ERROR;
					}
				});
				Assert.assertTrue("Resource contained errors : " + issues.toString(), issues.isEmpty()); //$NON-NLS-1$
			}
			return script;
		}

		private Validator validate(Resource resource) {
			Validator validator = new AbstractSarlTest.XtextValidator(resource, this.validator);
			this.injector.injectMembers(validator);
			return validator;
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	private static class CompilationStop extends RuntimeException {

		private static final long serialVersionUID = -1262393392111225428L;

		/** Constructor.
		 */
		CompilationStop() {
			//
		}

	}

	/** Runner for the massive compilation process.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	private static class MassiveCompilationRunner extends Runner {

		private final Class<?> testType;

		private final String functionName;

		private final String expected;

		private final String actual;

		/** Constructor.
		 *
		 * @param type the test type.
		 * @param functionName the name of the function.
		 * @param expected the expected value.
		 * @param actual the actual value.
		 * @throws InitializationError in case of error.
		 */
		MassiveCompilationRunner(Class<?> type, String functionName, String expected, String actual) throws InitializationError {
			this.testType = type;
			this.expected = expected;
			this.actual = actual;
			this.functionName = functionName;
		}

		@Override
		public void run(RunNotifier notifier) {
			final Description description = getDescription();
			notifier.fireTestStarted(description);
			try {
				AbstractSarlTest.assertEquals(this.functionName, this.expected, this.actual);
			} catch (Throwable exception) {
				final Failure failure = new Failure(description, exception);
				notifier.fireTestFailure(failure);
			} finally {
				notifier.fireTestFinished(description);
			}
		}

		@Override
		public Description getDescription() {
			return Description.createTestDescription(this.testType, this.functionName);
		}

	}

	/** Runner for notifying an error into the massive compilation process.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	private static class MassiveCompilationErrorRunner extends Runner {

		private final Class<?> testType;

		private final String functionName;

		private final Throwable exception;

		/** Constructor.
		 *
		 * @param type the test type.
		 * @param functionName the name of the function.
		 * @param exception the error.
		 * @throws InitializationError in case of error.
		 */
		MassiveCompilationErrorRunner(Class<?> type, String functionName, Throwable exception) throws InitializationError {
			this.testType = type;
			this.functionName = functionName;
			this.exception = exception;
		}

		@Override
		public void run(RunNotifier notifier) {
			final Description description = getDescription();
			final Failure failure = new Failure(description, this.exception);
			notifier.fireTestStarted(description);
			notifier.fireTestFailure(failure);
			notifier.fireTestFinished(description);
		}

		@Override
		public Description getDescription() {
			return Description.createTestDescription(this.testType, this.functionName);
		}

	}

	/** Annotation for marking the tests as a part of the massive compilation test suite.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target({ElementType.METHOD})
	public static @interface CompilationTest {
		//
	}

}
