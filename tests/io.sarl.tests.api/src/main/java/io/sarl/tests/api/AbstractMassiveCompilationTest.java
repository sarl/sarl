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

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.google.common.base.Throwables;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;

/** Abstract implementation of tests that needs a massive number of compilations.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8.6
 */
public abstract class AbstractMassiveCompilationTest extends AbstractSarlTest {

	@Nullable
	private Map<String, String> differedTests;

	@Nullable
	private String currentMethod;

	@Nullable
	private ResourceSet resourceSet = null;

	/** Assert that a single type is correctly generated.
	 * Correctly means that the expected Java code is generated and equals to the expected code.
	 *
	 * @param sarlExpression the SARL expression to compile, without the package definition.
	 * @param javaExpression the expected Java expression, without the package definition.
	 * @throws Exception in case of error.
	 */
	protected void diffSingleTypeCompileTo(String sarlExpression, String javaExpression) throws Exception {
		final String packageName = "io.sarl.lang.core.tests." + this.currentMethod; //$NON-NLS-1$
		final String inputCode = "package " + packageName + System.lineSeparator() + sarlExpression; //$NON-NLS-1$
		final SarlScript script;
		if (this.resourceSet == null) {
			script = getParseHelper().parse(inputCode);
			this.resourceSet = script.eResource().getResourceSet();
		} else {
			script = getParseHelper().parse(inputCode, this.resourceSet);
		}
		final String qualifiedName = packageName + "." + script.getXtendTypes().get(0).getName(); //$NON-NLS-1$
		this.differedTests.put(qualifiedName, "package " + packageName + ";" + System.lineSeparator() + System.lineSeparator() + javaExpression); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Assert that a single type is not correctly generated due to invalid implementation of the Xbase API.
	 * In future version of this class, this function may disappear.
	 *
	 * @param sarlExpression the SARL expression to compile, without the package definition.
	 * @throws Exception in case of error.
	 */
	protected void diffSingleTypeCompileTo_unexpectedCastError(String sarlExpression) throws Exception {
		final String packageName = "io.sarl.lang.core.tests." + this.currentMethod; //$NON-NLS-1$
		final String inputCode = "package " + packageName + System.lineSeparator() + sarlExpression; //$NON-NLS-1$
		validate(file(inputCode)).assertError(
						TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
						IssueCodes.INVALID_CAST);
	}

	/** Run the differed tests.
	 *
	 * @throws Throwable in case of error.
	 */
	@Test
	public void testAll() throws Throwable {
		this.differedTests = new TreeMap<>();
		for (final Method meth : getClass().getDeclaredMethods()) {
			if (meth.isAnnotationPresent(DifferedTest.class)) {
				this.currentMethod = meth.getName();
				try {
					meth.invoke(this);
				} catch (InvocationTargetException exception) {
					final Throwable cause = Throwables.getRootCause(exception);
					throw new DifferedException(meth, cause);
				}
			}
		}
		assertNotNull(this.resourceSet);
		getCompileHelper().compile(this.resourceSet, it -> {
			for (final Entry<String, String> entry : this.differedTests.entrySet()) {
				final String actual = it.getGeneratedCode(entry.getKey());
				assertEquals(entry.getKey(), entry.getValue(), actual);
			}
		});
	}

	/** Annotation for marking the tests as differed.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8.6
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target({ElementType.METHOD})
	public static @interface DifferedTest {
		//
	}
	
	/** Exception in the differed called function.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	public static class DifferedException extends Exception {

		private static final long serialVersionUID = 8240031883925453664L;

		/** Constructor.
		 *
		 * @param meth the method in which the error occurs.
		 * @param cause the cause.
		 */
		public DifferedException(Method meth, Throwable cause) {
			super(buildMessage(meth, cause), cause);
		}

		private static String buildMessage(Method meth, Throwable cause) {
			final StringBuilder name = new StringBuilder();
			name.append(meth.getName());
			name.append("("); //$NON-NLS-1$
			boolean first = true;
			for (final Parameter param : meth.getParameters()) {
				if (first) {
					first = false;
				} else {
					name.append(","); //$NON-NLS-1$
				}
				name.append(param.getType().toGenericString());
			}
			name.append(")"); //$NON-NLS-1$
			return name.toString() + " => " + cause.getLocalizedMessage(); //$NON-NLS-1$
		}

	}

}
