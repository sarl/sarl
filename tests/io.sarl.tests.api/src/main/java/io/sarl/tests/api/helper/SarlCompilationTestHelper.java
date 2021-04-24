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
package io.sarl.tests.api.helper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicBoolean;

import com.google.common.base.Strings;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;

/** Helper for running the SARL compiler during unit tests.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class SarlCompilationTestHelper extends CompilationTestHelper {

	/**
	 * Asserts that the expected code is generated for the given source and there is no Java compilation error.
	 * 
	 * @param checkJavaCompilation indicates if the generated code must be free of error. Warnings are allowed.
	 * @param source some valid source code written in the language under test
	 * @param expected the expected Java source code.
	 * @throws IOException if the resource loading fails 
	 */
	public void assertCompilesTo(boolean checkJavaCompilation, CharSequence source, final CharSequence expected) throws IOException {
		if (checkJavaCompilation) {
			final AtomicBoolean called = new AtomicBoolean(false);
			compile(source, (r) -> {
				assertEquals(expected.toString(), r.getSingleGeneratedCode());
				final Class<?> generatedJavaClass = r.getCompiledClass();
				assertNotNull(generatedJavaClass);
				called.set(true);
			});
			assertTrue(called.get(), "Nothing was generated but the expectation was:\n" + expected); //$NON-NLS-1$
		} else {
			super.assertCompilesTo(source, expected);
		}
	}

	/**
	 * Asserts that the expected code is generated for the given source and there is no Java compilation error.
	 * 
	 * @param javaClassname is the fully qualified name of the generated Java class. 
	 * @param source some valid source code written in the language under test
	 * @param expected the expected Java source code.
	 * @throws IOException if the resource loading fails 
	 */
	public void assertCompilesTo(String javaClassname, CharSequence source, final CharSequence expected) throws IOException {
		if (!Strings.isNullOrEmpty(javaClassname)) {
			final AtomicBoolean called = new AtomicBoolean(false);
			compile(source, (r) -> {
				assertEquals(expected.toString(), r.getGeneratedCode(javaClassname));
				final Class<?> generatedJavaClass = r.getCompiledClass(javaClassname);
				assertNotNull(generatedJavaClass);
				called.set(true);
			});
			assertTrue(called.get(), "Nothing was generated but the expectation was:\n" + expected); //$NON-NLS-1$
		} else {
			super.assertCompilesTo(source, expected);
		}
	}

}
