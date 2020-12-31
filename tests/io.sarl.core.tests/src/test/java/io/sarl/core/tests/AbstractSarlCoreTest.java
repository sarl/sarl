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
package io.sarl.core.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import io.sarl.tests.api.AbstractSarlTest;

/** Abstract implementation that provides tools for testing the "io.sarl.core" classes.
 *
 * @param <T> - the type of the expected loaded class.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSarlCoreTest<T> extends AbstractSarlTest {

	/** Name of the loaded class.
	 */
	protected String classname;

	/** Loaded type.
	 */
	protected Class<? extends T> type;

	/** Load the given class, generated from the SARL code.
	 *
	 * @param iclassname the name of the class.
	 * @param expectedType the type of the expected loaded class.
	 */
	protected void loadSARL(String iclassname, Class<T> expectedType) {
		assertNotNull(iclassname);
		assertNotNull(expectedType);
		assertNull(this.classname);
		this.classname = iclassname;
		Class<?> itype;
		try {
			itype = Class.forName(iclassname);
		} catch (ClassNotFoundException e) {
			throw new Error(e);
		}
		assertNotNull(itype);
		assertTrue(expectedType.isAssignableFrom(itype));
		this.type = itype.asSubclass(expectedType);
		assertNotNull(this.classname);
		assertNotNull(this.type);
	}

	/** Assert the the specified method is defined in the loaded class.
	 *
	 * @param methodName the name of the method.
	 * @param returnType the type of the returned value.
	 * @param parameterTypes the types of the paremeters.
	 * @return the method.
	 */
	protected Method assertMethod(String methodName, Class<?> returnType, Class<?>... parameterTypes) {
		assertNotNull(this.classname);
		assertNotNull(this.type);
		try {
			Method m = this.type.getDeclaredMethod(methodName, parameterTypes);
			assertEquals(returnType, m.getReturnType());
			return m;
		} catch (NoSuchMethodException | SecurityException e) {
			throw new Error(e);
		}
	}

	/** Assert the the specified field is defined in the loaded class.
	 *
	 * @param fieldName the name of the field.
	 * @param fieldType the type of the field.
	 * @return the field.
	 */
	protected Field assertField(String fieldName, Class<?> fieldType) {
		assertNotNull(this.classname);
		assertNotNull(this.type);
		try {
			Field f = this.type.getDeclaredField(fieldName);
			assertEquals(fieldType, f.getType());
			return f;
		} catch (NoSuchFieldException | SecurityException e) {
			throw new Error(e);
		}
	}

	/** Assert the the specified constructor is defined in the loaded class.
	 *
	 * @param parameterTypes the types of the constructor parameters.
	 * @return the constructor.
	 */
	@SuppressWarnings("unchecked")
	protected Constructor<T> assertConstructor(Class<?>... parameterTypes) {
		assertNotNull(this.classname);
		assertNotNull(this.type);
		try {
			Constructor<T> c = (Constructor<T>)this.type.getDeclaredConstructor(parameterTypes);
			return c;
		} catch (NoSuchMethodException | SecurityException e) {
			throw new Error(e);
		}
	}

}
