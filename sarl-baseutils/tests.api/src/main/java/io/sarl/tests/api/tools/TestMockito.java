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
package io.sarl.tests.api.tools;

import java.lang.reflect.Field;

import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.matchers.InstanceOf;

import io.sarl.tests.api.ManualMocking;

/** Set of additional utility related to Mockito.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public final class TestMockito {

	private TestMockito() {
		//
	}

	/** Temporary fixing a bug in the class loading of Mockito 2.
	 * 
	 * @param type the type to mock.
	 * @return the mocked instance.
	 * @see "http://stackoverflow.com/questions/37702952/classnotfoundexception-with-mockito-2-in-osgi"
	 */
	public static <T> T mock(Class<T> type) {
		if (type == null) {
			return null;
		}
		final var loader = Thread.currentThread().getContextClassLoader();
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
	 * @see "http://stackoverflow.com/questions/37702952/classnotfoundexception-with-mockito-2-in-osgi"
	 */
	public static <T> T spy(T instance) {
		if (instance == null) {
			return null;
		}
		final var loader = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(Mockito.class.getClassLoader());
		try {
			return Mockito.spy(instance);
		} finally {
			Thread.currentThread().setContextClassLoader(loader);
		}
	}

	/** Mockito matcher that matches {@code null} or an instance of the given type.
	 *
	 * @param type the expected type.
	 * @return the default value for the given type.
	 */
	public static <T> T anyInstanceOrNull(Class<T> type) {
		final var matcher = new InstanceOf(type);
		return ArgumentMatchers.argThat((it) -> {
			return it == null || matcher.matches(it);
		});
	}

	/** Replies the type of features in this test that could be or are mocked.
	 * A mockable feature is a field that has one of the following annotations:
	 * {@link Mock}, {@link InjectMocks}. If the feature is annotation with
	 * {@link ManualMocking}, is it not considered by this function.
	 *
	 * @param typeToExplore is the type to explore.
	 * @return the type of mockable features. If it is {@code 0}, no mockable feature was found.
	 */
	public static int getAutomaticMockableFeatures(Class<?> typeToExplore) {
		var features = 0;
		var type = typeToExplore;
		while (type != null) {
			if (type.getAnnotation(ManualMocking.class) != null) {
				return 0;
			}
			for (var field : type.getDeclaredFields()) {
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

	/** Replies if the given field could be reset with {@code null}.
	 * If the given field is annoted with {@link Mock} or {@link InjectMocks},
	 * or any annotation with the name {@code "Nullable"} or
	 * {@code "NonNullByDefault"}, then the field is nullable.
	 *
	 * @param field the field to test.
	 * @return {@code true} if the given field could be reset.
	 */
	public static boolean isNullable(Field field) {
		if (field.getAnnotation(Mock.class) != null
				|| field.getAnnotation(InjectMocks.class) != null) {
			return true;
		}
		for (var annotation : field.getAnnotations()) {
			if ("Nullable".equals(annotation.annotationType().getSimpleName()) //$NON-NLS-1$
					|| "NonNullByDefault".equals(annotation.annotationType().getSimpleName())) { //$NON-NLS-1$
				return true;
			}
		}
		return false;
	}

}
