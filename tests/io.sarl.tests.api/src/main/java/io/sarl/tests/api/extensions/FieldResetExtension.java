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
package io.sarl.tests.api.extensions;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.TestInstancePreDestroyCallback;

import io.sarl.tests.api.tools.TestMockito;

/** JUnit 5 extension that resets the fields that are injected or mocks.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class FieldResetExtension implements TestInstancePreDestroyCallback {

	@Override
	public void preDestroyTestInstance(ExtensionContext context) throws Exception {
		if (context.getTestInstance().isPresent()) {
			final Object instance = context.getTestInstance().get();
			if (context.getTestClass().isPresent()) {
				Class<?> type = context.getTestClass().get();
				while (type != null && !Object.class.equals(type)) {
					for (Field field : type.getDeclaredFields()) {
						if (TestMockito.isNullable(field) && (field.getModifiers() & (Modifier.FINAL | Modifier.STATIC)) == 0) {
							try {
								field.setAccessible(true);
								field.set(instance, null);
							} catch (Exception e) {
								throw new Error(e);
							}
						}
					}
					type = type.getSuperclass();
				}
			}
		}
	}

}
