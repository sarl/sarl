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

package io.sarl.docs.doclet.j11.utils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/** Reflexion utilities.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public final class Reflect {

	private Reflect() {
		//
	}

	/** Change the field.
	 *
	 * @param type the type.
	 * @param fieldName the name of the field.
	 * @param fieldValue the value of the field.
	 */
	public static void setField(Class<?> type, String fieldName, Object fieldValue) {
		try {
			final Field field = type.getDeclaredField(fieldName);
			field.setAccessible(true);

			final Field modifiersField = Field.class.getDeclaredField("modifiers");
			modifiersField.setAccessible(true);
			modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);

			field.set(null, fieldValue);
		} catch (Exception exception) {
			throw new Error(exception);
		}

	}

	/** Change the field.
	 *
	 * @param instance the instance to change.
	 * @param type the type in which the field is declared.
	 * @param fieldName the name of the field.
	 * @param fieldValue the value of the field.
	 * @throws Error a runtime exception.
	 */
	public static void setField(Object instance, Class<?> type, String fieldName, Object fieldValue) {
		try {
			final Field field = type.getDeclaredField(fieldName);
			field.setAccessible(true);

			final Field modifiersField = Field.class.getDeclaredField("modifiers");
			modifiersField.setAccessible(true);
			modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);

			field.set(instance, fieldValue);
		} catch (Exception exception) {
			throw new Error(exception);
		}
	}

	/** Call the method.
	 *
	 * @param instance the instance to call on.
	 * @param type the type.
	 * @param methodName the name of the method.
	 * @param types the types of the parameters.
	 * @param args the values of the arguments.
	 * @throws Error a runtime exception.
	 */
	public static void callProc(Object instance, Class<?> type, String methodName, Class<?>[] types, Object... args) {
		try {
			final Method method = type.getDeclaredMethod(methodName, types);
			method.setAccessible(true);
			method.invoke(instance, args);
		} catch (Exception exception) {
			throw new Error(exception);
		}
	}

	/** Call the method.
	 *
	 * @param <R> the type of the returned value.
	 * @param instance the instance to call on.
	 * @param type the type.
	 * @param returnType the type of the returned value.
	 * @param methodName the name of the method.
	 * @param types the types of the parameters.
	 * @param args the values of the arguments.
	 * @return the value.
	 * @throws Error a runtime exception.
	 */
	public static <R> R callFunc(Object instance, Class<?> type, Class<R> returnType,
			String methodName, Class<?>[] types, Object... args) {
		try {
			final Method method = type.getDeclaredMethod(methodName, types);
			method.setAccessible(true);
			return returnType.cast(method.invoke(instance, args));
		} catch (Exception exception) {
			throw new Error(exception);
		}
	}

	/** Replies the value of the field.
	 *
	 * @param <T> the field type.
	 * @param obj the instance.
	 * @param string the field name.
	 * @param clazz the container type.
	 * @param fieldType the field type.
	 * @return the value.
	 * @throws Error a runtime exception.
	 */
	public static <T> T getField(Object obj, String string, Class<?> clazz, Class<T> fieldType) {
		try {
			final Field field = clazz.getDeclaredField(string);
			field.setAccessible(true);
			final Object value = field.get(obj);
			return fieldType.cast(value);
		} catch (Exception exception) {
			throw new Error(exception);
		}
	}

	/** Clone the fields.
	 *
	 * @param <T> the type of the objects.
	 * @param type the type of the objects.
	 * @param dest the destination.
	 * @param source the source.
	 * @throws Error a runtime exception.
	 */
	public static <T> void copyFields(Class<T> type, T dest, T source) {
		Class<?> clazz = type;
		while (clazz != null && !Object.class.equals(clazz)) {
			for (final Field field : clazz.getDeclaredFields()) {
				if (!Modifier.isStatic(field.getModifiers())) {
					field.setAccessible(true);
					try {
						field.set(dest, field.get(source));
					} catch (IllegalArgumentException | IllegalAccessException e) {
						throw new Error(e);
					}
				}
			}
			clazz = clazz.getSuperclass();
		}
	}

}
