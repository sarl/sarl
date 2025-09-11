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

package io.sarl.lang.util;

import java.lang.reflect.Field;

/**
 * Utility class for accessing to a field by reflection.
 *
 * @param <RT> the type of the receiver.
 * @param <T> the type of the returned value.
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.13
 */
public final class ReflectField<RT, T> {

	private final Class<RT> receiverType;

	private final Class<T> returnType;

	private final String fieldName;

	private Field field;

	/** Constructor.
	 *
	 * @param receiverType the type of the receiver.
	 * @param returnType the type of the returned values.
	 * @param fieldName the name of the field.
	 */
	protected ReflectField(Class<RT> receiverType, Class<T> returnType, String fieldName) {
		this.receiverType = receiverType;
		this.returnType = returnType;
		this.fieldName = fieldName;
	}

	/** Static constructor.
	 *
	 * @param <RT> the type of the receiver.
	 * @param <T> the type of the returned value.
	 * @param receiverType the type of the receiver.
	 * @param returnType the type of the returned values.
	 * @param fieldName the name of the field.
	 * @return the instance.
	 */
	public static <RT, T> ReflectField<RT, T> of(Class<RT> receiverType, Class<T> returnType, String fieldName) {
		return new ReflectField<>(receiverType, returnType, fieldName);
	}

	/** Get the value from the field from an object.
	 *
	 * @param receiver the receiver.
	 * @return the value.
	 */
	public T get(RT receiver) {
		final Field field;
		synchronized (this) {
			if (this.field == null) {
				this.field = getField(receiver);
				field = this.field;
			} else {
				field = this.field;
			}
		}
		try {
			return this.returnType.cast(field.get(receiver));
		} catch (RuntimeException | Error exception) {
			throw exception;
		} catch (Throwable exception) {
			throw new RuntimeException(exception);
		}
	}

	/** Get the value from the static field.
	 *
	 * @return the value.
	 */
	public T get() {
		return get(null);
	}

	private Field getField(RT receiver) {
		try {
			Class<? extends Object> clazz = this.receiverType;
			Field compatible = null;
			do {
				for (final var candidate : clazz.getDeclaredFields()) {
					if (candidate != null && isCompatible(candidate, this.fieldName)) {
						if (compatible != null) {
							throw new IllegalStateException("Ambiguous field to access. Both " //$NON-NLS-1$
									+ compatible + " and " + candidate + " would be compatible choices.");  //$NON-NLS-1$//$NON-NLS-2$
						}
						compatible = candidate;
					}
				}
				clazz = clazz.getSuperclass();
			} while (compatible == null && clazz != null);
			if (compatible != null) {
				compatible.setAccessible(true);
				return compatible;
			}
			// not found provoke method not found exception
			if (receiver != null) {
				return receiver.getClass().getField(this.fieldName);
			}
			return this.receiverType.getField(this.fieldName);
		} catch (Throwable exception) {
			throw new Error(exception);
		}
	}

	private boolean isCompatible(Field candidate, String featureName) {
		if (!candidate.getName().equals(featureName)) {
			return false;
		}
		var class1 = candidate.getType();
		if (class1.isPrimitive()) {
			class1 = wrapperTypeFor(class1);
		}
		return this.returnType.isAssignableFrom(class1);
	}

	private static Class<?> wrapperTypeFor(Class<?> primitive) {
		assert primitive != null;
		if (primitive == Boolean.TYPE) {
			return Boolean.class;
		}
		if (primitive == Byte.TYPE) {
			return Byte.class;
		}
		if (primitive == Character.TYPE) {
			return Character.class;
		}
		if (primitive == Short.TYPE) {
			return Short.class;
		}
		if (primitive == Integer.TYPE) {
			return Integer.class;
		}
		if (primitive == Long.TYPE) {
			return Long.class;
		}
		if (primitive == Float.TYPE) {
			return Float.class;
		}
		if (primitive == Double.TYPE) {
			return Double.class;
		}
		if (primitive == Void.TYPE) {
			return Void.class;
		}
		throw new IllegalArgumentException(primitive + " is not a primitive"); //$NON-NLS-1$
	}

}
