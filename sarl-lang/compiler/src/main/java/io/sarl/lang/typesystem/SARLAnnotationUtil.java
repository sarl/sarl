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

package io.sarl.lang.typesystem;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmBooleanAnnotationValue;
import org.eclipse.xtext.common.types.JvmIntAnnotationValue;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.util.AnnotationLookup;

/**
 * Utilities for JVM annotations.
 *
 * <p>This class provides additional utilities than
 * {@link org.eclipse.xtext.xbase.annotations.typing.XAnnotationUtil} and {@link AnnotationLookup}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see org.eclipse.xtext.xbase.annotations.typing.XAnnotationUtil for Xbase annotations
 * @see AnnotationLookup for retreiving annotations.
 */
@Singleton
public class SARLAnnotationUtil {

	@Inject
	private AnnotationLookup lookup;

	/** Extract the string value of the given annotation, if it exists.
	 *
	 * @param op the annotated element.
	 * @param annotationType the type of the annotation to consider
	 * @return the value of the annotation, or {@code null} if no annotation or no
	 *     value.
	 */
	public String findStringValue(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final var reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findStringValue(reference);
		}
		return null;
	}

	/** Extract the string value of the given annotation, if it exists.
	 *
	 * @param reference the reference to the annotation.
	 * @return the value of the annotation, or {@code null} if no annotation or no
	 *     value.
	 */
	@SuppressWarnings("static-method")
	public String findStringValue(JvmAnnotationReference reference) {
		assert reference != null;
		for (final var value : reference.getValues()) {
			if (value instanceof JvmStringAnnotationValue cvalue) {
				for (final var strValue : cvalue.getValues()) {
					if (strValue != null) {
						return strValue;
					}
				}
			}
		}
		return null;
	}

	/** Extract the string values of the given annotation, if they exist.
	 *
	 * @param op the annotated element.
	 * @param annotationType the type of the annotation to consider
	 * @return the values of the annotation, never {@code null}.
	 */
	public List<String> findStringValues(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final var reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findStringValues(reference);
		}
		return null;
	}

	/** Extract the string values of the given annotation, if they exist.
	 *
	 * @param reference the reference to the annotation.
	 * @return the values of the annotation, never {@code null}.
	 */
	@SuppressWarnings("static-method")
	public List<String> findStringValues(JvmAnnotationReference reference) {
		assert reference != null;
		final var values = new ArrayList<String>();
		for (final var value : reference.getValues()) {
			if (value instanceof JvmStringAnnotationValue cvalue) {
				for (final var strValue : cvalue.getValues()) {
					if (strValue != null) {
						values.add(strValue);
					}
				}
			}
		}
		return values;
	}

	/** Extract the type values of the given annotation, if they exist.
	 *
	 * @param op the annotated element.
	 * @param annotationType the type of the annotation to consider
	 * @return the values of the annotation, never {@code null}.
	 */
	public List<JvmTypeReference> findTypeValues(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final var reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findTypeValues(reference);
		}
		return null;
	}

	/** Extract the type values of the given annotation, if they exist.
	 *
	 * @param reference the reference to the annotation.
	 * @return the values of the annotation, never {@code null}.
	 */
	@SuppressWarnings("static-method")
	public List<JvmTypeReference> findTypeValues(JvmAnnotationReference reference) {
		assert reference != null;
		final var values = new ArrayList<JvmTypeReference>();
		for (final var value : reference.getValues()) {
			if (value instanceof JvmTypeAnnotationValue cvalue) {
				for (final var strValue : cvalue.getValues()) {
					if (strValue != null) {
						values.add(strValue);
					}
				}
			}
		}
		return values;
	}

	/** Extract the integer value of the given annotation, if it exists.
	 *
	 * @param op the annotated element.
	 * @param annotationType the type of the annotation to consider
	 * @return the value of the annotation, or {@code null} if no annotation or no
	 *     value.
	 * @since 0.6
	 */
	public Integer findIntValue(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final var reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findIntValue(reference);
		}
		return null;
	}

	/** Extract the integer value of the given annotation, if it exists.
	 *
	 * @param reference the reference to the annotation.
	 * @return the value of the annotation, or {@code null} if no annotation or no
	 *     value.
	 * @since 0.6
	 */
	@SuppressWarnings("static-method")
	public Integer findIntValue(JvmAnnotationReference reference) {
		assert reference != null;
		for (final var value : reference.getValues()) {
			if (value instanceof JvmIntAnnotationValue cvalue) {
				for (final var intValue : cvalue.getValues()) {
					if (intValue != null) {
						return intValue;
					}
				}
			}
		}
		return null;
	}

	/** Extract the integer values of the given annotation, if they exist.
	 *
	 * @param op the annotated element.
	 * @param annotationType the type of the annotation to consider
	 * @return the values of the annotation, never {@code null}.
	 * @since 0.6
	 */
	public List<Integer> findIntValues(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final var reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findIntValues(reference);
		}
		return null;
	}

	/** Extract the integer values of the given annotation, if they exist.
	 *
	 * @param reference the reference to the annotation.
	 * @return the values of the annotation, never {@code null}.
	 * @since 0.6
	 */
	@SuppressWarnings("static-method")
	public List<Integer> findIntValues(JvmAnnotationReference reference) {
		assert reference != null;
		final var values = new ArrayList<Integer>();
		for (final var value : reference.getValues()) {
			if (value instanceof JvmIntAnnotationValue cvalue) {
				for (final var intValue : cvalue.getValues()) {
					if (intValue != null) {
						values.add(intValue);
					}
				}
			}
		}
		return values;
	}

	/** Find an annotation.
	 *
	 * @param annotationTarget the annotation target.
	 * @param lookupType the name of the type to look for.
	 * @return the annotation or {@code null}.
	 * @see AnnotationLookup#findAnnotation(JvmAnnotationTarget, Class)
	 */
	@SuppressWarnings("static-method")
	public JvmAnnotationReference findAnnotation(JvmAnnotationTarget annotationTarget, String lookupType) {
		// avoid creating an empty list for all given targets but check for #eIsSet first
		if (annotationTarget.eIsSet(TypesPackage.Literals.JVM_ANNOTATION_TARGET__ANNOTATIONS)) {
			for (final var annotation: annotationTarget.getAnnotations()) {
				final var annotationType = annotation.getAnnotation();
				if (annotationType != null && Objects.equals(lookupType, annotationType.getQualifiedName())) {
					return annotation;
				}
			}
		}
		return null;
	}

	/** Extract the boolean value of the given annotation, if it exists.
	 *
	 * @param op the annotated element.
	 * @param annotationType the type of the annotation to consider
	 * @return the value of the annotation, or {@code null} if no annotation or no
	 *     value.
	 * @since 0.6
	 */
	public Boolean findBooleanValue(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final var reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findBooleanValue(reference);
		}
		return null;
	}

	/** Extract the boolean value of the given annotation, if it exists.
	 *
	 * @param reference the reference to the annotation.
	 * @return the value of the annotation, or {@code null} if no annotation or no
	 *     value.
	 * @since 0.6
	 */
	@SuppressWarnings("static-method")
	public Boolean findBooleanValue(JvmAnnotationReference reference) {
		assert reference != null;
		for (final var value : reference.getValues()) {
			if (value instanceof JvmBooleanAnnotationValue cvalue) {
				for (final var intValue : cvalue.getValues()) {
					if (intValue != null) {
						return intValue;
					}
				}
			}
		}
		return null;
	}

	/** Extract the boolean values of the given annotation, if they exist.
	 *
	 * @param op the annotated element.
	 * @param annotationType the type of the annotation to consider
	 * @return the values of the annotation, never {@code null}.
	 * @since 0.6
	 */
	public List<Boolean> findBooleanValues(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final var reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findBooleanValues(reference);
		}
		return null;
	}

	/** Extract the boolean values of the given annotation, if they exist.
	 *
	 * @param reference the reference to the annotation.
	 * @return the values of the annotation, never {@code null}.
	 * @since 0.6
	 */
	@SuppressWarnings("static-method")
	public List<Boolean> findBooleanValues(JvmAnnotationReference reference) {
		assert reference != null;
		final var values = new ArrayList<Boolean>();
		for (final var value : reference.getValues()) {
			if (value instanceof JvmBooleanAnnotationValue cvalue) {
				for (final var boolValue : cvalue.getValues()) {
					if (boolValue != null) {
						values.add(boolValue);
					}
				}
			}
		}
		return values;
	}

}
