/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import javax.inject.Inject;

import com.google.inject.Singleton;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.xbase.annotations.typing.XAnnotationUtil;

/**
 * Utilities for JVM annotations.
 *
 * <p>This class provides additional utilities than {@link XAnnotationUtil} and {@link AnnotationLookup}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see XAnnotationUtil for Xbase annotations
 * @see AnnotationLookup for retreiving annotations.
 */
@Singleton
public class SARLAnnotationUtil {

	@Inject
	private AnnotationLookup lookup;

	/** Extract the string value of the given annotation, if it exists.
	 *
	 * @param op - the annoted element.
	 * @param annotationType - the type of the annotation to consider
	 * @return the value of the annotation, or <code>null</code> if no annotation or no
	 *     value.
	 */
	public String findStringValue(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final JvmAnnotationReference reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findStringValue(reference);
		}
		return null;
	}

	/** Extract the string value of the given annotation, if it exists.
	 *
	 * @param reference the reference to the annotation.
	 * @return the value of the annotation, or <code>null</code> if no annotation or no
	 *     value.
	 */
	@SuppressWarnings("static-method")
	public String findStringValue(JvmAnnotationReference reference) {
		assert reference != null;
		for (final JvmAnnotationValue value : reference.getValues()) {
			if (value instanceof JvmStringAnnotationValue) {
				for (final String strValue : ((JvmStringAnnotationValue) value).getValues()) {
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
	 * @param op - the annoted element.
	 * @param annotationType - the type of the annotation to consider
	 * @return the values of the annotation, never <code>null</code>.
	 */
	public List<String> findStringValues(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final JvmAnnotationReference reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findStringValues(reference);
		}
		return null;
	}

	/** Extract the string values of the given annotation, if they exist.
	 *
	 * @param reference the reference to the annotation.
	 * @return the values of the annotation, never <code>null</code>.
	 */
	@SuppressWarnings("static-method")
	public List<String> findStringValues(JvmAnnotationReference reference) {
		assert reference != null;
		final List<String> values = new ArrayList<>();
		for (final JvmAnnotationValue value : reference.getValues()) {
			if (value instanceof JvmStringAnnotationValue) {
				for (final String strValue : ((JvmStringAnnotationValue) value).getValues()) {
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
	 * @param op - the annoted element.
	 * @param annotationType - the type of the annotation to consider
	 * @return the values of the annotation, never <code>null</code>.
	 */
	public List<JvmTypeReference> findTypeValues(JvmAnnotationTarget op, Class<? extends Annotation> annotationType) {
		final JvmAnnotationReference reference = this.lookup.findAnnotation(op, annotationType);
		if (reference != null) {
			return findTypeValues(reference);
		}
		return null;
	}

	/** Extract the type values of the given annotation, if they exist.
	 *
	 * @param reference the reference to the annotation.
	 * @return the values of the annotation, never <code>null</code>.
	 */
	@SuppressWarnings("static-method")
	public List<JvmTypeReference> findTypeValues(JvmAnnotationReference reference) {
		assert reference != null;
		final List<JvmTypeReference> values = new ArrayList<>();
		for (final JvmAnnotationValue value : reference.getValues()) {
			if (value instanceof JvmTypeAnnotationValue) {
				for (final JvmTypeReference strValue : ((JvmTypeAnnotationValue) value).getValues()) {
					if (strValue != null) {
						values.add(strValue);
					}
				}
			}
		}
		return values;
	}

}
