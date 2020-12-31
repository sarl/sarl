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

package io.sarl.lang.typesystem;

import java.io.File;
import java.lang.annotation.Annotation;
import java.net.InetAddress;
import java.net.URI;
import java.net.URL;
import java.security.Permission;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.MonthDay;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Period;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.Locale;
import java.util.UUID;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.core.Event;

/**
 * Tool for validating the types against their immutability.
 *
 * <p>An immutable type is a type those state cannot be changed after it is constructed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class DefaultImmutableTypeValidator implements IImmutableTypeValidator {

	/** List of the well-known immutable types from Java, that are also considered as
	 * immutable in SARL.
	 *
	 * @since 0.12
	 */
	public static final Class<?>[] IMMUTABLE_TYPES = {
		String.class,
		UUID.class,
		URL.class,
		URI.class,
		Annotation.class,
		Enum.class,
		Byte.class,
		Short.class,
		Integer.class,
		Long.class,
		Double.class,
		Float.class,
		Character.class,
		Boolean.class,
		Date.class,
		File.class,
		Locale.class,
		InetAddress.class,
		StackTraceElement.class,
		Permission.class,
		Clock.class,
		Duration.class,
		Instant.class,
		LocalDate.class,
		LocalDateTime.class,
		LocalTime.class,
		MonthDay.class,
		OffsetDateTime.class,
		OffsetTime.class,
		Period.class,
		Year.class,
		YearMonth.class,
		ZonedDateTime.class,
		ZoneId.class,
		ZoneOffset.class,
		Pair.class,
		org.eclipse.xtext.xbase.lib.Pair.class,
		Event.class,
	};

	/** List of the annotations that are known to mark the types as immutable.
	 *
	 * @since 0.12
	 */
	@SuppressWarnings("deprecation")
	public static final Class<?>[] IMMUTABLE_TYPE_ANNOTATIONS = {
		Data.class,
		org.eclipse.xtend.lib.Data.class,
	};

	/** Finder of annotations.
	 */
	private AnnotationLookup annotationFinder;

	/** Change the JVM annotation finder that is used by this immutable type validator.
	 *
	 * @param finder the new finder, must not be {@code null}.
	 */
	@Inject
	public void setAnnotationLookup(AnnotationLookup finder) {
		assert finder != null;
		this.annotationFinder = finder;
	}

	@Override
	public boolean isImmutable(LightweightTypeReference type) {
		assert type != null;
		// Several special types are assumed to be always mutable
		if (isAlwaysMutable(type)) {
			return false;
		}
		// Even if the primitive wrappers are in the IMMUTABLE_TYPES list, this
		// test enables to exit early from the function in case the type is a
		// primitive type or one of the associated wrapper types
		if (isAlwaysImmutable(type)) {
			return true;
		}
		// Test if the type is annotated with one of the known annotations
		if (hasImmutableAnnotation(type)) {
			return true;
		}
		// Test the type itself
		return isRegisteredImmutableType(type);
	}

	/** Replies if the given type is always assumed to be mutable.
	 *
	 * @param ref the type to test.
	 * @return {@code true} if the given type is always mutable; or {@code false} if
	 *     it is not known yet that the type is mutable or not.
	 */
	protected boolean isAlwaysMutable(LightweightTypeReference ref) {
		return ref.isArray() || ref.isAnonymous() || ref.isAny() || ref.isUnknown() || ref.isFunctionType();
	}

	/** Replies if the given type is always assumed to be immutable.
	 *
	 * @param ref the type to test.
	 * @return {@code true} if the given type is always immutable; or {@code false} if
	 *     it is not known yet that the type is immutable or not.
	 */
	protected boolean isAlwaysImmutable(LightweightTypeReference ref) {
		final LightweightTypeReference pref = ref.getPrimitiveIfWrapperType();
		return pref.isPrimitive() || pref.isPrimitiveVoid();
	}

	/** Replies if the given type is marked with an annotation that makes it immutable.
	 *
	 * @param ref the type to test.
	 * @return {@code true} if the given type is annotated with an immutable type annotation; or {@code false} if
	 *     it is not known yet that the type is immutable or not.
	 * @see #IMMUTABLE_TYPE_ANNOTATIONS
	 */
	protected boolean hasImmutableAnnotation(LightweightTypeReference ref) {
		final JvmType backType = ref.getType();
		if (backType instanceof JvmAnnotationTarget) {
			final JvmAnnotationTarget target = (JvmAnnotationTarget) backType;
			for (final Class<?> jvmType : IMMUTABLE_TYPE_ANNOTATIONS) {
				if (this.annotationFinder.findAnnotation(target, jvmType.getCanonicalName()) != null) {
					return true;
				}
			}
		}
		return false;
	}

	/** Replies if the given type is known as an immutable type.
	 *
	 * @param ref the type to test.
	 * @return {@code true} if the given type is an immutable type; or {@code false} if
	 *     it is not known yet that the type is immutable or not.
	 * @see #IMMUTABLE_TYPES
	 */
	protected boolean isRegisteredImmutableType(LightweightTypeReference ref) {
		for (final Class<?> jvmType : IMMUTABLE_TYPES) {
			if (ref.isSubtypeOf(jvmType)) {
				return true;
			}
		}
		return false;
	}

}
