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

package io.sarl.docs.doclet.utils;

import java.io.IOException;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.sun.javadoc.AnnotationDesc;
import com.sun.javadoc.AnnotationDesc.ElementValuePair;
import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.ExecutableMemberDoc;
import com.sun.javadoc.FieldDoc;
import com.sun.javadoc.Parameter;
import com.sun.javadoc.ProgramElementDoc;
import com.sun.javadoc.SourcePosition;
import com.sun.tools.doclets.formats.html.markup.HtmlTree;
import com.sun.tools.doclets.formats.html.markup.StringContent;
import com.sun.tools.doclets.internal.toolkit.Content;
import com.sun.tools.doclets.internal.toolkit.util.DocFile;
import com.sun.tools.doclets.internal.toolkit.util.DocPath;
import com.sun.tools.doclets.internal.toolkit.util.DocletAbortException;

import io.sarl.docs.doclet.SarlConfiguration;

/** Utilities.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public final class Utils {

	private static final String HIDDEN_MEMBER_CHARACTER = "$"; //$NON-NLS-1$

	private static final String HIDDEN_MEMBER_REPLACEMENT_CHARACTER = "_"; //$NON-NLS-1$

	private static final String PREFIX_DEFAULT_VALUE = HIDDEN_MEMBER_CHARACTER + "DEFAULT_VALUE" //$NON-NLS-1$
			+ HIDDEN_MEMBER_CHARACTER;

	private static final String SARL_PACKAGE_PREFIX;

	static {
		final StringBuilder name = new StringBuilder();
		final String[] components = "io.sarl.lang.annotation".split("\\."); //$NON-NLS-1$ //$NON-NLS-2$
		final int len = Math.min(3, components.length);
		for (int i = 0; i < len; ++i) {
			name.append(components[i]);
			name.append("."); //$NON-NLS-1$
		}
		SARL_PACKAGE_PREFIX = name.toString();
	}

	private static SARLFeatureAccess keywords;

	private Utils() {
		//
	}

	/** Format a keyword.
	 *
	 * @param keyword the keyword.
	 * @return the formatted keyword.
	 */
	public static Content keyword(String keyword) {
		return HtmlTree.CODE(new StringContent(keyword));
	}

	/** Replies the SARL keywords.
	 *
	 * @return the SARL keywords.
	 */
	public static SARLFeatureAccess getKeywords() {
		return keywords;
	}

	/** Change the SARL keywords.
	 *
	 * @param sarlKeywords the SARL keywords.
	 */
	public static void setKeywords(SARLFeatureAccess sarlKeywords) {
		keywords = sarlKeywords;
	}

	/** Filter the given array.
	 *
	 * @param <T> the type of the elements to filter.
	 * @param type the type of the elements to filter.
	 * @param original the original array.
	 * @param ignoreHidden indicates if the hidden elements should be ignored.
	 * @param filter the filtering action. If it replies {@code null}, the element is removed from the list.
	 * @return the result.
	 */
	@SuppressWarnings("unchecked")
	public static <T extends ProgramElementDoc> T[] filter(Class<T> type, T[] original, boolean ignoreHidden, Filter<T> filter) {
		final List<T> list = new ArrayList<>();
		for (final T element : original) {
			if (!ignoreHidden || !isHiddenMember(element.name())) {
				final T newElement = filter.filter(element);
				if (newElement != null) {
					list.add(newElement);
				}
			}
		}
		final T[] result = (T[]) Array.newInstance(type, list.size());
		list.toArray(result);
		return result;
	}

	/** Filter the given array.
	 *
	 * @param <T> the type of the elements to filter.
	 * @param type the type of the elements to filter.
	 * @param original the original array.
	 * @param filter the filtering action. If it replies {@code null}, the element is removed from the list.
	 * @return the result.
	 */
	@SuppressWarnings("unchecked")
	public static <T extends AnnotationDesc> T[] filter(Class<T> type, T[] original, Filter<T> filter) {
		final List<T> list = new ArrayList<>();
		for (final T element : original) {
			final String annoName = element.annotationType().name();
			if (!isSARLAnnotation(annoName) && !isHiddenMember(annoName)) {
				final T newElement = filter.filter(element);
				if (newElement != null) {
					list.add(newElement);
				}
			}
		}
		final T[] result = (T[]) Array.newInstance(type, list.size());
		list.toArray(result);
		return result;
	}

	/** Find the first element into the given array.
	 *
	 * @param <T> the type of the elements to filter.
	 * @param original the original array.
	 * @param ignoreHidden indicates if the hidden elements should be ignored.
	 * @param filter the filtering action.
	 * @return the first element.
	 */
	public static <T extends ProgramElementDoc> T findFirst(T[] original, boolean ignoreHidden, Predicate<T> filter) {
		for (final T element : original) {
			if (!ignoreHidden || !isHiddenMember(element.name())) {
				if (filter.test(element)) {
					return element;
				}
			}
		}
		return null;
	}

	/** Find the first element into the given array.
	 *
	 * @param <T> the type of the elements to filter.
	 * @param original the original array.
	 * @param filter the filtering action.
	 * @return the first element.
	 */
	public static <T extends AnnotationDesc> T findFirst(T[] original, Predicate<T> filter) {
		for (final T element : original) {
			if (filter.test(element)) {
				return element;
			}
		}
		return null;
	}

	/** Replies the default value of the given parameter.
	 *
	 * @param member the member
	 * @param param the parameter.
	 * @param configuration the configuration.
	 * @return the default value or {@code null}.
	 */
	@SuppressWarnings("checkstyle:nestedifdepth")
	public static String getParameterDefaultValue(ExecutableMemberDoc member, Parameter param, SarlConfiguration configuration) {
		final AnnotationDesc annotation = Utils.findFirst(param.annotations(), it ->
				qualifiedNameEquals(it.annotationType().qualifiedTypeName(), getKeywords().getDefaultValueAnnnotationName()));
		if (annotation != null) {
			final ElementValuePair[] pairs = annotation.elementValues();
			if (pairs != null && pairs.length > 0) {
				final String fieldId = pairs[0].value().value().toString();

				final int index = fieldId.indexOf('#');
				ClassDoc fieldContainer;
				final String fieldName;
				if (index > 0) {
					final String referenceName = fieldId.substring(0, index);
					if (qualifiedNameEquals(referenceName, member.containingClass().qualifiedName())) {
						fieldContainer = member.containingClass();
					} else {
						fieldContainer = findFirst(configuration.classDocCatalog.allClasses(getPackageName(referenceName)),
								false, it -> false);
						if (fieldContainer == null) {
							fieldContainer = member.containingClass();
						}
					}
					fieldName = createNameForHiddenDefaultValueAttribute(fieldId.substring(index + 1));
				} else {
					fieldContainer = member.containingClass();
					fieldName = createNameForHiddenDefaultValueAttribute(fieldId);
				}

				final FieldDoc field = Utils.findFirst(fieldContainer.fields(),
						false, it -> simpleNameEquals(it.name(), fieldName));
				if (field != null) {
					final AnnotationDesc valueAnnotation = Utils.findFirst(field.annotations(), it ->
							qualifiedNameEquals(it.annotationType().qualifiedTypeName(), getKeywords().getSarlSourceCodeAnnotationName()));
					if (valueAnnotation != null) {
						return valueAnnotation.elementValues()[0].value().value().toString();
					}
				}
			}
		}
		return null;
	}

	/** Replies the default value of the given parameter.
	 *
	 * @param param the parameter.
	 * @param configuration the configuration.
	 * @return the default value or {@code null}.
	 */
	public static boolean isDefaultValuedParameter(Parameter param, SarlConfiguration configuration) {
		final AnnotationDesc annotation = Utils.findFirst(param.annotations(), it ->
				qualifiedNameEquals(it.annotationType().qualifiedTypeName(), getKeywords().getDefaultValueAnnnotationName()));
		return annotation != null;
	}

	private static String getPackageName(String name) {
		final int index = name.lastIndexOf("."); //$NON-NLS-1$
		return name.substring(0, index);
	}

	/** Replies the SARL element type of the given type.
	 *
	 * @param type the type.
	 * @return the SARL element type, or {@code null} if unknown.
	 */
	public static Integer getSarlClassification(ProgramElementDoc type) {
		final AnnotationDesc annotation = Utils.findFirst(type.annotations(), it ->
				qualifiedNameEquals(it.annotationType().qualifiedTypeName(), getKeywords().getSarlElementTypeAnnotationName()));
		if (annotation != null) {
			final ElementValuePair[] pairs = annotation.elementValues();
			if (pairs != null && pairs.length > 0) {
				return ((Number) pairs[0].value().value()).intValue();
			}
		}
		return null;
	}

	/** Replies if the given name is related to an hidden action.
	 *
	 * <p>An hidden action is an action that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name the name to test.
	 * @return <code>true</code> if the given name is reserved by SARL.
	 */
	public static boolean isHiddenMember(String name) {
		return name.contains(HIDDEN_MEMBER_CHARACTER);
	}

	/** Replies a fixed version of the given name assuming
	 * that it is an hidden action, and reformating
	 * the reserved text.
	 *
	 * <p>An hidden action is an action that is generated by the SARL
	 * compiler, and that cannot be defined by the SARL user.
	 *
	 * @param name the name to fix.
	 * @return the fixed name.
	 */
	public static String fixHiddenMember(String name) {
		return name.replaceAll(Pattern.quote(HIDDEN_MEMBER_CHARACTER),
				Matcher.quoteReplacement(HIDDEN_MEMBER_REPLACEMENT_CHARACTER));
	}

	/** Create the name of the hidden attribute that is containing a parameter's default value.
	 *
	 * @param id the id of the default value.
	 * @return the method name.
	 */
	public static String createNameForHiddenDefaultValueAttribute(String id) {
		return PREFIX_DEFAULT_VALUE + fixHiddenMember(id.toUpperCase());
	}

	/** Replies if the given annotation is an annotation from the SARL core library.
	 *
	 * @param qualifiedName the qualified name of the annotation type.
	 * @return <code>true</code> if the given type is a SARL annotation.
	 */
	public static boolean isSARLAnnotation(String qualifiedName) {
		return qualifiedName != null && qualifiedName.startsWith(SARL_PACKAGE_PREFIX);
	}

	/** Replies if the given string is null or empty.
	 *
	 * @param value the value.
	 * @return {@code true} if {@code null} or empty string.
	 */
	public static boolean isNullOrEmpty(String value) {
		return value == null || value.isEmpty();
	}

	/** Replies if the given qualified names are equal.
	 *
	 * <p>Because the Javadoc tool cannot create the fully qualified name, this function
	 * also test simple names.
	 *
	 * @param s1 the first string.
	 * @param s2 the first string.
	 * @return {@code true} if the strings are equal.
	 */
	public static boolean qualifiedNameEquals(String s1, String s2) {
		if (isNullOrEmpty(s1)) {
			return isNullOrEmpty(s2);
		}
		if (!s1.equals(s2)) {
			final String simple1 = simpleName(s1);
			final String simple2 = simpleName(s2);
			return simpleNameEquals(simple1, simple2);
		}
		return true;
	}

	/** Replies if the given unqualified/simple names are equal.
	 *
	 * @param s1 the first string.
	 * @param s2 the first string.
	 * @return {@code true} if the strings are equal.
	 */
	public static boolean simpleNameEquals(String s1, String s2) {
		if (isNullOrEmpty(s1)) {
			return isNullOrEmpty(s2);
		}
		return s1.equals(s2);
	}

	/** Replies the simple name for the given name.
	 *
	 * @param name the name.
	 * @return the simple name.
	 */
	public static String simpleName(String name) {
		if (name == null) {
			return null;
		}
		final int index = name.lastIndexOf('.');
		if (index > 0) {
			return name.substring(index + 1);
		}
		return name;
	}

	/** Replies the cause of the given exception.
	 *
	 * @param thr the exception.
	 * @return the cause.
	 */
	public static Throwable getCause(Throwable thr) {
		Throwable cause = thr.getCause();
		while (cause != null && cause != thr && cause != cause.getCause() && cause.getCause() != null) {
			cause = cause.getCause();
		}
		return cause == null ? thr : cause;
	}

	/** Copy the given file.
	 *
	 * @param filename the file to copy.
	 * @param configuration the configuration.
	 * @throws DocletAbortException a runtime exception.
	 */
	public static void performCopy(String filename, SarlConfiguration configuration) {
		if (filename.isEmpty()) {
			return;
		}

		try {
			final DocFile fromfile = DocFile.createFileForInput(configuration, filename);
			final DocPath path = DocPath.create(fromfile.getName());
			final DocFile toFile = DocFile.createFileForOutput(configuration, path);
			if (toFile.isSameFile(fromfile)) {
				return;
			}
			configuration.message.notice((SourcePosition) null,
					"doclet.Copying_File_0_To_File_1", //$NON-NLS-1$
					fromfile.toString(), path.getPath());
			toFile.copyFile(fromfile);
		} catch (IOException exc) {
			configuration.message.error((SourcePosition) null,
					"doclet.perform_copy_exception_encountered", //$NON-NLS-1$
					exc.toString());
			throw new DocletAbortException(exc);
		}
	}

	/** Create an empty Java list.
	 *
	 * @param <T> the type of the elements into the list.
	 * @return the list.
	 */
	public static <T> com.sun.tools.javac.util.List<T> emptyList() {
		return com.sun.tools.javac.util.List.from(Collections.emptyList());
	}

	/** Convert to an array.
	 *
	 * @param <T> the type of the elements.
	 * @param source the original array.
	 * @param newContent the new content.
	 * @return the new array.
	 */
	@SuppressWarnings("unchecked")
	public static <T> T[] toArray(T[] source, Collection<? extends T> newContent) {
		final Object array = Array.newInstance(source.getClass().getComponentType(), newContent.size());
		int i = 0;
		for (final T element : newContent) {
			Array.set(array, i, element);
			++i;
		}
		return (T[]) array;
	}

	/** Filter function.
	 *
	 * @param <T> the type of the element to filter.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	public interface Filter<T> {

		/** Filter the given value.
		 *
		 * @param value the value.
		 * @return the new value or {@code null}.
		 */
		T filter(T value);

	}

}
