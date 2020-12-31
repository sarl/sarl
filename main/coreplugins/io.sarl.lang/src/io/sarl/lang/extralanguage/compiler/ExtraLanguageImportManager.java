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

package io.sarl.lang.extralanguage.compiler;

import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmPrimitiveType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmVoid;
import org.eclipse.xtext.xbase.compiler.ImportManager;

/** Import manager for SARL extra target languages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ExtraLanguageImportManager extends ImportManager {

	private final ExtraLanguageTypeConverter converter;

	/** Constructor.
	 *
	 * @param converter the type conversion to be used.
	 */
	public ExtraLanguageImportManager(ExtraLanguageTypeConverter converter) {
		super(true);
		this.converter = converter;
	}

	/** Constructor.
	 *
	 * @param converter the type conversion to be used.
	 * @param thisType the name of the current type.
	 */
	public ExtraLanguageImportManager(ExtraLanguageTypeConverter converter, JvmDeclaredType thisType) {
		super(true, thisType);
		this.converter = converter;
	}

	/** Constructor.
	 *
	 * @param converter the type conversion to be used.
	 * @param innerSeparator the character to be used as inner separator.
	 */
	public ExtraLanguageImportManager(ExtraLanguageTypeConverter converter, char innerSeparator) {
		super(true, innerSeparator);
		this.converter = converter;
	}

	/** Constructor.
	 *
	 * @param converter the type conversion to be used.
	 * @param thisType the name of the current type.
	 * @param innerSeparator the character to be used as inner separator.
	 */
	public ExtraLanguageImportManager(ExtraLanguageTypeConverter converter,
			JvmDeclaredType thisType, char innerSeparator) {
		super(true, thisType, innerSeparator);
		this.converter = converter;
	}

	/** Convert the given qualified name.
	 *
	 * @param name the given qualified name.
	 * @return the conversion result, or {@code null} if no type equivalent exists.
	 */
	protected String convertQualifiedName(String name) {
		return this.converter.convert(name);
	}

	private String getSimpleName(String name) {
		if (name == null) {
			return null;
		}
		final int index = name.lastIndexOf(getInnerTypeSeparator());
		if (index < 0) {
			return name;
		}
		return name.substring(index + 1);
	}

	@Override
	public boolean addImportFor(JvmType type) {
		final String qualifiedName = convertQualifiedName(type.getQualifiedName(getInnerTypeSeparator()));
		if (qualifiedName == null) {
			return false;
		}
		final String simpleName = getSimpleName(qualifiedName);
		if (!allowsSimpleName(qualifiedName, simpleName) && !needsQualifiedName(qualifiedName, simpleName)
				&& !internalGetImports().containsKey(simpleName)) {
			internalGetImports().put(simpleName, qualifiedName);
			return true;
		}
		return false;
	}

	@Override
	public void appendType(Class<?> type, StringBuilder builder) {
		if (type.isPrimitive()) {
			final String qualifiedName = convertQualifiedName(type.getSimpleName());
			if (qualifiedName == null) {
				return;
			}
			builder.append(qualifiedName);
		} else {
			final String qualifiedName = convertQualifiedName(type.getCanonicalName());
			if (qualifiedName == null) {
				return;
			}
			if (!qualifiedName.contains(Character.toString(getInnerTypeSeparator()))) {
				builder.append(qualifiedName);
				return;
			}
			String nameToImport = qualifiedName;
			String shortName = getSimpleName(qualifiedName);
			String outerShortName = shortName;
			if (shouldUseQualifiedNestedName(qualifiedName)) {
				Class<?> outerContainer = type;
				while (outerContainer.getDeclaringClass() != null) {
					outerContainer = outerContainer.getDeclaringClass();
				}
				if (type != outerContainer) {
					outerShortName =  outerContainer.getSimpleName();
					if (!getThisTypeQualifiedNames().contains(outerContainer.getCanonicalName())
							&& getThisTypeSimpleNames().contains(outerShortName)) {
						outerShortName = qualifiedName;
						shortName = qualifiedName;
					} else {
						nameToImport = outerContainer.getCanonicalName();
						shortName = outerShortName + qualifiedName.substring(nameToImport.length());
					}
				}
			}
			appendType(qualifiedName, shortName, outerShortName, nameToImport, builder);
		}
	}

	@Override
	public void appendType(JvmType type, StringBuilder builder) {
		final String qualifiedName = convertQualifiedName(type.getQualifiedName(getInnerTypeSeparator()));
		if (qualifiedName == null) {
			return;
		}
		if (type instanceof JvmPrimitiveType || type instanceof JvmVoid || type instanceof JvmTypeParameter
				|| !qualifiedName.contains(Character.toString(getInnerTypeSeparator()))) {
			builder.append(qualifiedName);
		} else {
			String nameToImport = qualifiedName;
			String shortName = getSimpleName(qualifiedName);
			String outerShortName = shortName;
			if (shouldUseQualifiedNestedName(qualifiedName)) {
				JvmType outerContainer = type;
				while (outerContainer.eContainer() instanceof JvmType) {
					outerContainer = (JvmType) outerContainer.eContainer();
				}
				if (type != outerContainer) {
					outerShortName = outerContainer.getSimpleName();
					if (!getThisTypeQualifiedNames().contains(outerContainer.getQualifiedName(getInnerTypeSeparator()))
							&& getThisTypeSimpleNames().contains(outerShortName)) {
						outerShortName = qualifiedName;
						shortName = qualifiedName;
					} else {
						nameToImport = outerContainer.getQualifiedName(getInnerTypeSeparator());
						shortName = outerShortName + qualifiedName.substring(nameToImport.length());
					}
				}
			}
			appendType(qualifiedName, shortName, outerShortName, nameToImport, builder);
		}
	}

}
