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

package io.sarl.lang.validation.subvalidators;

import static io.sarl.lang.validation.IssueCodes.DUPLICATE_TYPE_NAME;
import static io.sarl.lang.validation.IssueCodes.INVALID_SARL_LIB_ON_CLASSPATH;
import static io.sarl.lang.validation.IssueCodes.SARL_LIB_NOT_ON_CLASSPATH;
import static org.eclipse.xtend.core.validation.IssueCodes.JDK_NOT_ON_CLASSPATH;

import java.text.MessageFormat;

import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;

import com.google.common.collect.HashMultimap;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.core.util.OutParameter;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;
import io.sarl.lang.util.Utils.SarlLibraryErrorCode;

/**
 * A specialized validator to deal with SARL script without considering the defined elements inside.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLScriptValidator extends AbstractSARLSubValidatorWithParentLink {

	/** Check if the correct SARL libraries are on the classpath.
	 *
	 * <p>This function is overriding the function given by the Xtend validator
	 * for having finer tests, and firing a warning in place of an error.
	 *
	 * @param sarlScript the SARL script.
	 */
	@Check(CheckType.FAST)
	public void checkClassPath(XtendFile sarlScript) {
		final var typeReferences = getServices().getTypeReferences();

		if (!Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment()) {
			error(
					MessageFormat.format(
							Messages.SARLScriptValidator_1,
							System.getProperty("java.specification.version"), //$NON-NLS-1$
							SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT,
							SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT),
					sarlScript,
					XtendPackage.Literals.XTEND_FILE__PACKAGE,
					JDK_NOT_ON_CLASSPATH);
		} else {
			final var generatorConfiguration = getParentValidator().getGeneratorConfig(sarlScript);
			final var generatorVersion = generatorConfiguration.getJavaSourceVersion();
			if (generatorVersion == null
					|| !Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath(generatorVersion.getQualifier())) {
				error(
						MessageFormat.format(
								Messages.SARLScriptValidator_2,
								generatorVersion,
								SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH,
								SARLVersion.INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH),
						sarlScript,
						XtendPackage.Literals.XTEND_FILE__PACKAGE,
						JDK_NOT_ON_CLASSPATH);
			}
		}

		final var sarlLibraryVersion = new OutParameter<String>();
		final var errorCode = Utils.getSARLLibraryVersionOnClasspath(typeReferences, sarlScript, sarlLibraryVersion);
		if (errorCode != SarlLibraryErrorCode.SARL_FOUND) {
			final var resourceSet = EcoreUtil2.getResourceSet(sarlScript);
			final var classPath = new StringBuilder();
			for (final var resource : resourceSet.getResources()) {
				classPath.append(resource.getURI().toString());
				classPath.append("\n"); //$NON-NLS-1$
			}
			final var fields = new StringBuilder();
			try {
				final var type = (JvmDeclaredType) typeReferences.findDeclaredType(SARLVersion.class, sarlScript);
				for (final var field : type.getDeclaredFields()) {
					fields.append(field.getIdentifier());
					fields.append(" / "); //$NON-NLS-1$
					fields.append(field.getSimpleName());
					fields.append("\n"); //$NON-NLS-1$
				}
			} catch (Exception e) {
				//
			}
			if (fields.length() == 0) {
				for (final var field : SARLVersion.class.getDeclaredFields()) {
					fields.append(field.getName());
					fields.append("\n"); //$NON-NLS-1$
				}
			}
			error(
					MessageFormat.format(Messages.SARLScriptValidator_3, errorCode.name(), classPath.toString(), fields.toString()),
					sarlScript,
					XtendPackage.Literals.XTEND_FILE__PACKAGE,
					SARL_LIB_NOT_ON_CLASSPATH);
		} else if (!Utils.isCompatibleSARLLibraryVersion(sarlLibraryVersion.get())) {
			error(
					MessageFormat.format(Messages.SARLScriptValidator_4,
							sarlLibraryVersion.get(), SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING),
					sarlScript,
					XtendPackage.Literals.XTEND_FILE__PACKAGE,
					INVALID_SARL_LIB_ON_CLASSPATH);
		}
	}

	/** Check the top elements within a script are not duplicated.
	 *
	 * @param script the SARL script
	 */
	@Check(CheckType.FAST)
	public void checkTopElementsAreUnique(SarlScript script) {
		final var name2type = HashMultimap.<String, XtendTypeDeclaration>create();
		for (final var declaration : script.getXtendTypes()) {
			final var name = declaration.getName();
			if (!Strings.isEmpty(name)) {
				name2type.put(name, declaration);
			}
		}
		for (final var name: name2type.keySet()) {
			final var types = name2type.get(name);
			if (types.size() > 1) {
				for (final var type: types) {
					error(
							MessageFormat.format(Messages.SARLScriptValidator_5, name),
							type,
							XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME,
							DUPLICATE_TYPE_NAME);
				}
			}
		}
	}

}
