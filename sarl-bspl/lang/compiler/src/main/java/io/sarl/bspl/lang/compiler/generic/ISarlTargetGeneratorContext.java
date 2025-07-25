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

package io.sarl.bspl.lang.compiler.generic;

import java.io.IOException;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.generator.IGeneratorContext;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import com.google.inject.Injector;

/** Context for the generation of SARL files.
 *
 * @param <NP> the type for the name provider.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public interface ISarlTargetGeneratorContext<NP> extends IGeneratorContext {

	/** Replies the injector to be used.
	 *
	 * @return the injector.
	 */
	Injector getInjector();

	/** Replies the provider of names that is used in this context.
	 *
	 * @return the provider of names.
	 */
	NP getNameProvider();

	/** Replies the package in which the generation occurs.
	 *
	 * @return the package name or {@code null}.
	 */
	String getPackage();

	/** Replies the name of the enclosing type name.
	 *
	 * @return the type name.
	 */
	String getEnclosingTypeName();
	
	/** Replies if the a generator is marked as in a pre-stage context.
	 * Pre-stage is the first step in the generation. It is supposed to
	 * generate the base elements for enabling the full generation. 
	 *
	 * @return the generator context.
	 */
	boolean isPreStage();
	
	/** Create a generator context that is marked as in a pre-stage context.
	 * Pre-stage is the first step in the generation. It is supposed to
	 * generate the base elements for enabling the full generation. 
	 *
	 * @return the generator context.
	 */
	ISarlTargetGeneratorContext<NP> forPreStage();

	/** Create a generator context that is linked to the given name provider.
	 *
	 * @param nameProvider the provider of names.
	 * @return the generator context.
	 */
	ISarlTargetGeneratorContext<NP> withNameProvider(NP nameProvider);

	/** Create a generator context that is linked to the given package name.
	 *
	 * @param packageName the name of the package.
	 * @return the generator context.
	 */
	ISarlTargetGeneratorContext<NP> withPackage(String packageName);

	/** Create a generator context that is linked to the given type name.
	 *
	 * @param typeName the name of the type.
	 * @return the generator context.
	 */
	ISarlTargetGeneratorContext<NP> withTypeName(String typeName);

	/** Create a import manager.
	 *
	 * @param packageName the name of the package for the current type.
	 * @param basename the basename of the current type.
	 * @return the import manager.
	 */
	ImportManager newImportManager(String packageName, String basename);

	/** Create an object for appending content.
	 *
	 * @param importManager the manager of imported files.
	 * @return the appendable object.
	 */
	ITreeAppendable newAppendableContent(ImportManager importManager);

	/** Create a Java file.
	 *
	 * @param packageName the name of the package. It may be {@code null}.
	 * @param typeName the base name of the type.
	 * @param fileExtension the extension in the filename.
	 * @param content the content of the file.
	 * @throws IOException error when it is impossible to create the file.
	 */
	void createFile(String packageName, String typeName, String fileExtension, String content) throws IOException;

	/** Build a filename based on the given arguments, using {@code /} as file separator.
	 *
	 * @param packageName the name of the package.
	 * @param typeName the basename.
	 * @param fileExtension the filename extension.
	 * @return the complete relative filename.
	 */
	String toRelativeFilename(String packageName, String typeName, String fileExtension);

	/** Create a Java file.
	 *
	 * @param packageName the name of the package. It may be {@code null}.
	 * @param typeName the base name of the type.
	 * @param importManager the manager of imports that are used by the appendable {@code content}.
	 * @param content the content of the file.
	 * @return the file content.
	 * @throws IOException error when it is impossible to create the file.
	 */
	default String createJavaFile(String packageName, String typeName, ImportManager importManager, ITreeAppendable content) throws IOException {
		final var finalContent = new StringBuilder();
		finalContent.append("/* This file was automatically generated. Do not change its content. */\n\n"); //$NON-NLS-1$
		if (!Strings.isEmpty(packageName)) {
			finalContent.append("package "); //$NON-NLS-1$
			finalContent.append(packageName);
			finalContent.append(";\n\n"); //$NON-NLS-1$
		}
		if (importManager != null) {
			for (final var importedType : importManager.getImports()) {
				finalContent.append("import ").append(importedType).append(";\n"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			finalContent.append("\n"); //$NON-NLS-1$
		}
		finalContent.append(content.getContent());
		final var stringContent = finalContent.toString();
		createFile(packageName, typeName, ".java", stringContent); //$NON-NLS-1$
		return stringContent;
	}

	/** Create a SARL file.
	 *
	 * @param packageName the name of the package. It may be {@code null}.
	 * @param typeName the base name of the type.
	 * @param importManager the manager of imports that are used by the appendable {@code content}.
	 * @param content the content of the file.
	 * @return the content of the file.
	 * @throws IOException error when it is impossible to create the file.
	 */
	default String createSarlFile(String packageName, String typeName, ImportManager importManager, ITreeAppendable content) throws IOException {
		final var finalContent = new StringBuilder();
		finalContent.append("/* This file was automatically generated. Do not change its content. */\n\n"); //$NON-NLS-1$
		if (!Strings.isEmpty(packageName)) {
			finalContent.append("package ").append(packageName).append("\n\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (importManager != null) {
			for (final var importedType : importManager.getImports()) {
				finalContent.append("import ").append(importedType).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			finalContent.append("\n"); //$NON-NLS-1$
		}
		finalContent.append(content.getContent());
		final var fileContent = finalContent.toString();
		createFile(packageName, typeName, ".sarl", fileContent); //$NON-NLS-1$
		return fileContent;
	}

	/** Replies the resource set used for storing the generated content.
	 *
	 * @return the resource set.
	 */
	ResourceSet getResourceSet();

	/** Create a resource with the given filename extension into the given resource set.
	 *
	 * @param packageName the name of the package. It may be {@code null}.
	 * @param typeName the base name of the type.
	 * @param filenameExtension the filename's extension.
	 * @param resourceSet the resource set in which the resource should be located.
	 * @return the resource.
	 */
	Resource createResource(String packageName, String typeName, String filenameExtension, ResourceSet  resourceSet);

	/** Find a reference to a type.
	 *
	 * @param typeName the name of the type.
	 * @param context the context in which types are defined.
	 * @return the reference.
	 */
	LightweightTypeReference findType(String typeName, EObject context);

}
