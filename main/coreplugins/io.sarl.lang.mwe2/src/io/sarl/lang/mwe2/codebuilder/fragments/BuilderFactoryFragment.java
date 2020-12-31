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

package io.sarl.lang.mwe2.codebuilder.fragments;

import java.lang.reflect.Type;
import java.util.Map;
import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Provider;

import com.google.inject.AbstractModule;
import com.google.inject.Binding;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.Module;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.util.Modules2;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;
import org.eclipse.xtext.xtext.generator.model.XtendFileAccess;

/** Generator of the script builder types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BuilderFactoryFragment extends AbstractSubCodeBuilderFragment {

	@Inject
	private BuilderFactoryContributions contributions;

	/** Replies the custom implementation for the builder factory.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getBuilderFactoryImplCustom() {
		final String runtimeBasePackage = getCodeElementExtractor().getBasePackage();
		return new TypeReference(runtimeBasePackage + ".CodeBuilderFactoryCustom"); //$NON-NLS-1$
	}

	/** Replies the contributions.
	 *
	 * @return the contributions.
	 */
	protected BuilderFactoryContributions getContributions() {
		return this.contributions;
	}

	@Override
	@SuppressWarnings("checkstyle:all")
	public void generate() {
		super.generate();
		final TypeReference factory = getBuilderFactoryImpl();
		StringConcatenationClient content = new StringConcatenationClient() {
			@SuppressWarnings("synthetic-access")
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/**"); //$NON-NLS-1$
				it.append(" * Creates {@link ICodeBuilder}s to insert SARL code snippets."); //$NON-NLS-1$
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(factory.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static final String[] FORBIDDEN_INJECTION_PREFIXES = new String[] {"); //$NON-NLS-1$
				it.newLine();
				for (final String forbiddenPackage : getCodeBuilderConfig().getForbiddenInjectionPrefixes()) {
					it.append("\t\t\""); //$NON-NLS-1$
					it.append(Strings.convertToJavaString(forbiddenPackage));
					if (!forbiddenPackage.endsWith(".")) { //$NON-NLS-1$
						it.append("."); //$NON-NLS-1$
					}
					it.append("\","); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t};"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static final String[] FORBIDDEN_INJECTION_POSTFIXES = new String[] {"); //$NON-NLS-1$
				it.newLine();
				for (final String forbiddenPostfix : getCodeBuilderConfig().getForbiddenInjectionPostfixes()) {
					it.append("\t\t\""); //$NON-NLS-1$
					if (!forbiddenPostfix.startsWith(".")) { //$NON-NLS-1$
						it.append("."); //$NON-NLS-1$
					}
					it.append(Strings.convertToJavaString(forbiddenPostfix));
					it.append("\","); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t};"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(IResourceFactory.class);
				it.append(" resourceFactory;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String fileExtension;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(Provider.class);
				it.append("<"); //$NON-NLS-1$
				it.append(ImportManager.class);
				it.append("> importManagerProvider;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(Injector.class);
				it.append(" originalInjector;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(Injector.class);
				it.append(" builderInjector;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tpublic void setFileExtensions(@"); //$NON-NLS-1$
				it.append(Named.class);
				it.append("("); //$NON-NLS-1$
				it.append(Constants.class);
				it.append(".FILE_EXTENSIONS) String fileExtensions) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.fileExtension = fileExtensions.split(\"[:;,]+\")[0];"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Compute a unused URI for a synthetic resource."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resourceSet the resource set in which the resource should be located."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the uri."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(URI.class);
				it.append(" computeUnusedUri("); //$NON-NLS-1$
				it.append(ResourceSet.class);
				it.append(" resourceSet) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString name = \"__synthetic\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor (int i = 0; i < Integer.MAX_VALUE; ++i) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(URI.class);
				it.append(" syntheticUri = "); //$NON-NLS-1$
				it.append(URI.class);
				it.append(".createURI(name + i + \".\" + getScriptFileExtension());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (resourceSet.getResource(syntheticUri, false) == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn syntheticUri;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthrow new IllegalStateException();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the script's file extension."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String getScriptFileExtension() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.fileExtension;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the resource factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(IResourceFactory.class);
				it.append(" getResourceFactory() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.resourceFactory;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the name of the foo package."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the name of the foo package."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected String getFooPackageName() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn \""); //$NON-NLS-1$
				it.append(Strings.convertToJavaString(GrammarUtil.getNamespace(getGrammar())));
				it.append(".foo\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the name of the foo type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the name of the foo type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected String getFooTypeName() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn \"FooType\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the name of the foo type member."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the name of the foo type member."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected String getFooMemberName() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn \"fooMember\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create a synthetic resource."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resourceSet the resourceSet."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the resource."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" createResource("); //$NON-NLS-1$
				it.append(ResourceSet.class);
				it.append("  resourceSet) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(URI.class);
				it.append(" uri = computeUnusedUri(resourceSet);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource = getResourceFactory().createResource(uri);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tresourceSet.getResources().add(resource);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn resource;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the injector."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the injector."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(Injector.class);
				it.append(" getInjector() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (this.builderInjector == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(ImportManager.class);
				it.append(" importManager = this.importManagerProvider.get();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.builderInjector = createOverridingInjector(this.originalInjector, new CodeBuilderModule(importManager));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn builderInjector;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create an injector that override the given injectors with the modules."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param originalInjector the original injector."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param modules the overriding modules."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the new injector."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic static "); //$NON-NLS-1$
				it.append(Injector.class);
				it.append(" createOverridingInjector("); //$NON-NLS-1$
				it.append(Injector.class);
				it.append(" originalInjector, "); //$NON-NLS-1$
				it.append(Module.class);
				it.append(" module) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(Map.class);
				it.append("<"); //$NON-NLS-1$
				it.append(Key.class);
				it.append("<?>, "); //$NON-NLS-1$
				it.append(Binding.class);
				it.append("<?>> bindings = originalInjector.getBindings();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn "); //$NON-NLS-1$
				it.append(Guice.class);
				it.append(".createInjector("); //$NON-NLS-1$
				it.append(Modules2.class);
				it.append(".mixin((binder) -> {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfor("); //$NON-NLS-1$
				it.append(Binding.class);
				it.append("<?> binding: bindings.values()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tfinal "); //$NON-NLS-1$
				it.append(Type.class);
				it.append(" typeLiteral = binding.getKey().getTypeLiteral().getType();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (typeLiteral != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tfinal String typeName = typeLiteral.getTypeName();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tif (isValid(typeName)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tbinding.applyTo(binder);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}, module));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();				
				it.append("\tprivate static boolean isValid(String name) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor (final String prefix : FORBIDDEN_INJECTION_PREFIXES) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (name.startsWith(prefix)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor (final String postfix : FORBIDDEN_INJECTION_POSTFIXES) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (name.endsWith(postfix)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies a provider for the given type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <p>The provider uses a local context singleton of the import manager."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type the type of the object to provide."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the provider."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected <T> "); //$NON-NLS-1$
				it.append(Provider.class);
				it.append("<T> getProvider(Class<T> type) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn getInjector().getProvider(type);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static class CodeBuilderModule extends "); //$NON-NLS-1$
				it.append(AbstractModule.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(ImportManager.class);
				it.append(" importManager;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic CodeBuilderModule("); //$NON-NLS-1$
				it.append(ImportManager.class);
				it.append(" importManager) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.importManager = importManager;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\t@Override"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprotected void configure() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tbind("); //$NON-NLS-1$
				it.append(ImportManager.class);
				it.append(".class).toInstance(this.importManager);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				for (StringConcatenationClient client : generateMembers()) {
					it.append(client);
				}
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess createJavaFile = getFileAccessFactory().createJavaFile(factory, content);
		createJavaFile.writeTo(getSrcGen());
	}

	/** Generate the members.
	 *
	 * @return the code.
	 */
	protected Iterable<StringConcatenationClient> generateMembers() {
		return getContributions().getContributions();
	}

	@Override
	public void generateXtendStubs() {
		super.generateXtendStubs();
		final TypeReference stub = getBuilderFactoryImplCustom();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** User-defined builder factory of the " + getLanguageName() //$NON-NLS-1$
						+ " scripts."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("class "); //$NON-NLS-1$
				it.append(stub);
				it.append(" extends "); //$NON-NLS-1$
				it.append(getBuilderFactoryImpl());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLine();
			}

		};
		final XtendFileAccess xtendFile = getFileAccessFactory().createXtendFile(stub, content);
		final IFileSystemAccess2 fileSystem = getSrc();
		if (!fileSystem.isFile(xtendFile.getPath())) {
			xtendFile.writeTo(fileSystem);
		}
	}

	@Override
	@SuppressWarnings("checkstyle:all")
	public void generateJavaStubs() {
		super.generateJavaStubs();
		final TypeReference stub = getBuilderFactoryImplCustom();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** User-defined builder factory of the " + getLanguageName() + " scripts."); //$NON-NLS-1$//$NON-NLS-2$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(stub);
				it.append(" extends "); //$NON-NLS-1$
				it.append(getBuilderFactoryImpl());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLine();
			}

		};
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(stub, content);
		IFileSystemAccess2 fileSystem = getSrc();
		if (!fileSystem.isFile(javaFile.getPath())) {
			javaFile.writeTo(fileSystem);
		}
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		final IFileSystemAccess2 fileSystem = getSrc();
		final TypeReference type;
		if ((fileSystem.isFile(getBuilderFactoryImplCustom().getJavaPath()))
				|| (fileSystem.isFile(getBuilderFactoryImplCustom().getXtendPath()))) {
			type = getBuilderFactoryImplCustom();
		} else {
			type = getBuilderFactoryImpl();
		}
		factory.addfinalTypeToType(getBuilderFactoryImpl(), type);
	}

}
