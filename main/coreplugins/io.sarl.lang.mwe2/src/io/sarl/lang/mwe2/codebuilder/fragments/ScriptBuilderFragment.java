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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import javax.inject.Inject;
import javax.inject.Provider;

import com.google.inject.Injector;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.resource.DerivedStateAwareResource;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;
import org.eclipse.xtext.xtext.generator.model.XtendFileAccess;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XImportSection;
import org.eclipse.xtext.xtype.XtypeFactory;

import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;

/** Generator of the script builder types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ScriptBuilderFragment extends AbstractSubCodeBuilderFragment {

	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	/** Replies the custom implementation for the script builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getScriptBuilderImplCustom() {
		return new TypeReference(getCodeElementExtractor().getBuilderPackage() + ".ScriptBuilderImplCustom"); //$NON-NLS-1$
	}

	/** Replies the implementation for the builder of scripts.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getScriptBuilderImpl() {
		return new TypeReference(getCodeElementExtractor().getBuilderPackage() + ".ScriptBuilderImpl"); //$NON-NLS-1$
	}

	/** Replies the appender for the scripts.
	 *
	 * @return the appender
	 */
	public TypeReference getScriptAppender() {
		return getCodeElementExtractor().getElementAppenderImpl("Script"); //$NON-NLS-1$
	}

	@Override
	protected Collection<AbstractSubCodeBuilderFragment> initializeSubGenerators(Injector injector) {
		return Arrays.asList(
				injector.getInstance(ExpressionBuilderFragment.class),
				injector.getInstance(BlockExpressionBuilderFragment.class),
				injector.getInstance(FormalParameterBuilderFragment.class),
				injector.getInstance(TypeParameterBuilderFragment.class),
				injector.getInstance(TopElementBuilderFragment.class));
	}

	@Override
	public void generate() {
		generateIScriptBuilder();
		generateScriptBuilderImpl();
		generateScriptSourceAppender();
		generateBuilderFactoryContributions();
		super.generate();
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		bindTypeReferences(factory, getScriptBuilderInterface(), getScriptBuilderImpl(), getScriptBuilderImplCustom());
	}

	/** Generate the contributions for the BuildFactory.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateBuilderFactoryContributions() {
		final boolean enableAppenders = getCodeBuilderConfig().isISourceAppendableEnable();
		this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\t/** Create the factory for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resourceSet the resource set in which the script is created."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" createScript(String packageName, "); //$NON-NLS-1$
				it.append(ResourceSet.class);
				it.append(" resourceSet) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn createScript(packageName, createResource(resourceSet), null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create the factory for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resource the resource in which the script is created."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" createScript(String packageName, "); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn createScript(packageName, resource, null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create the factory for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resource the resource in which the script is created."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the context for type resolution."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" createScript(String packageName, "); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource, "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" context) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" builder = getProvider("); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(".class).get();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tbuilder.eInit(resource, packageName, context);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn builder;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create the factory for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * <p>The resource set is provided by the context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the context for type resolution."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" createScript(String packageName, "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" context) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn createScript(packageName, createResource(context.getResourceSet()), context);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
		if (enableAppenders) {
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the appender for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param resourceSet the resource set in which the script is created."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the appender."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append(" buildScript(String packageName, "); //$NON-NLS-1$
					it.append(ResourceSet.class);
					it.append(" resourceSet) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append(" a = new "); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append("(createScript(packageName, resourceSet));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tgetInjector().injectMembers(a);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn a;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Create the appender for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param resource the resource in which the script is created."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the appender."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append(" buildScript(String packageName, "); //$NON-NLS-1$
					it.append(Resource.class);
					it.append(" resource) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append(" a = new "); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append("(createScript(packageName, resource));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tgetInjector().injectMembers(a);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn a;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Create the appender for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param resource the resource in which the script is created."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param context the context for type resolution."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the appender."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append(" buildScript(String packageName, "); //$NON-NLS-1$
					it.append(Resource.class);
					it.append(" resource, "); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" context) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append(" a = new "); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append("(createScript(packageName, resource, context));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tgetInjector().injectMembers(a);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn a;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Create the appender for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t * <p>The resource set is provided by the context."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param context the context for type resolution."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the appender."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append(" buildScript(String packageName, "); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" context) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append(" a = new "); //$NON-NLS-1$
					it.append(getScriptAppender());
					it.append("(createScript(packageName, context));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tgetInjector().injectMembers(a);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn a;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
		}
	}

	@Override
	public void generateXtendStubs() {
		super.generateXtendStubs();
		final TypeReference stub = getScriptBuilderImplCustom();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** User-defined builder of the " + getLanguageName() + " scripts."); //$NON-NLS-1$//$NON-NLS-2$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("class "); //$NON-NLS-1$
				it.append(stub);
				it.append(" extends "); //$NON-NLS-1$
				it.append(getScriptBuilderImpl());
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
	public void generateJavaStubs() {
		super.generateJavaStubs();
		final TypeReference stub = getScriptBuilderImplCustom();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** User-defined builder of the " + getLanguageName() + " scripts."); //$NON-NLS-1$//$NON-NLS-2$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(stub);
				it.append(" extends "); //$NON-NLS-1$
				it.append(getScriptBuilderImpl());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLine();
			}

		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(stub, content);
		final IFileSystemAccess2 fileSystem = getSrc();
		if (!fileSystem.isFile(javaFile.getPath())) {
			javaFile.writeTo(fileSystem);
		}
	}

	/** Extract a top element from the grammar.
	 *
	 * @param description the description of the top element.
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender indicates if the generated code is for appenders.
	 * @return the top element.
	 */
	@SuppressWarnings("checkstyle:all")
	protected StringConcatenationClient generateTopElement(CodeElementExtractor.ElementDescription description,
			boolean forInterface, boolean forAppender) {
		final String topElementName = Strings.toFirstUpper(description.getName());
		final TypeReference builderType = getCodeElementExtractor().getElementBuilderInterface(topElementName);
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				if (!forInterface && !forAppender) {
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(Provider.class);
					it.append("<"); //$NON-NLS-1$
					it.append(builderType);
					it.append("> "); //$NON-NLS-1$
					it.append(Strings.toFirstLower(topElementName));
					it.append("Provider;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Create " + getAorAnArticle(topElementName) //$NON-NLS-1$
						+ " "+ topElementName + " builder.");  //$NON-NLS-1$//$NON-NLS-2$
				it.newLine();
				it.append("\t * @param name the name of the " + topElementName + "."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @return the builder."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(builderType);
				it.append(" add"); //$NON-NLS-1$
				it.append(topElementName);
				it.append("(String name)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\t return this.builder.add"); //$NON-NLS-1$
						it.append(topElementName);
						it.append("(name);"); //$NON-NLS-1$
					} else {
						it.append("\t\t"); //$NON-NLS-1$
						it.append(builderType);
						it.append(" builder = this."); //$NON-NLS-1$
						it.append(Strings.toFirstLower(topElementName));
						it.append("Provider.get();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tbuilder.eInit(getScript(), name, getTypeResolutionContext());"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn builder;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
				}
				it.newLine();
			}
		};
	}

	/** Extract top elements from the grammar.
	 *
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender indicates if the generated code is for appender.
	 * @return the top elements.
	 */
	protected List<StringConcatenationClient> generateTopElements(boolean forInterface, boolean forAppender) {
		final List<StringConcatenationClient> topElements = new ArrayList<>();
		for (final CodeElementExtractor.ElementDescription description : getCodeElementExtractor().getTopElements(
				getGrammar(), getCodeBuilderConfig())) {
			topElements.add(generateTopElement(description, forInterface, forAppender));
		}
		return topElements;
	}

	/** Generate the script builder interface.
	 */
	protected void generateIScriptBuilder() {
		final List<StringConcatenationClient> topElements = generateTopElements(true, false);
		final TypeReference builder = getScriptBuilderInterface();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of " + getLanguageName() + " scripts."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append(" *"); //$NON-NLS-1$
				it.newLine();
				it.append(" * <p>This builder is provided for helping to create " //$NON-NLS-1$
						+ getLanguageName()
						+ " Ecore elements."); //$NON-NLS-1$
				it.newLine();
				it.append(" *"); //$NON-NLS-1$
				it.newLine();
				it.append(" * <p>Do not forget to invoke {@link #finalizeScript()} for creating imports, etc."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public interface "); //$NON-NLS-1$
				it.append(builder.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateFieldsAndMethods(true, false));
				for (final StringConcatenationClient element : topElements) {
					it.append(element);
				}
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the script appender.
	 */
	protected void generateScriptSourceAppender() {
		final List<StringConcatenationClient> topElements = generateTopElements(false, true);
		final TypeReference appender = getCodeElementExtractor().getElementAppenderImpl("Script"); //$NON-NLS-1$
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Appender of " + getLanguageName() + " scripts."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append(" *"); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(getScriptAppender().getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(getCodeElementExtractor().getAbstractAppenderImpl());
				it.append(" implements "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateFieldsAndMethods(false, true));
				for (final StringConcatenationClient element : topElements) {
					it.append(element);
				}
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(appender, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the script builder default implementation.
	 */
	protected void generateScriptBuilderImpl() {
		final List<StringConcatenationClient> topElements = generateTopElements(false, false);
		final TypeReference script = getScriptBuilderImpl();
		final TypeReference scriptInterface = getScriptBuilderInterface();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(script.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(getAbstractBuilderImpl());
				it.append(" implements "); //$NON-NLS-1$
				it.append(scriptInterface);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateFieldsAndMethods(false, false));
				for (final StringConcatenationClient element : topElements) {
					it.append(element);
				}
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
			}

		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(script, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the fields and the methods.
	 *
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender indicates if the generated code is for appender.
	 * @return the fields and methods.
	 */
	@SuppressWarnings("checkstyle:all")
	protected StringConcatenationClient generateFieldsAndMethods(boolean forInterface, boolean forAppender) {
		TypeReference scriptInterface = getCodeElementExtractor().getLanguageScriptInterface();
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				// Fields
				if (!forInterface && !forAppender) {
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(scriptInterface);
					it.append(" script;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate boolean isFinalized;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				} else if (forAppender) {
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(getScriptBuilderInterface());
					it.append(" builder;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (forInterface || forAppender) {
					it.append("\t/** Find the reference to the type with the given name."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param typeName the fully qualified name of the type"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the type reference."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmParameterizedTypeReference.class);
					it.append(" newTypeRef(String typeName)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn this.builder.newTypeRef(typeName);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Find the reference to the type with the given name."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param context the context for the type reference use"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param typeName the fully qualified name of the type"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the type reference."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmParameterizedTypeReference.class);
					it.append(" newTypeRef("); //$NON-NLS-1$
					it.append(Notifier.class);
					it.append(" context, String typeName)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn this.builder.newTypeRef(context, typeName);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (forInterface) {
					it.append("\t/** Replies the context for type resolution."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the context or {@code null} if the Ecore object is the context."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" getTypeResolutionContext();"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				} else if (forAppender) {
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" getTypeResolutionContext() {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn this.builder.getTypeResolutionContext();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (forAppender) {
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(getScriptAppender().getSimpleName());
					it.append("("); //$NON-NLS-1$
					it.append(getScriptBuilderInterface());
					it.append(" builder) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tthis.builder = builder;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tpublic void build("); //$NON-NLS-1$
					it.append(ISourceAppender.class);
					it.append(" appender) throws "); //$NON-NLS-1$
					it.append(IOException.class);
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tif (!isFinalized()) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tfinalizeScript();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tbuild(this.builder.getScript(), appender);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Create the internal " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource, String packageName, "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" context)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(resource, packageName, context);"); //$NON-NLS-1$
					} else {
						it.append("\t\tsetTypeResolutionContext(context);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (this.script == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.script = "); //$NON-NLS-1$
						it.append(getXFactoryFor(scriptInterface));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(scriptInterface.getSimpleName()));
						it.append("();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t"); //$NON-NLS-1$
						it.append(EList.class);
						it.append("<"); //$NON-NLS-1$
						it.append(EObject.class);
						it.append("> content = resource.getContents();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tif (!content.isEmpty()) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tcontent.clear();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcontent.add(this.script);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tif (!"); //$NON-NLS-1$
						it.append(Strings.class);
						it.append(".isEmpty(packageName)) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tscript.setPackage(packageName);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(scriptInterface);
				it.append(" getScript()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.getScript();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this.script;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource to which the script is attached."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(Resource.class);
				it.append(" eResource()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn getScript().eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Finalize the script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <p>The finalization includes: <ul>"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <li>The import section is created.</li>"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * </ul>"); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void finalizeScript()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.finalizeScript();"); //$NON-NLS-1$
					} else {
						it.append("\t\tif (this.isFinalized) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthrow new "); //$NON-NLS-1$
						it.append(IllegalStateException.class);
						it.append("(\"already finalized\");"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tthis.isFinalized = true;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(ImportManager.class);
						it.append(" concreteImports = new "); //$NON-NLS-1$
						it.append(ImportManager.class);
						it.append("(true);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(XImportSection.class);
						it.append(" importSection = getScript().getImportSection();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (importSection != null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tfor ("); //$NON-NLS-1$
						it.append(XImportDeclaration.class);
						it.append(" decl : importSection.getImportDeclarations()) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tconcreteImports.addImportFor(decl.getImportedType());"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tfor (String importName : getImportManager().getImports()) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t"); //$NON-NLS-1$
						it.append(JvmType.class);
						it.append(" type = findType(getScript(), importName).getType();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tif (concreteImports.addImportFor(type) && type instanceof "); //$NON-NLS-1$
						it.append(JvmDeclaredType.class);
						it.append(") {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t"); //$NON-NLS-1$
						it.append(XImportDeclaration.class);
						it.append(" declaration = "); //$NON-NLS-1$
						it.append(XtypeFactory.class);
						it.append(".eINSTANCE.createXImportDeclaration();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdeclaration.setImportedType(("); //$NON-NLS-1$
						it.append(JvmDeclaredType.class);
						it.append(") type);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tif (importSection == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\timportSection = "); //$NON-NLS-1$
						it.append(XtypeFactory.class);
						it.append(".eINSTANCE.createXImportSection();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\tgetScript().setImportSection(importSection);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\timportSection.getImportDeclarations().add(declaration);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(Resource.class);
						it.append(" resource = getScript().eResource();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (resource instanceof "); //$NON-NLS-1$
						it.append(DerivedStateAwareResource.class);
						it.append(") {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t(("); //$NON-NLS-1$
						it.append(DerivedStateAwareResource.class);
						it.append(") resource).discardDerivedState();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies if the script was finalized."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("boolean isFinalized()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.isFinalized();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this.isFinalized;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				if (!forInterface) {
					it.append("\t@"); //$NON-NLS-1$
					it.append(Override.class);
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(String.class);
					it.append(" toString() {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn "); //$NON-NLS-1$
					if (forAppender) {
						it.append("this.builder.toString();"); //$NON-NLS-1$
					} else {
						it.append(EmfFormatter.class);
						it.append(".objToStr(this.script);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			}
		};
	}

}
