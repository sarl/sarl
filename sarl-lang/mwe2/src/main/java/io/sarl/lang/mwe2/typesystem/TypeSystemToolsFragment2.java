/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.mwe2.typesystem;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.log4j.Logger;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtend2.lib.StringConcatenationClient.TargetStringConcatenation;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.model.FileAccessFactory;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import com.google.common.util.concurrent.AtomicDouble;
import com.google.inject.Inject;
import com.google.inject.Injector;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create tools for type system.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class TypeSystemToolsFragment2 extends AbstractXtextGeneratorFragment {

	private static final Logger LOG = Logger.getLogger(TypeSystemToolsFragment2.class);

	@Inject
	private FileAccessFactory fileAccessFactory;

	@Inject
	private TypeSystemToolsConfig configuration;

	/** Replies the type of the tool that permits to access to the grammar keywords.
	 *
	 * @return the accessor type.
	 */
	public TypeReference getLanguageKeywordAccessor() {
		final var grammar = getGrammar();
		return new TypeReference(getBasePackage() + "." //$NON-NLS-1$
				+ GrammarUtil.getSimpleName(grammar).toUpperCase() + "GrammarKeywordAccess"); //$NON-NLS-1$
	}

	/** Replies the type for {@code @Inject}.
	 *
	 * @return the inject annotation type.
	 */
	protected Class<?> getInjectType() {
		return this.configuration.getInjectionAPI().getInjectType();
	}

	/** Replies the language name.
	 *
	 * @return the language name.
	 */
	@Pure
	public String getLanguageName() {
		return Strings.toFirstUpper(GrammarUtil.getSimpleName(getGrammar()).toLowerCase());
	}

	/** Replies the base package for the language.
	 *
	 * @return the base package.
	 */
	@Pure
	public String getBasePackage() {
		return this.configuration.getBasePackage(getGrammar());
	}
	
	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
	}

	/** Create the runtime bindings for the builders.
	 *
	 * @return the bindings.
	 */
	protected BindingFactory createRuntimeBindings() {
		final var factory = new BindingFactory(getClass().getName());
		bindTypeReferences(factory,
				getTypeDefaultValueProviderInterface(),
				getTypeDefaultValueProviderImpl(),
				getTypeDefaultValueProviderImplCustom());
		return factory;
	}

	/** Create the Eclipse bindings for the builders.
	 *
	 * @return the bindings.
	 */
	protected BindingFactory createEclipseBindings() {
		final var factory = new BindingFactory(getClass().getName());
		return factory;
	}

	/** Create the IDEA bindings for the builders.
	 *
	 * @return the bindings.
	 */
	protected BindingFactory createIdeaBindings() {
		final var factory = new BindingFactory(getClass().getName());
		return factory;
	}

	/** Create the Web-interface bindings for the builders.
	 *
	 * @return the bindings.
	 */
	protected BindingFactory createWebBindings() {
		final var factory = new BindingFactory(getClass().getName());
		return factory;
	}

	/** Replies the type of the interface for the types' default value provider.
	 *
	 * @return the type.
	 */
	@Pure
	public TypeReference getTypeDefaultValueProviderInterface() {
		return this.configuration.getDefaultValueProviderInterface(getGrammar());
	}

	/** Replies the type of the implementation for the types' default value provider.
	 *
	 * @return the type.
	 */
	@Pure
	public TypeReference getTypeDefaultValueProviderImpl() {
		return this.configuration.getDefaultValueProviderImpl(getGrammar());
	}

	/** Replies the type of the custom implementation for the types' default value provider.
	 *
	 * @return the type.
	 */
	@Pure
	public TypeReference getTypeDefaultValueProviderImplCustom() {
		return this.configuration.getDefaultValueProviderImplCustom(getGrammar());
	}

	@Override
	public void generate() {
		LOG.info("Generating the type system tools for " + getLanguageName()); //$NON-NLS-1$

		if (this.configuration.getGenerateTypeDefaultValueProviderInterface()) {
			final var javaFile = this.fileAccessFactory.createJavaFile(getTypeDefaultValueProviderInterface(), new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					generateTypeDefaultValueProvider(it, getTypeDefaultValueProviderInterface(), null);
				}
			});
			javaFile.writeTo(getProjectConfig().getRuntime().getSrcGen());
		}

		if (this.configuration.getGenerateTypeDefaultValueProviderImpl()) {
			final var javaFile = this.fileAccessFactory.createJavaFile(getTypeDefaultValueProviderImpl(), new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					generateTypeDefaultValueProvider(it, getTypeDefaultValueProviderImpl(), getTypeDefaultValueProviderInterface());
				}
			});
			javaFile.writeTo(getProjectConfig().getRuntime().getSrcGen());
		}

		createRuntimeBindings().contributeTo(getLanguage().getRuntimeGenModule());
		createEclipseBindings().contributeTo(getLanguage().getEclipsePluginGenModule());
		createIdeaBindings().contributeTo(getLanguage().getIdeGenModule());
		createWebBindings().contributeTo(getLanguage().getWebGenModule());

		final var exportedPackages = getProjectConfig().getRuntime().getManifest().getExportedPackages();
		if (exportedPackages != null) {
			exportedPackages.add(getBasePackage());
		}
	}

	/** Binds the given references according to the standard policy.
	 *
	 * <p>If an custom implementation is defined, it is binded to. Otherwise, the default implementation
	 * is binded.
	 *
	 * @param factory the binding factory to use for creating the bindings.
	 * @param interfaceType the type to bind to an implementation type.
	 * @param implementationType the implementation to bind to the interface type.
	 * @param customImplementationType the custom implementation to bind to the interface type.
	 */
	protected void bindTypeReferences(BindingFactory factory, TypeReference interfaceType,
			TypeReference implementationType, TypeReference customImplementationType) {
		final var fileSystem = getProjectConfig().getRuntime().getSrc();
		final TypeReference type;
		if ((fileSystem.isFile(implementationType.getJavaPath()))
				|| (fileSystem.isFile(customImplementationType.getXtendPath()))) {
			type = customImplementationType;
		} else {
			type = implementationType;
		}
		factory.addfinalTypeToType(interfaceType, type);
	}

	/** Generate the code for the provider of the type's default values.
	 *
	 * @param it the receiver.
	 * @param type the type to be generated.
	 * @param implemented the type that is implemented.
	 * @param isInterface indicates if the code is for an interface or not.
	 */
	protected void generateTypeDefaultValueProvider(TargetStringConcatenation it, TypeReference type, TypeReference implemented) {
		final boolean isInterface = implemented == null;

		it.append("/** Replies the default value that is associated to a {@code JvmType}."); //$NON-NLS-1$
		it.newLine();
		it.append(" */"); //$NON-NLS-1$
		it.newLine();
		it.append("@"); //$NON-NLS-1$
		it.append(SuppressWarnings.class);
		it.append("(\"all\")"); //$NON-NLS-1$
		it.newLine();
		it.append("public "); //$NON-NLS-1$
		if (isInterface) {
			it.append("interface"); //$NON-NLS-1$
		} else {
			it.append("class"); //$NON-NLS-1$
		}
		it.append(" "); //$NON-NLS-1$
		it.append(type.getSimpleName());
		if (!isInterface) {
			it.append(" implements "); //$NON-NLS-1$
			it.append(implemented);
		}
		it.append(" {"); //$NON-NLS-1$
		if (!isInterface) {
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\tprivate "); //$NON-NLS-1$
			it.append(TypeReferences.class);
			it.append(" typeReferences;"); //$NON-NLS-1$
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\tprivate "); //$NON-NLS-1$
			it.append(getLanguageKeywordAccessor());
			it.append(" keywords;"); //$NON-NLS-1$
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\t@"); //$NON-NLS-1$
			it.append(getInjectType());
			it.newLine();
			it.append("\tpublic void setTypeReferences("); //$NON-NLS-1$
			it.append(TypeReferences.class);
			it.append(" typeReferences) {"); //$NON-NLS-1$
			it.newLine();
			it.append("\t\tthis.typeReferences = typeReferences;"); //$NON-NLS-1$
			it.newLine();
			it.append("\t}"); //$NON-NLS-1$
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\t@"); //$NON-NLS-1$
			it.append(getInjectType());
			it.newLine();
			it.append("\tpublic void setKeywords("); //$NON-NLS-1$
			it.append(getLanguageKeywordAccessor());
			it.append(" keywords) {"); //$NON-NLS-1$
			it.newLine();
			it.append("\t\tthis.keywords = keywords;"); //$NON-NLS-1$
			it.newLine();
			it.append("\t}"); //$NON-NLS-1$
		}
		if (this.configuration.getGenerateFunctionsForDefaultValueJavaObjects()) {
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\t/** Replies the default value that is associated to the given type."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @param type the type to consider."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @return the default value"); //$NON-NLS-1$
			it.newLine();
			it.append("\t */"); //$NON-NLS-1$
			it.newLine();
			it.append("\t@"); //$NON-NLS-1$
			it.append(Pure.class);
			it.newLine();
			it.append("\t"); //$NON-NLS-1$
			if (!isInterface) {
				it.append("public "); //$NON-NLS-1$
			}
			it.append("Object getDefaultValue("); //$NON-NLS-1$
			it.append(JvmType.class);
			it.append(" type)"); //$NON-NLS-1$
			if (isInterface) {
				it.append(";"); //$NON-NLS-1$
			} else {
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (type == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn getDefaultValue(type.getQualifiedName());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t};"); //$NON-NLS-1$
			}
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\t/** Replies the default value that is associated to the given type."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @param typeName the name of the type to consider."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @return the default value"); //$NON-NLS-1$
			it.newLine();
			it.append("\t */"); //$NON-NLS-1$
			it.newLine();
			it.append("\t@"); //$NON-NLS-1$
			it.append(Pure.class);
			it.newLine();
			it.append("\t"); //$NON-NLS-1$
			if (!isInterface) {
				it.append("public "); //$NON-NLS-1$
			}
			it.append("Object getDefaultValue("); //$NON-NLS-1$
			it.append(String.class);
			it.append(" typeName)"); //$NON-NLS-1$
			if (isInterface) {
				it.append(";"); //$NON-NLS-1$
			} else {
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(Object.class);
				it.append(" defaultValue = null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(typeName) && !\""); //$NON-NLS-1$
				it.append(void.class.getName());
				it.append("\".equals(typeName)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tswitch (typeName) {"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(boolean.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Boolean.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicBooleanSameAsBoolean()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicBoolean.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\tdefaultValue = "); //$NON-NLS-1$
				it.append(Boolean.class);
				it.append(".FALSE;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(double.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Double.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicDoubleSameAsDouble()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicDouble.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				if (this.configuration.getBigDecimalSameAsDouble()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(BigDecimal.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\tdefaultValue = "); //$NON-NLS-1$
				it.append(Double.class);
				it.append(".valueOf(0.);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(float.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Float.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = "); //$NON-NLS-1$
				it.append(Float.class);
				it.append(".valueOf(0f);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(int.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Integer.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicIntegerSameAsInt()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicInteger.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				if (this.configuration.getBigIntegerSameAsInt()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(BigInteger.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\tdefaultValue = "); //$NON-NLS-1$
				it.append(Integer.class);
				it.append(".valueOf(0);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(long.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Long.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicLongSameAsLong()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicLong.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\tdefaultValue = "); //$NON-NLS-1$
				it.append(Long.class);
				it.append(".valueOf(0);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(byte.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Byte.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = "); //$NON-NLS-1$
				it.append(Byte.class);
				it.append(".valueOf((byte) 0);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(short.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Short.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = "); //$NON-NLS-1$
				it.append(Short.class);
				it.append(".valueOf((short) 0);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(char.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Character.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = "); //$NON-NLS-1$
				it.append(Character.class);
				it.append(".valueOf((char) 0);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tdefault:"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn defaultValue;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t};"); //$NON-NLS-1$
			}
		}
		if (this.configuration.getGenerateFunctionsForDefaultValueStrings()) {
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\t/** Replies the default value in "); //$NON-NLS-1$
			it.append(getLanguageName());
			it.append(" syntax and that is associated to the given type."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @param type the type to consider."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @return the default value in "); //$NON-NLS-1$
			it.append(getLanguageName());
			it.append(" syntax"); //$NON-NLS-1$
			it.newLine();
			it.append("\t */"); //$NON-NLS-1$
			it.newLine();
			it.append("\t@"); //$NON-NLS-1$
			it.append(Pure.class);
			it.newLine();
			it.append("\t"); //$NON-NLS-1$
			if (!isInterface) {
				it.append("public "); //$NON-NLS-1$
			}
			it.append("String getDefaultValueIn"); //$NON-NLS-1$
			it.append(getLanguageName());
			it.append("Syntax("); //$NON-NLS-1$
			it.append(JvmType.class);
			it.append(" type)"); //$NON-NLS-1$
			if (isInterface) {
				it.append(";"); //$NON-NLS-1$
			} else {
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (type == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn getDefaultValueIn"); //$NON-NLS-1$
				it.append(getLanguageName());
				it.append("Syntax(type.getQualifiedName());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
			}
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\t/** Replies the default value in "); //$NON-NLS-1$
			it.append(getLanguageName());
			it.append(" syntax and that is associated to the given type."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @param typeName the name of the type to consider."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @return the default value in "); //$NON-NLS-1$
			it.append(getLanguageName());
			it.append(" syntax"); //$NON-NLS-1$
			it.newLine();
			it.append("\t */"); //$NON-NLS-1$
			it.newLine();
			it.append("\t@"); //$NON-NLS-1$
			it.append(Pure.class);
			it.newLine();
			it.append("\t"); //$NON-NLS-1$
			if (!isInterface) {
				it.append("public "); //$NON-NLS-1$
			}
			it.append("String getDefaultValueIn"); //$NON-NLS-1$
			it.append(getLanguageName());
			it.append("Syntax("); //$NON-NLS-1$
			it.append(String.class);
			it.append(" typeName)"); //$NON-NLS-1$
			if (isInterface) {
				it.append(";"); //$NON-NLS-1$
			} else {
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(String.class);
				it.append(" defaultValue = \"\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(typeName) && !\""); //$NON-NLS-1$
				it.append(void.class.getName());
				it.append("\".equals(typeName)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tswitch (typeName) {"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(boolean.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Boolean.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicBooleanSameAsBoolean()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicBoolean.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\tdefaultValue = \"false\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(double.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Double.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicDoubleSameAsDouble()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicDouble.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				if (this.configuration.getBigDecimalSameAsDouble()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(BigDecimal.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\tdefaultValue = \"0.0\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(float.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Float.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = \"0.0f\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(int.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Integer.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(long.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Long.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicIntegerSameAsInt()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicInteger.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				if (this.configuration.getAtomicLongSameAsLong()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicLong.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				if (this.configuration.getBigIntegerSameAsInt()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(BigInteger.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\tdefaultValue = \"0\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(byte.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Byte.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = \"(0 \" + this.keywords.getAsKeyword() + \" "); //$NON-NLS-1$
				it.append(byte.class.getName());
				it.append(")\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(short.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Short.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = \"(0 \" + this.keywords.getAsKeyword() + \" "); //$NON-NLS-1$
				it.append(short.class.getName());
				it.append(")\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(char.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Character.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = \"(0 \" + this.keywords.getAsKeyword() + \" "); //$NON-NLS-1$
				it.append(char.class.getName());
				it.append(")\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tdefault:"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdefaultValue = \"null\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn defaultValue;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
			}
		}
		if (this.configuration.getGenerateFunctionsForDefaultValueXExpressions()) {
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\t/** Replies the XExpression for the default value that is associated to the given type."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @param type the type to consider."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @param notifier the context from whic a type must be loaded if needed."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @return the XExpression for the default value"); //$NON-NLS-1$
			it.newLine();
			it.append("\t */"); //$NON-NLS-1$
			it.newLine();
			it.append("\t@"); //$NON-NLS-1$
			it.append(Pure.class);
			it.newLine();
			it.append("\t"); //$NON-NLS-1$
			if (!isInterface) {
				it.append("public "); //$NON-NLS-1$
			}
			it.append(XExpression.class);
			it.append(" getDefaultValueXExpression("); //$NON-NLS-1$
			it.append(JvmType.class);
			it.append(" type, "); //$NON-NLS-1$
			it.append(Notifier.class);
			it.append(" notifier)"); //$NON-NLS-1$
			if (isInterface) {
				it.append(";"); //$NON-NLS-1$
			} else {
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (type == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn getDefaultValueXExpression(type.getQualifiedName(), notifier);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
			}
			it.newLineIfNotEmpty();
			it.newLine();
			it.append("\t/** Replies the XExpression for the default value that is associated to the given type."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @param typeName the type to consider."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @param notifier the context from whic a type must be loaded if needed."); //$NON-NLS-1$
			it.newLine();
			it.append("\t * @return the XExpression for the default value"); //$NON-NLS-1$
			it.newLine();
			it.append("\t */"); //$NON-NLS-1$
			it.newLine();
			it.append("\t@"); //$NON-NLS-1$
			it.append(Pure.class);
			it.newLine();
			it.append("\t"); //$NON-NLS-1$
			if (!isInterface) {
				it.append("public "); //$NON-NLS-1$
			}
			it.append(XExpression.class);
			it.append(" getDefaultValueXExpression("); //$NON-NLS-1$
			it.append(String.class);
			it.append(" typeName, "); //$NON-NLS-1$
			it.append(Notifier.class);
			it.append(" notifier)"); //$NON-NLS-1$
			if (isInterface) {
				it.append(";"); //$NON-NLS-1$
			} else {
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t"); //$NON-NLS-1$
				it.append(XExpression.class);
				it.append(" expr = null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (typeName != null && !\""); //$NON-NLS-1$
				it.append(void.class.getName());
				it.append("\".equals(typeName) && !"); //$NON-NLS-1$
				it.append(Void.class);
				it.append(".class.getName().equals(typeName)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tswitch (typeName) {"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(boolean.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Boolean.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicBooleanSameAsBoolean()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicBoolean.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(XBooleanLiteral.class);
				it.append(" booleanLiteral = "); //$NON-NLS-1$
				it.append(XbaseFactory.class);
				it.append(".eINSTANCE.createXBooleanLiteral();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbooleanLiteral.setIsTrue(false);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\texpr = booleanLiteral;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(float.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Float.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(XNumberLiteral.class);
				it.append(" floatLiteral = "); //$NON-NLS-1$
				it.append(XbaseFactory.class);
				it.append(".eINSTANCE.createXNumberLiteral();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tfloatLiteral.setValue(\"0.0f\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\texpr = floatLiteral;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(double.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Double.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicDoubleSameAsDouble()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicDouble.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				if (this.configuration.getBigDecimalSameAsDouble()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(BigDecimal.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(XNumberLiteral.class);
				it.append(" doubleLiteral = "); //$NON-NLS-1$
				it.append(XbaseFactory.class);
				it.append(".eINSTANCE.createXNumberLiteral();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdoubleLiteral.setValue(\"0.0\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\texpr = doubleLiteral;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(int.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Integer.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(long.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Long.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				if (this.configuration.getAtomicIntegerSameAsInt()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicInteger.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				if (this.configuration.getAtomicLongSameAsLong()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(AtomicLong.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				if (this.configuration.getBigIntegerSameAsInt()) {
					it.append("\t\t\tcase \""); //$NON-NLS-1$
					it.append(BigInteger.class.getName());
					it.append("\":"); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(XNumberLiteral.class);
				it.append(" intLiteral = "); //$NON-NLS-1$
				it.append(XbaseFactory.class);
				it.append(".eINSTANCE.createXNumberLiteral();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tintLiteral.setValue(\"0\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\texpr = intLiteral;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
	
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(byte.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Byte.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\texpr = createCastExpression(\""); //$NON-NLS-1$
				it.append(byte.class.getName());
				it.append("\", notifier);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();

				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(short.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Short.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\texpr = createCastExpression(\""); //$NON-NLS-1$
				it.append(short.class.getName());
				it.append("\", notifier);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();

				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(char.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcase \""); //$NON-NLS-1$
				it.append(Character.class.getName());
				it.append("\":"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\texpr = createCastExpression(\""); //$NON-NLS-1$
				it.append(char.class.getName());
				it.append("\", notifier);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();

				it.append("\t\t\tdefault:"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\texpr = "); //$NON-NLS-1$
				it.append(XbaseFactory.class);
				it.append(".eINSTANCE.createXNullLiteral();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn expr;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
			}
			if (!isInterface) {
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(XExpression.class);
				it.append(" createCastExpression("); //$NON-NLS-1$
				it.append(String.class);
				it.append(" targetTypeName, "); //$NON-NLS-1$
				it.append(Notifier.class);
				it.append(" notifier) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(XNumberLiteral.class);
				it.append(" cnumberLiteral = "); //$NON-NLS-1$
				it.append(XbaseFactory.class);
				it.append(".eINSTANCE.createXNumberLiteral();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tcnumberLiteral.setValue(\"0\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(XCastedExpression.class);
				it.append(" castExpression = "); //$NON-NLS-1$
				it.append(XbaseFactory.class);
				it.append(".eINSTANCE.createXCastedExpression();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tcastExpression.setTarget(cnumberLiteral);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tcastExpression.setType(this.typeReferences.getTypeForName(targetTypeName, notifier));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn castExpression;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
			}
		}
		it.newLineIfNotEmpty();
		it.newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine();
	}

}
