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

package io.sarl.lang.mwe2.typesystem;

import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.IGuiceAwareGeneratorComponent;
import org.eclipse.xtext.xtext.generator.XtextGeneratorNaming;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import com.google.inject.Inject;
import com.google.inject.Injector;

import io.sarl.lang.mwe2.inject.InjectionAPI;

/**
 * The configuration for the type system tools.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 * @since 0.15
 */
public class TypeSystemToolsConfig implements IGuiceAwareGeneratorComponent {

	@Inject
	private XtextGeneratorNaming naming;

	private boolean generateTypeDefaultValueProviderInterface = true;
	
	private boolean generateTypeDefaultValueProviderImpl = true;
	
	private boolean supportAtomicBoolean = true;

	private boolean supportAtomicInteger = true;
	
	private boolean supportAtomicLong = true;

	private boolean supportAtomicDouble = true;

	private boolean supportBigInteger = true;

	private boolean supportBigDecimal = true;

	private InjectionAPI injectionAPI = InjectionAPI.getDefault();

	private boolean generateDefaultValueJavaObjects = true;

	private boolean generateDefaultValueStrings = true;

	private boolean generateDefaultValueXExpressions = true;

	private String defaultValueProviderInterfaceSimpleName = "ITypeDefaultValueProvider"; //$NON-NLS-1$

	private String defaultValueProviderImplSimpleName = "TypeDefaultValueProviderImpl"; //$NON-NLS-1$

	private String defaultValueProviderImplCustomSimpleName = "TypeDefaultValueProviderImplCustom"; //$NON-NLS-1$

	@Override
	public void initialize(Injector injector) {
		injector.injectMembers(this);
	}

	/** Replies if interface of the provider of the type default values must be generated.
	 *
	 * @return {@code true} if the provider interface is generated.
	 */
	@Pure
	public boolean getGenerateTypeDefaultValueProviderInterface() {
		return this.generateTypeDefaultValueProviderInterface;
	}

	/** Change if interface of the provider of the type default values must be generated.
	 *
	 * @param enable is {@code true} if the provider interface is generated.
	 */
	public void setGenerateTypeDefaultValueProviderInterface(boolean enable) {
		this.generateTypeDefaultValueProviderInterface = enable;
	}

	/** Replies if implementation of the provider of the type default values must be generated.
	 *
	 * @return {@code true} if the provider implementation is generated.
	 */
	@Pure
	public boolean getGenerateTypeDefaultValueProviderImpl() {
		return this.generateTypeDefaultValueProviderImpl;
	}

	/** Change if implementation of the provider of the type default values must be generated.
	 *
	 * @param enable is {@code true} if the provider implementation is generated.
	 */
	public void setGenerateTypeDefaultValueProviderImpl(boolean enable) {
		this.generateTypeDefaultValueProviderImpl = enable;
	}

	/** Replies if {@code AtomicBoolean} is assimilated to a {@code boolean}.
	 *
	 * @return {@code true} if the provider assimilates {@code AtomicBoolean} to {@code boolean}.
	 */
	@Pure
	public boolean getAtomicBooleanSameAsBoolean() {
		return this.supportAtomicBoolean;
	}

	/** Change if {@code AtomicBoolean} is assimilated to a {@code boolean}.
	 *
	 * @param same {@code true} if the provider assimilates {@code AtomicBoolean} to {@code boolean}.
	 */
	public void setAtomicBooleanSameAsBoolean(boolean same) {
		this.supportAtomicBoolean = same;
	}

	/** Replies if {@code AtomicInteger} is assimilated to a {@code int}.
	 *
	 * @return {@code true} if the provider assimilates {@code AtomicInteger} to {@code int}.
	 */
	@Pure
	public boolean getAtomicIntegerSameAsInt() {
		return this.supportAtomicInteger;
	}

	/** Change if {@code AtomicInteger} is assimilated to a {@code int}.
	 *
	 * @param same {@code true} if the provider assimilates {@code AtomicInteger} to {@code int}.
	 */
	public void setAtomicIntegerSameAsInt(boolean same) {
		this.supportAtomicInteger = same;
	}

	/** Replies if {@code AtomicLong} is assimilated to a {@code long}.
	 *
	 * @return {@code true} if the provider assimilates {@code AtomicLong} to {@code long}.
	 */
	@Pure
	public boolean getAtomicLongSameAsLong() {
		return this.supportAtomicLong;
	}

	/** Change if {@code AtomicLong} is assimilated to a {@code long}.
	 *
	 * @param same {@code true} if the provider assimilates {@code AtomicLong} to {@code long}.
	 */
	public void setAtomicLongSameAsLong(boolean same) {
		this.supportAtomicLong = same;
	}

	/** Replies if {@code AtomicDouble} is assimilated to a {@code double}.
	 *
	 * @return {@code true} if the provider assimilates {@code AtomicDouble} to {@code double}.
	 */
	@Pure
	public boolean getAtomicDoubleSameAsDouble() {
		return this.supportAtomicDouble;
	}

	/** Change if {@code AtomicDouble} is assimilated to a {@code double}.
	 *
	 * @param same {@code true} if the provider assimilates {@code AtomicDouble} to {@code double}.
	 */
	public void setAtomicDoubleSameAsDouble(boolean same) {
		this.supportAtomicDouble = same;
	}

	/** Replies if {@code BigInteger} is assimilated to a {@code int}.
	 *
	 * @return {@code true} if the provider assimilates {@code BigInteger} to {@code int}.
	 */
	@Pure
	public boolean getBigIntegerSameAsInt() {
		return this.supportBigInteger;
	}

	/** Change if {@code BigInteger} is assimilated to a {@code int}.
	 *
	 * @param same {@code true} if the provider assimilates {@code BigInteger} to {@code int}.
	 */
	public void setBigIntegerSameAsInt(boolean same) {
		this.supportBigInteger = same;
	}

	/** Replies if {@code BigDecimal} is assimilated to a {@code double}.
	 *
	 * @return {@code true} if the provider assimilates {@code BigDecimal} to {@code double}.
	 */
	@Pure
	public boolean getBigDecimalSameAsDouble() {
		return this.supportBigDecimal;
	}

	/** Change if {@code BigDecimal} is assimilated to a {@code double}.
	 *
	 * @param same {@code true} if the provider assimilates {@code BigDecimal} to {@code double}.
	 */
	public void setBigDecimalSameAsDouble(boolean same) {
		this.supportBigDecimal = same;
	}

	/** Replies the injection API to be considered in the generated code.
	 *
	 * @return the injection API.
	 * @since 0.15
	 */
	public InjectionAPI getInjectionAPI() {
		return this.injectionAPI;
	}

	/** Change the injection API to be considered in the generated code.
	 *
	 * @param api the injection API. If it is {@code null}, the value replied by {@link InjectionAPI#getDeclaringClass()} is assumed.
	 * @since 0.15
	 */
	public void setInjectionAPI(InjectionAPI api) {
		if (api == null) {
			this.injectionAPI = InjectionAPI.getDefault();
		} else {
			this.injectionAPI = api;
		}
	}

	/** Replies if the types for injection must be from the Google Guice.
	 *
	 * @return {@code true} if the injection types are from Google Guice, otherwise {@code false}.
	 * @since 0.14
	 * @deprecated Replaced by {@link #getInjectionAPI()}.
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public boolean getGoogleInjectionTypes() {
		return this.injectionAPI == InjectionAPI.GOOGLE_GUICE;
	}

	/** Replies if the types for injection must be from the Google Guice.
	 *
	 * @param isGoogle {@code true} if the injection types are from Google Guice, otherwise {@code false}.
	 * @since 0.14
	 * @deprecated Replaced by {@link #setInjectionAPI(InjectionAPI)}.
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public void setGoogleInjectionTypes(boolean isGoogle) {
		this.injectionAPI = isGoogle ? InjectionAPI.GOOGLE_GUICE : InjectionAPI.getDefault();
	}

	/** Replies the type for {@code @Inject}.
	 *
	 * @return the inject annotation type.
	 * @since 0.14
	 * @deprecated Replaced by {@link InjectionAPI#getInjectType()}
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public Class<?> getInjectType() {
		return getInjectionAPI().getInjectType();
	}

	/** Replies if the functions for obtaining the Java objects of the default values should be generated.
	 *
	 * @return {@code true} for regenerating the functions replying the Java object default values.
	 */
	@Pure
	public boolean getGenerateFunctionsForDefaultValueJavaObjects() {
		return this.generateDefaultValueJavaObjects;
	}

	/** Change if the functions for obtaining the Java objects of the default values should be generated.
	 *
	 * @param enable is {@code true} for regenerating the functions replying the Java object default values.
	 */
	public void setGenerateFunctionsForDefaultValueJavaObjects(boolean enable) {
		this.generateDefaultValueJavaObjects = enable;
	}

	/** Replies if the functions for obtaining the DSL Strings of the default values should be generated.
	 *
	 * @return {@code true} for regenerating the functions replying the DSL Strings default values.
	 */
	@Pure
	public boolean getGenerateFunctionsForDefaultValueStrings() {
		return this.generateDefaultValueStrings;
	}

	/** Change if the functions for obtaining the DSL Strings of the default values should be generated.
	 *
	 * @param enable is {@code true} for regenerating the functions replying the DSL Strings default values.
	 */
	public void setGenerateFunctionsForDefaultValueStrings(boolean enable) {
		this.generateDefaultValueStrings = enable;
	}

	/** Replies if the functions for obtaining the XExpression of the default values should be generated.
	 *
	 * @return {@code true} for regenerating the functions replying the XEXpression default values.
	 */
	@Pure
	public boolean getGenerateFunctionsForDefaultValueXExpressions() {
		return this.generateDefaultValueXExpressions;
	}

	/** Change if the functions for obtaining the XExpressions of the default values should be generated.
	 *
	 * @param enable is {@code true} for regenerating the functions replying the XExpressions default values.
	 */
	public void setGenerateFunctionsForDefaultValueXExpressions(boolean enable) {
		this.generateDefaultValueXExpressions = enable;
	}

	/** Replies the type simple name of the interface for the types' default value provider.
	 *
	 * @return the type simple name.
	 */
	@Pure
	public String getDefaultValueProviderInterfaceSimpleName() {
		return this.defaultValueProviderInterfaceSimpleName;
	}

	/** Change the type simple name of the interface for the types' default value provider.
	 *
	 * @param name the type simple name.
	 */
	@Pure
	public void setDefaultValueProviderInterfaceSimpleName(String name) {
		this.defaultValueProviderInterfaceSimpleName = name;
	}

	/** Replies the type simple name of the implementation for the types' default value provider.
	 *
	 * @return the type simple name.
	 */
	@Pure
	public String getDefaultValueProviderImplSimpleName() {
		return this.defaultValueProviderImplSimpleName;
	}

	/** Change the type simple name of the implementation for the types' default value provider.
	 *
	 * @param name the type simple name.
	 */
	@Pure
	public void setDefaultValueProviderImplSimpleName(String name) {
		this.defaultValueProviderImplSimpleName = name;
	}

	/** Replies the type simple name of the custom implementation for the types' default value provider.
	 *
	 * @return the type simple name.
	 */
	@Pure
	public String getDefaultValueProviderImplCustomSimpleName() {
		return this.defaultValueProviderImplCustomSimpleName;
	}

	/** Change the type simple name of the custom implementation for the types' default value provider.
	 *
	 * @param name the type simple name.
	 */
	@Pure
	public void setDefaultValueProviderImplCustomSimpleName(String name) {
		this.defaultValueProviderImplCustomSimpleName = name;
	}

	/** Replies the base package for the language from its grammar.
	 *
	 * @param grammar the grammar of he language.
	 * @return the base package.
	 */
	@Pure
	public String getBasePackage(Grammar grammar) {
		final String basePackage = this.naming.getRuntimeBasePackage(grammar);
		return basePackage + ".services"; //$NON-NLS-1$
	}

	/** Replies the type of the interface for the types' default value provider.
	 *
	 * @param grammar the grammar of the language.
	 * @return the type.
	 */
	@Pure
	public TypeReference getDefaultValueProviderInterface(Grammar grammar) {
		return new TypeReference(getBasePackage(grammar) + "." //$NON-NLS-1$
				+ getDefaultValueProviderInterfaceSimpleName());
	}

	/** Replies the type of the implementation for the types' default value provider.
	 *
	 * @param grammar the grammar of the language.
	 * @return the type.
	 */
	@Pure
	public TypeReference getDefaultValueProviderImpl(Grammar grammar) {
		return new TypeReference(getBasePackage(grammar) + "." //$NON-NLS-1$
				+ getDefaultValueProviderImplSimpleName());
	}

	/** Replies the type of the custom implementation for the types' default value provider.
	 *
	 * @param grammar the grammar of the language.
	 * @return the type.
	 */
	@Pure
	public TypeReference getDefaultValueProviderImplCustom(Grammar grammar) {
		return new TypeReference(getBasePackage(grammar) + "." //$NON-NLS-1$
				+ getDefaultValueProviderImplCustomSimpleName());
	}

}

