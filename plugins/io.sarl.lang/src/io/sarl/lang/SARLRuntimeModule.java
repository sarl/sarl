/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang;

import com.google.inject.Binder;
import com.google.inject.Singleton;
import com.google.inject.name.Names;
import org.eclipse.xtend.core.compiler.UnicodeAwarePostProcessor;
import org.eclipse.xtend.core.compiler.XtendCompiler;
import org.eclipse.xtend.core.compiler.XtendGenerator;
import org.eclipse.xtend.core.conversion.IntUnderscoreValueConverter;
import org.eclipse.xtend.core.conversion.JavaIDValueConverter;
import org.eclipse.xtend.core.conversion.StringValueConverter;
import org.eclipse.xtend.core.conversion.XtendValueConverterService;
import org.eclipse.xtend.core.documentation.XtendDocumentationProvider;
import org.eclipse.xtend.core.documentation.XtendFileHeaderProvider;
import org.eclipse.xtend.core.imports.XtendImportedTypesUsageCollector;
import org.eclipse.xtend.core.imports.XtendImportsConfiguration;
import org.eclipse.xtend.core.imports.XtendTypeUsageCollector;
import org.eclipse.xtend.core.linking.RuntimeLinker;
import org.eclipse.xtend.core.linking.XtendEObjectAtOffsetHelper;
import org.eclipse.xtend.core.linking.XtendLinkingDiagnosticMessageProvider;
import org.eclipse.xtend.core.macro.declaration.IResourceChangeRegistry;
import org.eclipse.xtend.core.macro.declaration.NopResourceChangeRegistry;
import org.eclipse.xtend.core.naming.XtendQualifiedNameProvider;
import org.eclipse.xtend.core.parser.XtendPartialParsingHelper;
import org.eclipse.xtend.core.resource.XtendLocationInFileProvider;
import org.eclipse.xtend.core.resource.XtendResourceDescriptionManager;
import org.eclipse.xtend.core.resource.XtendResourceDescriptionStrategy;
import org.eclipse.xtend.core.scoping.AnonymousClassConstructorScopes;
import org.eclipse.xtend.core.scoping.XtendImportedNamespaceScopeProvider;
import org.eclipse.xtend.core.serializer.XtendSerializerScopeProvider;
import org.eclipse.xtend.core.typesystem.LocalClassAwareTypeNames;
import org.eclipse.xtend.core.typesystem.TypeDeclarationAwareBatchTypeResolver;
import org.eclipse.xtend.core.typesystem.XtendReentrantTypeResolver;
import org.eclipse.xtend.core.typesystem.XtendTypeComputer;
import org.eclipse.xtend.core.validation.CachingResourceValidatorImpl;
import org.eclipse.xtend.core.validation.XtendImplicitReturnFinder;
import org.eclipse.xtend.core.xtend.XtendFactory;
import org.eclipse.xtext.conversion.IValueConverterService;
import org.eclipse.xtext.conversion.impl.IDValueConverter;
import org.eclipse.xtext.conversion.impl.STRINGValueConverter;
import org.eclipse.xtext.documentation.IEObjectDocumentationProvider;
import org.eclipse.xtext.documentation.IFileHeaderProvider;
import org.eclipse.xtext.findReferences.ReferenceFinder;
import org.eclipse.xtext.findReferences.TargetURICollector;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;
import org.eclipse.xtext.linking.ILinker;
import org.eclipse.xtext.linking.ILinkingDiagnosticMessageProvider;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.parser.antlr.IPartialParsingHelper;
import org.eclipse.xtext.resource.EObjectAtOffsetHelper;
import org.eclipse.xtext.resource.IDefaultResourceDescriptionStrategy;
import org.eclipse.xtext.resource.ILocationInFileProvider;
import org.eclipse.xtext.resource.IResourceDescription.Manager;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.impl.EagerResourceSetBasedResourceDescriptions;
import org.eclipse.xtext.resource.persistence.IResourceStorageFacade;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider;
import org.eclipse.xtext.serializer.acceptor.ISyntacticSequenceAcceptor;
import org.eclipse.xtext.serializer.analysis.IContextPDAProvider;
import org.eclipse.xtext.serializer.sequencer.IHiddenTokenSequencer;
import org.eclipse.xtext.serializer.tokens.SerializerScopeProviderBinding;
import org.eclipse.xtext.validation.CompositeEValidator;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;
import org.eclipse.xtext.validation.IResourceValidator;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.compiler.JvmModelGenerator;
import org.eclipse.xtext.xbase.compiler.XbaseCompiler;
import org.eclipse.xtext.xbase.compiler.output.TraceAwarePostProcessor;
import org.eclipse.xtext.xbase.controlflow.IEarlyExitComputer;
import org.eclipse.xtext.xbase.conversion.XbaseValueConverterService;
import org.eclipse.xtext.xbase.file.AbstractFileSystemSupport;
import org.eclipse.xtext.xbase.file.JavaIOFileSystemSupport;
import org.eclipse.xtext.xbase.imports.IImportsConfiguration;
import org.eclipse.xtext.xbase.imports.ImportedTypesCollector;
import org.eclipse.xtext.xbase.imports.TypeUsageCollector;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelInferrer;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelTargetURICollector;
import org.eclipse.xtext.xbase.resource.BatchLinkableResourceStorageFacade;
import org.eclipse.xtext.xbase.scoping.batch.ConstructorScopes;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputer;
import org.eclipse.xtext.xbase.typesystem.internal.CachingBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.internal.DefaultBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.internal.DefaultReentrantTypeResolver;
import org.eclipse.xtext.xbase.typesystem.util.ExtendedEarlyExitComputer;
import org.eclipse.xtext.xbase.typesystem.util.HumanReadableTypeNames;
import org.eclipse.xtext.xbase.util.XExpressionHelper;
import org.eclipse.xtext.xbase.validation.EarlyExitValidator;
import org.eclipse.xtext.xbase.validation.FeatureNameValidator;
import org.eclipse.xtext.xbase.validation.ImplicitReturnFinder;

import io.sarl.lang.actionprototype.ActionPrototypeProvider;
import io.sarl.lang.actionprototype.DefaultActionPrototypeProvider;
import io.sarl.lang.bugfixes.SARLContextPDAProvider;
import io.sarl.lang.compiler.SarlOutputConfigurationProvider;
import io.sarl.lang.controlflow.SARLEarlyExitComputer;
import io.sarl.lang.controlflow.SARLExtendedEarlyExitComputer;
import io.sarl.lang.ecoregenerator.helper.SARLHiddenTokenSequencer;
import io.sarl.lang.findreferences.SARLReferenceFinder;
import io.sarl.lang.jvmmodel.SARLJvmModelInferrer;
import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlFactory;
import io.sarl.lang.scoping.batch.SARLImplicitlyImportedFeatures;
import io.sarl.lang.typing.SARLExpressionHelper;
import io.sarl.lang.validation.DefaultFeatureCallValidator;
import io.sarl.lang.validation.FeatureCallValidator;
import io.sarl.lang.validation.SARLConfigurableIssueCodesProvider;
import io.sarl.lang.validation.SARLEarlyExitValidator;
import io.sarl.lang.validation.SARLFeatureNameValidator;

/**
 * Use this class to register components to be used at runtime / without the
 * Equinox extension registry.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({"static-method", "checkstyle:methodcount"})
public class SARLRuntimeModule extends io.sarl.lang.AbstractSARLRuntimeModule {

	/** Replies the resolver of types that uses caching data.
	 * @return the resolver.
	 */
	public Class<? extends CachingBatchTypeResolver> bindCachingBatchTypeResolver() {
		return CachingBatchTypeResolver.class;
	}

	@Override
	public Class<? extends IPartialParsingHelper> bindIPartialParserHelper() {
		return XtendPartialParsingHelper.class;
	}

	/** Replies the finder of type references used by the SARL tools.
	 * @return the finder.
	 */
	public Class<? extends ReferenceFinder> bindReferenceFinder() {
		return SARLReferenceFinder.class;
	}

	@Override
	public void configure(Binder binder) {
		super.configure(binder);
		binder.bind(boolean.class).annotatedWith(Names.named(CompositeEValidator.USE_EOBJECT_VALIDATOR))
				.toInstance(false);
	}

	/** Replies the Xbase factory.
	 * @return the Xbase factory.
	 */
	@Singleton
	public XbaseFactory bindXbaseFactory() {
		return XbaseFactory.eINSTANCE;
	}

	/** Replies the Xtend factory.
	 * @return the Xtend factory.
	 */
	@Singleton
	public XtendFactory bindXtendFactory() {
		return XtendFactory.eINSTANCE;
	}

	/** Replies the SARL factory.
	 * @return the SARL factory.
	 */
	@Singleton
	public SarlFactory bindSarlFactory() {
		// Initialize simple dependencies
		return SarlFactory.eINSTANCE;
	}

	/** Replies the type of the provider of SARL action signatures.
	 * @return the type of provider for inferred action signatures.
	 */
	public Class<? extends FeatureCallValidator> bindFeatureCallValidator() {
		return DefaultFeatureCallValidator.class;
	}

	/** Replies the type of the Xbase compiler.
	 * @return the type of the Xbase compiler.
	 */
	public Class<? extends XbaseCompiler> bindXbaseCompiler() {
		return XtendCompiler.class;
	}

	/** Replies the type of the provider of SARL action signatures.
	 * @return the type of provider for inferred action signatures.
	 */
	public Class<? extends ActionPrototypeProvider> bindActionSignatureProvider() {
		return DefaultActionPrototypeProvider.class;
	}

	/** Replies the type of the extended component that computes the early-exit flags.
	 * @return the type of the early-exit computer.
	 */
	public Class<? extends ExtendedEarlyExitComputer> bindExtendedEarlyExitComputer() {
		return SARLExtendedEarlyExitComputer.class;
	}

	/** Replies the type of the extended component that computes the early-exit flags.
	 * @return the type of the early-exit computer.
	 */
	public Class<? extends IEarlyExitComputer> bindEarlyExitComputer() {
		return SARLEarlyExitComputer.class;
	}

	/** Replies the type of the extended component that validates the early-exit flags.
	 * @return the type of the early-exit validator.
	 */
	public Class<? extends EarlyExitValidator> bindEarlyExitValidator() {
		return SARLEarlyExitValidator.class;
	}

	/** Replies the type of the implicitly imported code extensions.
	 * @return the type of the implicitly imported code extensions.
	 */
	public Class<? extends ImplicitlyImportedFeatures> bindImplicitlyImportedFeatures() {
		return SARLImplicitlyImportedFeatures.class;
	}

	/** Replies the type of the provider of the issues that could be configured by the end user.
	 * @return the type of the provider of configurable issue codes.
	 */
	@Override
	public Class<? extends ConfigurableIssueCodesProvider> bindConfigurableIssueCodesProvider() {
		assert (
				super.bindConfigurableIssueCodesProvider().isAssignableFrom(
						SARLConfigurableIssueCodesProvider.class))
				: "The class SARLConfigurableIssueCodesProvider does not extend " //$NON-NLS-1$
					+ "the class provided by default by Xbase"; //$NON-NLS-1$
		//
		return SARLConfigurableIssueCodesProvider.class;
	}

	/** Replies the type of the validator of the feature names.
	 * @return the type of the validator of the feature names.
	 */
	@Override
	public Class<? extends FeatureNameValidator> bindFeatureNameValidator() {
		assert (super.bindFeatureNameValidator().isAssignableFrom(
			SARLFeatureNameValidator.class))
			: "The class SARLFeatureNameValidator does not extend " //$NON-NLS-1$
				+ "the class provided by default by Xbase"; //$NON-NLS-1$
		//
		return SARLFeatureNameValidator.class;
	}

	/** Replies the provider of hidden token sequencer.
	 * @return the provider of hidden token sequencer.
	 */
	public Class<? extends IHiddenTokenSequencer> bindHiddenTokenSequencer() {
		return SARLHiddenTokenSequencer.class;
	}

	/** Replies the provider of hidden token sequencer.
	 * @return the provider of hidden token sequencer.
	 */
	public Class<? extends ISyntacticSequenceAcceptor> bindSyntacticSequenceAcceptor() {
		return SARLHiddenTokenSequencer.class;
	}

	/** Replies the provider of context PDA that is used by the serializer.
	 * This specific SARL implementation is provided for fixing the
	 * <a href="https://github.com/sarl/sarl/issues/277">issue #277</a>.
	 * @return the context PDA provider
	 */
	public Class<? extends IContextPDAProvider> bindContextPDAProvider() {
		return SARLContextPDAProvider.class;
	}

	/** Bind to the SARL JVM model inferred.
	 *
	 * <p>This should be contributed by org.eclipse.xtext.generator.xbase.XbaseGeneratorFragment
	 * in the supertype, but is not since SARL extends the Xtend model inferrer.
	 *
	 * @return the type of the SARL model inferrer.
	 */
	public Class<? extends IJvmModelInferrer> bindIJvmModelInferrer() {
		return SARLJvmModelInferrer.class;
	}

	/** Replies the type of the component that is managing the file system.
	 * @return the type of the file system support.
	 */
	public Class<? extends AbstractFileSystemSupport> bindAbstractFileSystemSupport() {
		return JavaIOFileSystemSupport.class;
	}

	/** Replies the type of the component that dispatchs the changes in resources.
	 * @return the type of the resource change registry.
	 */
	public Class<? extends IResourceChangeRegistry> bindResourceChangeRegistry() {
		return NopResourceChangeRegistry.class;
	}

	/** Replies the type of the helper for using XExpressions.
	 * @return the type of the XExpression helper.
	 */
	public Class<? extends XExpressionHelper> bindXExpressionHelper() {
		return SARLExpressionHelper.class;
	}

	@Override
	public Class<? extends IValueConverterService> bindIValueConverterService() {
		return XtendValueConverterService.class;
	}

	@Override
	public void configureIScopeProviderDelegate(Binder binder) {
		binder.bind(IScopeProvider.class).annotatedWith(Names.named(AbstractDeclarativeScopeProvider.NAMED_DELEGATE))
		.to(XtendImportedNamespaceScopeProvider.class);
	}

	@Override
	public void configureSerializerIScopeProvider(Binder binder) {
		binder.bind(IScopeProvider.class).annotatedWith(SerializerScopeProviderBinding.class)
		.to(XtendSerializerScopeProvider.class);
	}

	/** Replies the type of scope container for constructors.
	 * @return the type of scope container for constructors.
	 */
	public Class<? extends ConstructorScopes> bindConstructorScopes() {
		return AnonymousClassConstructorScopes.class;
	}

	@Override
	public Class<? extends IQualifiedNameProvider> bindIQualifiedNameProvider() {
		return XtendQualifiedNameProvider.class;
	}

	@Override
	public Class<? extends IDefaultResourceDescriptionStrategy> bindIDefaultResourceDescriptionStrategy() {
		return XtendResourceDescriptionStrategy.class;
	}

	/** Replies the type of the JVM model associator.
	 * @return the type of the JVM model associator.
	 */
	public Class<? extends JvmModelAssociator> bindJvmModelAssociator() {
		return SarlJvmModelAssociations.Impl.class;
	}

	/** Replies the type of the provider of output configurations.
	 * @return the type of the provider of output configurations.
	 */
	public Class<? extends IOutputConfigurationProvider> bindIOutputConfigurationProvider() {
		return SarlOutputConfigurationProvider.class;
	}

	/** Replies the type of the collector of the URIs of the inferred/generated elements.
	 * @return the type of the collector.
	 */
	public Class<? extends TargetURICollector> bindTargetURICollector() {
		return JvmModelTargetURICollector.class;
	}

	@Override
	public Class<? extends ILocationInFileProvider> bindILocationInFileProvider() {
		return XtendLocationInFileProvider.class;
	}

	@Override
	public Class<? extends ILinkingDiagnosticMessageProvider> bindILinkingDiagnosticMessageProvider() {
		return XtendLinkingDiagnosticMessageProvider.class;
	}

	/** Replies the type of the configuration for imports.
	 * @return the type of the configuration for imports.
	 */
	public Class<? extends IImportsConfiguration> bindIImportsConfiguration() {
		return XtendImportsConfiguration.class;
	}

	/** Replies the type of the collector for type usages.
	 * @return the type of the collector for type usages.
	 */
	public Class<? extends TypeUsageCollector> bindTypeUsageCollector() {
		return XtendTypeUsageCollector.class;
	}

	/** Replies the type of the collector for type usages.
	 * @return the type of the collector for type usages.
	 */
	public Class<? extends ImportedTypesCollector> bindImportedTypesCollector() {
		return XtendImportedTypesUsageCollector.class;
	}

	@Override
	public Class<? extends DefaultBatchTypeResolver> bindDefaultBatchTypeResolver() {
		return TypeDeclarationAwareBatchTypeResolver.class;
	}

	@Override
	public Class<? extends DefaultReentrantTypeResolver> bindDefaultReentrantTypeResolver() {
		return XtendReentrantTypeResolver.class;
		//return LogicalContainerAwareReentrantTypeResolver.class;
	}

	/** Replies the type of the processor of traces.
	 * @return the type of the processor.
	 */
	public Class<? extends TraceAwarePostProcessor> bindTraceAwarePostProcessor() {
		return UnicodeAwarePostProcessor.class;
	}

	@Override
	public Class<? extends ITypeComputer> bindITypeComputer() {
		return XtendTypeComputer.class;
	}

	@Override
	public Class<? extends Manager> bindIResourceDescription$Manager() {
		return XtendResourceDescriptionManager.class;
	}

	@Override
	public Class<? extends IResourceValidator> bindIResourceValidator() {
		return CachingResourceValidatorImpl.class;
	}

	@Override
	public Class<? extends ILinker> bindILinker() {
		return RuntimeLinker.class;
	}

	@Override
	public void configureIResourceDescriptions(Binder binder) {
		binder.bind(IResourceDescriptions.class).to(EagerResourceSetBasedResourceDescriptions.class);
	}

	/** Replies the type Grammar converter.
	 * @return the type of the converter.
	 */
	public Class<? extends IDValueConverter> bindIDValueConverter() {
		return JavaIDValueConverter.class;
	}

	/** Replies the type documentation provider.
	 * @return the type of the provider.
	 */
	public Class<? extends IEObjectDocumentationProvider> bindIEObjectDocumentationProvider() {
		return XtendDocumentationProvider.class;
	}

	/** Replies the type file header provider.
	 * @return the type of the provider.
	 */
	public Class<? extends IFileHeaderProvider> bindFileHeaderProvider() {
		return XtendFileHeaderProvider.class;
	}

	/** Replies the type of the converter for int-underscore values from the Grammar.
	 * @return the type of the converter.
	 */
	public Class<? extends XbaseValueConverterService.IntUnderscoreValueConverter> bindIntUnderscoreValueConverter() {
		return IntUnderscoreValueConverter.class;
	}

	/** Replies the type of the converter for rich strings.
	 * @return the type of the converter.
	 */
	public Class<? extends STRINGValueConverter> bindSTRINGValueConverter() {
		return StringValueConverter.class;
	}

	@Override
	public Class<? extends EObjectAtOffsetHelper> bindEObjectAtOffsetHelper() {
		return XtendEObjectAtOffsetHelper.class;
	}

	/** Replies the type of the finder of return type.
	 * @return the type of the finder of return type.
	 */
	public Class<? extends ImplicitReturnFinder> bindImplicitReturnFinder() {
		return XtendImplicitReturnFinder.class;
	}

	/** Replies the type of the provider of human readable type names.
	 * @return the type of the provider.
	 */
	public Class<? extends HumanReadableTypeNames> bindHumanReadableTypeNames() {
		return LocalClassAwareTypeNames.class;
	}

	/** Replies the type of the JVM generator.
	 * @return the type of the generator.
	 */
	public Class<? extends JvmModelGenerator> bindJvmModelGenerator() {
		return XtendGenerator.class;
	}

	/** Replies the type of the facade for resource storage.
	 * @return the type of the facade.
	 */
	public Class<? extends IResourceStorageFacade> bindResourceStorageFacade() {
		return BatchLinkableResourceStorageFacade.class;
	}

}
