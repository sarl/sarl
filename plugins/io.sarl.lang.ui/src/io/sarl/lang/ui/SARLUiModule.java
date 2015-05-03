/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.ui;

import io.sarl.lang.ui.highlighting.SARLHighlightingCalculator;
import io.sarl.lang.ui.outline.SARLBehaviorUnitOutlineFilter;
import io.sarl.lang.ui.outline.SARLFieldOutlineFilter;
import io.sarl.lang.ui.outline.SARLOperationOutlineFilter;
import io.sarl.lang.ui.outline.SARLOutlineNodeComparator;
import io.sarl.lang.ui.outline.SARLOutlinePage;
import io.sarl.lang.ui.preferences.SARLPreferenceStoreInitializer;
import io.sarl.lang.ui.preferences.SARLValidatorConfigurationBlock;
import io.sarl.lang.ui.tasks.SarlTaskTagProvider;
import io.sarl.lang.ui.validation.SARLUIValidator;

import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.eclipse.xtend.ide.XtendResourceUiServiceProvider;
import org.eclipse.xtend.ide.autoedit.AutoEditStrategyProvider;
import org.eclipse.xtend.ide.builder.SourceRelativeFileSystemAccess;
import org.eclipse.xtend.ide.common.editor.bracketmatching.XtendBracePairProvider;
import org.eclipse.xtend.ide.editor.OccurrenceComputer;
import org.eclipse.xtend.ide.editor.OverrideIndicatorModelListener;
import org.eclipse.xtend.ide.editor.OverrideIndicatorRulerAction;
import org.eclipse.xtend.ide.editor.RichStringAwareSourceViewer;
import org.eclipse.xtend.ide.editor.RichStringAwareToggleCommentAction;
import org.eclipse.xtend.ide.editor.SingleLineCommentHelper;
import org.eclipse.xtend.ide.editor.XtendDoubleClickStrategyProvider;
import org.eclipse.xtend.ide.highlighting.XtendHighlightingConfiguration;
import org.eclipse.xtend.ide.hover.XtendAnnotationHover;
import org.eclipse.xtend.ide.hover.XtendHoverProvider;
import org.eclipse.xtend.ide.hyperlinking.HyperLinkingLabelProvider;
import org.eclipse.xtend.ide.hyperlinking.XtendHyperlinkHelper;
import org.eclipse.xtend.ide.refactoring.XtendRefactoringPreferences;
import org.eclipse.xtext.builder.EclipseResourceFileSystemAccess2;
import org.eclipse.xtext.builder.EclipseSourceFolderProvider;
import org.eclipse.xtext.builder.JDTAwareSourceFolderProvider;
import org.eclipse.xtext.generator.AbstractFileSystemAccess2;
import org.eclipse.xtext.ide.editor.bracketmatching.IBracePairProvider;
import org.eclipse.xtext.service.SingletonBinding;
import org.eclipse.xtext.tasks.ITaskTagProvider;
import org.eclipse.xtext.ui.editor.IXtextEditorCallback;
import org.eclipse.xtext.ui.editor.XtextSourceViewer;
import org.eclipse.xtext.ui.editor.actions.IActionContributor;
import org.eclipse.xtext.ui.editor.autoedit.AbstractEditStrategyProvider;
import org.eclipse.xtext.ui.editor.doubleClicking.DoubleClickStrategyProvider;
import org.eclipse.xtext.ui.editor.hover.IEObjectHoverProvider;
import org.eclipse.xtext.ui.editor.hyperlinking.IHyperlinkHelper;
import org.eclipse.xtext.ui.editor.occurrences.IOccurrenceComputer;
import org.eclipse.xtext.ui.editor.outline.actions.IOutlineContribution;
import org.eclipse.xtext.ui.editor.outline.impl.OutlineFilterAndSorter.IComparator;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfiguration;
import org.eclipse.xtext.ui.editor.syntaxcoloring.ISemanticHighlightingCalculator;
import org.eclipse.xtext.ui.editor.toggleComments.ISingleLineCommentHelper;
import org.eclipse.xtext.ui.editor.toggleComments.ToggleSLCommentAction;
import org.eclipse.xtext.ui.resource.IResourceUIServiceProvider;
import org.eclipse.xtext.ui.validation.AbstractValidatorConfigurationBlock;
import org.eclipse.xtext.xbase.file.AbstractFileSystemSupport;
import org.eclipse.xtext.xbase.ui.contentassist.ImportingTypesProposalProvider;
import org.eclipse.xtext.xbase.ui.file.EclipseFileSystemSupportImpl;

import com.google.inject.Binder;
import com.google.inject.name.Names;

/**
 * Use this class to register components to be used within the IDE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("static-method")
public class SARLUiModule extends AbstractSARLUiModule {

	/** Construct an injection module for the UI of SARL.
	 *
	 * @param plugin - the Eclipse plugin.
	 */
	public SARLUiModule(AbstractUIPlugin plugin) {
		super(plugin);
	}

	/** Replies the type of the configuration page for the SARL validator.
	 *
	 * @return the type of the SARL validator configuration page.
	 */
	public Class<? extends AbstractValidatorConfigurationBlock> bindAbstractValidatorConfigurationBlock() {
		return SARLValidatorConfigurationBlock.class;
	}

	/** Initialize the preference store with the SARL specific pairs.
	 *
	 * @param binder - the Google binder.
	 */
	@Override
	public void configureSmartCaretPreferenceInitializer(Binder binder) {
		binder.bind(IPreferenceStoreInitializer.class).annotatedWith(Names.named("smartCaretPreferenceInitializer")) //$NON-NLS-1$
		.to(SARLPreferenceStoreInitializer.class);
	}

	/** Provides a syntax highlighting for the statements that are specific to SARL, such as keywords.
	 *
	 * @return the hilighting calculator.
	 */
	@Override
	public Class<? extends ISemanticHighlightingCalculator> bindISemanticHighlightingCalculator() {
		assert (super.bindISemanticHighlightingCalculator().isAssignableFrom(SARLHighlightingCalculator.class))
		: "The class SARLHighlightingCalculator does not extend the " //$NON-NLS-1$
			+ "class provided by default by Xbase"; //$NON-NLS-1$
		//
		return SARLHighlightingCalculator.class;
	}

	/** Validate the SARL script from an UI-based point of view.
	 *
	 * @return the UI validator.
	 */
	@Override
	@org.eclipse.xtext.service.SingletonBinding(eager = true)
	public Class<? extends org.eclipse.xtext.xbase.ui.validation.XbaseUIValidator> bindXbaseUIValidator() {
		assert (super.bindXbaseUIValidator().isAssignableFrom(SARLUIValidator.class))
		: "The class SARLUIValidator does not extend the class " //$NON-NLS-1$
			+ "provided by default by Xbase"; //$NON-NLS-1$
		//
		return SARLUIValidator.class;
	}

	/** Provides the page for the outline.
	 *
	 * @return the type of the outline page.
	 */
	@Override
	public Class<? extends IContentOutlinePage> bindIContentOutlinePage() {
		return SARLOutlinePage.class;
	}

	/** Provides the comparator that permits to sort the outline entries.
	 *
	 * @return the comparator.
	 */
	@Override
	public Class<? extends IComparator> bindOutlineFilterAndSorter$IComparator() {
		return SARLOutlineNodeComparator.class;
	}

	/** Configure the contribution to the filtering operations in the outline.
	 *
	 * @param binder - the Google binder.
	 */
	public void configureFilterOperationsContribution(Binder binder) {
		binder.bind(IOutlineContribution.class).annotatedWith(
				Names.named("SARLFieldOutlineFilter")) //$NON-NLS-1$
				.to(SARLFieldOutlineFilter.class);
		binder.bind(IOutlineContribution.class).annotatedWith(
				Names.named("SARLOperationOutlineFilter")) //$NON-NLS-1$
				.to(SARLOperationOutlineFilter.class);
		binder.bind(IOutlineContribution.class).annotatedWith(
				Names.named("SARLBehaviorUnitOutlineFilter")) //$NON-NLS-1$
				.to(SARLBehaviorUnitOutlineFilter.class);
	}

	/** Provides the tool for building the list of the proposals.
	 * @return the proposal provider.
	 */
	@Override
	public Class<? extends org.eclipse.xtext.common.types.xtext.ui.ITypesProposalProvider> bindITypesProposalProvider() {
		return ImportingTypesProposalProvider.class;
	}

	/** Provides the tool for providing task tags.
	 * @return the proposal task tag provider.
	 */
	public Class<? extends ITaskTagProvider> bindTaskTagProvider() {
		return SarlTaskTagProvider.class;
	}

	//************************************************************
	//
	// THE FOLLOWING ELEMENTS ARE COPIED FROM Xtend Modules

	//	/** Configure the debug mode.
	//	 *
	//	 * @param binder the binder.
	//	 */
	//	public void configureDebugMode(Binder binder) {
	//		if (Boolean.getBoolean("io.sarl.lang.debug") //$NON-NLS-1$
	//			|| Boolean.getBoolean("org.eclipse.xtext.xtend.debug")) { //$NON-NLS-1$
	//			binder.bindConstant().annotatedWith(Names.named(AbstractEditStrategy.DEBUG)).to(true);
	//		}
	//		// matches ID of org.eclipse.ui.contexts extension registered in plugin.xml
	//		binder.bindConstant().annotatedWith(Names.named(XtextEditor.KEY_BINDING_SCOPE))
	//						.to("org.eclipse.xtend.ide.XtendEditorScope");
	//	}

	@Override
	public Class<? extends AbstractEditStrategyProvider> bindAbstractEditStrategyProvider() {
		return AutoEditStrategyProvider.class;
	}

	/** Configure the double-click strategry provider.
	 *
	 * @return the provider.
	 */
	public Class<? extends DoubleClickStrategyProvider> bindDoubleClickStrategyProvider() {
		return XtendDoubleClickStrategyProvider.class;
	}

	@Override
	public Class<? extends EclipseResourceFileSystemAccess2> bindEclipseResourceFileSystemAccess2() {
		return SourceRelativeFileSystemAccess.class;
	}

	@Override
	public Class<? extends AbstractFileSystemAccess2> bindAbstractFileSystemAccess2() {
		return SourceRelativeFileSystemAccess.class;
	}

	/** Bind the support to the file system.
	 *
	 * @return the type.
	 */
	public Class<? extends AbstractFileSystemSupport> bindAbstractFileSystemSupport() {
		return EclipseFileSystemSupportImpl.class;
	}

	@Override
	public Class<? extends EclipseSourceFolderProvider> bindEclipseSourceFolderProvider() {
		return JDTAwareSourceFolderProvider.class;
	}

	@Override
	public Class<? extends ISingleLineCommentHelper> bindISingleLineCommentHelper() {
		return SingleLineCommentHelper.class;
	}

	/** Configure the Override indicator.
	 * 
	 * @param binder the binder.
	 */
	public void configureOverrideIndicatorSupport(Binder binder) {
		binder.bind(IXtextEditorCallback.class).annotatedWith(Names.named("OverrideIndicatorModelListener")) //$NON-NLS-1$
		.to(OverrideIndicatorModelListener.class);
		binder.bind(IActionContributor.class).annotatedWith(Names.named("OverrideIndicatorRulerAction")).to( //$NON-NLS-1$
				OverrideIndicatorRulerAction.class);
	}

	@Override
	public void configureHyperlinkLabelProvider(com.google.inject.Binder binder) {
		binder.bind(org.eclipse.jface.viewers.ILabelProvider.class)
		.annotatedWith(org.eclipse.xtext.ui.editor.hyperlinking.HyperlinkLabelProvider.class)
		.to(HyperLinkingLabelProvider.class);
	}


	@Override
	public Class<? extends IHyperlinkHelper> bindIHyperlinkHelper() {
		return XtendHyperlinkHelper.class;
	}

	@Override
	public Class<? extends IAnnotationHover> bindIAnnotationHover() {
		return XtendAnnotationHover.class;
	}

	@Override
	public Class<? extends IHighlightingConfiguration> bindIHighlightingConfiguration() {
		return XtendHighlightingConfiguration.class;
	}

	/** Bind the factory of source viewer.
	 *
	 * @return the type.
	 */
	public Class<? extends XtextSourceViewer.Factory> bindSourceViewerFactory() {
		return RichStringAwareSourceViewer.Factory.class;
	}

	/** Bind the factory of comment toggler.
	 *
	 * @return the type.
	 */
	public Class<? extends ToggleSLCommentAction.Factory> bindToggleCommentFactory() {
		return RichStringAwareToggleCommentAction.Factory.class;
	}

	@Override
	public Class<? extends IEObjectHoverProvider> bindIEObjectHoverProvider() {
		return XtendHoverProvider.class;
	}

	@Override
	public Class<? extends IOccurrenceComputer> bindIOccurrenceComputer() {
		return OccurrenceComputer.class;
	}

	@Override
	@SingletonBinding
	public Class<? extends IBracePairProvider> bindIBracePairProvider() {
		return XtendBracePairProvider.class;
	}

	@Override
	public void configureIPreferenceStoreInitializer(Binder binder) {
		binder.bind(IPreferenceStoreInitializer.class)
		.annotatedWith(Names.named("RefactoringPreferences")) //$NON-NLS-1$
		.to(XtendRefactoringPreferences.Initializer.class);
	}

	/** Bind the provider of the UI services related to the resources.
	 *
	 * @return the type.
	 */
	public Class<? extends IResourceUIServiceProvider> bindIResourceUIServiceProvider() {
		return XtendResourceUiServiceProvider.class;
	}

	//	// contributed by org.eclipse.xtext.generator.formatting2.Formatter2Fragment
	//	public Class<? extends org.eclipse.xtext.ui.editor.formatting.IContentFormatterFactory> bindIContentFormatterFactory() {
	//		super.bindIContentFormatterFactory()
	//		return org.eclipse.xtext.ui.editor.formatting2.ContentFormatterFactory.class;
	//	}

	//	// contributed by org.eclipse.xtext.generator.parser.antlr.XtextAntlrUiGeneratorFragment
	//	public Class<? extends org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext.Factory> bindContentAssistContext$Factory() {
	//		return org.eclipse.xtext.ui.editor.contentassist.antlr.DelegatingContentAssistContextFactory.class;
	//	}

	//	// contributed by org.eclipse.xtext.generator.parser.antlr.XtextAntlrUiGeneratorFragment
	//	public Class<? extends org.eclipse.xtext.ide.editor.contentassist.antlr.ContentAssistContextFactory> bindContentAssistContextFactory() {
	//		return org.eclipse.xtext.ide.editor.contentassist.antlr.PartialContentAssistContextFactory.class;
	//	}

	//	// contributed by org.eclipse.xtext.ui.generator.templates.CodetemplatesGeneratorFragment
	//	public Class<? extends org.eclipse.xtext.ui.codetemplates.ui.partialEditing.IPartialEditingContentAssistContextFactory> bindIPartialEditingContentAssistContextFactory() {
	//		return org.eclipse.xtext.ui.codetemplates.ui.partialEditing.PartialEditingContentAssistContextFactory.class;
	//	}

	//	@Override
	//	public Class<? extends AbstractAntlrTokenToAttributeIdMapper> bindAbstractAntlrTokenToAttributeIdMapper() {
	//		return TokenToAttributeIdMapper.class;
	//	}
	//
	//	@Override
	//	public Class<? extends ITokenScanner> bindITokenScanner() {
	//		return RichStringAwareTokenScanner.class;
	//	}
	//
	////	public Class<? extends TerminalsTokenTypeToPartitionMapper> bindTerminalsTokenTypeToPartitionMapper() {
	////		return TokenTypeToPartitionMapper.class;
	////	}
	//
	//	/** Bind the quick outline filter and sorter.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends QuickOutlineFilterAndSorter> bindQuickOutlineFilterAndSorter() {
	//		return XtendQuickOutlineFilterAndSorter.class;
	//	}

	//		/** Bind the provider of region folding.
	//		 *
	//		 * @return the type.
	//		 */
	//		public Class<? extends IFoldingRegionProvider> bindIFoldingRegionProvider() {
	//			return XtendFoldingRegionProvider.class;
	//		}

	//	@Override
	//	public Class<? extends IXtextBuilderParticipant> bindIXtextBuilderParticipant() {
	//		return XtendParallelBuilderParticipant.class;
	//	}

	//	@Override
	//	public Class<? extends IXtextEditorCallback> bindIXtextEditorCallback() {
	//		return XtendNatureAddingEditorCallback.class;
	//	}
		
	//	@Override
	//	public Class<? extends IRenameContextFactory> bindIRenameContextFactory() {
	//		return XtendRenameContextFactory.class;
	//	}
	//
	//	/** Bind the processor for element renaming.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends RenameElementProcessor> bindRenameElementProcessor() {
	//		return XtendRenameElementProcessor.class;
	//	}
	//
	//	@Override
	//	public Class<? extends IRenameStrategy> bindIRenameStrategy() {
	//		return XtendRenameStrategy.class;
	//	}
	//
	//	@Override
	//	public java.lang.Class<? extends IDependentElementsCalculator> bindIDependentElementsCalculator() {
	//		return XtendDependentElementsCalculator.class;
	//	}
	//	
	//	@Override
	//	public void configureJvmMemberRenameStrategy$Provider$Delegate(Binder binder) {
	//		binder.bind(IRenameStrategy.Provider.class)
	//			.annotatedWith(JvmMemberRenameStrategy.Provider.Delegate.class)
	//			.to(XtendRenameStrategyProvider.class);
	//	}
	//		
	//	/** Bind the processor for element renaming.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends JdtRenameRefactoringParticipantProcessor> bindJdtRenameRefactoringParticipantProcessor() {
	//		return XtendJdtRenameParticipantProcessor.class;
	//	}
	//	
	//	/** Bind the processor for element renaming.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends XbaseDeclarativeHoverSignatureProvider> bindXbaseDeclarativeHoverSignatureProvider() {
	//		return XtendHoverSignatureProvider.class;
	//	}
	//
	//	@Override
	//	public Class<? extends IEObjectHoverDocumentationProvider> bindIEObjectHoverDocumentationProvider() {
	//		return XtendHoverDocumentationProvider.class;
	//	}
	//
	//	@Override
	//	public Class<? extends org.eclipse.xtext.ui.editor.XtextEditor> bindXtextEditor() {
	//		return XbaseEditor.class;
	//	}
	//
	//	@Override
	//	public Class<? extends ITemplateProposalProvider> bindITemplateProposalProvider() {
	//		return TemplateProposalProvider.class;
	//	}
	//
	//	/** Bind the trace information provider.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends ITraceForStorageProvider> bindTraceInformation() {
	//		return TraceForStorageProvider.class;
	//	}
	//
	//	/** Bind the provider of edited resource.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends IEditedResourceProvider> bindIEditedResourceProvider() {
	//		return FormatterResourceProvider.class;
	//	}
	//
	//	/** Configure the contribution for synthetic members.
	//	 *
	//	 * @param binder the binder.
	//	 */
	//	public void configureFilterSyntheticMembersContribution(Binder binder) {
	//		binder.bind(IOutlineContribution.class).annotatedWith(Names.named("FilterSyntheticsContribution")) //$NON-NLS-1$
	//				.to(ShowSyntheticMembersContribution.class);
	//	}
	//
	//	/** Bind the initializer of the Java project preferences.
	//	 * 
	//	 * @return the type.
	//	 */
	//	@SingletonBinding(eager = true)
	//	public Class<? extends JavaProjectPreferencesInitializer> bindJavaProjectPreferencesInitializer() {
	//		return JavaProjectPreferencesInitializer.class;
	//	}
	//
	//	/** Bind the provider of issue severity.
	//	 * 
	//	 * @return the type.
	//	 */
	//	public Class<? extends IssueSeveritiesProvider> bindIssueSeverityServiceProvider() {
	//		return XbaseIssueSeveritiesProvider.class;
	//	}
	//	
	//	/** Bind the provider of source viewer configuration.
	//	 * 
	//	 * @return the type.
	//	 */
	//	public Class<? extends XtextSourceViewerConfiguration> bindSourceViewerConfiguration(){
	//		return XtendSourceViewerConfiguration.class;
	//	}
	//
	//	/** Bind the source of document token.
	//	 * 
	//	 * @return the type.
	//	 */
	//	public Class<? extends DocumentTokenSource> bindDocumentTokenSource(){
	//		return XtendDocumentTokenSource.class;
	//	}
	//	
	//	/** Bind the provider of type instance for JVM type.
	//	 * 
	//	 * @return the type.
	//	 */
	//	public Class<? extends ProcessorInstanceForJvmTypeProvider> bindProcessorInstanceForJvmTypeProvider() {
	//		return JdtBasedProcessorProvider.class;
	//	}
	//
	//	@Override
	//	public Class<? extends IContentAssistantFactory> bindIContentAssistantFactory() {
	//		return XtendContentAssistFactory.class;
	//	}
	//	
	//	@Override
	//	public Class<? extends IResourceForEditorInputFactory> bindIResourceForEditorInputFactory() {
	//		return XbaseResourceForEditorInputFactory.class;
	//	}
	//	
	//	/** Bind the provider of context information.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends IContextInformationProvider> bindIContextInformationProvider() {
	//		return ParameterContextInformationProvider.class;
	//	}
	//	
	//	/** Bind the string matcher based on the Camel case heuristic.
	//	 * @return the type.
	//	 */
	//	public Class<? extends PrefixMatcher.CamelCase> bindCamelCasePrefixMatcher() {
	//		return EscapeSequenceAwarePrefixMatcher.class;
	//	}
	//	
	//	/** Bind the finder of type reference.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends IReferenceFinder> bindIReferenceFinder() {
	//		return DelegatingReferenceFinder.class;
	//	}
	//	
	//	/** Bind the quick fix for mispelled and not yet imported Java types.
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends JavaTypeQuickfixes> bindJavaTypeQuickfixes() {
	//		return TypeLiteralAwareJavaTypeQuickfixes.class;
	//	}
	//	
	//	/** Bind the comparator of auto-completion proposal
	//	 *
	//	 * @return the type.
	//	 */
	//	public Class<? extends ICompletionProposalComparator> bindICompletionProposalComparator() {
	//		return OperatorAwareComparator.class;
	//	}
	//	
	//	@Override
	//	public Class<? extends CopyQualifiedNameService> bindCopyQualifiedNameService() {
	//		return XtendCopyQualifiedNameService.class;
	//	}
	//	
	//	@Override
	//	public Class<? extends IContentAssistParser> bindIContentAssistParser() {
	//		return FlexerBasedContentAssistParser.class;
	//	}
	//	
	//	@Override
	//	public void configureContentAssistLexerProvider(Binder binder) {
	//		super.configureContentAssistLexerProvider(binder);
	//		binder.bind(InternalXtendLexer.class).toProvider(LexerProvider.create(DisabledInternalLexer.class));
	//		binder.bind(DisabledInternalLexer.class).toProvider(LexerProvider.create(DisabledInternalLexer.class));
	//	}
	//
	//	@Override
	//	public void configureContentAssistLexer(Binder binder) {
	//		binder.bind(Lexer.class).annotatedWith(Names.named(LexerIdeBindings.CONTENT_ASSIST)).to(DisabledInternalLexer.class);
	//	}
	//	
	//	@Override
	//	public Class<? extends ContentAssistContextFactory> bindContentAssistContextFactory() {
	//		return FlexerBasedContentAssistContextFactory.class;
	//	}
	//
	//	@Override
	//	public Class<? extends IProposalConflictHelper> bindIProposalConflictHelper() {
	//		return FlexProposalConflictHelper.class;
	//	}
	//	
	//	public Class<? extends TemplateBodyHighlighter> bindTemplateBodyHighlighter() {
	//		return FlexerBasedTemplateBodyHighlighter.class;
	//	}
	//
	//	public Class<? extends IPreferenceStoreAccess> bindPreferenceStoreAccess() {
	//		return XtendPreferenceStoreAccess.class;
	//	}
	//	
	//	public Class<? extends ExpressionUtil> bindExpressionUtil() {
	//		return XtendExpressionUtil.class;
	//	}
	//	
	//	public Class<? extends OutlineNodeFactory> bindOutlineNodeFactory() {
	//		return XtendOutlineNodeFactory.class;
	//	}
	//	
	//	@Override
	//	public Class<? extends ITypesProposalProvider> bindITypesProposalProvider() {
	//		return XtendImportingTypesProposalProvider.class;
	//	}
	//	
	//	public Class<? extends IOutlineTreeProvider.ModeAware> bindIOutlineTreeProvider_ModeAware() {
	//		return org.eclipse.xtend.ide.outline.XtendOutlineModes.class;
	//	}
	//	
	//	public void configureSwitchOutlineModeContribution(Binder binder) {
	//		binder.bind(IOutlineContribution.class).annotatedWith(Names.named("SwitchOutlineModeContribution"))
	//		.to(SwitchOutlineModeContribution.class);
	//	}
	//	
	//	public void configureSwitchQuickOutlineModeContribution(Binder binder) {
	//		binder.bind(IQuickOutlineContribution.class).annotatedWith(Names.named("SwitchQuickOutlineModeContribution"))
	//				.to(SwitchOutlineModeContribution.class);
	//	}
	//	
	//	@org.eclipse.xtext.service.SingletonBinding(eager = true)
	//	public Class<? extends IResourceChangeRegistry> bindResourceChangeRegistry() {
	//		return UIResourceChangeRegistry.class;
	//	}
	//	
	//	@Override
	//	public Class<? extends  IPartialEditingContentAssistParser> bindIPartialEditingContentAssistParser() {
	//		return FlexerBasedPartialXtendContentAssistParser.class;
	//	}
	//
	//	public Class<? extends ILinker> bindILinker() {
	//		return Linker.class;
	//	}
	//	
	//	public Class<? extends OutlineWithEditorLinker> bindOutlineWithEditorLinker() {
	//		return XtendOutlineWithEditorLinker.class;
	//	}
	//	
	//	public Class<? extends XtendResourceDescriptionManager> bindXtendUIResourceDescriptionManager() {
	//		return XtendUIResourceDescriptionManager.class;
	//	}
	//
	//	public Class<? extends IClipboardActionFactory> bindIClipboardActionFactory() {
	//		return ImportsAwareClipboardAction.Factory.class;
	//	}
	//	
	//	public Class<? extends IContextualOutputConfigurationProvider> bindIContextualOutputConfigurationProvider() {
	//		return EclipseOutputConfigurationProvider.class;
	//	}
	//	
	//	public Class<? extends IResourceValidator> bindIResourceValidator() {
	//		return XtendResourceValidator.class;
	//	}

	// THE PREVIOUS ELEMENTS ARE COPIED FROM Xtend Modules
	//
	//************************************************************

}
