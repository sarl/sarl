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

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;
import javax.inject.Inject;

import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EModelElement;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractElement;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.Group;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.UntilToken;
import org.eclipse.xtext.documentation.IEObjectDocumentationProvider;
import org.eclipse.xtext.documentation.IEObjectDocumentationProviderExtension;
import org.eclipse.xtext.documentation.impl.MultiLineCommentDocumentationProvider;
import org.eclipse.xtext.formatting2.ITextReplacerContext;
import org.eclipse.xtext.formatting2.regionaccess.IComment;
import org.eclipse.xtext.formatting2.regionaccess.ILineRegion;
import org.eclipse.xtext.formatting2.regionaccess.ITextRegionAccess;
import org.eclipse.xtext.formatting2.regionaccess.ITextSegment;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.serializer.ISerializationContext;
import org.eclipse.xtext.serializer.acceptor.ISequenceAcceptor;
import org.eclipse.xtext.serializer.acceptor.ISyntacticSequenceAcceptor;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynState;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynTransition;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.SynStateType;
import org.eclipse.xtext.serializer.diagnostic.ISerializationDiagnostic.Acceptor;
import org.eclipse.xtext.serializer.sequencer.HiddenTokenSequencer;
import org.eclipse.xtext.serializer.sequencer.ISyntacticSequencer;
import org.eclipse.xtext.serializer.sequencer.RuleCallStack;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;
import org.eclipse.xtext.xbase.compiler.IAppendable;
import org.eclipse.xtext.xbase.compiler.StringBuilderBasedAppendable;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.grammarAccess.GrammarAccessExtensions;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/** Generate the documentation adapters and related types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DocumentationBuilderFragment extends AbstractSubCodeBuilderFragment {

	@Inject
	private GrammarAccessExtensions grammarAccessExtensions;

	/** Replies the interface for the documentation builder.
	 *
	 * @return the documentation builder interface.
	 */
	@Pure
	public TypeReference getIEcoreDocumentationBuilder() {
		return new TypeReference(getCodeElementExtractor().getDocumentationPackage()
				+ ".IEcoreDocumentationBuilder"); //$NON-NLS-1$
	}

	/** Replies the implementation for the documentation builder.
	 *
	 * @return the documentation builder implementation.
	 */
	@Pure
	public TypeReference getEcoreDocumentationBuilderImpl() {
		return new TypeReference(getCodeElementExtractor().getDocumentationPackage()
				+ ".EcoreDocumentationBuilder"); //$NON-NLS-1$
	}

	/** Replies the interface for the documentation formatter.
	 *
	 * @return the documentation formatter interface.
	 */
	@Pure
	public TypeReference getIDocumentationFormatter() {
		return new TypeReference(getCodeElementExtractor().getDocumentationPackage()
				+ ".IDocumentationFormatter"); //$NON-NLS-1$
	}

	/** Replies the implementation for the documentation formatter.
	 *
	 * @return the documentation formatter implementation.
	 */
	@Pure
	public TypeReference getDocumentationFormatterImpl() {
		return new TypeReference(getCodeElementExtractor().getDocumentationPackage()
				+ ".DocumentationFormatter"); //$NON-NLS-1$
	}

	/** Replies the implementation for the syntactic sequencer supporting Ecore documentation.
	 *
	 * @return the syntactic sequencer implementation.
	 */
	@Pure
	public TypeReference getEcoreDocumentationSyntacticSequencer() {
		return new TypeReference(getCodeElementExtractor().getSerializerPackage() + "." //$NON-NLS-1$
				+ getLanguageName().toUpperCase() + "EcoreDocumentationSyntacticSequencer"); //$NON-NLS-1$
	}

	/** Replies the implementation for the custom syntactic sequencer supporting Ecore documentation.
	 *
	 * @return the syntactic sequencer implementation.
	 */
	@Pure
	public TypeReference getEcoreDocumentationSyntacticSequencerCustom() {
		return new TypeReference(getCodeElementExtractor().getSerializerPackage() + "." //$NON-NLS-1$
				+ getLanguageName().toUpperCase() + "EcoreDocumentationSyntacticSequencerCustom"); //$NON-NLS-1$
	}

	/** Replies the implementation for the syntactic sequencer.
	 *
	 * @return the syntactic sequencer implementation.
	 */
	@Pure
	public TypeReference getSyntacticSequencer() {
		return new TypeReference(getCodeElementExtractor().getSerializerPackage() + "." //$NON-NLS-1$
				+ getLanguageName().toUpperCase() + "SyntacticSequencer"); //$NON-NLS-1$
	}

	/** Replies the implementation for the documentation provider.
	 *
	 * @return the documentation provider implementation.
	 */
	@Pure
	public TypeReference getDocumentationProviderImpl() {
		return new TypeReference(getCodeElementExtractor().getDocumentationPackage() + "." //$NON-NLS-1$
				+ getLanguageName() + "DocumentationProvider"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the documentation provider.
	 *
	 * @return the custom documentation provider implementation.
	 */
	@Pure
	public TypeReference getDocumentationProviderImplCustom() {
		return new TypeReference(getCodeElementExtractor().getDocumentationPackage() + "." //$NON-NLS-1$
				+ getLanguageName().toUpperCase() + "DocumentationProviderCustom"); //$NON-NLS-1$
	}

	@Override
	public void generate() {
		super.generate();
		generateInnerDocumentationAdapter();
		generateIDocumentationFormatter();
		generateIEcoreDocumentationBuilder();
		generateDocumentationFormatterImpl();
		generateEcoreDocumentationBuilderImpl();
		generateDocumentationProviderImpl();
		generateEcoreDocumentationSyntacticSequencer();
	}

	@Override
	public void getExportedPackages(Set<String> exportedPackages) {
		if (exportedPackages != null) {
			super.getExportedPackages(exportedPackages);
			exportedPackages.add(getCodeElementExtractor().getSerializerPackage());
			exportedPackages.add(getCodeElementExtractor().getDocumentationPackage());
		}
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);

		factory.addfinalTypeToType(getIDocumentationFormatter(), getDocumentationFormatterImpl());
		factory.addfinalTypeToTypeSingleton(getIEcoreDocumentationBuilder(), getEcoreDocumentationBuilderImpl());

		bindTypeReferences(factory,
				new TypeReference(IEObjectDocumentationProvider.class),
				getDocumentationProviderImpl(),
				getDocumentationProviderImplCustom());
		bindTypeReferences(factory,
				new TypeReference(IEObjectDocumentationProviderExtension.class),
				getDocumentationProviderImpl(),
				getDocumentationProviderImplCustom());
		bindTypeReferences(factory,
				new TypeReference(ISyntacticSequencer.class),
				getEcoreDocumentationSyntacticSequencer(),
				getEcoreDocumentationSyntacticSequencerCustom());
	}

	/** Generate the adapter that supports the inner documentation.
	 */
	protected void generateInnerDocumentationAdapter() {
		final TypeReference adapter = getCodeElementExtractor().getInnerBlockDocumentationAdapter();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("public class "); //$NON-NLS-1$
				it.append(adapter.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(AdapterImpl.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String documentation;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String getDocumentation() {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\treturn this.documentation;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic void setDocumentation(String documentation) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.documentation = documentation;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic boolean isAdapterForType(Object type) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn type == "); //$NON-NLS-1$
				it.append(adapter.getSimpleName());
				it.append(".class;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
			}

		};
		final JavaFileAccess createJavaFile = getFileAccessFactory().createJavaFile(adapter, content);
		createJavaFile.writeTo(getSrcGen());
	}

	/** Generate the interface for the documentation formatter.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateIDocumentationFormatter() {
		final TypeReference formatter = getIDocumentationFormatter();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Format a documentation string."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public interface "); //$NON-NLS-1$
				it.append(formatter.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the characters that must be used as prefix of each line in a multiline comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the prefix."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString getMultilineCommentLinePrefix();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the characters that must be used to start a comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the symbols."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString getMultilineCommentStartSymbols();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the characters that must be used to end a comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the symbols."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString getMultilineCommentEndSymbols();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the string that must be used as prefix of a singleline comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the prefix."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString getSinglelineCommentPrefix();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the formatted comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString formatMultilineComment(String doc);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param indentation the string to put for indenting the comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the formatted comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString formatMultilineComment(String doc, String indentation);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appendable the receiver of the formatted string."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tvoid formatMultilineComment(String doc, "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param indentation the string to put for indenting the comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appendable the receiver of the formatted string."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tvoid formatMultilineComment(String doc, String indentation, "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Format the given multiline documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the formatting context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param comment the comment to format out."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic void formatMultilineComment("); //$NON-NLS-1$
				it.append(ITextReplacerContext.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(IComment.class);
				it.append(" comment);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the formatted comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString formatSinglelineComment(String doc);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param indentation the string to put for indenting the comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the formatted comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString formatSinglelineComment(String doc, String indentation);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appendable the receiver of the formatted string."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tvoid formatSinglelineComment(String doc, "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param indentation the string to put for indenting the comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appendable the receiver of the formatted string."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tvoid formatSinglelineComment(String doc, String indentation, "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Format the given singleline documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the formatting context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param comment the comment to format out."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tvoid formatSinglelineComment("); //$NON-NLS-1$
				it.append(ITextReplacerContext.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(IComment.class);
				it.append(" comment);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess createJavaFile = getFileAccessFactory().createJavaFile(formatter, content);
		createJavaFile.writeTo(getSrcGen());
	}

	/** Generate the interface for the documentation builder.
	 */
	protected void generateIEcoreDocumentationBuilder() {
		final TypeReference builder = getIEcoreDocumentationBuilder();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Build a documentation string for specific objects."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public interface "); //$NON-NLS-1$
				it.append(builder.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the grammar rule that corresponds to multiline comments."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the ML grammar rule."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" getMLCommentRule();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the grammar rule that corresponds to singleline comments."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the SL grammar rule."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" getSLCommentRule();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the documentation formatter used by this builder."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return documentation formatter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				it.append(getIDocumentationFormatter());
				it.append(" getDocumentationFormatter();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the formatted string that corresponds to the given documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation text. It may be on multiple lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param objectType the type of the object for which the document must be built."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the formatted comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tString build(String doc, Class<?> objectType);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies if multiline comments are the default for the given type of objects."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type the type of objects."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return <code>true</code> if multiline comment is the default."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *      Otherwise singleline comment is the default."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tboolean isMultilineCommentFor(Class<?> type);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		final JavaFileAccess createJavaFile = getFileAccessFactory().createJavaFile(builder, content);
		createJavaFile.writeTo(getSrcGen());
	}

	/** Generate the implementation for the documentation formatter.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateDocumentationFormatterImpl() {
		final TypeReference formatter = getDocumentationFormatterImpl();
		StringConcatenationClient content = new StringConcatenationClient() {
			@SuppressWarnings("synthetic-access")
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Formatter a documentation string."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public class DocumentationFormatter implements "); //$NON-NLS-1$
				it.append(getIDocumentationFormatter());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static final String SPACE_CHAR = \" \";"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprivate static final String NL_CHAR = \"\\n\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprivate static final String EMPTY_STR = \"\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprivate String mlLinePrefix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprivate String mlStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprivate String mlEnd;"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprivate String slPrefix;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected static boolean isNewLine(char character) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (character == '\\n' || character == '\\r' || character == '\\f') {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\treturn ((((1 << Character.LINE_SEPARATOR)"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t| (1 << Character.PARAGRAPH_SEPARATOR)) >> Character.getType((int) character)) & 1) != 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String getMultilineCommentStartSymbols() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.mlStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic void setMultilineCommentStartSymbols(String symbols) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.mlStart = symbols;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String getMultilineCommentEndSymbols() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.mlEnd;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic void setMultilineCommentEndSymbols(String symbols) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.mlEnd = symbols;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String getMultilineCommentLinePrefix() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.mlLinePrefix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic void setMultilineCommentLinePrefix(String prefix) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.mlLinePrefix = prefix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String getSinglelineCommentPrefix() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.slPrefix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<Character> getSinglelineCommentSpecialChars() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<Character> set = new "); //$NON-NLS-1$
				it.append(TreeSet.class);
				it.append("<>();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tset.add('*');"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tset.add('+');"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tset.add('-');"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tset.add('=');"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn set;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic void setSinglelineCommentPrefix(String prefix) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.slPrefix = prefix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tpublic void setGrammarAccess("); //$NON-NLS-1$
				it.append(DocumentationBuilderFragment.this.grammarAccessExtensions.getGrammarAccess(getGrammar()));
				it.append(" access) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (this.mlStart == null || this.mlEnd == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" mlRule = access.getML_COMMENTRule();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfor ("); //$NON-NLS-1$
				it.append(AbstractElement.class);
				it.append(" element : (("); //$NON-NLS-1$
				it.append(Group.class);
				it.append(") mlRule.getAlternatives()).getElements()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (element instanceof "); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(" && this.mlStart == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tthis.mlStart = (("); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") element).getValue();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t} else if (element instanceof "); //$NON-NLS-1$
				it.append(UntilToken.class);
				it.append(" && this.mlEnd == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tthis.mlEnd = (("); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") (("); //$NON-NLS-1$
				it.append(UntilToken.class);
				it.append(") element).getTerminal()).getValue();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (this.mlLinePrefix == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.mlLinePrefix = this.mlStart.substring(this.mlStart.length() - 1);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (this.slPrefix == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" slRule = access.getSL_COMMENTRule();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfor ("); //$NON-NLS-1$
				it.append(AbstractElement.class);
				it.append(" element : (("); //$NON-NLS-1$
				it.append(Group.class);
				it.append(") slRule.getAlternatives()).getElements()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (element instanceof "); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tthis.slPrefix = (("); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") element).getValue().trim();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String formatMultilineComment(String doc) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn formatMultilineComment(doc, (String) null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String formatMultilineComment(String doc, String indentation) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable = new "); //$NON-NLS-1$
				it.append(StringBuilderBasedAppendable.class);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tformatMultilineComment(doc, indentation, appendable);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn appendable.getContent();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic void formatMultilineComment(String doc, "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tformatMultilineComment(doc, null, appendable);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic void formatMultilineComment(String doc, String indentation, "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(doc)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal "); //$NON-NLS-1$
				it.append(SortedMap.class);
				it.append("<Integer, Replacement> replacements = new "); //$NON-NLS-1$
				it.append(TreeMap.class);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tformatMultlineComment(indentation, Strings.newLine(), new AppendableAccessor(appendable, doc, replacements, 0, doc.length()));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic void formatMultilineComment("); //$NON-NLS-1$
				it.append(ITextReplacerContext.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(IComment.class);
				it.append(" comment) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tformatMultlineComment(context.getIndentationString(), context.getNewLinesString(1), new RegionAccessor(context, comment));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String formatSinglelineComment(String doc) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn formatSinglelineComment(doc, (String) null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t\tpublic String formatSinglelineComment(String doc, String indentation) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(StringBuilderBasedAppendable.class);
				it.append(" appendable = new "); //$NON-NLS-1$
				it.append(StringBuilderBasedAppendable.class);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tformatSinglelineComment(doc, indentation, appendable);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn appendable.getContent();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic void formatSinglelineComment(String doc, "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tformatSinglelineComment(doc, null, appendable);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic void formatSinglelineComment(String doc, String indentation, "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(doc)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal "); //$NON-NLS-1$
				it.append(SortedMap.class);
				it.append("<Integer, Replacement> replacements = new "); //$NON-NLS-1$
				it.append(TreeMap.class);
				it.append("<>();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint offset = doc.indexOf(getSinglelineCommentPrefix());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (offset < 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\toffset = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint endOffset = doc.indexOf(NL_CHAR, offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (endOffset < 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tendOffset = doc.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tformatSinglelineComment("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tindentation,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tnew AppendableAccessor(appendable, doc, replacements, offset, endOffset));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic void formatSinglelineComment("); //$NON-NLS-1$
				it.append(ITextReplacerContext.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(IComment.class);
				it.append(" comment) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tformatSinglelineComment(context.getIndentationString(), new RegionAccessor(context, comment));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate <T> void formatSinglelineComment(String indentationString, FormattedTextAccessor<T> backend) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String indent = "); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".emptyIfNull(indentationString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String comment = backend.getCommentText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Compute the starting offset of the text inside the comment"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint offset = comment.indexOf(getSinglelineCommentPrefix());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (offset < 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tbackend.replace(0, 0, getSinglelineCommentPrefix());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\toffset = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\toffset += getSinglelineCommentPrefix().length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal int endOffset = comment.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tT currentLine = backend.getFirstLine(backend.getCommentOffset());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tboolean firstLine = true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\twhile (currentLine != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tString lineText = backend.getLineText(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint lineOffset = backend.getLineOffset(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint lineLength = backend.getLineLength(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Clamp the line text to the comment area."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (firstLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (lineOffset < offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tfinal int len = offset - lineOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineText = lineText.substring(len);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineOffset += len;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineLength -= len;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else if (lineOffset >= endOffset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// After the end of comment"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbackend.applyReplacements();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tfinal String prefix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (!startsWith(lineText, 0, getSinglelineCommentPrefix())) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tprefix = indent + getSinglelineCommentPrefix();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tprefix = indent;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbackend.replace(lineOffset, 0, prefix);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Skip the comment characters that corresponds to the Javadoc format: //[*-+=]."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint realCommentStart = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal "); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<Character> specialChars = getSinglelineCommentSpecialChars();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (realCommentStart < lineLength && specialChars.contains(lineText.charAt(realCommentStart))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t++realCommentStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Search for the first non whitespace"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint firstNonWhiteSpacePos = realCommentStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (firstNonWhiteSpacePos < lineLength && Character.isWhitespace(lineText.charAt(firstNonWhiteSpacePos))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t++firstNonWhiteSpacePos;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Add whitespace at the beginning."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (firstNonWhiteSpacePos == lineLength) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Empty comment"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (realCommentStart < firstNonWhiteSpacePos) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(realCommentStart + lineOffset, lineLength - realCommentStart, EMPTY_STR);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tfinal int expectedNbWhiteSpaces = getWhiteSpacesOnFirstLine();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tfinal int nbWhiteSpaces = firstNonWhiteSpacePos - realCommentStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (nbWhiteSpaces != expectedNbWhiteSpaces) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(realCommentStart + lineOffset, nbWhiteSpaces, makeWhiteSpaces(expectedNbWhiteSpaces));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Format the comment text"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tformatLineText("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineText.substring(firstNonWhiteSpacePos, lineLength), true,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tnew SubAccessor<>(backend, lineOffset + firstNonWhiteSpacePos));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Remove trailing whitespaces"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tint endOfText = lineLength;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\twhile ((endOfText - 1) > firstNonWhiteSpacePos && Character.isWhitespace(lineText.charAt(endOfText - 1))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t--endOfText;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (endOfText < lineLength) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(endOfText + lineOffset, lineLength - endOfText, EMPTY_STR);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfirstLine = false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcurrentLine = backend.getNextLine(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tbackend.applyReplacements();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static String safeSubstring(String text, int start, int length) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (text == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn EMPTY_STR;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal int index = Math.max(0, start);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal int len = Math.max(0, Math.min(length, text.length()));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn text.substring(index, index + len);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static boolean startsWith(String text, int start, String pattern) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn safeSubstring(text, start, pattern.length()).equals(pattern);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static String makeWhiteSpaces(int nb) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal StringBuilder b = new StringBuilder();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor (int i = 0; i < nb; ++i) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tb.append(SPACE_CHAR);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn b.toString();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected int getWhiteSpacesOnFirstLine() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn 1;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected int getWhiteSpacesOnOtherLines() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn 1;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected <T> void formatLineText(String lineText, boolean isMultlineComment, FormattedTextAccessor<T> backend) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate <T> boolean formatMultlineCommentFirstLine(String lineText, String indentationString, String newLineString, int endCommentOffset, FormattedTextAccessor<T> backend) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Skip the comment characters that corresponds to the Javadoc format: /**."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint realCommentStart = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\twhile (realCommentStart < lineText.length() && startsWith(lineText, realCommentStart, getMultilineCommentLinePrefix())) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\trealCommentStart += getMultilineCommentLinePrefix().length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Search for the first non whitespace"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint firstNonWhiteSpacePos = realCommentStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tboolean hasNonSpaceChar = false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\twhile (firstNonWhiteSpacePos < lineText.length() && Character.isWhitespace(lineText.charAt(firstNonWhiteSpacePos))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!Character.isSpaceChar(lineText.charAt(firstNonWhiteSpacePos))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\thasNonSpaceChar = true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t++firstNonWhiteSpacePos;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Add whitespace at the beginning."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal int expectedNbWhiteSpaces = getWhiteSpacesOnFirstLine();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal int nbWhiteSpaces = firstNonWhiteSpacePos - realCommentStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (hasNonSpaceChar || nbWhiteSpaces != expectedNbWhiteSpaces) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tbackend.replace(realCommentStart, nbWhiteSpaces, makeWhiteSpaces(expectedNbWhiteSpaces));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Treat the end of comment"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (endCommentOffset <= lineText.length()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Comment end at the first line. Insert a newline character"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Search for the end of comment text."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint endPos = endCommentOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal int end = endPos;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile ((endPos - 1) > firstNonWhiteSpacePos && Character.isWhitespace(lineText.charAt(endPos - 1))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t--endPos;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Format the comment text"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tformatLineText(lineText.substring(firstNonWhiteSpacePos, endPos), true, new SubAccessor<>(backend, firstNonWhiteSpacePos));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Do the replacement"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tbackend.replace(endPos, end - endPos, newLineString + indentationString + SPACE_CHAR);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// We don't need to treat more line"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Format the comment text"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tformatLineText(lineText.substring(firstNonWhiteSpacePos, lineText.length()), true, new SubAccessor<>(backend, firstNonWhiteSpacePos));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate <T> boolean formatMultlineCommentOtherLines(String lineText, String indentationString, String newLineString, int endCommentOffset, FormattedTextAccessor<T> backend) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Search for the comment prefix (usually \" * \""); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint realCommentStart = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\twhile (realCommentStart < lineText.length() && Character.isWhitespace(lineText.charAt(realCommentStart))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t++realCommentStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tboolean foundStar = false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (realCommentStart < lineText.length() && startsWith(lineText, realCommentStart, getMultilineCommentLinePrefix())) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\trealCommentStart += getMultilineCommentLinePrefix().length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfoundStar = true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (realCommentStart < lineText.length() && Character.isWhitespace(lineText.charAt(realCommentStart))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t++realCommentStart;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Compute the standard prefix."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tStringBuilder prefix = new StringBuilder(indentationString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprefix.append(SPACE_CHAR);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprefix.append(getMultilineCommentLinePrefix());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprefix.append(makeWhiteSpaces(getWhiteSpacesOnOtherLines()));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Force replacement by the line's prefix"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint minBoundForEnd = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (endCommentOffset > lineText.length() || foundStar || realCommentStart < endCommentOffset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tbackend.replace(0, realCommentStart, prefix.toString());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (foundStar) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tminBoundForEnd = prefix.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Format the comment text"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (endCommentOffset <= lineText.length()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// End of comment on the current line."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint endPosition = endCommentOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal int end = endPosition;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile ((endPosition - 1) >= minBoundForEnd && Character.isWhitespace(lineText.charAt(endPosition - 1))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t--endPosition;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (endPosition > 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Comment end with a text before. Insert a newline character"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbackend.replace(endPosition, end - endPosition, newLineString + indentationString + SPACE_CHAR);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Replace spaces before end of comment if they exist"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbackend.replace(endPosition, end - endPosition, indentationString + SPACE_CHAR);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// We don't need to treat more line"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate <T> void formatMultlineComment(String indentationString, String newLineString, FormattedTextAccessor<T> backend) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String indent = Strings.emptyIfNull(indentationString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String comment = backend.getCommentText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Compute the starting offset of the text inside the comment"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint offset = comment.indexOf(getMultilineCommentStartSymbols());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (offset < 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tbackend.replace(0, 0, getMultilineCommentStartSymbols());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\toffset = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\toffset += getMultilineCommentStartSymbols().length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Compute the ending offset of the text inside the comment"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint endOffset = comment.indexOf(getMultilineCommentEndSymbols(), offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (endOffset < 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tendOffset = comment.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbackend.replace(endOffset, 0, getMultilineCommentEndSymbols());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Go through the lines"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tT currentLine = backend.getFirstLine(backend.getCommentOffset());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tboolean firstLine = true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (currentLine != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tString lineText = backend.getLineText(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tint lineOffset = backend.getLineOffset(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tint lineLength = backend.getLineLength(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Clamp the line text to the comment area."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (lineOffset < offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tfinal int len = offset - lineOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineText = lineText.substring(len);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineOffset += len;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineLength -= len;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif ((lineOffset + lineLength) > endOffset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tfinal int len = lineOffset + lineLength - endOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineText = lineText.substring(0, lineText.length() - len);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tlineLength -= len;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (firstLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tif (formatMultlineCommentFirstLine(lineText, indent, newLineString, endOffset - lineOffset, new SubAccessor(backend, lineOffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tbackend.applyReplacements();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\treturn;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (formatMultlineCommentOtherLines(lineText, indent, newLineString, endOffset - lineOffset, new SubAccessor(backend, lineOffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.applyReplacements();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\treturn;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfirstLine = false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcurrentLine = backend.getNextLine(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tbackend.applyReplacements();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLine();
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic interface FormattedTextAccessor<T> {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tT getFirstLine(int offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tT getNextLine(T currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint getLineOffset(T currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint getLineLength(T currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString getLineText(T line);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString getCommentText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint getCommentOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint getCommentEndOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tReplacement replace(int offset, int length, String newText);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tvoid applyReplacements();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic class SubAccessor<T> implements FormattedTextAccessor<T> {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final FormattedTextAccessor<T> parent;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int offsetInParent;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic SubAccessor(FormattedTextAccessor<T> parent, int offsetInParent) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tassert parent != null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.parent = parent;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.offsetInParent = offsetInParent;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic T getFirstLine(int offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.getFirstLine(offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic T getNextLine(T currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.getNextLine(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getLineOffset(T currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.getLineOffset(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getLineLength(T currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.getLineLength(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic String getLineText(T line) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.getLineText(line);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic String getCommentText() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.getCommentText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getCommentOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.getCommentOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getCommentEndOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.getCommentEndOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic Replacement replace(int offset, int length, String newText) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.parent.replace(this.offsetInParent + offset, length, newText);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic final void applyReplacements() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthrow new UnsupportedOperationException();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic static class Line {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int length;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic static Line newInstance(String text, int offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (offset < 0 || offset >= text.length()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint soffset = offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (soffset >= 0 && !isNewLine(text.charAt(soffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t--soffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t++soffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint eoffset = soffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (eoffset < text.length() && !isNewLine(text.charAt(eoffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t++eoffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal int length = "); //$NON-NLS-1$
				it.append(Math.class);
				it.append(".max(0, eoffset - soffset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn new Line(soffset, length);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate Line(int offset, int length) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.offset = offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.length = length;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getLength() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.length;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic String toString() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn \"offset: \" + getOffset() + \"; length: \" + getLength();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic static abstract class AbstractReplacementAccessor<T> implements FormattedTextAccessor<T> {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final String documentation;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate "); //$NON-NLS-1$
				it.append(SortedMap.class);
				it.append("<Integer, Replacement> replacements;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate boolean applied;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic AbstractReplacementAccessor(String documentation, "); //$NON-NLS-1$
				it.append(SortedMap.class);
				it.append("<Integer, Replacement> replacements) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.documentation = documentation;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.replacements = replacements;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprotected final void checkNotApplied() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (this.applied) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthrow new IllegalStateException(\"Changes are already applied\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.applied = true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic String getCommentText() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.documentation;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprotected "); //$NON-NLS-1$
				it.append(SortedMap.class);
				it.append("<Integer, Replacement> getReplacements() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (this.replacements == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.replacements = new "); //$NON-NLS-1$
				it.append(TreeMap.class);
				it.append("<>();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.replacements;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic Replacement replace(int offset, int length, String newText) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tReplacement rep = getReplacements().remove(offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (rep == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\trep = new Replacement(offset, length, newText);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\trep = new Replacement(offset, rep.getLength() + length, rep.getText() + newText);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tgetReplacements().put(offset, rep);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn rep;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprotected static void applyReplacements("); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable, String text, "); //$NON-NLS-1$
				it.append(Map.class);
				it.append("<Integer, Replacement> replacements) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint offset = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfor (final Replacement replacement : replacements.values()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (replacement.getOffset() < offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tappendable.append(\"<<<Conflicting replacements>>>\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tassert offset >= 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tassert replacement.getOffset() <= text.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tString notReplacedString = text.substring(offset, replacement.getOffset());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tappendable.append(notReplacedString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\toffset += notReplacedString.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tappendable.append(replacement.getText());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\toffset += replacement.getLength();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (offset < text.length()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tString notReplacedString = text.substring(offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tappendable.append(notReplacedString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic static abstract class AbstractDebuggingAccessor<T> extends AbstractReplacementAccessor<T> {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate String buffer;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic AbstractDebuggingAccessor(String text, "); //$NON-NLS-1$
				it.append(SortedMap.class);
				it.append("<Integer, Replacement> replacements) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tsuper(text, replacements);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate String computeBuffer() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable = new "); //$NON-NLS-1$
				it.append(FakeTreeAppendable.class);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tapplyReplacements(appendable, getCommentText(), getReplacements());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn appendable.getContent();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic String toString() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (this.buffer == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.buffer = computeBuffer();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.buffer;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic Replacement replace(int offset, int length, String newText) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal Replacement rep = super.replace(offset, length, newText);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.buffer = computeBuffer();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn rep;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic static class RegionAccessor extends AbstractReplacementAccessor<"); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append("> {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(ITextReplacerContext.class);
				it.append(" context;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(ITextRegionAccess.class);
				it.append(" access;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(IComment.class);
				it.append(" comment;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic RegionAccessor("); //$NON-NLS-1$
				it.append(ITextReplacerContext.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(IComment.class);
				it.append(" comment) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tsuper(comment.getText(), null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.context = context;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.comment = comment;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.access = comment.getTextRegionAccess();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic String getCommentText() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.comment.getText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic String getLineText("); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append(" line) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(ITextSegment.class);
				it.append(" segment = this.access.regionForOffset(line.getOffset(), line.getLength());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn segment.getText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getCommentOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.comment.getOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getCommentEndOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.comment.getEndOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic "); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append(" getFirstLine(int offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.access.regionForLineAtOffset(offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic "); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append(" getNextLine("); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append(" currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn currentLine.getNextLine();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getLineOffset("); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append(" currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn currentLine.getOffset() - getCommentOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getLineLength("); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append(" currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn currentLine.getLength();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void applyReplacements() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcheckNotApplied();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfor (Replacement replacement : getReplacements().values()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(ITextSegment.class);
				it.append(" target = this.access.regionForOffset(replacement.getOffset() + getCommentOffset(), replacement.getLength());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.context.addReplacement(target.replaceWith(replacement.getText()));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static class AppendableAccessor extends AbstractReplacementAccessor<Line> {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" target;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int commentOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int commentEndOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic AppendableAccessor("); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" target, String documentation, "); //$NON-NLS-1$
				it.append(SortedMap.class);
				it.append("<Integer, Replacement> replacements, int commentOffset, int commentEndOffset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tsuper(documentation, replacements);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.target = target;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.commentOffset = commentOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.commentEndOffset = commentEndOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic Line getFirstLine(int offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn Line.newInstance(getCommentText(), offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic Line getNextLine(Line currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint index = getCommentText().indexOf(NL_CHAR, currentLine.getOffset());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (index < 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn Line.newInstance(getCommentText(), index + 1);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getLineOffset(Line currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn currentLine.getOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getLineLength(Line currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn currentLine.getLength();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic String getLineText(Line line) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal int offset = line.getOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn getCommentText().substring(offset, offset + line.getLength());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getCommentOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.commentOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getCommentEndOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.commentEndOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void applyReplacements() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcheckNotApplied();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tapplyReplacements(this.target, getCommentText(), getReplacements());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic static class Replacement {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int length;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final String text;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic Replacement(int offset, int length, String text) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.offset = offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.length = length;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.text = text;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getLength() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.length;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic String getText() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.text;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic String toString() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn \"offset: \" + getOffset() + \"; length: \" + getLength() + \"; new text: \" + getText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess createJavaFile = getFileAccessFactory().createJavaFile(formatter, content);
		createJavaFile.writeTo(getSrcGen());
	}

	/** Generate the implementation for the documentation builder.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateEcoreDocumentationBuilderImpl() {
		final TypeReference builder = getEcoreDocumentationBuilderImpl();
		StringConcatenationClient content = new StringConcatenationClient() {
			@SuppressWarnings("synthetic-access")
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Build a documentation string."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(builder.getSimpleName());
				it.append(" implements "); //$NON-NLS-1$
				it.append(getIEcoreDocumentationBuilder());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" mlRule;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" slRule;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String mlStartSymbols;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String mlEndTagSymbols;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String slStartSymbols;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(getIDocumentationFormatter());
				it.append(" documentationFormatter;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tpublic void setGrammarAccess("); //$NON-NLS-1$
				it.append(DocumentationBuilderFragment.this.grammarAccessExtensions.getGrammarAccess(getGrammar()));
				it.append(" access) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.mlRule = access.getML_COMMENTRule();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.slRule = access.getSL_COMMENTRule();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor ("); //$NON-NLS-1$
				it.append(AbstractElement.class);
				it.append(" element : (("); //$NON-NLS-1$
				it.append(Group.class);
				it.append(") this.mlRule.getAlternatives()).getElements()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (element instanceof "); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(" && "); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(this.mlStartSymbols)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.mlStartSymbols = (("); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") element).getValue();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else if (element instanceof "); //$NON-NLS-1$
				it.append(UntilToken.class);
				it.append(" && "); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(this.mlEndTagSymbols)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.mlEndTagSymbols = (("); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") (("); //$NON-NLS-1$
				it.append(UntilToken.class);
				it.append(") element).getTerminal()).getValue();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" slRule = access.getSL_COMMENTRule();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor ("); //$NON-NLS-1$
				it.append(AbstractElement.class);
				it.append(" element : (("); //$NON-NLS-1$
				it.append(Group.class);
				it.append(") slRule.getAlternatives()).getElements()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (element instanceof "); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.slStartSymbols = (("); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") element).getValue().trim();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" getMLCommentRule() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.mlRule;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" getSLCommentRule() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.slRule;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getIDocumentationFormatter());
				it.append(" getDocumentationFormatter() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.documentationFormatter;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic boolean isMultilineCommentFor(Class<?> type) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn "); //$NON-NLS-1$
				Set<String> multilineCommentedTypes = getCodeBuilderConfig().getMultilineCommentedTypes();
				if (multilineCommentedTypes.isEmpty()) {
					it.append("false"); //$NON-NLS-1$
				} else {
					boolean firstTest = true;
					for (String typeName : multilineCommentedTypes) {
						if (firstTest) {
							firstTest = false;
						} else {
							it.newLine();
							it.append("\t\t\t\t|| "); //$NON-NLS-1$
						}
						TypeReference reference = new TypeReference(typeName);
						it.append(reference);
						it.append(".class.isAssignableFrom(type)"); //$NON-NLS-1$
					}
				}
				it.append(";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String build(String doc, Class<?> objectType) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString givenDocumentation = "); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".emptyIfNull(doc).trim();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tStringBuilder documentation = new StringBuilder();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(getIDocumentationFormatter());
				it.append(" formatter = getDocumentationFormatter();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (isMultilineCommentFor(objectType)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!givenDocumentation.startsWith(this.mlStartSymbols)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdocumentation.append(this.mlStartSymbols);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tdocumentation.append(givenDocumentation);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!givenDocumentation.endsWith(this.mlEndTagSymbols)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdocumentation.append(this.mlEndTagSymbols);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn formatter.formatMultilineComment(documentation.toString());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tdocumentation.append(\"\\n\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!givenDocumentation.startsWith(this.slStartSymbols)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tdocumentation.append(this.slStartSymbols);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tdocumentation.append(givenDocumentation);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!givenDocumentation.isEmpty() && !isNewLine(givenDocumentation.charAt(givenDocumentation.length() - 1))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tdocumentation.append(\"\\n\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn formatter.formatSinglelineComment(documentation.toString());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static boolean isNewLine(char character) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (character == '\\n' || character == '\\r' || character == '\\f') {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn ((((1 << "); //$NON-NLS-1$
				it.append(Character.class);
				it.append(".LINE_SEPARATOR)"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t| (1 << "); //$NON-NLS-1$
				it.append(Character.class);
				it.append(".PARAGRAPH_SEPARATOR)) >> "); //$NON-NLS-1$
				it.append(Character.class);
				it.append(".getType((int) character)) & 1) != 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess createJavaFile = getFileAccessFactory().createJavaFile(builder, content);
		createJavaFile.writeTo(getSrcGen());
	}

	/** Generate the implementation for the documentation provider.
	 */
	protected void generateDocumentationProviderImpl() {
		TypeReference etype = null;
		final LinkedList<Grammar> grammars = new LinkedList<>();
		grammars.addAll(getGrammar().getUsedGrammars());
		while (etype == null && !grammars.isEmpty()) {
			final Grammar grammar = grammars.removeFirst();
			grammars.addAll(grammar.getUsedGrammars());
			final String gbase = getNaming().getRuntimeBasePackage(grammar);
			final String providerClassname = gbase + ".documentation." //$NON-NLS-1$
					+ GrammarUtil.getSimpleName(grammar) + "DocumentationProvider"; //$NON-NLS-1$
			try {
				getClass().getClassLoader().loadClass(providerClassname);
				etype = new TypeReference(providerClassname);
			} catch (Throwable exception) {
				//
			}
		}
		if (etype == null) {
			etype = new TypeReference(MultiLineCommentDocumentationProvider.class);
		}
		final TypeReference extendType = etype;
		final TypeReference provider = getDocumentationProviderImpl();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Provider a documentation string."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(provider.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(extendType);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic String getDocumentation("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" o) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Get the documentation from the Xtext grammar hidden nodes."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString text = super.getDocumentation(o);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (text == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Get the grammar from the Ecore model element."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (o instanceof "); //$NON-NLS-1$
				it.append(EModelElement.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\ttext = "); //$NON-NLS-1$
				it.append(EcoreUtil.class);
				it.append(".getDocumentation(("); //$NON-NLS-1$
				it.append(EModelElement.class);
				it.append(") o);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (text == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Get the grammar from the code builder extension."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(DocumentationAdapter.class);
				it.append(" adapter = ("); //$NON-NLS-1$
				it.append(DocumentationAdapter.class);
				it.append(") "); //$NON-NLS-1$
				it.append(EcoreUtil.class);
				it.append(".getAdapter("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\to.eAdapters(), "); //$NON-NLS-1$
				it.append(DocumentationAdapter.class);
				it.append(".class);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (adapter != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\treturn adapter.getDocumentation();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn text;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		final JavaFileAccess createJavaFile = getFileAccessFactory().createJavaFile(provider, content);
		createJavaFile.writeTo(getSrcGen());
	}

	/** Generate the syntactic sequencer that supports Ecore documentation.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateEcoreDocumentationSyntacticSequencer() {
		final TypeReference sequencer = getSyntacticSequencer();
		final TypeReference customSequencer = getEcoreDocumentationSyntacticSequencer();
		final TypeReference innerBlockComment = getCodeElementExtractor().getInnerBlockDocumentationAdapter();
		final TypeReference keywordAccessor = getCodeElementExtractor().getLanguageKeywordAccessor();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Syntactic sequencer which supports documentations of Ecore elements."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(customSequencer.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(sequencer);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate final "); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<"); //$NON-NLS-1$
				it.append(EObject.class);
				it.append("> documentedSemanticObjects = new "); //$NON-NLS-1$
				it.append(HashSet.class);
				it.append("<>();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate final "); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<"); //$NON-NLS-1$
				it.append(EObject.class);
				it.append("> indocumentedSemanticObjects = new "); //$NON-NLS-1$
				it.append(HashSet.class);
				it.append("<>();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(innerBlockComment);
				it.append(" lastInnerBlock;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(getIEcoreDocumentationBuilder());
				it.append(" documentationBuilder;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(keywordAccessor);
				it.append(" keywords;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(ISequenceAcceptor.class);
				it.append(" trailingSequenceAcceptor;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic void init("); //$NON-NLS-1$
				it.append(ISerializationContext.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" semanticObject,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(ISyntacticSequenceAcceptor.class);
				it.append(" sequenceAcceptor, "); //$NON-NLS-1$
				it.append(Acceptor.class);
				it.append(" errorAcceptor) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tsuper.init(context, semanticObject, sequenceAcceptor, errorAcceptor);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (sequenceAcceptor instanceof "); //$NON-NLS-1$
				it.append(ISequenceAcceptor.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.trailingSequenceAcceptor = ("); //$NON-NLS-1$
				it.append(ISequenceAcceptor.class);
				it.append(") sequenceAcceptor;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.documentedSemanticObjects.clear();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.indocumentedSemanticObjects.clear();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.lastInnerBlock = null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(ISequenceAcceptor.class);
				it.append(" getTrailingSequenceAcceptor() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (this.trailingSequenceAcceptor == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\ttry {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(Field.class);
				it.append(" delegateField = "); //$NON-NLS-1$
				it.append(HiddenTokenSequencer.class);
				it.append(".class.getDeclaredField(\"delegate\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdelegateField.setAccessible(true);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.trailingSequenceAcceptor = ("); //$NON-NLS-1$
				it.append(ISequenceAcceptor.class);
				it.append(") delegateField.get(this.delegate);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} catch (Throwable exception) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthrow new RuntimeException(exception);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.trailingSequenceAcceptor;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected void emitDocumentation(Class<?> semanticObjectType, String comment) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String fmtcomment = this.documentationBuilder.build(comment, semanticObjectType);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(fmtcomment)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal "); //$NON-NLS-1$
				it.append(AbstractRule.class);
				it.append(" rule = this.documentationBuilder.isMultilineCommentFor(semanticObjectType) ? this.documentationBuilder.getMLCommentRule() : this.documentationBuilder.getSLCommentRule();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tgetTrailingSequenceAcceptor().acceptComment(rule, fmtcomment, null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected void emitDocumentation("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" semanticObject) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (this.documentedSemanticObjects.add(semanticObject)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(DocumentationAdapter.class);
				it.append(" documentationAdapter = ("); //$NON-NLS-1$
				it.append(DocumentationAdapter.class);
				it.append(") "); //$NON-NLS-1$
				it.append(EcoreUtil.class);
				it.append(".getAdapter(semanticObject.eAdapters(), "); //$NON-NLS-1$
				it.append(DocumentationAdapter.class);
				it.append(".class);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (documentationAdapter != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\temitDocumentation(semanticObject.getClass(), documentationAdapter.getDocumentation());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected void emitInnerDocumentation("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" semanticObject) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (this.indocumentedSemanticObjects.add(semanticObject)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(innerBlockComment);
				it.append(" documentationAdapter = ("); //$NON-NLS-1$
				it.append(innerBlockComment);
				it.append(") "); //$NON-NLS-1$
				it.append(EcoreUtil.class);
				it.append(".getAdapter(semanticObject.eAdapters(), "); //$NON-NLS-1$
				it.append(innerBlockComment);
				it.append(".class);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (documentationAdapter != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\temitDocumentation(semanticObject.getClass(), documentationAdapter.getDocumentation());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(innerBlockComment);
				it.append(" getInnerDocumentation(EObject semanticObject) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (this.indocumentedSemanticObjects.add(semanticObject)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn ("); //$NON-NLS-1$
				it.append(innerBlockComment);
				it.append(") EcoreUtil.getAdapter(semanticObject.eAdapters(), "); //$NON-NLS-1$
				it.append(innerBlockComment);
				it.append(".class);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected void emitUnassignedTokens("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" semanticObject, "); //$NON-NLS-1$
				it.append(ISynTransition.class);
				it.append(" transition,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(INode.class);
				it.append(" fromNode, "); //$NON-NLS-1$
				it.append(INode.class);
				it.append(" toNode) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tsuper.emitUnassignedTokens(semanticObject, transition, fromNode, toNode);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\temitDocumentation(semanticObject);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (semanticObject instanceof "); //$NON-NLS-1$
				it.append(XBlockExpression.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.lastInnerBlock = getInnerDocumentation(semanticObject);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected void accept("); //$NON-NLS-1$
				it.append(ISynState.class);
				it.append(" emitter, "); //$NON-NLS-1$
				it.append(INode.class);
				it.append(" node, "); //$NON-NLS-1$
				it.append(RuleCallStack.class);
				it.append(" stack) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tsuper.accept(emitter, node, stack);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(innerBlockComment);
				it.append(" documentation = this.lastInnerBlock;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (documentation != null && emitter.getType() == "); //$NON-NLS-1$
				it.append(SynStateType.class);
				it.append(".UNASSIGEND_KEYWORD) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(" keyword = ("); //$NON-NLS-1$
				it.append(Keyword.class);
				it.append(") emitter.getGrammarElement();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tString token = node != null ? node.getText() : keyword.getValue();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif ("); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".equal(token, this.keywords.getLeftCurlyBracketKeyword())) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.lastInnerBlock = null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\temitDocumentation(documentation.getTarget().getClass(), documentation.getDocumentation());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess createJavaFile = getFileAccessFactory().createJavaFile(customSequencer, content);
		createJavaFile.writeTo(getSrcGen());
	}
}

