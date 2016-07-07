/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

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
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.serializer.ISerializationContext;
import org.eclipse.xtext.serializer.acceptor.ISequenceAcceptor;
import org.eclipse.xtext.serializer.acceptor.ISyntacticSequenceAcceptor;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynTransition;
import org.eclipse.xtext.serializer.diagnostic.ISerializationDiagnostic.Acceptor;
import org.eclipse.xtext.serializer.sequencer.HiddenTokenSequencer;
import org.eclipse.xtext.serializer.sequencer.ISyntacticSequencer;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;
import org.eclipse.xtext.xbase.compiler.IAppendable;
import org.eclipse.xtext.xbase.compiler.StringBuilderBasedAppendable;
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
		return new TypeReference(getDocumentationPackage() + ".IEcoreDocumentationBuilder"); //$NON-NLS-1$
	}

	/** Replies the implementation for the documentation builder.
	 *
	 * @return the documentation builder implementation.
	 */
	@Pure
	public TypeReference getEcoreDocumentationBuilderImpl() {
		return new TypeReference(getDocumentationPackage() + ".EcoreDocumentationBuilder"); //$NON-NLS-1$
	}

	/** Replies the interface for the documentation formatter.
	 *
	 * @return the documentation formatter interface.
	 */
	@Pure
	public TypeReference getIDocumentationFormatter() {
		return new TypeReference(getDocumentationPackage() + ".IDocumentationFormatter"); //$NON-NLS-1$
	}

	/** Replies the implementation for the documentation formatter.
	 *
	 * @return the documentation formatter implementation.
	 */
	@Pure
	public TypeReference getDocumentationFormatterImpl() {
		return new TypeReference(getDocumentationPackage() + ".DocumentationFormatter"); //$NON-NLS-1$
	}

	/** Replies the implementation for the syntactic sequencer supporting Ecore documentation.
	 *
	 * @return the syntactic sequencer implementation.
	 */
	@Pure
	public TypeReference getEcoreDocumentationSyntacticSequencer() {
		return new TypeReference(getSerializerPackage() + "." //$NON-NLS-1$
				+ getLanguageName().toUpperCase() + "EcoreDocumentationSyntacticSequencer"); //$NON-NLS-1$
	}

	/** Replies the implementation for the custom syntactic sequencer supporting Ecore documentation.
	 *
	 * @return the syntactic sequencer implementation.
	 */
	@Pure
	public TypeReference getEcoreDocumentationSyntacticSequencerCustom() {
		return new TypeReference(getSerializerPackage() + "." //$NON-NLS-1$
				+ getLanguageName().toUpperCase() + "EcoreDocumentationSyntacticSequencerCustom"); //$NON-NLS-1$
	}

	/** Replies the implementation for the syntactic sequencer.
	 *
	 * @return the syntactic sequencer implementation.
	 */
	@Pure
	public TypeReference getSyntacticSequencer() {
		return new TypeReference(getSerializerPackage() + "." //$NON-NLS-1$
				+ getLanguageName().toUpperCase() + "SyntacticSequencer"); //$NON-NLS-1$
	}

	/** Replies the implementation for the documentation provider.
	 *
	 * @return the documentation provider implementation.
	 */
	@Pure
	public TypeReference getDocumentationProviderImpl() {
		return new TypeReference(getDocumentationPackage() + "." //$NON-NLS-1$
				+ getLanguageName() + "DocumentationProvider"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the documentation provider.
	 *
	 * @return the custom documentation provider implementation.
	 */
	@Pure
	public TypeReference getDocumentationProviderImplCustom() {
		return new TypeReference(getDocumentationPackage() + "." //$NON-NLS-1$
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
			exportedPackages.add(getSerializerPackage());
			exportedPackages.add(getDocumentationPackage());
		}
	}

	@Override
	public void generateBindings(BindingFactory factory) {
		super.generateBindings(factory);

		factory.addfinalTypeToType(getIDocumentationFormatter(), getDocumentationFormatterImpl());
		factory.addfinalTypeToTypeSingleton(getIEcoreDocumentationBuilder(), getEcoreDocumentationBuilderImpl());

		final IFileSystemAccess2 fileSystem = getSrc();
		TypeReference type;
		if ((fileSystem.isFile(getDocumentationProviderImplCustom().getJavaPath()))
				|| (fileSystem.isFile(getDocumentationProviderImplCustom().getXtendPath()))) {
			type = getDocumentationProviderImplCustom();
		} else {
			type = getDocumentationProviderImpl();
		}
		factory.addfinalTypeToTypeSingleton(new TypeReference(IEObjectDocumentationProvider.class), type);
		factory.addfinalTypeToTypeSingleton(new TypeReference(IEObjectDocumentationProviderExtension.class), type);

		if ((fileSystem.isFile(getEcoreDocumentationSyntacticSequencerCustom().getJavaPath()))
				|| (fileSystem.isFile(getEcoreDocumentationSyntacticSequencerCustom().getXtendPath()))) {
			type = getEcoreDocumentationSyntacticSequencerCustom();
		} else {
			type = getEcoreDocumentationSyntacticSequencer();
		}
		factory.addfinalTypeToType(new TypeReference(ISyntacticSequencer.class), type);
	}

	/** Generate the adapter that supports the inner documentation.
	 */
	protected void generateInnerDocumentationAdapter() {
		final TypeReference adapter = getInnerBlockDocumentationAdapter();
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
				it.append("\tprivate String mlLinePrefix;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String mlStart;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String mlEnd;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String slPrefix;"); //$NON-NLS-1$
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
				it.append("\t/** Change the characters that must be used to start a comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param symbol the start comment symbols."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
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
				it.append("\t/** Change the characters that must be used to end a comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param symbol the end comment symbols."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
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
				it.append("\t/** Change the line prefix for the multiline comments."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param prefix the new prefix."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
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
				it.append("\t/** Change the line prefix for the singleline comments."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param prefix the new prefix."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
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
				it.append(Map.class);
				it.append("<Integer, "); //$NON-NLS-1$
				it.append(List.class);
				it.append("<Replacement>> replacements = new "); //$NON-NLS-1$
				it.append(TreeMap.class);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tformatMultlineComment("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tindentation,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".newLine(),"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tnew AppendableBackend(doc, replacements, 0, doc.length()));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tapplyReplacements(appendable, doc, replacements);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate void applyReplacements("); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable, String documentation, "); //$NON-NLS-1$
				it.append(Map.class);
				it.append("<Integer, "); //$NON-NLS-1$
				it.append(List.class);
				it.append("<Replacement>> replacements) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint offset = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor ("); //$NON-NLS-1$
				it.append(List.class);
				it.append("<Replacement> replacementList : replacements.values()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfor (Replacement replacement : replacementList) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (replacement.getOffset() < offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tthrow new IllegalStateException(\"replacements are overlapping\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (replacement.getOffset() > offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tString notReplacedString = documentation.substring(offset, replacement.getOffset());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tappendable.append(notReplacedString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\toffset = replacement.getOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tappendable.append(replacement.getText());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\toffset += replacement.getLength();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (offset < documentation.length()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tString notReplacedString = documentation.substring(offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tappendable.append(notReplacedString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the string that should appear at the start of each documentation line."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param prefix the default prefix text."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param isFirstLine indicates if the prefix string is for the first line of the documentation,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t *    or the other lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param isLastLine indicates if the prefix string is for the last line of the documentation,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t *    or the other lines."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the real prefix text."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected String getLinePrefix(String prefix, boolean isFirstLine, boolean isLastLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (isFirstLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn \" \";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t} else if (isLastLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn \" \" + prefix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn \" \" + prefix + \" \";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the string that should appear at the end of each documentation line."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param postfix the default postfix text."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the real postfix text."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@Pure"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprotected String getLinePostfix(String postfix) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn postfix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Format a line of comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appendable the result of the format."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param line the line to format."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param prefix the expected prefix text for the line."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param postfix the expected postfix text for the line."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected void formatLine("); //$NON-NLS-1$
				it.append(IAppendable.class);
				it.append(" appendable, String line, String prefix, String postfix) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString simpleLine = (line == null) ? \"\" : line.trim();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString simplePrefix = (prefix == null) ? \"\" : prefix.trim();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString simplePostfix = (postfix == null) ? \"\" : postfix.trim();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (prefix != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tappendable.append(prefix);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(simplePrefix) && simpleLine.startsWith(simplePrefix)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint index = simplePrefix.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (index < simpleLine.length() && Character.isSpaceChar(simpleLine.charAt(index))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t++index;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tsimpleLine = simpleLine.substring(index);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!Strings.isEmpty(simplePostfix) && simpleLine.endsWith(simplePostfix)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint index = simpleLine.length() - simplePostfix.length() - 1;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (index < simpleLine.length() && Character.isSpaceChar(simpleLine.charAt(index))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t--index;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tsimpleLine = simpleLine.substring(0, index + 1);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tappendable.append(simpleLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (postfix != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tappendable.append(postfix);"); //$NON-NLS-1$
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
				it.append("\t\tformatMultlineComment("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tcontext.getIndentationString(),"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tcontext.getNewLinesString(1),"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tnew RegionBackend(context, comment));"); //$NON-NLS-1$
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
				it.append("\tpublic String formatSinglelineComment(String doc, String indentation) {"); //$NON-NLS-1$
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
				it.append(Map.class);
				it.append("<Integer, "); //$NON-NLS-1$
				it.append(List.class);
				it.append("<Replacement>> replacements = new "); //$NON-NLS-1$
				it.append(TreeMap.class);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tformatSinglelineComment("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tindentation,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tnew AppendableBackend(doc, replacements,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tMath.max(0, doc.indexOf(getSinglelineCommentPrefix())),"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tdoc.length()));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tapplyReplacements(appendable, doc, replacements);"); //$NON-NLS-1$
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
				it.append("\t\tformatSinglelineComment("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tcontext.getIndentationString(),"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tnew RegionBackend(context, comment));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate <T> void formatSinglelineComment("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tString indentationString,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tFormatterBackend<T> backend) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString indent = "); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".emptyIfNull(indentationString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString prefix = getSinglelineCommentPrefix();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tT currentLine = backend.getFirstLine(backend.getCommentOffset());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tboolean firstLine = true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\twhile (currentLine != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint lineOffset = backend.getLineOffset(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (lineOffset >= backend.getCommentEndOffset()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Ok, break is not the best statement, but it makes the code easier to read."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tString lineText = backend.getLineText(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcurrentLine = backend.getNextLine(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint symbolOffset = lineText.indexOf(prefix);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint textZoneOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (symbolOffset >= 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (firstLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tfirstLine = false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(symbolOffset + lineOffset, 0, indent);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\ttextZoneOffset = symbolOffset + prefix.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (firstLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tfirstLine = false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(lineOffset, 0, prefix);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(lineOffset, 0, indent + prefix);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\ttextZoneOffset = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint textOffset = textZoneOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (textOffset < lineText.length() && isSpaceChar(lineText.charAt(textOffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t++textOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif ((textOffset >= lineText.length()) || Character.isWhitespace(lineText.charAt(textOffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// No text in the comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (textOffset != textZoneOffset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t// Remove trailing spaces"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(lineOffset + textZoneOffset, textOffset - textZoneOffset, \"\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Text in comment."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif ((textZoneOffset + 1) != textOffset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t// Fixing the invalid number of spaces after the start symbols."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(lineOffset + textZoneOffset, textOffset - textZoneOffset, \" \");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Remove trailing white spaces."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tint endTextOffset = lineText.length() - 1;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\twhile (endTextOffset > textOffset && isSpaceChar(lineText.charAt(endTextOffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t--endTextOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t++endTextOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (endTextOffset != lineText.length()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace(lineOffset + endTextOffset, lineText.length() - endTextOffset, \"\");"); //$NON-NLS-1$
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
				it.append("\tprivate static boolean isSpaceChar(char character) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn (((1 << Character.SPACE_SEPARATOR) >> Character.getType((int) character)) & 1) != 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate <T> void formatMultlineComment("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tString indentationString, String newLineString,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tFormatterBackend<T> backend) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String indent = "); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".emptyIfNull(indentationString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String postfix = getLinePostfix(null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String startSymbols = getMultilineCommentStartSymbols();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String fullText = backend.getCommentText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal int commentOffset = backend.getCommentOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal int endCommentOffset = backend.getCommentEndOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint startOffset = fullText.indexOf(startSymbols);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (startOffset < 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tbackend.replace(0, 0, startSymbols);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tstartOffset = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tstartOffset += startSymbols.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String endSymbols = getMultilineCommentEndSymbols();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint endOffset = fullText.indexOf(endSymbols, startOffset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (endOffset < 0) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tendOffset = fullText.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal String startLineSymbols = getMultilineCommentLinePrefix();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tT currentLine = backend.getFirstLine(startOffset + commentOffset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tboolean firstLine = true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\twhile (currentLine != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint lineOffset = backend.getLineOffset(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (lineOffset >= endCommentOffset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Ok, break is not the best statement, but it makes the code easier to read."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint textStartOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tString referenceText = fullText.substring("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tMath.max(lineOffset - commentOffset, startOffset),"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tMath.min(backend.getLineEndOffset(currentLine) - commentOffset, endOffset));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Move to next line"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tcurrentLine = backend.getNextLine(currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(referenceText) || currentLine != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tString prefix;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (firstLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tfirstLine = false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\ttextStartOffset = 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t// Skip prefix symbols (for Javadoc-like comments)."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\twhile ((textStartOffset < referenceText.length())"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t&& referenceText.regionMatches(textStartOffset, startLineSymbols, 0, startLineSymbols.length())) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\ttextStartOffset += startLineSymbols.length();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\treferenceText = referenceText.substring(textStartOffset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tprefix = getLinePrefix(null, true, false);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\ttextStartOffset += Math.max(lineOffset - commentOffset, startOffset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\ttextStartOffset = Math.max(lineOffset - commentOffset, startOffset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tprefix = indent + getLinePrefix(startLineSymbols, false, false);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Format the line"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(StringBuilderBasedAppendable.class);
				it.append(" appendable = new "); //$NON-NLS-1$
				it.append(StringBuilderBasedAppendable.class);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tformatLine(appendable, referenceText, prefix, postfix);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tString newText = appendable.getContent();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t// Change the text"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".equal(referenceText, newText)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tbackend.replace("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\ttextStartOffset + commentOffset,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\treferenceText.length(),"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\tnewText);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// Format the closing symbols."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint textEndOffset = endOffset - 1;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\twhile (textEndOffset > startOffset"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t&& isSpaceChar(fullText.charAt(textEndOffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t--textEndOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString referenceText;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(StringBuilderBasedAppendable.class);
				it.append(" appendable = new "); //$NON-NLS-1$
				it.append(StringBuilderBasedAppendable.class);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (textEndOffset <= startOffset || !Character.isWhitespace(fullText.charAt(textEndOffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Do not find a new line."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tappendable.append(newLineString);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\ttextEndOffset = endOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treferenceText = endSymbols;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t++textEndOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treferenceText = fullText.substring(textEndOffset, endOffset) + endSymbols;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString prefix = indent + getLinePrefix(endSymbols, false, true);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tformatLine(appendable, \"\", prefix, null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString newText = appendable.getContent();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".equal(referenceText, newText)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tbackend.replace("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\ttextEndOffset + commentOffset,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\treferenceText.length(),"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tnewText);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate interface FormatterBackend<T> {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tT getFirstLine(int offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tT getNextLine(T currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint getLineOffset(T currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint getLineEndOffset(T currentLine);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tvoid replace(int offset, int length, String newText);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString getCommentText();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString getLineText(T line);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint getCommentOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tint getCommentEndOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static class Line {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int startOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate final int endOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic Line(String text, int offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Search for the begining of the line."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint soffset = offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (soffset >= 0 && !isNewLine(text.charAt(soffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t--soffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.startOffset = soffset + 1;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t// Search for the end of the line."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tint eoffset = offset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\twhile (eoffset < text.length() && !isNewLine(text.charAt(eoffset))) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t++eoffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.endOffset = eoffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getStartOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.startOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tpublic int getEndOffset() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.endOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tprivate static boolean isNewLine(char character) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (character == '\\n' || character == '\\r' || character == '\\f') {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn true;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn ((((1 << Character.LINE_SEPARATOR)"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t| (1 << Character.PARAGRAPH_SEPARATOR)) >> Character.getType((int) character)) & 1) != 0;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static class Replacement {"); //$NON-NLS-1$
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
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static class RegionBackend implements FormatterBackend<"); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append("> {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(ITextReplacerContext.class);
				it.append(" context;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(ITextRegionAccess.class);
				it.append(" access;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(IComment.class);
				it.append(" comment;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic RegionBackend("); //$NON-NLS-1$
				it.append(ITextReplacerContext.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(IComment.class);
				it.append(" comment) {"); //$NON-NLS-1$
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
				it.append(" getNextLine(ILineRegion currentLine) {"); //$NON-NLS-1$
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
				it.append("\t\t\treturn currentLine.getOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getLineEndOffset("); //$NON-NLS-1$
				it.append(ILineRegion.class);
				it.append(" currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn currentLine.getEndOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void replace(int offset, int length, String newText) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(ITextSegment.class);
				it.append(" target = this.access.regionForOffset(offset, length);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.context.addReplacement(target.replaceWith(newText));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static class AppendableBackend implements FormatterBackend<Line> {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final String documentation;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(Map.class);
				it.append("<Integer, "); //$NON-NLS-1$
				it.append(List.class);
				it.append("<Replacement>> replacements;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final int commentOffset;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final int commentEndOffset;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic AppendableBackend(String documentation, "); //$NON-NLS-1$
				it.append(Map.class);
				it.append("<Integer, "); //$NON-NLS-1$
				it.append(List.class);
				it.append("<Replacement>> replacements,"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tint commentOffset, int commentEndOffset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.documentation = documentation;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.replacements = replacements;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.commentOffset = commentOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.commentEndOffset = commentEndOffset;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic String getCommentText() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.documentation;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic String getLineText(Line line) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.documentation.substring(line.getStartOffset(), line.getEndOffset());"); //$NON-NLS-1$
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
				it.append("\t\tpublic Line getFirstLine(int offset) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn new Line(this.documentation, offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic Line getNextLine(Line currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\ttry {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn new Line(this.documentation, currentLine.getEndOffset() + 1);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} catch (Throwable exception) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getLineOffset(Line currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn currentLine.getStartOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic int getLineEndOffset(Line currentLine) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn currentLine.getEndOffset();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void replace(int offset, int length, String newText) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(List.class);
				it.append("<Replacement> list = this.replacements.get(offset);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (list == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tlist = new "); //$NON-NLS-1$
				it.append(ArrayList.class);
				it.append("<>();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.replacements.put(offset, list);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tlist.add(new Replacement(offset, length, newText));"); //$NON-NLS-1$
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
				it.append("\tprotected boolean isMultilineCommentFor(Class<?> type) {"); //$NON-NLS-1$
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
				it.append("\t\t\tif (!doc.startsWith(this.mlStartSymbols)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdocumentation.append(this.mlStartSymbols);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tdocumentation.append(givenDocumentation);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!doc.endsWith(this.mlEndTagSymbols)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tdocumentation.append(this.mlEndTagSymbols);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn formatter.formatMultilineComment(documentation.toString());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!doc.startsWith(this.slStartSymbols)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tdocumentation.append(this.slStartSymbols);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tdocumentation.append(givenDocumentation);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn formatter.formatSinglelineComment(documentation.toString());"); //$NON-NLS-1$
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
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(getIEcoreDocumentationBuilder());
				it.append(" documentationBuilder;"); //$NON-NLS-1$
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
				it.append(".getAdapter("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tsemanticObject.eAdapters(), "); //$NON-NLS-1$
				it.append(DocumentationAdapter.class);
				it.append(".class);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (documentationAdapter != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tString comment = documentationAdapter.getDocumentation();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tcomment = this.documentationBuilder.build(comment, semanticObject.getClass());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(comment)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tgetTrailingSequenceAcceptor().acceptComment("); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\tthis.documentationBuilder.getMLCommentRule(), comment, null);"); //$NON-NLS-1$
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
				it.append("\t}"); //$NON-NLS-1$
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

