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

package io.sarl.lang.mwe2.keywords;

import static org.eclipse.xtext.EcoreUtil2.eAllContentsAsList;
import static org.eclipse.xtext.EcoreUtil2.typeSelect;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.base.Objects;
import com.google.inject.Inject;
import com.google.inject.Injector;
import org.apache.log4j.Logger;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.EnumRule;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.ParserRule;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.XtextGeneratorNaming;
import org.eclipse.xtext.xtext.generator.grammarAccess.GrammarAccessExtensions;
import org.eclipse.xtext.xtext.generator.model.FileAccessFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create a code builder for the generated language.
 *
 * <p>The generated builder could be used for helping to create Eobjects from scratch
 * (in ui wizard for example).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class GrammarKeywordAccessFragment2 extends AbstractXtextGeneratorFragment {

	private static final Logger LOG = Logger.getLogger(GrammarKeywordAccessFragment2.class);

	@Inject
	private FileAccessFactory fileAccessFactory;

	@Inject
	private XtextGeneratorNaming naming;

	@Inject
	private GrammarAccessExtensions grammarAccessExtensions;

	@Inject
	private GrammarKeywordAccessConfig configuration;

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
		final Grammar grammar = getGrammar();
		final String basePackage = this.naming.getRuntimeBasePackage(grammar);
		return basePackage + ".services"; //$NON-NLS-1$
	}

	/** Replies the type of the accessor.
	 *
	 * @return the accessor's type.
	 */
	@Pure
	public TypeReference getAccessorType() {
		return new TypeReference(getBasePackage() + "." //$NON-NLS-1$
				+ getLanguageName().toUpperCase() + "GrammarKeywordAccess"); //$NON-NLS-1$
	}

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
	}

	@Override
	public void generate() {
		LOG.info("Generating the grammar keyword access for " + getLanguageName()); //$NON-NLS-1$
		final StringConcatenationClient content = new StringConcatenationClient() {
			@SuppressWarnings("synthetic-access")
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Set of SARL keywords that are not directly supported by the"); //$NON-NLS-1$
				it.newLine();
				it.append(" * {@link SARLGrammarAccess} or hardly accessible."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@"); //$NON-NLS-1$
				it.append(SuppressWarnings.class);
				it.append("(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(getAccessorType().getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(GrammarKeywordAccessFragment2.this.grammarAccessExtensions.getGrammarAccess(getGrammar()));
				it.append(" grammarAccess;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				final Set<String> addedKeywords = new HashSet<>();
				final LinkedList<Grammar> grammars = new LinkedList<>();
				grammars.add(getGrammar());
				while (!grammars.isEmpty()) {
					final Grammar grammar = grammars.removeFirst();
					if (isValidGrammar(grammar)) {
						it.append(generateMembers(grammar, addedKeywords));
						if (GrammarKeywordAccessFragment2.this.configuration.getDependencyGrammarInheritance()) {
							grammars.addAll(getEffectivelyUsedGrammars(grammar));
						}
					}
				}
				it.append(generateMembersFromConfig(addedKeywords));
				it.append("}"); //$NON-NLS-1$
				it.newLine();
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final JavaFileAccess javaFile = this.fileAccessFactory.createJavaFile(
				getAccessorType(), content);
		javaFile.writeTo(getProjectConfig().getRuntime().getSrcGen());
	}

	/** Replies if the given grammar may be treated.
	 *
	 * @param grammar the grammar to test.
	 * @return <code>true</code> if the grammar could be treated.
	 */
	@SuppressWarnings("static-method")
	protected boolean isValidGrammar(Grammar grammar) {
		return !Objects.equal(grammar.getName(), "org.eclipse.xtext.common.Terminals"); //$NON-NLS-1$
	}

	/** Generate the members of the accessors.
	 *
	 * @param addedKeywords the set of keywords that are added to the output.
	 * @return the content.
	 */
	protected StringConcatenationClient generateMembersFromConfig(Set<String> addedKeywords) {
		return new StringConcatenationClient() {
			@SuppressWarnings("synthetic-access")
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				final GrammarKeywordAccessConfig config = GrammarKeywordAccessFragment2.this.configuration;
				for (final String keyword : config.getKeywords()) {
					final String id = keyword.toLowerCase();
					if (!addedKeywords.contains(id) && !config.getIgnoredKeywords().contains(keyword)) {
						it.append(generateKeyword(keyword, getGrammar().getName()));
						addedKeywords.add(id);
					}
				}
				for (final String keyword : config.getLiterals()) {
					final String id = keyword.toLowerCase();
					if (!addedKeywords.contains(id) && !config.getIgnoredKeywords().contains(keyword)) {
						it.append(generateKeyword(keyword, getGrammar().getName()));
						addedKeywords.add(id);
					}
				}
			}
		};
	}

	/** Generate the members of the accessors.
	 *
	 * @param grammar the grammar for which the keyword accessors must be generated.
	 * @param addedKeywords the set of keywords that are added to the output.
	 * @return the content.
	 */
	protected StringConcatenationClient generateMembers(Grammar grammar, Set<String> addedKeywords) {
		return new StringConcatenationClient() {
			@SuppressWarnings("synthetic-access")
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				final GrammarKeywordAccessConfig config = GrammarKeywordAccessFragment2.this.configuration;
				for (final Keyword grammarKeyword : getAllKeywords(grammar)) {
					final String keyword = grammarKeyword.getValue().trim();
					if (!keyword.isEmpty()) {
						final String id = keyword.toLowerCase();
						if (!addedKeywords.contains(id) && !config.getIgnoredKeywords().contains(keyword)) {
							it.append(generateKeyword(grammarKeyword, grammar.getName()));
							addedKeywords.add(id);
						}
					}
				}
			}
		};
	}

	/** Replies the keywords in the given grammar.
	 *
	 * @param grammar the grammar.
	 * @return the keywords.
	 */
	protected static Iterable<Keyword> getAllKeywords(Grammar grammar) {
		final Map<String, Keyword> keywords = new HashMap<>();
		final List<ParserRule> rules = GrammarUtil.allParserRules(grammar);
		for (final ParserRule parserRule : rules) {
			final List<Keyword> list = typeSelect(eAllContentsAsList(parserRule), Keyword.class);
			for (final Keyword keyword : list) {
				keywords.put(keyword.getValue(), keyword);
			}
		}
		final List<EnumRule> enumRules = GrammarUtil.allEnumRules(grammar);
		for (final EnumRule enumRule : enumRules) {
			final List<Keyword> list = typeSelect(eAllContentsAsList(enumRule), Keyword.class);
			for (final Keyword keyword : list) {
				keywords.put(keyword.getValue(), keyword);
			}
		}
		return keywords.values();
	}

	/** Generate a basic keyword.
	 *
	 * @param keyword the keyword to add.
	 * @param comment a comment for the javadoc.
	 * @return the content.
	 */
	protected StringConcatenationClient generateKeyword(final String keyword, final String comment) {
		final String fieldName = keyword.toUpperCase().replaceAll("[^a-zA-Z0-9_]+", "_"); //$NON-NLS-1$ //$NON-NLS-2$
		final String methodName = Strings.toFirstUpper(keyword.replaceAll("[^a-zA-Z0-9_]+", "_")) //$NON-NLS-1$ //$NON-NLS-2$
				+ "Keyword"; //$NON-NLS-1$
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\tprivate static final String "); //$NON-NLS-1$
				it.append(fieldName);
				it.append(" = \""); //$NON-NLS-1$
				it.append(Strings.convertToJavaString(keyword));
				it.append("\";"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Keyword: {@code "); //$NON-NLS-1$
				it.append(protectCommentKeyword(keyword));
				it.append("}."); //$NON-NLS-1$
				it.newLine();
				if (!Strings.isEmpty(comment)) {
					it.append("\t * Source: "); //$NON-NLS-1$
					it.append(comment);
					it.newLine();
				}
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic String get"); //$NON-NLS-1$
				it.append(methodName);
				it.append("() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn "); //$NON-NLS-1$
				it.append(fieldName);
				it.append(";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
	}

	/** Generate a grammar keyword.
	 *
	 * @param keyword the keyword to add.
	 * @param comment a comment for the javadoc.
	 * @return the content.
	 */
	protected StringConcatenationClient generateKeyword(final Keyword keyword, final String comment) {
		try {
			final String methodName = getIdentifier(keyword);
			final String accessor = GrammarKeywordAccessFragment2.this.grammarAccessExtensions.gaAccessor(keyword);
			if (!Strings.isEmpty(methodName) && !Strings.isEmpty(accessor)) {
				return new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation it) {
						it.append("\t/** Keyword: {@code "); //$NON-NLS-1$
						it.append(protectCommentKeyword(keyword.getValue()));
						it.append("}."); //$NON-NLS-1$
						it.newLine();
						if (!Strings.isEmpty(comment)) {
							it.append("\t * Source: "); //$NON-NLS-1$
							it.append(comment);
							it.newLine();
						}
						it.append("\t */"); //$NON-NLS-1$
						it.newLine();
						it.append("\tpublic String get"); //$NON-NLS-1$
						it.append(methodName);
						it.append("() {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn this.grammarAccess."); //$NON-NLS-1$
						it.append(accessor);
						it.append(".getValue();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
				};
			}
		} catch (Exception e) {
			//
		}
		return null;
	}

	/** Replies the identifier for the given keyword.
	 *
	 * @param keyword the keyword.
	 * @return the identifier.
	 */
	protected String getIdentifier(Keyword keyword) {
		return this.grammarAccessExtensions.gaElementIdentifier(keyword)
				.replaceFirst("[0-9_]+$", ""); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Protect the keywword for Java comments.
	 *
	 * @param keyword the keyword to protect.
	 * @return the protected keyword.
	 */
	@SuppressWarnings("static-method")
	protected String protectCommentKeyword(String keyword) {
		if ("*/".equals(keyword)) { //$NON-NLS-1$
			return "* /"; //$NON-NLS-1$
		}
		if ("/*".equals(keyword)) { //$NON-NLS-1$
			return "/ *"; //$NON-NLS-1$
		}
		if ("//".equals(keyword)) { //$NON-NLS-1$
			return "/ /"; //$NON-NLS-1$
		}
		return keyword;
	}

	/** Returns all grammars from the hierarchy that are used from rules of this grammar.
	 *
	 * @param grammar the grammar.
	 * @return the used grammars.
	 */
	protected static List<Grammar> getEffectivelyUsedGrammars(final Grammar grammar) {
		final List<AbstractRule> allRules = GrammarUtil.allRules(grammar);
		final List<Grammar> map = ListExtensions.<AbstractRule, Grammar>map(allRules, (it) -> GrammarUtil.getGrammar(it));
		final Iterable<Grammar> filter = IterableExtensions.<Grammar>filter(map, (it) -> Boolean.valueOf(it != grammar));
		final Set<Grammar> set = IterableExtensions.<Grammar>toSet(filter);
		return IterableExtensions.<Grammar>toList(set);
	}

}
