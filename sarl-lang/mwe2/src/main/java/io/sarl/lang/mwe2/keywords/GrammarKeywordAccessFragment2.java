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

package io.sarl.lang.mwe2.keywords;

import static org.eclipse.xtext.EcoreUtil2.eAllContentsAsList;
import static org.eclipse.xtext.EcoreUtil2.typeSelect;

import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import com.google.common.base.Objects;
import com.google.inject.Inject;
import com.google.inject.Injector;
import org.apache.log4j.Logger;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.JavaKeywords;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.XtextGeneratorNaming;
import org.eclipse.xtext.xtext.generator.grammarAccess.GrammarAccessExtensions;
import org.eclipse.xtext.xtext.generator.model.FileAccessFactory;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create a code builder for the generated language.
 *
 * <p>The generated builder could be used for helping to create Eobjects from scratch
 * (in ui wizard for example).
 *
 * @author $Author: sgalland$
 * @version mwe2 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
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

	@Inject
	private JavaKeywords javaKeywords;

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

	/** Replies the type for {@code @Inject}.
	 *
	 * @return the inject annotation type.
	 * @since 0.14
	 */
	public Class<?> getInjectType() {
		return this.configuration.getInjectionAPI().getInjectType();
	}

	@Override
	public void generate() {
		LOG.info("Generating the grammar keyword access for " + getLanguageName()); //$NON-NLS-1$
		final var content = new StringConcatenationClient() {
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
				final var addedKeywords = new HashSet<String>();
				final var getters = new HashMap<String, String>();
				final var grammars = new LinkedList<Grammar>();
				grammars.add(getGrammar());
				while (!grammars.isEmpty()) {
					final var grammar = grammars.removeFirst();
					if (isValidGrammar(grammar)) {
						it.append(generateMembers(grammar, addedKeywords, getters));
						if (GrammarKeywordAccessFragment2.this.configuration.getDependencyGrammarInheritance()) {
							grammars.addAll(getEffectivelyUsedGrammars(grammar));
						}
					}
				}
				it.append(generateMembersFromConfig(addedKeywords, getters));
				it.append(generateAccessors(addedKeywords, getters));
				it.append("}"); //$NON-NLS-1$
				it.newLine();
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final var javaFile = this.fileAccessFactory.createJavaFile(
				getAccessorType(), content);
		javaFile.writeTo(getProjectConfig().getRuntime().getSrcGen());
	}

	/** Replies if the given grammar may be treated.
	 *
	 * @param grammar the grammar to test.
	 * @return {@code true} if the grammar could be treated.
	 */
	@SuppressWarnings("static-method")
	protected boolean isValidGrammar(Grammar grammar) {
		return !Objects.equal(grammar.getName(), "org.eclipse.xtext.common.Terminals"); //$NON-NLS-1$
	}

	/** Generate the members of the accessors.
	 *
	 * @param addedKeywords the set of keywords that are added to the output.
	 * @param getters filled by this function with the getters' names.
	 * @return the content.
	 */
	protected StringConcatenationClient generateMembersFromConfig(Set<String> addedKeywords, Map<String, String> getters) {
		final var clients = new ArrayList<StringConcatenationClient>();
		final var scopedKeywords = this.configuration.getRemoveKeywordsDefinedInScopes()
				? this.configuration.getAllKeywordsDefinedInScope()
				: Collections.emptySet();
		for (final var keyword : this.configuration.getKeywords()) {
			final var id = keyword.toLowerCase();
			if (!addedKeywords.contains(id) && !this.configuration.getIgnoredKeywords().contains(keyword)
					&& !scopedKeywords.contains(keyword)) {
				clients.add(generateKeyword(keyword, getGrammar().getName(), getters));
				addedKeywords.add(id);
			}
		}
		for (final var keyword : this.configuration.getLiterals()) {
			final var id = keyword.toLowerCase();
			if (!addedKeywords.contains(id) && !this.configuration.getIgnoredKeywords().contains(keyword)) {
				clients.add(generateKeyword(keyword, getGrammar().getName(), getters));
				addedKeywords.add(id);
			}
		}
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				for (final var client : clients) {
					it.append(client);
				}
			}
		};
	}

	/** Generate the members of the accessors.
	 *
	 * @param grammar the grammar for which the keyword accessors must be generated.
	 * @param addedKeywords the set of keywords that are added to the output.
	 * @param getters filled by this function with the getters' names.
	 * @return the content.
	 */
	protected StringConcatenationClient generateMembers(Grammar grammar, Set<String> addedKeywords,
			Map<String, String> getters) {
		final var clients = new ArrayList<StringConcatenationClient>();
		final var scopedKeywords = this.configuration.getRemoveKeywordsDefinedInScopes()
				? this.configuration.getAllKeywordsDefinedInScope()
				: Collections.emptySet();
		for (final var grammarKeyword : getAllKeywords(grammar)) {
			final var keyword = grammarKeyword.getValue().trim();
			if (!keyword.isEmpty()) {
				if (!addedKeywords.contains(keyword) && !this.configuration.getIgnoredKeywords().contains(keyword)
						&& !scopedKeywords.contains(keyword)) {
					clients.add(generateKeyword(grammarKeyword, grammar.getName(), getters));
					addedKeywords.add(keyword);
				}
			}
		}
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				for (final var client : clients) {
					it.append(client);
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
		final var keywords = new HashMap<String, Keyword>();
		final var rules = GrammarUtil.allParserRules(grammar);
		for (final var parserRule : rules) {
			final var list = typeSelect(eAllContentsAsList(parserRule), Keyword.class);
			for (final var keyword : list) {
				keywords.put(keyword.getValue(), keyword);
			}
		}
		final var enumRules = GrammarUtil.allEnumRules(grammar);
		for (final var enumRule : enumRules) {
			final var list = typeSelect(eAllContentsAsList(enumRule), Keyword.class);
			for (final var keyword : list) {
				keywords.put(keyword.getValue(), keyword);
			}
		}
		return keywords.values();
	}

	/** Generate a basic keyword.
	 *
	 * @param keyword the keyword to add.
	 * @param comment a comment for the javadoc.
	 * @param getters filled by this function with the getters' names.
	 * @return the content.
	 */
	protected StringConcatenationClient generateKeyword(final String keyword, final String comment, Map<String, String> getters) {
		final var fieldName = keyword.toUpperCase().replaceAll("[^a-zA-Z0-9_]+", "_"); //$NON-NLS-1$ //$NON-NLS-2$
		final var methodName = Strings.toFirstUpper(keyword.replaceAll("[^a-zA-Z0-9_]+", "_")) //$NON-NLS-1$ //$NON-NLS-2$
				+ "Keyword"; //$NON-NLS-1$
		if (getters != null) {
			getters.put(methodName, keyword);
		}
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
	 * @param getters filled by this function with the getters' names.
	 * @return the content.
	 */
	protected StringConcatenationClient generateKeyword(final Keyword keyword, final String comment,
			Map<String, String> getters) {
		try {
			final var methodName = getIdentifier(keyword);
			final var accessor = GrammarKeywordAccessFragment2.this.grammarAccessExtensions.gaAccessor(keyword);
			if (!Strings.isEmpty(methodName) && !Strings.isEmpty(accessor)) {
				if (getters != null) {
					getters.put(methodName, keyword.getValue());
				}
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

	/** Protect the keyword for Java comments.
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
		return keyword
				.replace("{", "&#123;") //$NON-NLS-1$ //$NON-NLS-2$
				.replace("}", "&#125;"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Returns all grammars from the hierarchy that are used from rules of this grammar.
	 *
	 * @param grammar the grammar.
	 * @return the used grammars.
	 */
	protected static List<Grammar> getEffectivelyUsedGrammars(final Grammar grammar) {
		final var allRules = GrammarUtil.allRules(grammar);
		final var map = ListExtensions.<AbstractRule, Grammar>map(allRules, it -> GrammarUtil.getGrammar(it));
		final var filter = IterableExtensions.<Grammar>filter(map, it -> Boolean.valueOf(it != grammar));
		final var set = IterableExtensions.<Grammar>toSet(filter);
		return IterableExtensions.<Grammar>toList(set);
	}

	/** Generate the members of the accessors.
	 *
	 * @param addedKeywords the set of keywords that are added to the output.
	 * @param getters filled by this function with the getters' names.
	 * @return the content.
	 */
	protected StringConcatenationClient generateAccessors(Set<String> addedKeywords, Map<String, String> getters) {
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(SoftReference.class);
				it.append("<"); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<String>> allKeywords;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the SARL keywords."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the SARL keywords."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @see #getPureKeywords()"); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<String> getKeywords() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<String> kws = this.allKeywords == null ? null : this.allKeywords.get();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (kws == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tkws = new "); //$NON-NLS-1$
				it.append(TreeSet.class);
				it.append("<>();"); //$NON-NLS-1$
				it.newLine();
				final var pattern = Pattern.compile("^[a-zA-Z_$]+$"); //$NON-NLS-1$
				for (final var getter : getters.entrySet()) {
					if (pattern.matcher(getter.getValue()).matches()) {
						it.append("\t\t\tkws.add(get"); //$NON-NLS-1$
						it.append(getter.getKey());
						it.append("());"); //$NON-NLS-1$
						it.newLine();
					}
				}
				it.append("\t\t\tthis.allKeywords = new "); //$NON-NLS-1$
				it.append(SoftReference.class);
				it.append("<>(kws);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn "); //$NON-NLS-1$
				it.append(Collections.class);
				it.append(".unmodifiableSet(kws);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies if the given string of characters is a SARL keyword."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param str the string of characters."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return {@code true} if the string of characters is a SARL keyword."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic boolean isKeyword(String str) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tassert !"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(str);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn getKeywords().contains(str);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(SoftReference.class);
				it.append("<"); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<String>> pureSarlKeywords;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the pure SARL keywords."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * Pure SARL keywords are SARL keywords that are not Java keywords."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the pure SARL keywords."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<String> getPureKeywords() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(Set.class);
				it.append("<String> kws = this.pureSarlKeywords == null ? null : this.pureSarlKeywords.get();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (kws == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tkws = new "); //$NON-NLS-1$
				it.append(HashSet.class);
				it.append("<>();"); //$NON-NLS-1$
				it.newLine();
				for (final var getter : getters.entrySet()) {
					if (pattern.matcher(getter.getValue()).matches()
							&& !GrammarKeywordAccessFragment2.this.javaKeywords.isJavaKeyword(getter.getValue())) {
						it.append("\t\t\tkws.add(get"); //$NON-NLS-1$
						it.append(getter.getKey());
						it.append("());"); //$NON-NLS-1$
						it.newLine();
					}
				}
				it.append("\t\t\tthis.pureSarlKeywords = new "); //$NON-NLS-1$
				it.append(SoftReference.class);
				it.append("<>(kws);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn "); //$NON-NLS-1$
				it.append(Collections.class);
				it.append(".unmodifiableSet(kws);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies if the given string of characters is a pure SARL keyword."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * Pure SARL keywords are SARL keywords that are not Java keywords."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param str the string of characters."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return {@code true} if the string of characters is a SARL keyword."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic boolean isPureKeyword(String str) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tassert !"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(str);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn getPureKeywords().contains(str);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Protect the given text if it is a keyword."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param text the text to protect."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the protected text."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic String protectKeyword(String text) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(text) && isKeyword(text)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn \""); //$NON-NLS-1$
				it.append(Strings.convertToJavaString(GrammarKeywordAccessFragment2.this.configuration.getKeywordProtectionSymbol()));
				it.append("\" + text;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn text;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
	}

}
