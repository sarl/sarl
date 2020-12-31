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
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractStubGeneratingFragment;
import org.eclipse.xtext.xtext.generator.IXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.Issues;
import org.eclipse.xtext.xtext.generator.XtextGeneratorNaming;
import org.eclipse.xtext.xtext.generator.model.FileAccessFactory;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import io.sarl.lang.mwe2.codebuilder.config.CodeBuilderConfig;
import io.sarl.lang.mwe2.codebuilder.config.ExpressionConfig;
import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;

/** Abstract sub generator of code builder.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:methodcount")
public abstract class AbstractSubCodeBuilderFragment extends AbstractStubGeneratingFragment {

	@Inject
	private FileAccessFactory fileAccessFactory;

	@Inject
	private CodeBuilderConfig configuration;

	private Collection<AbstractSubCodeBuilderFragment> subFragments;

	@Inject
	private CodeElementExtractor grammarExtractor;

	@Inject
	private XtextGeneratorNaming naming;

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		this.subFragments = initializeSubGenerators(injector);
		for (final IXtextGeneratorFragment subFragment : this.subFragments) {
			subFragment.initialize(injector);
		}
	}

	/** Replies the naming conventions.
	 *
	 * @return the naming conventions.
	 */
	protected XtextGeneratorNaming getNaming() {
		return this.naming;
	}

	/** Replies the grammar extractor.
	 *
	 * @return the grammar extractor.
	 */
	protected CodeElementExtractor getCodeElementExtractor() {
		return this.grammarExtractor;
	}

	/** Initialize the sub generators.
	 *
	 * @param injector the injector.
	 * @return the list of the generators.
	 */
	@SuppressWarnings("static-method")
	protected Collection<AbstractSubCodeBuilderFragment> initializeSubGenerators(Injector injector) {
		return Collections.emptyList();
	}

	@Pure
	private void checkNoEmpty(String name, String value, Issues issues) {
		if (Strings.isEmpty(value)) {
			issues.addError(MessageFormat.format("the configuration entry ''{0}'' is not set", name), this); //$NON-NLS-1$
		}
	}

	@Override
	@Pure
	public void checkConfiguration(Issues issues) {
		super.checkConfiguration(issues);
		final CodeBuilderConfig config = getCodeBuilderConfig();
		if (config == null) {
			issues.addError("No code builder configuration", this); //$NON-NLS-1$
		} else {
			checkNoEmpty("scriptRuleName", config.getScriptRuleName(), issues); //$NON-NLS-1$
			checkNoEmpty("topElementRuleName", config.getTopElementRuleName(), issues); //$NON-NLS-1$
			checkNoEmpty("formalParameterContainerType", config.getFormalParameterContainerType(), issues); //$NON-NLS-1$
		}
		if (this.subFragments == null) {
			issues.addError("Sub generators are not created"); //$NON-NLS-1$
		} else {
			for (final IXtextGeneratorFragment subFragment : this.subFragments) {
				subFragment.checkConfiguration(issues);
			}
		}
	}

	/** Fill the given set with the exported packages for this fragment.
	 *
	 * @param exportedPackages the set to fill in.
	 */
	public void getExportedPackages(Set<String> exportedPackages) {
		if (exportedPackages != null) {
			exportedPackages.add(getCodeElementExtractor().getBasePackage());
			exportedPackages.add(getCodeElementExtractor().getBuilderPackage());
			if (getCodeBuilderConfig().isISourceAppendableEnable()) {
				exportedPackages.add(getCodeElementExtractor().getAppenderPackage());
			}
		}
	}

	@Override
	public void generate() {
		for (final IXtextGeneratorFragment subFragment : this.subFragments) {
			subFragment.generate();
		}
	}

	/** Generates the Xtend stubs.
	 */
	public void generateXtendStubs() {
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateXtendStubs();
		}
	}

	/** Generates the Java stubs.
	 */
	public void generateJavaStubs() {
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateJavaStubs();
		}
	}

	/** Generates the bindings for the Runtime module.
	 *
	 * @param factory the factory for creating the bindings.
	 */
	public void generateRuntimeBindings(BindingFactory factory) {
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateRuntimeBindings(factory);
		}
	}

	/** Generates the bindings for the UI/Eclipse module.
	 *
	 * @param factory the factory for creating the bindings.
	 */
	public void generateEclipseBindings(BindingFactory factory) {
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateEclipseBindings(factory);
		}
	}

	/** Generates the bindings for the UI/IDEA module.
	 *
	 * @param factory the factory for creating the bindings.
	 */
	public void generateIdeaBindings(BindingFactory factory) {
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateIdeaBindings(factory);
		}
	}

	/** Generates the bindings for the UI/Web-interface module.
	 *
	 * @param factory the factory for creating the bindings.
	 */
	public void generateWebBindings(BindingFactory factory) {
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateWebBindings(factory);
		}
	}

	/** Replies the code builder configuration.
	 *
	 * @return the configuration.
	 */
	@Pure
	protected CodeBuilderConfig getCodeBuilderConfig() {
		return this.configuration;
	}

	/** Replies the expression builder configuration.
	 *
	 * @return the configuration.
	 */
	@Pure
	protected ExpressionConfig getExpressionConfig() {
		return this.configuration.getExpression();
	}

	/** Replies the file access factory.
	 *
	 * @return the factory.
	 */
	@Pure
	protected FileAccessFactory getFileAccessFactory() {
		return this.fileAccessFactory;
	}

	/** Replies the generated source folder for the builder.
	 *
	 * @return the generated source folder for the builder.
	 */
	@Pure
	protected IFileSystemAccess2 getSrcGen() {
		return getProjectConfig().getRuntime().getSrcGen();
	}

	/** Replies the not-generated source folder for the builder.
	 *
	 * @return the not-generated source folder for the builder.
	 */
	@Pure
	protected IFileSystemAccess2 getSrc() {
		return getProjectConfig().getRuntime().getSrc();
	}

	/** Replies the language name.
	 *
	 * @return the language name.
	 */
	@Pure
	protected String getLanguageName() {
		return Strings.toFirstUpper(GrammarUtil.getSimpleName(getGrammar()).toLowerCase());
	}

	/** Replies the implementation for the builder factory.
	 *
	 * @return the implementation.
	 */
	@Pure
	protected TypeReference getBuilderFactoryImpl() {
		return new TypeReference(getCodeElementExtractor().getBasePackage() + ".CodeBuilderFactory"); //$NON-NLS-1$
	}

	/** Replies the interface for the builder of scripts.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getScriptBuilderInterface() {
		return getCodeElementExtractor().getElementBuilderInterface("Script"); //$NON-NLS-1$
	}

	/** Replies the interface for the expression builder.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getExpressionBuilderInterface() {
		return getCodeElementExtractor().getElementBuilderInterface("Expression"); //$NON-NLS-1$
	}

	/** Replies the implementation for the expression builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getExpressionBuilderImpl() {
		return getCodeElementExtractor().getElementBuilderImpl("Expression"); //$NON-NLS-1$
	}

	/** Replies the interface for the block expression builder.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getBlockExpressionBuilderInterface() {
		return getCodeElementExtractor().getElementBuilderInterface("BlockExpression"); //$NON-NLS-1$
	}

	/** Replies the interface for the formal parameter builder.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getFormalParameterBuilderInterface() {
		return getCodeElementExtractor().getElementBuilderInterface("FormalParameter"); //$NON-NLS-1$
	}

	/** Replies the interface for the type parameter builder.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getTypeParameterBuilderInterface() {
		return getCodeElementExtractor().getElementBuilderInterface("TypeParameter"); //$NON-NLS-1$
	}

	/** Replies the adapter for pre documentation.
	 *
	 * @return the adapter.
	 */
	@SuppressWarnings("static-method")
	@Pure
	protected TypeReference getPreDocumentationAdapter() {
		return new TypeReference(DocumentationAdapter.class);
	}

	/** Replies the implementation for the code builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	protected TypeReference getAbstractBuilderImpl() {
		return new TypeReference(getCodeElementExtractor().getBuilderPackage() + ".AbstractBuilder"); //$NON-NLS-1$
	}

	/** Replies the getter function for accessing to the top element collection of the script.
	 *
	 * @return the name of the getter function.
	 */
	@Pure
	protected String getLanguageScriptMemberGetter() {
		final Grammar grammar = getGrammar();
		final AbstractRule scriptRule = GrammarUtil.findRuleForName(grammar, getCodeBuilderConfig().getScriptRuleName());
		for (final Assignment assignment : GrammarUtil.containedAssignments(scriptRule)) {
			if ((assignment.getTerminal() instanceof RuleCall)
					&& Objects.equals(((RuleCall) assignment.getTerminal()).getRule().getName(),
					getCodeBuilderConfig().getTopElementRuleName())) {
				return "get" + Strings.toFirstUpper(assignment.getFeature()); //$NON-NLS-1$
			}
		}
		throw new IllegalStateException("member not found"); //$NON-NLS-1$
	}

	/** Replies the getter function for accessing to the member collection of the declarating types.
	 *
	 * @return the name of the getter function.
	 */
	@Pure
	protected String getLanguageContainerMemberGetter() {
		return "get" + Strings.toFirstUpper(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName()); //$NON-NLS-1$
	}

	/** Replies the type for the factory for the given type.
	 *
	 * @param type the type of the object to create.
	 * @return the factory.
	 */
	@Pure
	protected TypeReference getXFactoryFor(TypeReference type) {
		final String packageName = type.getPackageName();
		final Grammar grammar = getGrammar();
		TypeReference reference = getXFactoryFor(packageName, grammar);
		if (reference != null) {
			return reference;
		}

		for (final Grammar usedGrammar : GrammarUtil.allUsedGrammars(grammar)) {
			reference = getXFactoryFor(packageName, usedGrammar);
			if (reference != null) {
				return reference;
			}
		}
		throw new IllegalStateException("Cannot find the XFactory for " + type); //$NON-NLS-1$
	}

	/** Replies the type for the factory for the given type.
	 *
	 * @param type the type of the object to create.
	 * @return the factory.
	 */
	@Pure
	protected TypeReference getXFactoryFor(Class<?> type) {
		return getXFactoryFor(new TypeReference(type));
	}

	private TypeReference getXFactoryFor(String packageName, Grammar grammar) {
		final String languageName = GrammarUtil.getSimpleName(grammar).toLowerCase();
		final String basePackage = this.naming.getRuntimeBasePackage(grammar) + "." + languageName; //$NON-NLS-1$
		if (basePackage.equals(packageName)) {
			return new TypeReference(basePackage + "." //$NON-NLS-1$
					+ Strings.toFirstUpper(languageName) + "Factory"); //$NON-NLS-1$
		}
		return null;
	}

	/** Generate the members related to appenders.
	 *
	 * @param appenderSimpleName the simple name of the appender.
	 * @param builderInterface the interface of the code builder to wrap.
	 * @param elementAccessor the code for accessing to the generated element.
	 * @return the code.
	 */
	@SuppressWarnings("static-method")
	protected StringConcatenationClient generateAppenderMembers(String appenderSimpleName,
			TypeReference builderInterface, String elementAccessor) {
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\tprivate final "); //$NON-NLS-1$
				it.append(builderInterface);
				it.append(" builder;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(appenderSimpleName);
				it.append("("); //$NON-NLS-1$
				it.append(builderInterface);
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
				it.append("\t\tbuild(this.builder."); //$NON-NLS-1$
				it.append(elementAccessor);
				it.append(", appender);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
	}

	/** Generate the functions related to the comments.
	 *
	 * @param forInterface indicates if the code must be generated for an interface.
	 * @param forAppender indicates if the code must be generated for an appender.
	 * @param elementAccessor the code for accessing to the generated element.
	 * @param functionName the name of the function.
	 * @param comment the comment for the function.
	 * @param documentationAdapterType the type of the documentation adapter.
	 * @return the code.
	 */
	@SuppressWarnings("static-method")
	protected StringConcatenationClient generateCommentFunction(
			boolean forInterface, boolean forAppender,
			String elementAccessor, String functionName, String comment,
			TypeReference documentationAdapterType) {
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\t/** Change the documentation of the element."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <p>"); //$NON-NLS-1$
				it.append(comment);
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param doc the documentation."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void "); //$NON-NLS-1$
				it.append(functionName);
				it.append("(String doc)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.setDocumentation(doc);"); //$NON-NLS-1$
					} else {
						it.append("\t\tif ("); //$NON-NLS-1$
						it.append(Strings.class);
						it.append(".isEmpty(doc)) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t"); //$NON-NLS-1$
						it.append(elementAccessor);
						it.append(".eAdapters().removeIf(new "); //$NON-NLS-1$
						it.append(Predicate.class);
						it.append("<"); //$NON-NLS-1$
						it.append(Adapter.class);
						it.append(">() {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tpublic boolean test("); //$NON-NLS-1$
						it.append(Adapter.class);
						it.append(" adapter) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\treturn adapter.isAdapterForType("); //$NON-NLS-1$
						it.append(documentationAdapterType);
						it.append(".class);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t});"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t} else {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t"); //$NON-NLS-1$
						it.append(documentationAdapterType);
						it.append(" adapter = ("); //$NON-NLS-1$
						it.append(documentationAdapterType);
						it.append(") "); //$NON-NLS-1$
						it.append(EcoreUtil.class);
						it.append(".getExistingAdapter("); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\t"); //$NON-NLS-1$
						it.append(elementAccessor);
						it.append(", "); //$NON-NLS-1$
						it.append(documentationAdapterType);
						it.append(".class);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tif (adapter == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tadapter = new "); //$NON-NLS-1$
						it.append(documentationAdapterType);
						it.append("();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t"); //$NON-NLS-1$
						it.append(elementAccessor);
						it.append(".eAdapters().add(adapter);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tadapter.setDocumentation(doc);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
	}

	/** Generate the functions related to the comments.
	 *
	 * @param forInterface indicates if the code must be generated for an interface.
	 * @param forAppender indicates if the code must be generated for an appender.
	 * @param elementAccessor the code for accessing to the generated element.
	 * @return the code.
	 */
	protected StringConcatenationClient generateStandardCommentFunctions(boolean forInterface, boolean forAppender,
			String elementAccessor) {
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append(generateCommentFunction(forInterface, forAppender, elementAccessor,
						"setDocumentation", //$NON-NLS-1$
						"The documentation will be displayed just before the element.", //$NON-NLS-1$
						getPreDocumentationAdapter()));
			}
		};
	}

	/** Replies the fully qualified name for the classifier.
	 *
	 * @param classifier the classifier.
	 * @return the fully qualified name for the given classifier.
	 * @deprecated see {@link CodeElementExtractor#newTypeReference(EClassifier)}
	 */
	@Deprecated
	protected TypeReference newTypeReference(EClassifier classifier) {
		return getCodeElementExtractor().newTypeReference(classifier);
	}

	/** Replies the "an" or "a" article according to the given word.
	 *
	 * <p>This function does not follow the real English grammatical rule, but it is
	 * an acceptable approximation.
	 *
	 * @param word the word that follows the article.
	 * @return the article.
	 */
	protected static String getAorAnArticle(String word) {
		if (Arrays.asList('a', 'e', 'i', 'o', 'u', 'y').contains(Character.toLowerCase(word.charAt(0)))) {
			return "an"; //$NON-NLS-1$
		}
		return "a"; //$NON-NLS-1$
	}

	/** Replies the singular version of the word.
	 *
	 * <p>This function does not follow the real English grammatical rule, but it is
	 * an acceptable approximation.
	 *
	 * @param word the word.
	 * @return the singular word.
	 */
	protected static String toSingular(String word) {
		if (word.endsWith("ies")) { //$NON-NLS-1$
			return word.substring(0, word.length() - 3) + "y"; //$NON-NLS-1$
		}
		if (word.endsWith("s")) { //$NON-NLS-1$
			return word.substring(0, word.length() - 1);
		}
		return word;
	}

	/** Replies if the name of the given element is matching the pattern.
	 *
	 * @param element the element.
	 * @param pattern the name pattern.
	 * @return <code>true</code> if the element's name is matching.
	 */
	protected static boolean nameMatches(EObject element, String pattern) {
		if (element instanceof RuleCall) {
			return nameMatches(((RuleCall) element).getRule(), pattern);
		}
		if (element instanceof AbstractRule) {
			final String name = ((AbstractRule) element).getName();
			final Pattern compilerPattern = Pattern.compile(pattern);
			final Matcher matcher = compilerPattern.matcher(name);
			if (matcher.find()) {
				return true;
			}
		}
		return false;
	}

	/** Replies the first assignement with the given name in the given rule.
	 *
	 * @param rule the rule.
	 * @param name the name.
	 * @return the assignment or {@code null}.
	 */
	protected static Assignment findAssignmentFromFeatureName(EObject rule, String name) {
		return IterableExtensions.findFirst(GrammarUtil.containedAssignments(rule),
			assignment -> name.equals(assignment.getFeature()));
	}

	/** Replies the first assignement with the given name in the given rule.
	 *
	 * @param rule the rule.
	 * @param pattern pattern for the name of the terminal.
	 * @return the assignment or {@code null}.
	 */
	protected static Assignment findAssignmentFromTerminalPattern(EObject rule, String pattern) {
		return IterableExtensions.findFirst(GrammarUtil.containedAssignments(rule),
			assignment -> nameMatches(assignment.getTerminal(), pattern));
	}

	/** Binds the given descriptions according to the standard policy.
	 *
	 * <p>If an custom implementation is defined, it is binded to. Otherwise, the default implementation
	 * is binded.
	 *
	 * @param factory the binding factory to use for creating the bindings.
	 * @param descriptions the descriptions to bind to.
	 */
	protected void bindElementDescription(BindingFactory factory, CodeElementExtractor.ElementDescription... descriptions) {
		for (final CodeElementExtractor.ElementDescription description : descriptions) {
			bindTypeReferences(factory,
					description.getBuilderInterfaceType(),
					description.getBuilderImplementationType(),
					description.getBuilderCustomImplementationType());
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
		final IFileSystemAccess2 fileSystem = getSrc();
		final TypeReference type;
		if ((fileSystem.isFile(implementationType.getJavaPath()))
				|| (fileSystem.isFile(customImplementationType.getXtendPath()))) {
			type = customImplementationType;
		} else {
			type = implementationType;
		}
		factory.addfinalTypeToType(interfaceType, type);
	}

	/** Replies the rule used for defining the members of the given element.
	 *
	 * @param description description of the container.
	 * @return the rule that is defining the members.
	 */
	protected AbstractRule getMemberRule(CodeElementExtractor.ElementDescription description) {
		for (final Assignment assignment : GrammarUtil.containedAssignments(description.getGrammarComponent())) {
			if (Objects.equals(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName(), assignment.getFeature())) {
				if (assignment.getTerminal() instanceof RuleCall) {
					return ((RuleCall) assignment.getTerminal()).getRule();
				}
			}
		}
		return null;
	}

}
