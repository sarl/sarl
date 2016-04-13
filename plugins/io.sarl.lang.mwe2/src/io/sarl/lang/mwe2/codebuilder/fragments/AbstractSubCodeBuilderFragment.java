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

package io.sarl.lang.mwe2.codebuilder.fragments;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
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
import org.eclipse.xtext.Action;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractStubGeneratingFragment;
import org.eclipse.xtext.xtext.generator.IXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.Issues;
import org.eclipse.xtext.xtext.generator.XtextGeneratorNaming;
import org.eclipse.xtext.xtext.generator.model.FileAccessFactory;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.TypeReference;
import org.eclipse.xtext.xtext.generator.util.GenModelUtil2;

import io.sarl.lang.mwe2.codebuilder.CodeBuilderConfig;

/** Abstract sub generator of code builder. 
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSubCodeBuilderFragment extends AbstractStubGeneratingFragment {

	@Inject
	private XtextGeneratorNaming naming;

	@Inject
	private FileAccessFactory fileAccessFactory;

	@Inject
	private CodeBuilderConfig configuration;

	private Collection<AbstractSubCodeBuilderFragment> subFragments;

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		this.subFragments = initializeSubGenerators(injector);
		for (IXtextGeneratorFragment subFragment : this.subFragments) {
			subFragment.initialize(injector);
		}
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
		CodeBuilderConfig config = getCodeBuilderConfig();
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
			for (IXtextGeneratorFragment subFragment : this.subFragments) {
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
			exportedPackages.add(getBasePackage());
			exportedPackages.add(getBuilderPackage());
			if (getCodeBuilderConfig().isISourceAppendableEnable()) {
				exportedPackages.add(getAppenderPackage());
			}
		}
	}

	@Override
	public void generate() {
		for (IXtextGeneratorFragment subFragment : this.subFragments) {
			subFragment.generate();
		}
	}

	/** Generates the Xtend stubs.
	 */
	public void generateXtendStubs() {
		for (AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateXtendStubs();
		}
	}

	/** Generates the Java stubs.
	 */
	public void generateJavaStubs() {
		for (AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateJavaStubs();
		}
	}

	/** Generates the bindings.
	 * 
	 * @param factory the factory for creating the bindings. 
	 */
	public void generateBindings(BindingFactory factory) {
		for (AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateBindings(factory);
		}
	}

	/** Replies the naming conventions.
	 *
	 * @return the naming conventions.
	 */
	@Pure
	protected XtextGeneratorNaming getNaming() {
		return this.naming;
	}

	/** Replies the code builder configuration.
	 *
	 * @return the configuration.
	 */
	@Pure
	protected CodeBuilderConfig getCodeBuilderConfig() {
		return this.configuration;
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

	/** Replies the base package for the code builder.
	 *
	 * @return the base package for the code builder. 
	 */
	@Pure
	protected String getBasePackage() {
		Grammar grammar = getGrammar();
		String basePackage = getNaming().getRuntimeBasePackage(grammar);
		return basePackage + ".codebuilder"; //$NON-NLS-1$
	}

	/** Replies the base package for the builders.
	 *
	 * @return the base package for the builders. 
	 */
	@Pure
	protected String getBuilderPackage() {
		return getBasePackage() + ".builders"; //$NON-NLS-1$
	}

	/** Replies the base package for the builders.
	 *
	 * @return the base package for the builders. 
	 */
	@Pure
	protected String getAppenderPackage() {
		return getBasePackage() + ".appenders"; //$NON-NLS-1$
	}

	/** Replies the base package for the documentation tools.
	 *
	 * @return the base package for the documentation tools. 
	 */
	@Pure
	protected String getDocumentationPackage() {
		Grammar grammar = getGrammar();
		String basePackage = getNaming().getRuntimeBasePackage(grammar);
		return basePackage + ".documentation"; //$NON-NLS-1$
	}

	/** Replies the base package for the serialization tools.
	 *
	 * @return the base package for the serialization tools. 
	 */
	@Pure
	protected String getSerializerPackage() {
		Grammar grammar = getGrammar();
		String basePackage = getNaming().getRuntimeBasePackage(grammar);
		return basePackage + ".serializer"; //$NON-NLS-1$
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
		return new TypeReference(getBasePackage() + ".CodeBuilderFactory"); //$NON-NLS-1$
	}

	/** Replies the interface for the builder of scripts.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getScriptBuilderInterface() {
		return getElementBuilderInterface("Script"); //$NON-NLS-1$
	}

	/** Replies the interface for the expression builder.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getExpressionBuilderInterface() {
		return getElementBuilderInterface("Expression"); //$NON-NLS-1$
	}

	/** Replies the interface for the block expression builder.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getBlockExpressionBuilderInterface() {
		return getElementBuilderInterface("BlockExpression"); //$NON-NLS-1$
	}

	/** Replies the interface for the formal parameter builder.
	 *
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getFormalParameterBuilderInterface() {
		return getElementBuilderInterface("FormalParameter"); //$NON-NLS-1$
	}

	/** Replies the interface for the code builder that is creating the element of the given name.
	 *
	 * @param elementName the name of the element.
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getElementBuilderInterface(String elementName) {
		return new TypeReference(getBuilderPackage() + ".I" //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "Builder"); //$NON-NLS-1$
	}

	/** Replies the implementation for the code builder that is creating the element of the given name.
	 *
	 * @param elementName the name of the element.
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getElementBuilderImpl(String elementName) {
		return new TypeReference(getBuilderPackage() + "." //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "BuilderImpl"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the code builder that is creating the element of the given name.
	 *
	 * @param elementName the name of the element.
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getElementBuilderImplCustom(String elementName) {
		return new TypeReference(getBuilderPackage() + "." //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "BuilderImplCustom"); //$NON-NLS-1$
	}

	/** Replies the adapter for post documentation.
	 *
	 * @return the adapter.
	 */
	@SuppressWarnings("static-method")
	@Pure
	protected TypeReference getPreDocumentationAdapter() {
		return new TypeReference(DocumentationAdapter.class);
	}

	/** Replies the adapter for inner-block documentation.
	 *
	 * @return the adapter.
	 */
	@Pure
	protected TypeReference getInnerBlockDocumentationAdapter() {
		return new TypeReference(getDocumentationPackage() + ".InnerBlockDocumentationAdapter"); //$NON-NLS-1$
	}

	/** Replies the implementation for the code builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	protected TypeReference getAbstractBuilderImpl() {
		return new TypeReference(getBuilderPackage() + ".AbstractBuilder"); //$NON-NLS-1$
	}

	/** Replies the implementation for the code appender that is creating the element of the given name.
	 *
	 * @param elementName the name of the element.
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getElementAppenderImpl(String elementName) {
		return new TypeReference(getAppenderPackage() + "." //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "SourceAppender"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the code appender that is creating the element of the given name.
	 *
	 * @param elementName the name of the element.
	 * @return the interface.
	 */
	@Pure
	protected TypeReference getElementAppenderImplCustom(String elementName) {
		return new TypeReference(getAppenderPackage() + "." //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "SourceAppenderCustom"); //$NON-NLS-1$
	}

	/** Replies the implementation for the code adapter.
	 *
	 * @return the implementation.
	 */
	@Pure
	protected TypeReference getAbstractAppenderImpl() {
		return new TypeReference(getAppenderPackage() + ".AbstractSourceAppender"); //$NON-NLS-1$
	}

	/** Replies the type for the scripts from the grammar.
	 *
	 * @return the language script.
	 */
	@Pure
	protected TypeReference getLanguageScriptInterface() {
		AbstractRule rule = GrammarUtil.findRuleForName(getGrammar(), getCodeBuilderConfig().getScriptRuleName());
		EClassifier type = getGeneratedTypeFor(rule); 
		return newTypeReference(type);
	}

	/** Replies the base package for the ecore elements of the grammar.
	 *
	 * @return the base package for the ecore elements. 
	 */
	@Pure
	protected String getLanguageBasePackage() {
		Grammar grammar = getGrammar();
		String basePackage = getNaming().getRuntimeBasePackage(grammar);
		String ecorePackage = basePackage + "." //$NON-NLS-1$
				+ GrammarUtil.getSimpleName(grammar).toLowerCase();
		return ecorePackage;
	}

	/** Replies the getter function for accessing to the top element collection of the script.
	 *
	 * @return the name of the getter function.
	 */
	@Pure
	protected String getLanguageScriptMemberGetter() {
		Grammar grammar = getGrammar();
		AbstractRule scriptRule = GrammarUtil.findRuleForName(grammar, getCodeBuilderConfig().getScriptRuleName());
		for (Assignment assignment : GrammarUtil.containedAssignments(scriptRule)) {
			if ((assignment.getTerminal() instanceof RuleCall)
					&& Objects.equals(((RuleCall) assignment.getTerminal()).getRule().getName(),
							getCodeBuilderConfig().getTopElementRuleName())) {
				return "get" + Strings.toFirstUpper(assignment.getFeature()); //$NON-NLS-1$
			}
		}
		throw new IllegalStateException("member not found"); //$NON-NLS-1$
	}

	/** Replies the type associated to the top elements.
	 *
	 * @return the type of the top elements.
	 */
	@Pure
	protected TypeReference getLanguageTopElementType() {
		Grammar grammar = getGrammar();
		AbstractRule rule = GrammarUtil.findRuleForName(grammar, getCodeBuilderConfig().getTopElementRuleName());
		return newTypeReference(rule.getType().getClassifier());
	}

	/** Replies the type for the factory for the given type.
	 *
	 * @param type the type of the object to create.
	 * @return the factory.
	 */
	@Pure
	protected TypeReference getXFactoryFor(TypeReference type) {
		String packageName = type.getPackageName();
		Grammar grammar = getGrammar();
		TypeReference reference = getXFactoryFor(packageName, grammar);
		if (reference != null) {
			return reference;
		}
		for (Grammar usedGrammar : GrammarUtil.allUsedGrammars(grammar)) {
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
		String packageName = type.getPackage().getName();
		Grammar grammar = getGrammar();
		TypeReference reference = getXFactoryFor(packageName, grammar);
		if (reference != null) {
			return reference;
		}
		for (Grammar usedGrammar : GrammarUtil.allUsedGrammars(grammar)) {
			reference = getXFactoryFor(packageName, usedGrammar);
			if (reference != null) {
				return reference;
			}
		}
		throw new IllegalStateException("Cannot find the XFactory for " + type); //$NON-NLS-1$
	}

	private TypeReference getXFactoryFor(String packageName, Grammar grammar) {
		String languageName = GrammarUtil.getSimpleName(grammar).toLowerCase();
		String basePackage = getNaming().getRuntimeBasePackage(grammar) + "." + languageName; //$NON-NLS-1$
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

	/** Replies the type that is generated by a rule.
	 *
	 * @param rule the rule.
	 * @return the generated type.
	 */
	protected static EClassifier getGeneratedTypeFor(AbstractRule rule) {
		List<Action> actions = GrammarUtil.containedActions(rule);
		EClassifier classifier;
		if (actions.isEmpty()) {
			classifier = rule.getType().getClassifier();
		} else {
			classifier = actions.get(0).getType().getClassifier();
		}
		return classifier;
	}

	/** Replies the fully qualified name for the classifier.
	 *
	 * @param classifier the classifier.
	 * @return the fully qualified name for the given classifier.
	 */
	protected static TypeReference newTypeReference(EClassifier classifier) {
		if (classifier == null) {
			return new TypeReference(Object.class);
		}
		String name = GenModelUtil2.getJavaTypeName(classifier, classifier.eResource().getResourceSet());
		if (Strings.isEmpty(name)) {
			return new TypeReference(Object.class);
		}
		return new TypeReference(name);
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
			String name = ((AbstractRule) element).getName();
			Pattern compilerPattern = Pattern.compile(pattern);
			Matcher matcher = compilerPattern.matcher(name);
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
	 * @return the assignment or <code>null</code>.
	 */
	protected static Assignment findAssignmentFromFeatureName(AbstractRule rule, String name) {
		return IterableExtensions.findFirst(GrammarUtil.containedAssignments(rule),
				new Functions.Function1<Assignment, Boolean>() {
			@Override
			public Boolean apply(Assignment p) {
				return name.equals(p.getFeature());
			}
		});
	}

	/** Replies the first assignement with the given name in the given rule.
	 *
	 * @param rule the rule.
	 * @param pattern pattern for the name of the terminal. 
	 * @return the assignment or <code>null</code>.
	 */
	protected static Assignment findAssignmentFromTerminalPattern(AbstractRule rule, String pattern) {
		return IterableExtensions.findFirst(GrammarUtil.containedAssignments(rule),
				new Functions.Function1<Assignment, Boolean>() {
			@Override
			public Boolean apply(Assignment p) {
				return nameMatches(p.getTerminal(), pattern);
			}
		});
	}

	/** Extract type members from the given rule.
	 * 
	 * @param containerRule the member's container rule.
	 * @param memberRule the member's rule.
	 * @param treatedRules the set of rules' names that must be ignored as members.
	 * @param constructorCallback the function to call on each discovered constructor.
	 * @param namedMemberCallback the function to call on each discovered named member.
	 * @return the extract value, or <code>null</code>.
	 */
	protected <T> T visitMemberElements(
			AbstractRule containerRule,
			AbstractRule memberRule, Set<String> treatedRules,
			Functions.Function2<AbstractRule, AbstractRule, T> constructorCallback,
			Functions.Function3<AbstractRule, AbstractRule, String, T> namedMemberCallback) {
		LinkedList<AbstractRule> rules = new LinkedList<>();
		treatedRules.add(memberRule.getName());
		rules.add(memberRule);
		while (!rules.isEmpty()) {
			AbstractRule rule = rules.removeFirst();
			Assignment assignment = IterableExtensions.findFirst(
					GrammarUtil.containedAssignments(rule),
					new Functions.Function1<Assignment, Boolean>(){
						@Override
						public Boolean apply(Assignment p) {
							return getCodeBuilderConfig().getMemberNameExtensionGrammarName().equals(p.getFeature());
						}
					});
			if (assignment != null) {
				if (namedMemberCallback != null) {
					T retVal = namedMemberCallback.apply(containerRule, rule, assignment.getFeature());
					if (retVal != null) {
						return retVal;
					}
				}
			} else {
				for (RuleCall ruleCall : GrammarUtil.containedRuleCalls(rule)) {
					if (treatedRules.contains(ruleCall.getRule().getName())) {
						continue;
					}
					// Is a constructor rule?
					Pattern constructorPattern = Pattern.compile(getCodeBuilderConfig().getConstructorGrammarPattern());
					Matcher constructorMatcher = constructorPattern.matcher(ruleCall.getRule().getName());
					if (constructorMatcher.find()) {
						if (!getCodeBuilderConfig().getConstructorFreeRuleNames().contains(containerRule.getName())) {
							Pattern blockPattern = Pattern.compile(getCodeBuilderConfig().getBlockExpressionGrammarPattern());
							RuleCall block = IterableExtensions.findFirst(
									GrammarUtil.containedRuleCalls(ruleCall.getRule()),
									new Functions.Function1<RuleCall, Boolean>(){
										@Override
										public Boolean apply(RuleCall p) {
											Matcher matcher = blockPattern.matcher(p.getRule().getName());
											return matcher.find();
										}
									});
							if (block != null && constructorCallback != null) {
								T retVal = constructorCallback.apply(containerRule, ruleCall.getRule());
								if (retVal != null) {
									return retVal;
								}
							}
						}
					} else {
						treatedRules.add(ruleCall.getRule().getName());
						rules.add(ruleCall.getRule());
					}
				}
			}
		}
		return null;
	}

	/** Replies the top element rules from the grammar.
	 *
	 * @return the top element rules.
	 */
	protected Set<String> getTopElementRules() {
		Set<String> ruleNames = new TreeSet<>();
		Grammar grammar = getGrammar();
		AbstractRule rule = GrammarUtil.findRuleForName(grammar, getCodeBuilderConfig().getTopElementRuleName());
		if (rule != null) {
			for (RuleCall ruleCall : GrammarUtil.containedRuleCalls(rule)) {
				String elementName = Strings.toFirstUpper(ruleCall.getRule().getName());
				ruleNames.add(elementName);
			}
		}
		return ruleNames;
	}

}
