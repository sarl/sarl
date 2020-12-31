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

package io.sarl.lang.mwe2.codebuilder.config;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.google.inject.Injector;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.IGuiceAwareGeneratorComponent;
import org.eclipse.xtext.xtext.generator.util.BooleanGeneratorOption;

/**
 * A component for configuring the CodeBuilderFragment2.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:methodcount")
public class CodeBuilderConfig implements IGuiceAwareGeneratorComponent {

	/** Default name of the assignment in the grammar that is used for representing a type extension.
	 */
	private static final String TYPE_EXTENSION_GRAMMAR_NAME = "extends"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing a type implementation.
	 */
	private static final String TYPE_IMPLEMENTATION_GRAMMAR_NAME = "implements"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing an annotation assignment.
	 */
	private static final String ANNOTATION_LIST_GRAMMAR_NAME = "annotations"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing a modifier assignment.
	 */
	private static final String MODIFIER_LIST_GRAMMAR_NAME = "modifiers"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing a type parameter assignment.
	 */
	private static final String TYPE_PARAMETER_LIST_GRAMMAR_NAME = "typeParameters"; //$NON-NLS-1$

	/** Default name of the rule in the grammar that is used for parsing a type parameter.
	 */
	private static final String TYPE_PARAMETER_RULE_NAME = "JvmTypeParameter"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing a collection of members.
	 */
	private static final String MEMBER_COLLECTION_GRAMMAR_NAME = "members"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the name of a member.
	 */
	private static final String MEMBER_NAME_GRAMMAR_NAME = "name"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the thrown exceptions  of a member.
	 */
	private static final String MEMBER_THROWS_GRAMMAR_NAME = "exceptions"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the fired events of a member.
	 */
	private static final String MEMBER_FIRES_GRAMMAR_NAME = "firedEvents"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the return type of a member.
	 */
	private static final String MEMBER_RETURN_TYPE_GRAMMAR_NAME = "returnType"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the type of a member.
	 */
	private static final String MEMBER_TYPE_GRAMMAR_NAME = "type"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the block expression of a member.
	 */
	private static final String MEMBER_BLOCK_EXPRESSION_GRAMMAR_NAME = "expression"; //$NON-NLS-1$

	/** Default regular expression that is matching a type reference in the grammar.
	 */
	private static final String TYPE_REFERENCE_GRAMMAR_PATTERN = "TypeReference$"; //$NON-NLS-1$

	/** Default text for the "auto-generated" comment.
	 */
	private static final String AUTO_GENERATED_COMMENT = "Auto-generated code."; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the name of a formal parameter.
	 */
	private static final String PARAMETER_NAME_GRAMMAR_NAME = "name"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the type of a formal parameter.
	 */
	private static final String PARAMETER_TYPE_GRAMMAR_NAME = "parameterType"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the variadic argument
	 * flag of a formal parameter.
	 */
	private static final String PARAMETER_VARARG_GRAMMAR_NAME = "varArg"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the default value of a formal parameter.
	 */
	private static final String PARAMETER_DEFAULT_VALUE_GRAMMAR_NAME = "defaultValue"; //$NON-NLS-1$

	/** Default name of the assignment in the grammar that is used for representing the list of formal parameters.
	 */
	private static final String PARAMETER_LIST_GRAMMAR_NAME = "parameters"; //$NON-NLS-1$

	/** Default type for the formal parameter container.
	 */
	private static final String PARAMETER_CONTAINER_TYPE = "org.eclipse.xtend.core.xtend.XtendExecutable"; //$NON-NLS-1$

	/** Default flag that is indicating if Xtend support is enable.
	 */
	private static final boolean XTEND_SUPPORT_ENABLED = true;

	/** Default prefix for avoiding overriding of Google inject types.
	 */
	private static final String FORBIDDEN_GOOGLE_INJECT = "com.google.inject"; //$NON-NLS-1$

	/** Default postfix for avoiding overriding of loggers.
	 */
	private static final String FORBIDDEN_LOGGER = "Logger"; //$NON-NLS-1$

	private String scriptRuleName;

	private String topElementRuleName;

	private String typeParameterRuleName = TYPE_PARAMETER_RULE_NAME;

	private String formalParameterRuleName;

	private String formalParameterContainerType = PARAMETER_CONTAINER_TYPE;

	private final Set<String> multilineCommentedTypes = new HashSet<>();

	private String typeExtensionGrammarName = TYPE_EXTENSION_GRAMMAR_NAME;

	private String typeImplementationGrammarName = TYPE_IMPLEMENTATION_GRAMMAR_NAME;

	private String annotationListGrammarName = ANNOTATION_LIST_GRAMMAR_NAME;

	private String modifierListGrammarName = MODIFIER_LIST_GRAMMAR_NAME;

	private String typeParameterListGrammarName = TYPE_PARAMETER_LIST_GRAMMAR_NAME;

	private String memberCollectionGrammarName = MEMBER_COLLECTION_GRAMMAR_NAME;

	private String memberNameGrammarName = MEMBER_NAME_GRAMMAR_NAME;

	private final Set<String> unnamedMemberGrammarNames = new HashSet<>();

	private String memberTypeGrammarName = MEMBER_TYPE_GRAMMAR_NAME;

	private String memberThrowsGrammarName = MEMBER_THROWS_GRAMMAR_NAME;

	private String memberFiresGrammarName = MEMBER_FIRES_GRAMMAR_NAME;

	private String memberReturnTypeGrammarName = MEMBER_RETURN_TYPE_GRAMMAR_NAME;

	private String memberBlockExpressionGrammarName = MEMBER_BLOCK_EXPRESSION_GRAMMAR_NAME;

	private String typeReferenceGrammarPattern = TYPE_REFERENCE_GRAMMAR_PATTERN;

	private String autoGeneratedComment = AUTO_GENERATED_COMMENT;

	private Set<String> noActionBodyContainer = new TreeSet<>();

	private String parameterNameGrammarName = PARAMETER_NAME_GRAMMAR_NAME;

	private String parameterTypeGrammarName = PARAMETER_TYPE_GRAMMAR_NAME;

	private String parameterVarArgGrammarName = PARAMETER_VARARG_GRAMMAR_NAME;

	private String parameterDefaultValueGrammarName = PARAMETER_DEFAULT_VALUE_GRAMMAR_NAME;

	private String parameterListGrammarName = PARAMETER_LIST_GRAMMAR_NAME;

	private boolean isXtendSupportEnable = XTEND_SUPPORT_ENABLED;

	private final Map<String, String> superTypeMapping = new TreeMap<>();

	private final Map<String, List<String>> modifiers = new TreeMap<>();

	private final Set<String> constructorFree = new TreeSet<>();

	private final BooleanGeneratorOption enableISourceAppendable = new BooleanGeneratorOption(true);

	private final BooleanGeneratorOption generateUnitTests = new BooleanGeneratorOption(true);

	private final ExpressionConfig expression = new ExpressionConfig();

	private final Set<String> forbiddenInjectionPrefixes = new TreeSet<>(Collections.singleton(FORBIDDEN_GOOGLE_INJECT));

	private final Set<String> forbiddenInjectionPostfixes = new TreeSet<>(Collections.singleton(FORBIDDEN_LOGGER));

	/** Add a prefix of typenames that should not be considered for injection overriding.
	 *
	 * @param prefix the prefix.
	 */
	public void addForbiddenInjectionPrefix(String prefix) {
		if (!Strings.isEmpty(prefix)) {
			final String real = prefix.endsWith(".") ? prefix.substring(0, prefix.length() - 1) : prefix; //$NON-NLS-1$
			this.forbiddenInjectionPrefixes.add(real);
		}
	}

	/** Add a postfix of typenames that should not be considered for injection overriding.
	 *
	 * @param postfix the postfix.
	 */
	public void addForbiddenInjectionPostfixes(String postfix) {
		if (!Strings.isEmpty(postfix)) {
			final String real = postfix.startsWith(".") ? postfix.substring(1) : postfix; //$NON-NLS-1$
			this.forbiddenInjectionPrefixes.add(real);
		}
	}

	/** Replies the prefixes of typenames that should not be considered for injection overriding.
	 *
	 * @return the prefixes.
	 */
	@Pure
	public Set<String> getForbiddenInjectionPrefixes() {
		return this.forbiddenInjectionPrefixes;
	}

	/** Replies the postfixes of typenames that should not be considered for injection overriding.
	 *
	 * @return the postfixes.
	 */
	@Pure
	public Set<String> getForbiddenInjectionPostfixes() {
		return this.forbiddenInjectionPostfixes;
	}

	/** Add a type that should be commented with multiline comments.
	 *
	 * @param type the qualified name of the type.
	 */
	public void addMultilineCommentedType(String type) {
		if (!Strings.isEmpty(type)) {
			this.multilineCommentedTypes.add(type);
		}
	}

	/** Replies if the Xtend support is enabled.
	 *
	 * @return <code>true</code> if the Xtend support is enabled.
	 */
	@Pure
	public boolean isXtendSupportEnabled() {
		return this.isXtendSupportEnable;
	}

	/** Change if the Xtend support is enabled.
	 *
	 * @param enable <code>true</code> if the Xtend support is enabled.
	 */
	public void setXtendSupportEnabled(boolean enable) {
		this.isXtendSupportEnable = enable;
	}

	/** Replies the types that should be commented with multiline comments.
	 *
	 * @return the qualified names of the types.
	 */
	@Pure
	public Set<String> getMultilineCommentedTypes() {
		return this.multilineCommentedTypes;
	}

	/** Enable or disable the generation of the unit tests.
	 *
	 * @param enable <code>true</code> for enabling, <code>false</code> for disabling.
	 */
	public void setGenerateUnitTests(boolean enable) {
		this.generateUnitTests.set(enable);
	}

	/** Replies if the generation of the unit tests is enable.
	 *
	 * @return <code>true</code> for enabling, <code>false</code> for disabling.
	 */
	@Pure
	public boolean isUnitTestGenerationEnable() {
		return this.generateUnitTests.get();
	}

	/** Enable or disable the generation of the <code>ISourceAppendable}</code> API.
	 *
	 * @param enable <code>true</code> for enabling, <code>false</code> for disabling.
	 */
	public void setEnableISourceAppendable(boolean enable) {
		this.enableISourceAppendable.set(enable);
	}

	/** Replies if the generation of the <code>ISourceAppendable}</code> API is enable.
	 *
	 * @return <code>true</code> for enabling, <code>false</code> for disabling.
	 */
	@Pure
	public boolean isISourceAppendableEnable() {
		return this.enableISourceAppendable.get();
	}

	/** Add a type name that corresponds to a constructor free element.
	 *
	 * @param typeName the simple name of the type without constructor member.
	 */
	public void addConstructorFreeType(String typeName) {
		if (!Strings.isEmpty(typeName)) {
			this.constructorFree.add(typeName);
		}
	}

	/** Replies the types that corresponds to a constructor free element.
	 *
	 * @return the names of the types without constructor.
	 */
	@Pure
	public Set<String> getConstructorFreeTypes() {
		return this.constructorFree;
	}

	/** Set the name that is used for representing the list of a formal parameters in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the list of a formal parameters.
	 */
	public void setParameterListGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.parameterListGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the list of a formal parameters in the grammar's assignments.
	 *
	 * @return the name of the assignment for the list of a formal parameters.
	 */
	@Pure
	public String getParameterListGrammarName() {
		return this.parameterListGrammarName;
	}

	/** Set the name that is used for representing the default value of a formal parameter in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the default value of a formal parameter.
	 */
	public void setParameterDefaultValueGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.parameterDefaultValueGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the default value of a formal parameter in the grammar's assignments.
	 *
	 * @return the name of the assignment for the default value of a formal parameter.
	 */
	@Pure
	public String getParameterDefaultValueGrammarName() {
		return this.parameterDefaultValueGrammarName;
	}

	/** Set the name that is used for representing the variadic argument flag of a formal parameter in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the vararg of a formal parameter.
	 */
	public void setParameterVarArgGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.parameterVarArgGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the variadic argument flag
	 * of a formal parameter in the grammar's assignments.
	 *
	 * @return the name of the assignment for the name of a formal parameter.
	 */
	@Pure
	public String getParameterVarArgGrammarName() {
		return this.parameterVarArgGrammarName;
	}

	/** Set the name that is used for representing the type of a formal parameter in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the type of a formal parameter.
	 */
	public void setParameterTypeGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.parameterTypeGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the type of a formal parameter in the grammar's assignments.
	 *
	 * @return the name of the assignment for the type of a formal parameter.
	 */
	@Pure
	public String getParameterTypeGrammarName() {
		return this.parameterTypeGrammarName;
	}

	/** Set the name that is used for representing the name of a formal parameter in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the name of a formal parameter.
	 */
	public void setParameterNameGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.parameterNameGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the name of a formal parameter in the grammar's assignments.
	 *
	 * @return the name of the assignment for the name of a formal parameter.
	 */
	@Pure
	public String getParameterNameGrammarName() {
		return this.parameterNameGrammarName;
	}

	/** Add a modifier for a rule.
	 *
	 * <p>The first modifier is the default modifier.
	 *
	 * @param modifier the modifier.
	 */
	public void addModifier(Modifier modifier) {
		if (modifier != null) {
			final String ruleName = modifier.getType();
			if (!Strings.isEmpty(ruleName)) {
				List<String> modifiers = this.modifiers.get(ruleName);
				if (modifiers == null) {
					modifiers = new ArrayList<>();
					this.modifiers.put(ruleName, modifiers);
				}
				modifiers.addAll(modifier.getModifiers());
			}
		}
	}

	/** Replies the modifiers per rule.
	 *
	 * @return the modifiers per rule.
	 */
	@Pure
	public Map<String, List<String>> getModifiers() {
		return this.modifiers;
	}

	/** Add a type that contains actions/functions without body.
	 *
	 * @param containerName the name of the type.
	 */
	public void addNoActionBodyType(String containerName) {
		if (!Strings.isEmpty(containerName)) {
			this.noActionBodyContainer.add(containerName);
		}
	}

	/** Replies the types that contain actions/functions without body.
	 *
	 * @return the type names.
	 */
	@Pure
	public Set<String> getNoActionBodyTypes() {
		return this.noActionBodyContainer;
	}

	/** Set the comment for the auto generated blocks.
	 *
	 * @param comment the text of the comment.
	 */
	public void setAutoGeneratedComment(String comment) {
		this.autoGeneratedComment = Strings.emptyIfNull(comment);
	}

	/** Replies the comment for the auto generated blocks.
	 *
	 * @return the text of the comment.
	 */
	@Pure
	public String getAutoGeneratedComment() {
		return this.autoGeneratedComment;
	}

	/** Set the pattern that is matching a type reference in the grammar.
	 *
	 * @param pattern the pattern for a type reference.
	 */
	public void setTypeReferenceGrammarPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.typeReferenceGrammarPattern = pattern;
		}
	}

	/** Replies the pattern that is matching a type reference in the grammar.
	 *
	 * @return the pattern for a type reference.
	 */
	@Pure
	public String getTypeReferenceGrammarPattern() {
		return this.typeReferenceGrammarPattern;
	}

	/** Set the name that is used for representing the block expression of a member in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the block expression of a member.
	 */
	public void setMemberBlockExpressionGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.memberBlockExpressionGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the block expression of a member in the grammar's assignments.
	 *
	 * @return the name of the assignment for the block expression of a member.
	 */
	@Pure
	public String getMemberBlockExpressionExtensionGrammarName() {
		return this.memberBlockExpressionGrammarName;
	}

	/** Set the name that is used for representing the return type of a member in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the return type of a member.
	 */
	public void setMemberReturnTypeGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.memberReturnTypeGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the return type of a member in the grammar's assignments.
	 *
	 * @return the name of the assignment for the return type of a member.
	 */
	@Pure
	public String getMemberReturnTypeExtensionGrammarName() {
		return this.memberReturnTypeGrammarName;
	}

	/** Set the name that is used for representing the thrown exceptions of a member in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the thrown exceptions of a member.
	 */
	public void setMemberThrowsGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.memberThrowsGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the thrown exceptions of a member in the grammar's assignments.
	 *
	 * @return the name of the assignment for the thrown exceptions of a member.
	 */
	@Pure
	public String getMemberThrowsExtensionGrammarName() {
		return this.memberThrowsGrammarName;
	}

	/** Set the name that is used for representing the fired events of a member in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the fired events of a member.
	 */
	public void setMemberFiresGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.memberFiresGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the fired events of a member in the grammar's assignments.
	 *
	 * @return the name of the assignment for the fired events of a member.
	 */
	@Pure
	public String getMemberFiresExtensionGrammarName() {
		return this.memberFiresGrammarName;
	}

	/** Set the name that is used for representing the type of a member in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the type of a member.
	 */
	public void setMemberTypeGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.memberTypeGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the type of a member in the grammar's assignments.
	 *
	 * @return the name of the assignment for the type of a member.
	 */
	@Pure
	public String getMemberTypeExtensionGrammarName() {
		return this.memberTypeGrammarName;
	}

	/** Set the name that is used for representing the name of a member in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the name of a member.
	 */
	public void setMemberNameGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.memberNameGrammarName = name;
		}
	}

	/** Replies the name that is used for representing the name of a member in the grammar's assignments.
	 *
	 * @return the name of the assignment for the name of a member.
	 */
	@Pure
	public String getMemberNameExtensionGrammarName() {
		return this.memberNameGrammarName;
	}

	/** Set the name that is used for representing the name of a member in the grammar's assignments.
	 *
	 * @param name the name of the assignment for the name of a member.
	 */
	public void addUnnamedMemberExtensionGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.unnamedMemberGrammarNames.add(name);
		}
	}

	/** Replies the name that is used for representing the name of a member in the grammar's assignments.
	 *
	 * @return the name of the assignment for the name of a member.
	 */
	@Pure
	public Set<String> getUnnamedMemberExtensionGrammarNames() {
		return this.unnamedMemberGrammarNames;
	}

	/** Set the name that is used for representing collection of members in the grammar's assignments.
	 *
	 * @param name the name of the assignment for collection of members.
	 */
	public void setMemberCollectionGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.memberCollectionGrammarName = name;
		}
	}

	/** Replies the name that is used for representing collection of members in the grammar's assignments.
	 *
	 * @return the name of the assignment for collection of members.
	 */
	@Pure
	public String getMemberCollectionExtensionGrammarName() {
		return this.memberCollectionGrammarName;
	}

	/** Set the name that is used for representing type extensions in the grammar's assignments.
	 *
	 * @param name the name of the assignment for type extensions.
	 */
	public void setTypeExtensionGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.typeExtensionGrammarName = name;
		}
	}

	/** Replies the name that is used for representing type extensions in the grammar's assignments.
	 *
	 * @return the name of the assignment for type extensions.
	 */
	@Pure
	public String getTypeExtensionGrammarName() {
		return this.typeExtensionGrammarName;
	}

	/** Set the name that is used for representing type implementations in the grammar's assignments.
	 *
	 * @param name the name of the assignment for type implementations.
	 */
	public void setTypeImplementationGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.typeImplementationGrammarName = name;
		}
	}

	/** Replies the name that is used for representing annotations in the grammar's assignments.
	 *
	 * @return the name of the assignment for annotations.
	 */
	@Pure
	public String getAnnotationListGrammarName() {
		return this.annotationListGrammarName;
	}

	/** Set the name that is used for representing annotations in the grammar's assignments.
	 *
	 * @param name the name of the assignment for annotations.
	 */
	public void setAnnotationListGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.annotationListGrammarName = name;
		}
	}

	/** Replies the name that is used for representing modifiers in the grammar's assignments.
	 *
	 * @return the name of the assignment for modifiers.
	 */
	@Pure
	public String getModifierListGrammarName() {
		return this.modifierListGrammarName;
	}

	/** Set the name that is used for representing modifiers in the grammar's assignments.
	 *
	 * @param name the name of the assignment for modifiers.
	 */
	public void setModifierListGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.modifierListGrammarName = name;
		}
	}

	/** Replies the name that is used for representing type parameters in the grammar's assignments.
	 *
	 * @return the name of the assignment for type parameters.
	 */
	@Pure
	public String getTypeParameterListGrammarName() {
		return this.typeParameterListGrammarName;
	}

	/** Set the name that is used for representing type parameters in the grammar's assignments.
	 *
	 * @param name the name of the assignment for type parameters.
	 */
	public void setTypeParameterListGrammarName(String name) {
		if (!Strings.isEmpty(name)) {
			this.typeParameterListGrammarName = name;
		}
	}

	/** Replies the name that is used for representing type implementations in the grammar's assignments.
	 *
	 * @return the name of the assignment for type implementations.
	 */
	@Pure
	public String getTypeImplementationGrammarName() {
		return this.typeImplementationGrammarName;
	}

	/** Add a default super type.
	 *
	 * @param mapping the mapping for the super type.
	 */
	public void addDefaultSuper(SuperTypeMapping mapping) {
		if (mapping != null) {
			this.superTypeMapping.put(mapping.getType(), mapping.getSuper());
		}
	}

	/** Replies the default super types.
	 *
	 * @return the default super types.
	 */
	@Pure
	public Map<String, String> getDefaultSupers() {
		return this.superTypeMapping;
	}

	/** Change the name of the grammar rule that defines the script.
	 *
	 * @param name the name of the rule.
	 */
	public void setScriptRuleName(String name) {
		if (!Strings.isEmpty(name)) {
			this.scriptRuleName = name;
		}
	}

	/** Replies the name of the grammar rule that defines the script.
	 *
	 * @return the name of the rule.
	 */
	@Pure
	public String getScriptRuleName() {
		return this.scriptRuleName;
	}

	/** Change the name of the grammar rule that defines the top elements.
	 *
	 * @param name the name of the rule.
	 */
	public void setTopElementRuleName(String name) {
		if (!Strings.isEmpty(name)) {
			this.topElementRuleName = name;
		}
	}

	/** Replies the name of the grammar rule that defines the top elements.
	 *
	 * @return the name of the rule.
	 */
	@Pure
	public String getTopElementRuleName() {
		return this.topElementRuleName;
	}

	/** Change the name of the grammar rule that defines the type parameters.
	 *
	 * @param name the name of the rule.
	 */
	public void setTypeParameterRuleName(String name) {
		if (!Strings.isEmpty(name)) {
			this.typeParameterRuleName = name;
		}
	}

	/** Replies the name of the grammar rule that defines the type parameters.
	 *
	 * @return the name of the rule.
	 */
	@Pure
	public String getTypeParameterRuleName() {
		return this.typeParameterRuleName;
	}

	/** Change the name of the type that is a formal parameter container.
	 *
	 * @param name the name of the type.
	 */
	public void setFormalParameterContainerType(String name) {
		if (!Strings.isEmpty(name)) {
			this.formalParameterContainerType = name;
		}
	}

	/** Replies the name of the type that is a formal parameter container.
	 *
	 * @return the name of the type.
	 */
	@Pure
	public String getFormalParameterContainerType() {
		return this.formalParameterContainerType;
	}

	/** Change the rule name for a formal parameter.
	 *
	 * @param rule the rule name for a formal parameter.
	 */
	public void setFormalParameterRuleName(String rule) {
		if (!Strings.isEmpty(rule)) {
			this.formalParameterRuleName = rule;
		}
	}

	/** Replies the rule name for a formal parameter.
	 *
	 * @return the rule name for a formal parameter.
	 */
	@Pure
	public String getFormalParameterRuleName() {
		return this.formalParameterRuleName;
	}

	@Override
	public void initialize(Injector injector) {
		injector.injectMembers(this);
		this.expression.initialize(injector);
	}

	/** Replies the configuration for expressions.
	 *
	 * @return the configuration.
	 */
	public ExpressionConfig getExpression() {
		return this.expression;
	}

}
