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

import com.google.inject.Injector;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.IGuiceAwareGeneratorComponent;

/**
 * A component for configuring the CodeBuilderFragment2.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:methodcount")
public class ExpressionConfig implements IGuiceAwareGeneratorComponent {

	/** Default regular expression that is matching an expression in the grammar.
	 */
	private static final String EXPRESSION_GRAMMAR_PATTERN = "^XExpression$"; //$NON-NLS-1$

	/** Default regular expression that is matching a block expression in the grammar.
	 */
	private static final String BLOCK_EXPRESSION_GRAMMAR_PATTERN = "^XBlockExpression$"; //$NON-NLS-1$

	/** Default regular expression that is matching the typename of a field that is containing an expression.
	 */
	private static final String EXPRESSION_FIELD_TYPENAME_PATTERN = "Field$"; //$NON-NLS-1$

	/** Default keyword for declaring a field.
	 */
	private static final String DEFAULT_FIELD_DECLARATION_KEYWORD = "var"; //$NON-NLS-1$

	/** Default keyword for declaring the container of a field.
	 */
	private static final String DEFAULT_FIELD_CONTAINER_DECLARATION_KEYWORD = "class"; //$NON-NLS-1$

	/** Default keyword for declaring a block member.
	 */
	private static final String DEFAULT_BLOCK_MEMBER_DECLARATION_KEYWORD = "def"; //$NON-NLS-1$

	/** Default keyword for declaring the container of a block member.
	 */
	private static final String DEFAULT_BLOCK_MEMBER_CONTAINER_DECLARATION_KEYWORD = DEFAULT_FIELD_CONTAINER_DECLARATION_KEYWORD;

	private String expressionFieldTypenamePattern = EXPRESSION_FIELD_TYPENAME_PATTERN;

	private String expressionGrammarPattern = EXPRESSION_GRAMMAR_PATTERN;

	private String blockExpressionGrammarPattern = BLOCK_EXPRESSION_GRAMMAR_PATTERN;

	private String fieldDeclarationKeyword = DEFAULT_FIELD_DECLARATION_KEYWORD;

	private String fieldContainerDeclarationKeyword = DEFAULT_FIELD_CONTAINER_DECLARATION_KEYWORD;

	private String blockMemberDeclarationKeyword = DEFAULT_BLOCK_MEMBER_DECLARATION_KEYWORD;

	private String blockMemberContainerDeclarationKeyword = DEFAULT_BLOCK_MEMBER_CONTAINER_DECLARATION_KEYWORD;

	/** Set the regular expression that is matching the typename of a field that is containing an expression.
	 *
	 * @param pattern the pattern for the type.
	 */
	public void setExpressionFieldTypenamePattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.expressionFieldTypenamePattern = pattern;
		}
	}

	/** Replies the regular expression that is matching the typename of a field that is containing an expression.
	 *
	 * @return the pattern for typenames.
	 */
	@Pure
	public String getExpressionFieldTypenamePattern() {
		return this.expressionFieldTypenamePattern;
	}

	/** Set the pattern that is matching an expression in the grammar.
	 *
	 * @param pattern the pattern for an expression.
	 */
	public void setExpressionGrammarPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.expressionGrammarPattern = pattern;
		}
	}

	/** Replies the pattern that is matching an expression in the grammar.
	 *
	 * @return the pattern for an expression.
	 */
	@Pure
	public String getExpressionGrammarPattern() {
		return this.expressionGrammarPattern;
	}

	@Override
	public void initialize(Injector injector) {
		injector.injectMembers(this);
	}

	/** Set the pattern that is matching a block expression in the grammar.
	 *
	 * @param pattern the pattern for a block expression.
	 */
	public void setBlockExpressionGrammarPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.blockExpressionGrammarPattern = pattern;
		}
	}

	/** Replies the pattern that is matching a block expression in the grammar.
	 *
	 * @return the pattern for a block expression.
	 */
	@Pure
	public String getBlockExpressionGrammarPattern() {
		return this.blockExpressionGrammarPattern;
	}

	/** Set the keyword for declaring a field.
	 *
	 * @param keyword the keyword.
	 */
	public void setFieldDeclarationKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.fieldDeclarationKeyword = keyword;
		}
	}

	/** Replies the keyword for declaring a field.
	 *
	 * @return the keyword.
	 */
	public String getFieldDeclarationKeyword() {
		return this.fieldDeclarationKeyword;
	}

	/** Set the keyword for declaring the container of a field.
	 *
	 * @param keyword the keyword.
	 */
	public void setFieldContainerDeclarationKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.fieldContainerDeclarationKeyword = keyword;
		}
	}

	/** Replies the keyword for declaring the container of a field.
	 *
	 * @return the keyword.
	 */
	public String getFieldContainerDeclarationKeyword() {
		return this.fieldContainerDeclarationKeyword;
	}

	/** Set the keyword for declaring a block member.
	 *
	 * @param keyword the keyword.
	 */
	public void setBlockMemberDeclarationKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.blockMemberDeclarationKeyword = keyword;
		}
	}

	/** Replies the keyword for declaring a block member.
	 *
	 * @return the keyword.
	 */
	public String getBlockMemberDeclarationKeyword() {
		return this.blockMemberDeclarationKeyword;
	}

	/** Set the keyword for declaring the container of a block member.
	 *
	 * @param keyword the keyword.
	 */
	public void setBlockMemberContainerDeclarationKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.blockMemberContainerDeclarationKeyword = keyword;
		}
	}

	/** Replies the keyword for declaring the container of a block member.
	 *
	 * @return the keyword.
	 */
	public String getBlockMemberContainerDeclarationKeyword() {
		return this.blockMemberContainerDeclarationKeyword;
	}

}
