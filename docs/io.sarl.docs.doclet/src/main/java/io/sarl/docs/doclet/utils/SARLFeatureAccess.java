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

package io.sarl.docs.doclet.utils;

/** Test the definition of the SARL language elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SARLFeatureAccess {

	/** Internal id for the agent type.
	 */
	public static final int SARL_AGENT = 19;

	/** Internal id for the event type.
	 */
	public static final int SARL_EVENT = 15;

	/** Internal id for the behavior type.
	 */
	public static final int SARL_BEHAVIOR = 21;

	/** Internal id for the capacity type.
	 */
	public static final int SARL_CAPACITY = 20;

	/** Internal id for the skill type.
	 */
	public static final int SARL_SKILL = 22;

	/** Internal id for the space type.
	 */
	public static final int SARL_SPACE = 17;

	/** Internal id for the artifact type.
	 */
	public static final int SARL_ARTIFACT = 18;

	/** The "def" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getDefKeyword() {
		return "def"; //$NON-NLS-1$
	}

	/** The ":" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getColonKeyword() {
		return ":"; //$NON-NLS-1$
	}

	/** The "with" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getWithKeyword() {
		return "with"; //$NON-NLS-1$
	}

	/** The "=" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getEqualsSignKeyword() {
		return "="; //$NON-NLS-1$
	}

	/** The "," keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getCommaKeyword() {
		return ","; //$NON-NLS-1$
	}

	/** The "extends" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getExtendsKeyword() {
		return "extends"; //$NON-NLS-1$
	}

	/** The "super" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getSuperKeyword() {
		return "super"; //$NON-NLS-1$
	}

	/** The "&amp;" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getAmpersandKeyword() {
		return "&"; //$NON-NLS-1$
	}

	/** The "[]" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getSquareBracketKeywords() {
		return "[]"; //$NON-NLS-1$
	}

	/** The "*" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getWildcardAsteriskKeyword() {
		return "*"; //$NON-NLS-1$
	}

	/** The "(" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getLeftParenthesisKeyword() {
		return "("; //$NON-NLS-1$
	}

	/** The ")" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getRightParenthesisKeyword() {
		return ")"; //$NON-NLS-1$
	}

	/** The "=>" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getEqualsSignGreaterThanSignKeyword() {
		return "=>"; //$NON-NLS-1$
	}

	/** The "void" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getVoidKeyword() {
		return "void"; //$NON-NLS-1$
	}

	/** The "val" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getValKeyword() {
		return "val"; //$NON-NLS-1$
	}

	/** The "var" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getVarKeyword() {
		return "var"; //$NON-NLS-1$
	}

	/** The "new" keyword.
	 *
	 * @return the keyword.
	 */
	@SuppressWarnings("static-method")
	public String getNewKeyword() {
		return "new"; //$NON-NLS-1$
	}

	/** The qualified name of the {@code @SarlElementType} annotation.
	 *
	 * @return the qualified name.
	 */
	@SuppressWarnings("static-method")
	public String getSarlElementTypeAnnotationName() {
		return "io.sarl.lang.annotation.SarlElementType"; //$NON-NLS-1$
	}

	/** The qualified name of the {@code @DefaultValue} annotation.
	 *
	 * @return the qualified name.
	 */
	@SuppressWarnings("static-method")
	public String getDefaultValueAnnnotationName() {
		return "io.sarl.lang.annotation.DefaultValue"; //$NON-NLS-1$
	}

	/** The qualified name of the {@code @SarlSourceCode} annotation.
	 *
	 * @return the qualified name.
	 */
	@SuppressWarnings("static-method")
	public String getSarlSourceCodeAnnotationName() {
		return "io.sarl.lang.annotation.SarlSourceCode"; //$NON-NLS-1$
	}

	/** The qualified name of the {@code @Pure} annotation.
	 *
	 * @return the qualified name.
	 */
	@SuppressWarnings("static-method")
	public String getPureAnnotationName() {
		return "org.eclipse.xtext.xbase.lib.Pure"; //$NON-NLS-1$
	}

	/** The qualified name of the {@code @SyntheticMember} annotation.
	 *
	 * @return the qualified name.
	 */
	@SuppressWarnings("static-method")
	public String getSyntheticMemberAnnotationName() {
		return "io.sarl.lang.annotation.SyntheticMember"; //$NON-NLS-1$
	}

	/** The qualified name of the {@code @Procedures} class.
	 *
	 * @return the qualified name.
	 */
	@SuppressWarnings("static-method")
	public String getProceduresName() {
		return "org.eclipse.xtext.xbase.lib.Procedures"; //$NON-NLS-1$
	}

	/** The qualified name of the {@code @Functions} class.
	 *
	 * @return the qualified name.
	 */
	@SuppressWarnings("static-method")
	public String getFunctionsName() {
		return "org.eclipse.xtext.xbase.lib.Functions"; //$NON-NLS-1$
	}

}
