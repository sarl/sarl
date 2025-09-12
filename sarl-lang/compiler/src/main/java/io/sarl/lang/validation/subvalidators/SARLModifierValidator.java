/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.validation.subvalidators;

import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_MEMBER__MODIFIERS;

import java.text.MessageFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.validation.Check;

import com.google.inject.Inject;

import io.sarl.lang.jvmmodel.IDefaultVisibilityProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** The modifier validator for constructs.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLModifierValidator {

	private IDefaultVisibilityProvider defaultVisibilityProvider;

	private SARLGrammarKeywordAccess grammarAccess;

	private final ErrorRegisterer errorRegisterer;
	
	private final IssueRegisterer issueRegisterer;

	private final Set<String> allowedModifiers;

	private final String allowedModifiersAsStringWithAnd;
	
	private final String allowedModifiersAsStringWithOr;

	private final List<String> visibilityModifiers;

	/** Constructor.
	 *
	 * @param modifiers the list of the supported modifiers.
	 * @param visibilityModifiers the list of the visibility modifiers defined in the compiler.
	 * @param errorRegisterer the callback for registering an error.
	 * @param issueRegisterer the callback for registering an issue without specifying if it is an error or a warning.
	 */
	public SARLModifierValidator(List<String> modifiers, List<String> visibilityModifiers,
			ErrorRegisterer errorRegisterer, IssueRegisterer issueRegisterer) {
		assert modifiers != null && !modifiers.isEmpty();
		this.errorRegisterer = errorRegisterer;
		this.issueRegisterer = issueRegisterer;
		this.visibilityModifiers = visibilityModifiers;
		this.allowedModifiers = new HashSet<>(modifiers);
		final var andBuffer = new StringBuffer();
		final var orBuffer = new StringBuffer();
		var modifier = modifiers.get(0);
		andBuffer.append(modifier);
		orBuffer.append(modifier);
		final var endSize = modifiers.size() - 1;
		if (endSize > 0) {
			for (var i = 1; i < endSize; ++i) {
				modifier = modifiers.get(i);
				andBuffer.append(", ").append(modifier); //$NON-NLS-1$
				orBuffer.append(", ").append(modifier); //$NON-NLS-1$
			}
			modifier = modifiers.get(endSize);
			andBuffer.append(' ').append(Messages.SARLModifierValidator_4).append(' ').append(modifier);
			orBuffer.append(' ').append(Messages.SARLModifierValidator_5).append(' ').append(modifier);
		}
		this.allowedModifiersAsStringWithAnd = andBuffer.toString();
		this.allowedModifiersAsStringWithOr = orBuffer.toString();
	}

	/** Change the grammar access.
	 *
	 * @param grammarAccess the accessor.
	 */
	@Inject
	public void setGrammarAccess(SARLGrammarKeywordAccess grammarAccess) {
		this.grammarAccess = grammarAccess;
	}
	
	/** Change the default visibility provider.
	 *
	 * @param provider the provider of default visibility.
	 */
	@Inject
	public void setDefaultVisibilityProvider(IDefaultVisibilityProvider provider) {
		this.defaultVisibilityProvider = provider;
	}

	/** Check the modifiers for the given member.
	 *
	 * @param member the member to check.
	 * @param memberName the name of the member.
	 */
	@Check
	protected void checkModifiers(XtendMember member, String memberName) {
		final var seenModifiers = new HashSet<String>();
		var visibilitySeen = false;
		var abstractSeen = false;
		var defSeen = false;
		var staticSeen = false;
		var finalSeen = false;
		var varSeen = false;
		var defKeywordIndex = -1;
		var finalKeywordIndex = -1;

		final var privateKeyword = SARLModifierValidator.this.grammarAccess.getPrivateKeyword();
		final var protectedKeyword = SARLModifierValidator.this.grammarAccess.getProtectedKeyword();
		final var packageKeyword = SARLModifierValidator.this.grammarAccess.getPackageKeyword();
		final var publicKeyword = SARLModifierValidator.this.grammarAccess.getPublicKeyword();
		final var abstractKeyword = SARLModifierValidator.this.grammarAccess.getAbstractKeyword();
		final var staticKeyword = SARLModifierValidator.this.grammarAccess.getStaticStaticKeyword();
		final var finalKeyword = SARLModifierValidator.this.grammarAccess.getFinalKeyword();
		final var varKeyword = SARLModifierValidator.this.grammarAccess.getWriteableVarKeyword();
		final var valKeyword = SARLModifierValidator.this.grammarAccess.getValKeyword();
		final var defKeyword = SARLModifierValidator.this.grammarAccess.getDefKeyword();
		final var overrideKeyword = SARLModifierValidator.this.grammarAccess.getOverrideKeyword();

		var i = 0;
		for (final var modifier : member.getModifiers()) {
			if (!this.allowedModifiers.contains(modifier)) { 
				error(MessageFormat.format(Messages.SARLModifierValidator_2, memberName, this.allowedModifiersAsStringWithAnd),
						member, i);
			}
			if (seenModifiers.contains(modifier)) { 
				error(MessageFormat.format(Messages.SARLModifierValidator_3, memberName), 
						member, i);
			} else {
				seenModifiers.add(modifier);
				if (this.visibilityModifiers.contains(modifier)) {
					if (visibilitySeen) { 
						error(MessageFormat.format(Messages.SARLModifierValidator_6, memberName, this.allowedModifiersAsStringWithOr),
								member, i);
					}
					visibilitySeen = true;
					if (privateKeyword.equals(modifier) && isPrivateByDefault(member)) {
						unnecessaryModifierIssue(privateKeyword, memberName, member, i);
					}
					if (protectedKeyword.equals(modifier) && isProtectedByDefault(member)) {
						unnecessaryModifierIssue(protectedKeyword, memberName, member, i);
					}
					if (packageKeyword.equals(modifier) && isPackageByDefault(member)) {
						unnecessaryModifierIssue(packageKeyword, memberName, member, i);
					}
					if (publicKeyword.equals(modifier) && isPublicByDefault(member)) {
						unnecessaryModifierIssue(publicKeyword, memberName, member, i);
					}
				}
			} 
			if (Objects.equals(modifier, abstractKeyword)) {
				if (finalSeen) {
					error(MessageFormat.format(Messages.SARLModifierValidator_7, memberName),
							member, i);
				}
				if (staticSeen && !(member instanceof XtendTypeDeclaration)) {
					error(MessageFormat.format(Messages.SARLModifierValidator_8, memberName),
							member, i);
				}
				abstractSeen = true;
			} else if (Objects.equals(modifier, staticKeyword)) {
				if (abstractSeen && !(member instanceof XtendTypeDeclaration)) {
					error(MessageFormat.format(Messages.SARLModifierValidator_9, memberName),
							member, i);
				}
				staticSeen = true;
			} else if (Objects.equals(modifier, finalKeyword) || Objects.equals(modifier, valKeyword)) {
				if (abstractSeen) {
					error(MessageFormat.format(Messages.SARLModifierValidator_10, memberName),
							member, i);
				}
				if (varSeen) {
					error(MessageFormat.format(Messages.SARLModifierValidator_11, memberName),
							member, i);
				}
				if (Objects.equals(modifier, finalKeyword)) {
					finalKeywordIndex = i;
				}
				if (finalSeen) {
					// Independent of the order of the keywords (such as 'final val' or 'val final'), 
					// the 'final' keyword should be marked with the issue marker
					unnecessaryModifierIssue(finalKeyword, memberName, member, finalKeywordIndex);
				}
				finalSeen = true;
			} else if (Objects.equals(modifier, varKeyword)) {
				if (finalSeen) {
					error(MessageFormat.format(Messages.SARLModifierValidator_11, memberName),
							member, i);
				}
				varSeen = true;
			} else if ((Objects.equals(modifier, defKeyword) || Objects.equals(modifier, overrideKeyword))
					&& member instanceof XtendFunction) {
				if (Objects.equals(modifier, defKeyword)) {
					defKeywordIndex = i;					
				}
				if(defSeen) {
					// Independent of the order of the keywords (such as 'override def' or 'def override'), 
					// the 'def' keyword should be marked with the issue marker
					unnecessaryModifierIssue(defKeyword, memberName, member, defKeywordIndex);
				}
				defSeen = true;
			}

			++i;
		}
	}

	/** Report an issue for unnecessary modifier
	 * 
	 * @param modifier the unnecessary modifier.
	 * @param memberName the name of the member to which the unnecessary modifier is attached.
	 * @param source the source of the issue.
	 * @param index the index of the element to which the issue is attached.
	 */
	protected void unnecessaryModifierIssue(String modifier, String memberName, EObject source, int index) {
		issue(MessageFormat.format(Messages.SARLModifierValidator_1, modifier, memberName),
				source, index, IssueCodes.UNNECESSARY_MODIFIER, modifier);
	}

	/** Report an issue through the associated validator.
	 * 
	 * @param message the issue message.
	 * @param source the source of the issue.
	 * @param index the index of the element to which the issue is attached.
	 * @param code the code of the issue.
	 * @param issueData data associated to the issue.
	 */
	protected void issue(String message, EObject source, int index, String code, String... issueData) {
		this.issueRegisterer.issue(message, source, XTEND_MEMBER__MODIFIERS, index, code, issueData);
	}

	/** Report an "invalid modifier" error through the associated validator.
	 * 
	 * @param message the error message.
	 * @param source the source of the error.
	 * @param index the index of the element to which the error is attached.
	 */
	protected void error(String message, EObject source, int index) {
		this.errorRegisterer.error(message, source, XTEND_MEMBER__MODIFIERS, index, IssueCodes.INVALID_MODIFIER);
	}

	/** Replies if the default visibility modifier for the given member is "private".
	 * If this function replies {@code true}, the "private" modifier is assumed to be the default one for the given member.
	 * This function may be used for printing out an "unnecessary modifier" warning when the "private" modifier is explicitly
	 * attached to the given member.
	 *
	 * <p>This function is defined for being overridden by subclasses.
	 *
	 * @param member the member to test.
	 * @return {@code true} if the "private" modifier is the modifier by default for the given member.
	 */
	protected boolean isPrivateByDefault(XtendMember member) {
		final var defaultVisibility = SARLModifierValidator.this.defaultVisibilityProvider.getDefaultJvmVisibility(member);
		return defaultVisibility == JvmVisibility.PRIVATE;
	}

	/** Replies if the default visibility modifier for the given member is "protected".
	 * If this function replies {@code true}, the "protected" modifier is assumed to be the default one for the given member.
	 * This function may be used for printing out an "unnecessary modifier" warning when the "protected" modifier is explicitly
	 * attached to the given member.
	 *
	 * <p>This function is defined for being overridden by subclasses.
	 *
	 * @param member the member to test.
	 * @return {@code true} if the "protected" modifier is the modifier by default for the given member.
	 */
	protected boolean isProtectedByDefault(XtendMember member) {
		final var defaultVisibility = SARLModifierValidator.this.defaultVisibilityProvider.getDefaultJvmVisibility(member);
		return defaultVisibility == JvmVisibility.PROTECTED;
	}

	/** Replies if the default visibility modifier for the given member is "package".
	 * If this function replies {@code true}, the "package" modifier is assumed to be the default one for the given member.
	 * This function may be used for printing out an "unnecessary modifier" warning when the "package" modifier is explicitly
	 * attached to the given member.
	 *
	 * <p>This function is defined for being overridden by subclasses.
	 *
	 * @param member the member to test.
	 * @return {@code true} if the "package" modifier is the modifier by default for the given member.
	 */
	protected boolean isPackageByDefault(XtendMember member) {
		final var defaultVisibility = SARLModifierValidator.this.defaultVisibilityProvider.getDefaultJvmVisibility(member);
		return defaultVisibility == JvmVisibility.DEFAULT;
	}

	/** Replies if the default visibility modifier for the given member is "public".
	 * If this function replies {@code true}, the "public" modifier is assumed to be the default one for the given member.
	 * This function may be used for printing out an "unnecessary modifier" warning when the "public" modifier is explicitly
	 * attached to the given member.
	 *
	 * <p>This function is defined for being overridden by subclasses.
	 *
	 * @param member the member to test.
	 * @return {@code true} if the "public" modifier is the modifier by default for the given member.
	 */
	protected boolean isPublicByDefault(XtendMember member) {
		final var defaultVisibility = SARLModifierValidator.this.defaultVisibilityProvider.getDefaultJvmVisibility(member);
		return defaultVisibility == JvmVisibility.PUBLIC;
	}

	/** Registerer of error.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@FunctionalInterface
	public interface ErrorRegisterer {

		/** Register an error.
		 * 
		 * @param message the error message.
		 * @param object the object with error.
		 * @param feature the feature with error.
		 * @param index the index of the element in the {@code feature}.
		 * @param code the error code.
		 * @param issueData several data associated to the error.
		 */
		void error(String message, EObject object, EStructuralFeature feature, int index, String code,
			String... issueData);
			
	}

	/** Registerer of issue (no specification of the type of issue).
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@FunctionalInterface
	public interface IssueRegisterer {

		/** Register an error.
		 * 
		 * @param message the error message.
		 * @param object the object with error.
		 * @param feature the feature with error.
		 * @param index the index of the element in the {@code feature}.
		 * @param code the error code.
		 * @param issueData several data associated to the error.
		 */
		void issue(String message, EObject object, EStructuralFeature feature, int index, String code,
			String... issueData);
			
	}

}
