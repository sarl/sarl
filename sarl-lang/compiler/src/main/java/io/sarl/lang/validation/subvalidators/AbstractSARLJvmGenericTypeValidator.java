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

package io.sarl.lang.validation.subvalidators;

import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FIELD__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__RETURN_TYPE;

import java.lang.ref.WeakReference;
import java.util.Map;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.xbase.typesystem.override.OverrideHelper;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.IVisibilityHelper;
import org.eclipse.xtext.xbase.validation.JvmGenericTypeValidator;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.validation.ISARLValidator;

/**
 * An abstract implementation for all the validators based on JVM metamodel..
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.14
 */
public abstract class AbstractSARLJvmGenericTypeValidator extends JvmGenericTypeValidator {

	@Inject
	private SarlJvmModelAssociations associations;

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	@Inject
	private OverrideHelper overrideHelper;

	@Inject
	private IVisibilityHelper visibilityHelper;

	@Inject
	private Provider<ISARLValidator> parentValidatorProvider;

	private WeakReference<ISARLValidator> parentValidator;

	@Override
	public IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject eObject) {
		return getParentValidator().getIssueSeverities(context, eObject);
	}

	/** Replies the parent validator if defined.
	 *
	 * @return parent validator.
	 */
	public ISARLValidator getParentValidator() {
		var ref = this.parentValidator == null ? null : this.parentValidator.get();
		if (ref == null) {
			ref = this.parentValidatorProvider.get();
			this.parentValidator = new WeakReference<>(ref);
		}
		return ref;
	}

	/** Replies the tool for managing visibility modifiers.
	 *
	 * @return the tool.
	 */
	protected IVisibilityHelper getVisibilityHelper() {
		return this.visibilityHelper;
	}

	/** Replies the tool for managing overrides.
	 *
	 * @return the tool.
	 */
	protected OverrideHelper getOverrideHelper() {
		return this.overrideHelper;
	}

	/** Replies the accessor to the grammar.
	 *
	 * @return the accessor.
	 */
	protected SARLGrammarKeywordAccess getGrammarAccess() {
		return this.grammarAccess;
	}

	/** Replies the associations between SARL and JVM models.
	 *
	 * @return the associations.
	 */
	protected SarlJvmModelAssociations getAssociations() {
		return this.associations;
	}

	/** Replies the feature that corresponds to the name attribute of the given member.
	 * 
	 * @param member the member.
	 * @return the feature.
	 */
	@SuppressWarnings("static-method")
	protected EStructuralFeature nameFeature(EObject member) {
		if (member instanceof XtendFunction) { 
			return XTEND_FUNCTION__NAME;
		}
		if (member instanceof XtendField) {
			return XTEND_FIELD__NAME;
		}
		return null;
	}

	/** Replies the feature that corresponds to the return type of the given member.
	 * 
	 * @param member the member.
	 * @return the feature.
	 */
	@SuppressWarnings("static-method")
	protected EStructuralFeature returnTypeFeature(EObject member) {
		if (member instanceof XtendFunction) { 
			return XTEND_FUNCTION__RETURN_TYPE;
		}
		return null;
	}

	/** Replies the canonical name for the given type.
	 *
	 * @param typeRef the type.
	 * @return the canonical name.
	 */
	protected static String canonicalName(LightweightTypeReference typeRef) {
		return (typeRef == null) ? Messages.AbstractSARLSubValidator_4 : typeRef.getHumanReadableName();
	}

	/** Replies if the given issue is ignored for the given object.
	 *
	 * @param issueCode the code if the issue.
	 * @param currentObject the current object.
	 * @return {@code true} if the issue is ignored.
	 * @see #isIgnored(String)
	 */
	protected boolean isIgnored(String issueCode, EObject currentObject) {
		final var severities = getIssueSeverities(getContext(), currentObject);
		return severities.isIgnored(issueCode);
	}

}
