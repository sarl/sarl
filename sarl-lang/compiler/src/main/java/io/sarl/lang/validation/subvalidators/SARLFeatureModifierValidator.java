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

import static io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL;
import static org.eclipse.xtend.core.validation.IssueCodes.CONSTRUCTOR_NOT_PERMITTED;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER;
import static org.eclipse.xtend.core.validation.IssueCodes.MODIFIER_DOES_NOT_MATCH_TYPENAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FIELD__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_MEMBER__MODIFIERS;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME;

import java.text.MessageFormat;
import java.util.List;

import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.xbase.validation.JvmGenericTypeValidator;

import com.google.common.collect.Lists;
import com.google.inject.Inject;
import com.google.inject.Injector;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.util.Utils;

/**
 * A specialization of {@link JvmGenericTypeValidator} to deal with specific features of SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLFeatureModifierValidator extends AbstractSARLSubValidator {

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;
	
	@Inject
	private Injector injector;

	private SARLModifierValidator constructorModifierValidatorForSpecialContainer;

	private SARLModifierValidator staticConstructorModifierValidator;

	private SARLModifierValidator agentModifierValidator;

	private SARLModifierValidator methodInAgentModifierValidator;

	private SARLModifierValidator fieldInAgentModifierValidator;

	private SARLModifierValidator behaviorModifierValidator;

	private SARLModifierValidator methodInBehaviorModifierValidator;

	private SARLModifierValidator fieldInBehaviorModifierValidator;

	private SARLModifierValidator capacityModifierValidator;
	
	private SARLModifierValidator methodInCapacityModifierValidator;

	private SARLModifierValidator eventModifierValidator;

	private SARLModifierValidator fieldInEventModifierValidator;

	private SARLModifierValidator skillModifierValidator;

	private SARLModifierValidator methodInSkillModifierValidator;

	private SARLModifierValidator fieldInSkillModifierValidator;

	private SARLModifierValidator classModifierValidator;
	
	private SARLModifierValidator interfaceModifierValidator;
		
	private SARLModifierValidator enumModifierValidator;
	
	private SARLModifierValidator annotationTypeModifierValidator;
	
	private SARLModifierValidator nestedClassModifierValidator;
	
	private SARLModifierValidator nestedInterfaceModifierValidator;
		
	private SARLModifierValidator nestedEnumModifierValidator;
	
	private SARLModifierValidator nestedAnnotationTypeModifierValidator;
		
	private SARLModifierValidator fieldModifierValidator;
		
	private SARLModifierValidator fieldInInterfaceModifierValidator;
		
	private SARLModifierValidator constructorModifierValidator;
		
	private SARLModifierValidator methodModifierValidator;

	private SARLModifierValidator methodInInterfaceModifierValidator;

	private SARLModifierValidator nestedClassInAgentModifierValidator;

	private SARLModifierValidator nestedInterfaceInAgentModifierValidator;

	private SARLModifierValidator nestedEnumerationInAgentModifierValidator;

	private SARLModifierValidator nestedAnnotationTypeInAgentModifierValidator;
	
	private SARLModifierValidator mainFunctionModifierValidator;

	@Check(CheckType.FAST)
	protected void checkModifierMatchesTypename(XtendClass xtendClass) {
		final var name = xtendClass.getName();
		if (name != null) {
			if (name.equals("Abstract") && !xtendClass.isAbstract()) { //$NON-NLS-1$
				addIssue(MessageFormat.format(Messages.SARLModifierValidator_12, name),
						xtendClass, XTEND_TYPE_DECLARATION__NAME, -1,
						MODIFIER_DOES_NOT_MATCH_TYPENAME);
			}
		}
	}

	/** Check the modifiers for the class constructors.
	 *
	 * @param constructor the construct.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(XtendConstructor constructor) {
		final var declaringType = constructor.getDeclaringType();
		if (declaringType != null) {
			final var typeName = declaringType.getName();
			final var msg = MessageFormat.format(Messages.SARLFeatureModifierValidator_1, typeName);
			if (constructor.isStatic()) {
				getStaticConstructorModifierValidator().checkModifiers(constructor, msg);
			} else if (isAOConstructorContainer(declaringType)) {
				getConstructorModifierValidatorForSpecialContainer().checkModifiers(constructor, msg);
			} else if(!(constructor.getDeclaringType() instanceof XtendClass)) {
				error(Messages.SARLFeatureModifierValidator_2, null, CONSTRUCTOR_NOT_PERMITTED);
			} else {
				final var container = (XtendTypeDeclaration) constructor.eContainer();
				getConstructorModifierValidator().checkModifiers(constructor, typeName(container));
			}
		}
	}

	/** Check the modifiers for the actions and methods.
	 *
	 * @param function the construct.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(XtendFunction function) {
		final var declaringType = function.getDeclaringType();
		if (declaringType != null) {
			if (declaringType instanceof SarlAgent) {
				final var typeName = ((XtendTypeDeclaration) function.eContainer()).getName();
				getMethodInAgentModifierValidator().checkModifiers(function,
						MessageFormat.format(Messages.SARLFeatureModifierValidator_3, function.getName(), typeName));
			} else if (declaringType instanceof SarlCapacity) {
				final var typeName = ((XtendTypeDeclaration) function.eContainer()).getName();
				getMethodInCapacityModifierValidator().checkModifiers(function,
						MessageFormat.format(Messages.SARLFeatureModifierValidator_3, function.getName(), typeName));
			} else if (declaringType instanceof SarlSkill) {
				final var typeName = ((XtendTypeDeclaration) function.eContainer()).getName();
				getMethodInSkillModifierValidator().checkModifiers(function,
						MessageFormat.format(Messages.SARLFeatureModifierValidator_3, function.getName(), typeName));
			} else if (declaringType instanceof SarlBehavior) {
				final var typeName = ((XtendTypeDeclaration) function.eContainer()).getName();
				getMethodInBehaviorModifierValidator().checkModifiers(function,
						MessageFormat.format(Messages.SARLFeatureModifierValidator_3, function.getName(), typeName));
			} else if (declaringType instanceof XtendClass && Utils.isNameForJavaMainFunction(function.getName())) {
				// Special case of the main function, whatever the notation of the main function
				getMainFunctionModifierValidator().checkModifiers(function, memberName(function));
			} else {
				final var abstractIndex = function.getModifiers().indexOf(getGrammarAccess().getAbstractKeyword());
				if (declaringType instanceof XtendClass || declaringType instanceof AnonymousClass) {
					getMethodModifierValidator().checkModifiers(function, memberName(function));
					final var nativeIndex = function.getModifiers().indexOf(getGrammarAccess().getNativeKeyword());
					if (function.getExpression() != null) {
						if (abstractIndex != -1) {
							error(MessageFormat.format(Messages.SARLFeatureModifierValidator_5, function.getName()), XTEND_MEMBER__MODIFIERS, abstractIndex, INVALID_MODIFIER);
						} else if (function.isNative()) {
							error(Messages.SARLFeatureModifierValidator_6, XTEND_FUNCTION__NAME, -1, INVALID_MODIFIER);
						}
					} else if (nativeIndex == -1) {
						final var finalIndex = function.getModifiers().indexOf(getGrammarAccess().getFinalKeyword());
						if (finalIndex != -1) { 
							error(MessageFormat.format(Messages.SARLFeatureModifierValidator_7, function.getName()), XTEND_MEMBER__MODIFIERS, finalIndex, INVALID_MODIFIER);
						}
						final var privateIndex = function.getModifiers().indexOf(getGrammarAccess().getPrivateKeyword());
						if (privateIndex != -1) { 
							error(MessageFormat.format(Messages.SARLFeatureModifierValidator_8, function.getName()), XTEND_MEMBER__MODIFIERS, privateIndex, INVALID_MODIFIER);
						}
						final var staticIndex = function.getModifiers().indexOf(getGrammarAccess().getStaticStaticKeyword());
						if (staticIndex != -1) { 
							error(MessageFormat.format(Messages.SARLFeatureModifierValidator_9, function.getName()), XTEND_MEMBER__MODIFIERS, staticIndex, INVALID_MODIFIER);
						}
					}
				} else if (declaringType instanceof XtendInterface) {
					getMethodInInterfaceModifierValidator().checkModifiers(function, memberName(function));
					if (function.getExpression() != null && abstractIndex != -1) {
						error(MessageFormat.format(Messages.SARLFeatureModifierValidator_10, function.getName()), XTEND_MEMBER__MODIFIERS, abstractIndex, INVALID_MODIFIER);
					}
				}
			}
		}
	}

	/** Check the modifiers for the fields and attributes.
	 *
	 * @param field the construct.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(XtendField field) {
		final var declaringType = field.getDeclaringType();
		if (declaringType != null) {
			var generateMemorySharingWarning = false;
			if (declaringType instanceof SarlEvent) {
				final var typeName = ((XtendTypeDeclaration) field.eContainer()).getName();
				getFieldInEventModifierValidator().checkModifiers(field,
						MessageFormat.format(Messages.SARLFeatureModifierValidator_3, field.getName(), typeName));
			} else if (declaringType instanceof SarlAgent) {
				generateMemorySharingWarning = true;
				final var typeName = ((XtendTypeDeclaration) field.eContainer()).getName();
				getFieldInAgentModifierValidator().checkModifiers(field,
						MessageFormat.format(Messages.SARLFeatureModifierValidator_3, field.getName(), typeName));
			} else if (declaringType instanceof SarlSkill) {
				generateMemorySharingWarning = true;
				final var typeName = ((XtendTypeDeclaration) field.eContainer()).getName();
				getFieldInSkillModifierValidator().checkModifiers(field,
						MessageFormat.format(Messages.SARLFeatureModifierValidator_3, field.getName(), typeName));
			} else if (declaringType instanceof SarlBehavior) {
				generateMemorySharingWarning = true;
				final var typeName = ((XtendTypeDeclaration) field.eContainer()).getName();
				getFieldInBehaviorModifierValidator().checkModifiers(field,
						MessageFormat.format(Messages.SARLFeatureModifierValidator_3, field.getName(), typeName));
			} else if (declaringType instanceof XtendClass || declaringType instanceof AnonymousClass) {
				if (field.isFinal() && field.isVolatile()) {
					error(MessageFormat.format(Messages.SARLFeatureModifierValidator_4, field.getName()), XTEND_FIELD__NAME, -1, INVALID_MODIFIER);
				}
				getFieldModifierValidator().checkModifiers(field, memberName(field));
			}
			else if(declaringType instanceof XtendInterface || declaringType instanceof XtendAnnotationType) {
				getFieldInInterfaceModifierValidator().checkModifiers(field, memberName(field));
			}
			if (generateMemorySharingWarning && field.isStatic() && !isIgnored(POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL)) {
				if (!field.isFinal()
						|| field.getInitialValue() == null
						|| getExpressionHelper().hasSideEffects(field.getInitialValue())) {
					addIssue(
							MessageFormat.format(Messages.SARLFeatureModifierValidator_13, field.getName()),
							field,
							POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL);
				}
			}
		}

	}

	/** Check if the modifiers for the SARL events.
	 *
	 * @param event the event.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(SarlEvent event) {
		getEventModifierValidator().checkModifiers(event,
				MessageFormat.format(Messages.SARLFeatureModifierValidator_11, event.getName()));
	}

	/** Check the modifiers for the SARL agents.
	 *
	 * @param agent the agent.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(SarlAgent agent) {
		getAgentModifierValidator().checkModifiers(agent,
				MessageFormat.format(Messages.SARLFeatureModifierValidator_11, agent.getName()));
	}

	/** Check the modifiers for the SARL behaviors.
	 *
	 * @param behavior the behavior.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(SarlBehavior behavior) {
		getBehaviorModifierValidator().checkModifiers(behavior,
				MessageFormat.format(Messages.SARLFeatureModifierValidator_11, behavior.getName()));
	}

	/** Check the modifiers for the SARL capacities.
	 *
	 * @param capacity the capacity.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(SarlCapacity capacity) {
		getCapacityModifierValidator().checkModifiers(capacity,
				MessageFormat.format(Messages.SARLFeatureModifierValidator_11, capacity.getName()));
	}

	/** Check the modifiers for the SARL skills.
	 *
	 * @param skill the skill.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(SarlSkill skill) {
		getSkillModifierValidator().checkModifiers(skill,
				MessageFormat.format(Messages.SARLFeatureModifierValidator_11, skill.getName()));
	}

	/** Check the modifiers of the interface.
	 * 
	 * @param oopInterface the interface to check.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(XtendInterface oopInterface) {
		final var econtainer = oopInterface.eContainer();
		if (econtainer instanceof SarlAgent) {
			getNestedInterfaceInAgentModifierValidator().checkModifiers(oopInterface,
					MessageFormat.format(Messages.SARLFeatureModifierValidator_11, oopInterface.getName()));
		} else {
			final var eContainer = oopInterface.eContainer();
			if (eContainer instanceof XtendFile) {
				getInterfaceModifierValidator().checkModifiers(oopInterface, typeName(oopInterface));
			} else {
				getNestedInterfaceModifierValidator().checkModifiers(oopInterface, typeName(oopInterface));
			}
		}
	}

	/** Check the modifiers of the class.
	 * 
	 * @param oopClass the class to check.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(XtendClass oopClass) {
		final var econtainer = oopClass.eContainer();
		if (econtainer instanceof SarlAgent) {
			getNestedClassInAgentModifierValidator().checkModifiers(oopClass,
					MessageFormat.format(Messages.SARLFeatureModifierValidator_11, oopClass.getName()));
		} else {
			final var eContainer = oopClass.eContainer();
			if (eContainer instanceof XtendFile) {
				getClassModifierValidator().checkModifiers(oopClass, typeName(oopClass));
			} else {
				getNestedClassModifierValidator().checkModifiers(oopClass, typeName(oopClass));
			}
		}
		if (!oopClass.isStatic() && (econtainer instanceof XtendTypeDeclaration)) {
			error(Messages.SARLFeatureModifierValidator_12, XTEND_TYPE_DECLARATION__NAME, -1, MISSING_STATIC_MODIFIER);
		}
	}

	/** Check the modifiers of the enumeration.
	 * 
	 * @param oopEnum the class to check.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(XtendEnum oopEnum) {
		final var econtainer = oopEnum.eContainer();
		if (econtainer instanceof SarlAgent) {
			getNestedEnumerationInAgentModifierValidator().checkModifiers(oopEnum,
					MessageFormat.format(Messages.SARLFeatureModifierValidator_11, oopEnum.getName()));
		} else {
			final var eContainer = oopEnum.eContainer();
			if (eContainer instanceof XtendFile) {
				getEnumModifierValidator().checkModifiers(oopEnum, typeName(oopEnum));
			} else {
				getNestedEnumModifierValidator().checkModifiers(oopEnum, typeName(oopEnum));
			}
		}
	}

	/** Check the modifiers of the annotation type.
	 * 
	 * @param oopAnnotationType the annotation type to check.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(XtendAnnotationType oopAnnotationType) {
		final var econtainer = oopAnnotationType.eContainer();
		if (econtainer instanceof SarlAgent) {
			getNestedAnnotationTypeInAgentModifierValidator().checkModifiers(oopAnnotationType,
					MessageFormat.format(Messages.SARLFeatureModifierValidator_11, oopAnnotationType.getName()));
		} else {
			final var eContainer = oopAnnotationType.eContainer();
			if (eContainer instanceof XtendFile) {
				getAnnotationTypeModifierValidator().checkModifiers(oopAnnotationType, typeName(oopAnnotationType));
			} else {
				getNestedAnnotationTypeModifierValidator().checkModifiers(oopAnnotationType, typeName(oopAnnotationType));
			}
		}
	}

	/** Create a new validator.
	 *
	 * @return the validator.
	 * @since 0.15
	 */
	protected SARLModifierValidator newSARLModifierValidator(List<String> modifiers) {
		final var validator = new SARLModifierValidator(modifiers, getVisibilityModifiers(), this::error, this::addIssue);
		this.injector.injectMembers(validator);
		return validator;
	}

	/** Replies the modifier validator for the constructors in special containers.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getConstructorModifierValidatorForSpecialContainer() {
		if (this.constructorModifierValidatorForSpecialContainer == null) {
			this.constructorModifierValidatorForSpecialContainer = newSARLModifierValidator(getVisibilityModifiers());
		}
		return this.constructorModifierValidatorForSpecialContainer;
	}

	/** Replies the modifier validator for the static constructors.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getStaticConstructorModifierValidator() {
		if (this.staticConstructorModifierValidator == null) {
			this.staticConstructorModifierValidator = newSARLModifierValidator(Lists.newArrayList(this.grammarAccess.getStaticStaticKeyword()));
		}
		return this.staticConstructorModifierValidator;
	}

	/** Replies the modifier validator for the agents.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getAgentModifierValidator() {
		if (this.agentModifierValidator == null) {
			this.agentModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getFinalKeyword()));
		}
		return this.agentModifierValidator;
	}

	/** Replies the modifier validator for methods in the agents.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getMethodInAgentModifierValidator() {
		if (this.methodInAgentModifierValidator == null) {
			this.methodInAgentModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getDispatchKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getDefKeyword(),
					this.grammarAccess.getOverrideKeyword(),
					this.grammarAccess.getSynchronizedKeyword()));
		}
		return this.methodInAgentModifierValidator;
	}

	/** Replies the modifier validator for fields in the agents.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getFieldInAgentModifierValidator() {
		if (this.fieldInAgentModifierValidator == null) {
			this.fieldInAgentModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getExtensionExtensionKeyword(),
					this.grammarAccess.getWriteableVarKeyword(),
					this.grammarAccess.getValKeyword(),
					this.grammarAccess.getStaticStaticKeyword()));
		}
		return this.fieldInAgentModifierValidator;
	}

	/** Replies the modifier validator for the behaviors.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getBehaviorModifierValidator() {
		if (this.behaviorModifierValidator == null) {
			this.behaviorModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getFinalKeyword()));
		}
		return this.behaviorModifierValidator;
	}

	/** Replies the modifier validator for methods in the behaviors.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getMethodInBehaviorModifierValidator() {
		if (this.methodInBehaviorModifierValidator == null) {
			this.methodInBehaviorModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getDispatchKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getDefKeyword(),
					this.grammarAccess.getOverrideKeyword(),
					this.grammarAccess.getSynchronizedKeyword()));
		}
		return this.methodInBehaviorModifierValidator;
	}

	/** Replies the modifier validator for fields in the behaviors.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getFieldInBehaviorModifierValidator() {
		if (this.fieldInBehaviorModifierValidator == null) {
			this.fieldInBehaviorModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getExtensionExtensionKeyword(),
					this.grammarAccess.getWriteableVarKeyword(),
					this.grammarAccess.getValKeyword()));
		}
		return this.fieldInBehaviorModifierValidator;
	}

	/** Replies the modifier validator for the capacities.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getCapacityModifierValidator() {
		if (this.capacityModifierValidator == null) {
			this.capacityModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword()));
		}
		return this.capacityModifierValidator;
	}

	/** Replies the modifier validator for methods in the capacities.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getMethodInCapacityModifierValidator() {
		if (this.methodInCapacityModifierValidator == null) {
			this.methodInCapacityModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getDefKeyword(),
					this.grammarAccess.getOverrideKeyword()));
		}
		return this.methodInCapacityModifierValidator;
	}

	/** Replies the modifier validator for the events.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getEventModifierValidator() {
		if (this.eventModifierValidator == null) {
			this.eventModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getAbstractKeyword()));
		}
		return this.eventModifierValidator;
	}

	/** Replies the modifier validator for fields in the events.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getFieldInEventModifierValidator() {
		if (this.fieldInEventModifierValidator == null) {
			this.fieldInEventModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getWriteableVarKeyword(),
					this.grammarAccess.getValKeyword()));
		}
		return this.fieldInEventModifierValidator;
	}

	/** Replies the modifier validator for the skills.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getSkillModifierValidator() {
		if (this.skillModifierValidator == null) {
			this.skillModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getAbstractKeyword()));
		}
		return this.skillModifierValidator;
	}

	/** Replies the modifier validator for methods in the skills.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getMethodInSkillModifierValidator() {
		if (this.methodInSkillModifierValidator == null) {
			this.methodInSkillModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getDispatchKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getDefKeyword(),
					this.grammarAccess.getOverrideKeyword(),
					this.grammarAccess.getSynchronizedKeyword()));
		}
		return this.methodInSkillModifierValidator;
	}

	/** Replies the modifier validator for fields in the skills.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getFieldInSkillModifierValidator() {
		if (this.fieldInSkillModifierValidator == null) {
			this.fieldInSkillModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getExtensionExtensionKeyword(),
					this.grammarAccess.getWriteableVarKeyword(),
					this.grammarAccess.getValKeyword()));
		}
		return this.fieldInSkillModifierValidator;
	}

	/** Replies the modifier validator for classes.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getClassModifierValidator() {
		if (this.classModifierValidator == null) {
			this.classModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getStrictfpKeyword()));
		}
		return this.classModifierValidator;
	}

	/** Replies the modifier validator for interfaces.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getInterfaceModifierValidator() {
		if (this.interfaceModifierValidator == null) {
			this.interfaceModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getAbstractKeyword()));
		}
		return this.interfaceModifierValidator;
	}
	
	/** Replies the modifier validator for enumerations.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getEnumModifierValidator() {
		if (this.enumModifierValidator == null) {
			this.enumModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword()));
		}
		return this.enumModifierValidator;
	}

	/** Replies the modifier validator for annotations.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getAnnotationTypeModifierValidator() {
		if (this.annotationTypeModifierValidator == null) {
			this.annotationTypeModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getAbstractKeyword()));
		}
		return this.annotationTypeModifierValidator;
	}
	
	/** Replies the modifier validator for nested classes.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getNestedClassModifierValidator() {
		if (this.nestedClassModifierValidator == null) {
			this.nestedClassModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getStrictfpKeyword()));
		}
		return this.nestedClassModifierValidator;
	}

	/** Replies the modifier validator for nested interfaces.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getNestedInterfaceModifierValidator() {
		if (this.nestedInterfaceModifierValidator == null) {
			this.nestedInterfaceModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getStrictfpKeyword()));
		}
		return this.nestedInterfaceModifierValidator;
	}

	/** Replies the modifier validator for nested enumerations.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getNestedEnumModifierValidator() {
		if (this.nestedEnumModifierValidator == null) {
			this.nestedEnumModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword()));
		}
		return this.nestedEnumModifierValidator;
	}

	/** Replies the modifier validator for nested annotations types.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getNestedAnnotationTypeModifierValidator() {
		if (this.nestedAnnotationTypeModifierValidator == null) {
			this.nestedAnnotationTypeModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getAbstractKeyword()));
		}
		return this.nestedAnnotationTypeModifierValidator;
	}
	
	/** Replies the modifier validator for fields in classes.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getFieldModifierValidator() {
		if (this.fieldModifierValidator == null) {
			this.fieldModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getValKeyword(),
					this.grammarAccess.getWriteableVarKeyword(),
					this.grammarAccess.getExtensionExtensionKeyword(),
					this.grammarAccess.getVolatileKeyword(),
					this.grammarAccess.getTransientKeyword()));
		}
		return this.fieldModifierValidator;
	}
		
	/** Replies the modifier validator for fields in interfaces.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getFieldInInterfaceModifierValidator() {
		if (this.fieldInInterfaceModifierValidator == null) {
			this.fieldInInterfaceModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getValKeyword()));
		}
		return this.fieldInInterfaceModifierValidator;
	}
		
	/** Replies the modifier validator for class constructors.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getConstructorModifierValidator() {
		if (this.constructorModifierValidator == null) {
			this.constructorModifierValidator = newSARLModifierValidator(getVisibilityModifiers());
		}
		return this.constructorModifierValidator;
	}

	/** Replies the modifier validator for class methods.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getMethodModifierValidator() {
		if (this.methodModifierValidator == null) {
			this.methodModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getDispatchKeyword(),
					this.grammarAccess.getDefKeyword(),
					this.grammarAccess.getOverrideKeyword(),
					this.grammarAccess.getStrictfpKeyword(),
					this.grammarAccess.getNativeKeyword(),
					this.grammarAccess.getSynchronizedKeyword()));
		}
		return this.methodModifierValidator;
	}

	/** Replies the modifier validator for interface methods.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getMethodInInterfaceModifierValidator() {
		if (this.methodInInterfaceModifierValidator == null) {
			this.methodInInterfaceModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getDefKeyword(),
					this.grammarAccess.getOverrideKeyword()));
		}
		return this.methodInInterfaceModifierValidator;
	}

	/** Replies the modifier validator for classes that are defined in an agent type.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getNestedClassInAgentModifierValidator() {
		if (this.nestedClassInAgentModifierValidator == null) {
			this.nestedClassInAgentModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getFinalKeyword(),
					this.grammarAccess.getStaticStaticKeyword()));
		}
		return this.nestedClassInAgentModifierValidator;
	}

	/** Replies the modifier validator for interfaces that are defined in an agent type.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getNestedInterfaceInAgentModifierValidator() {
		if (this.nestedInterfaceInAgentModifierValidator == null) {
			this.nestedInterfaceInAgentModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getStaticStaticKeyword()));
		}
		return this.nestedInterfaceInAgentModifierValidator;
	}

	/** Replies the modifier validator for enumerations that are defined in an agent type.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getNestedEnumerationInAgentModifierValidator() {
		if (this.annotationTypeModifierValidator == null) {
			this.nestedEnumerationInAgentModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword()));
		}
		return this.nestedEnumerationInAgentModifierValidator;
	}

	/** Replies the modifier validator for annotations that are defined in an agent type.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getNestedAnnotationTypeInAgentModifierValidator() {
		if (this.nestedAnnotationTypeInAgentModifierValidator == null) {
			this.nestedAnnotationTypeInAgentModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getProtectedKeyword(),
					this.grammarAccess.getPrivateKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getAbstractKeyword()));
		}
		return this.nestedAnnotationTypeInAgentModifierValidator;
	}


	/** Replies the modifier validator for main function in a class.
	 *
	 * @return the validator.
	 * @since 0.15
	 */
	protected SARLModifierValidator getMainFunctionModifierValidator() {
		if (this.mainFunctionModifierValidator == null) {
			this.mainFunctionModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getStaticStaticKeyword(),
					this.grammarAccess.getDefKeyword()));
		}
		return this.mainFunctionModifierValidator;
	}

}
