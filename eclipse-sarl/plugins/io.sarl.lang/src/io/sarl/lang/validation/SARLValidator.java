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

package io.sarl.lang.validation;

import static com.google.common.collect.Iterables.any;
import static com.google.common.collect.Iterables.toArray;
import static com.google.common.collect.Lists.newArrayList;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_AGENT__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_BEHAVIOR__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_CAPACITY_USES__CAPACITIES;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_CAPACITY__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_EVENT__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_FORMAL_PARAMETER__DEFAULT_VALUE;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_SKILL__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_SKILL__IMPLEMENTS;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_CAPACITY_DEFINITION;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_FUNCTION_NAME;
import static io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE;
import static io.sarl.lang.validation.IssueCodes.INVALID_EXTENDED_TYPE;
import static io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE;
import static io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE;
import static io.sarl.lang.validation.IssueCodes.INVALID_NESTED_DEFINITION;
import static io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE;
import static io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION;
import static io.sarl.lang.validation.IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED;
import static io.sarl.lang.validation.IssueCodes.UNREACHABLE_BEHAVIOR_UNIT;
import static io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY;
import static org.eclipse.xtend.core.validation.IssueCodes.ABSTRACT_METHOD_WITH_BODY;
import static org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED;
import static org.eclipse.xtend.core.validation.IssueCodes.CREATE_FUNCTIONS_MUST_NOT_BE_ABSTRACT;
import static org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE;
import static org.eclipse.xtend.core.validation.IssueCodes.DISPATCH_FUNCTIONS_MUST_NOT_BE_ABSTRACT;
import static org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME;
import static org.eclipse.xtend.core.validation.IssueCodes.JDK_NOT_ON_CLASSPATH;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT_IN_ANONYMOUS;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_CONSTRUCTOR;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER;
import static org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR;
import static org.eclipse.xtend.core.validation.IssueCodes.OBSOLETE_OVERRIDE;
import static org.eclipse.xtend.core.validation.IssueCodes.OVERRIDDEN_FINAL;
import static org.eclipse.xtend.core.validation.IssueCodes.OVERRIDE_REDUCES_VISIBILITY;
import static org.eclipse.xtend.core.validation.IssueCodes.XBASE_LIB_NOT_ON_CLASSPATH;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_CLASS__IMPLEMENTS;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FIELD__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_INTERFACE__EXTENDS;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME;
import static org.eclipse.xtext.util.JavaVersion.JAVA8;
import static org.eclipse.xtext.xbase.validation.IssueCodes.DISCOURAGED_REFERENCE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES;
import static org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.TYPE_BOUNDS_MISMATCH;
import static org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED;
import static org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING;

import java.lang.annotation.ElementType;
import java.lang.reflect.Field;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.typesystem.LocalClassAwareTypeNames;
import org.eclipse.xtend.core.validation.ModifierValidator;
import org.eclipse.xtend.core.validation.XtendValidator;
import org.eclipse.xtend.core.xtend.XtendAnnotationTarget;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.XtextVersion;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.eclipse.xtext.xbase.typesystem.override.IOverrideCheckResult.OverrideCheckDetails;
import org.eclipse.xtext.xbase.typesystem.override.IResolvedOperation;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.validation.FeatureNameValidator;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.SARLExpressionHelper;
import io.sarl.lang.util.Utils;

/**
 * Validator for the SARL elements.
 *
 * <p>The check type may be one of:<ul>
 * <li>{@link CheckType#FAST}: is executed after a delay of 500ms after ANY editing action (type, enter, delete);</li>
 * <li>{@link CheckType#NORMAL}: is executed after a build (manual, or automatic);</li>
 * <li>{@link CheckType#EXPENSIVE}: is executed by right clicking ANYWHERE in the editor window and chooseing "Validate".</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation"
 */
@SuppressWarnings({"checkstyle:classfanoutcomplexity", "checkstyle:methodcount"})
public class SARLValidator extends AbstractSARLValidator {

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator constructorModifierValidator = new SARLModifierValidator(
			newArrayList(SARLValidator.this.visibilityModifers));

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator agentModifierValidator = new SARLModifierValidator(
			newArrayList("public", "package", "abstract", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
			"final")); //$NON-NLS-1$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator methodInAgentModifierValidator = new SARLModifierValidator(
			newArrayList(
			"package",  //$NON-NLS-1$
			"protected", "private", "static", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
			"abstract", "dispatch", "final", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
			"def", "override", "synchronized")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator fieldInAgentModifierValidator = new SARLModifierValidator(
			newArrayList(
			"package",  //$NON-NLS-1$
			"protected", "private", //$NON-NLS-1$//$NON-NLS-2$
			"final", "val", "var")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator behaviorModifierValidator = new SARLModifierValidator(
			newArrayList("public", "package", "abstract", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
			"final")); //$NON-NLS-1$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator methodInBehaviorModifierValidator = new SARLModifierValidator(
			newArrayList(
			"public", "package", //$NON-NLS-1$ //$NON-NLS-2$
			"protected", "private", //$NON-NLS-1$//$NON-NLS-2$
			"abstract", "dispatch", "final", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
			"def", "override", "synchronized")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator fieldInBehaviorModifierValidator = new SARLModifierValidator(
			newArrayList(
			"package",  //$NON-NLS-1$
			"protected", "private", //$NON-NLS-1$//$NON-NLS-2$
			"final", "val", "var")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator capacityModifierValidator = new SARLModifierValidator(
			newArrayList("public", "package")); //$NON-NLS-1$//$NON-NLS-2$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator methodInCapacityModifierValidator = new SARLModifierValidator(
			newArrayList(
			"public", "def", "override")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator eventModifierValidator = new SARLModifierValidator(
			newArrayList("public", "package", "final")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator fieldInEventModifierValidator = new SARLModifierValidator(
			newArrayList(
			"public", //$NON-NLS-1$
			"final", "val", "var")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator skillModifierValidator = new SARLModifierValidator(
			newArrayList("public", "package", "abstract", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
			"final")); //$NON-NLS-1$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator methodInSkillModifierValidator = new SARLModifierValidator(
			newArrayList(
			"public", "package", //$NON-NLS-1$ //$NON-NLS-2$
			"protected", "private", //$NON-NLS-1$//$NON-NLS-2$
			"abstract", "dispatch", "final", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
			"def", "override", "synchronized")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator fieldInSkillModifierValidator = new SARLModifierValidator(
			newArrayList(
			"public", "package", //$NON-NLS-1$ //$NON-NLS-2$
			"protected", "private", //$NON-NLS-1$//$NON-NLS-2$
			"final", "val", "var")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator nestedClassInAgentModifierValidator = new SARLModifierValidator(
			newArrayList(
			"package", "protected", //$NON-NLS-1$ //$NON-NLS-2$
			"private", "static", "final", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"abstract")); //$NON-NLS-1$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator nestedInterfaceInAgentModifierValidator = new SARLModifierValidator(
			newArrayList(
			"package", "protected", "private", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"static", "abstract")); //$NON-NLS-1$ //$NON-NLS-2$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator nestedEnumerationInAgentModifierValidator = new SARLModifierValidator(
			newArrayList(
			"package", "protected", "private", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"static")); //$NON-NLS-1$

	@SuppressWarnings("synthetic-access")
	private final SARLModifierValidator nestedAnnotationTypeInAgentModifierValidator = new SARLModifierValidator(
			newArrayList(
			"package", "protected", "private", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"static", "abstract")); //$NON-NLS-1$ //$NON-NLS-2$

	@Inject
	private SarlJvmModelAssociations associations;

	@Inject
	private FeatureNameValidator featureNames;

	@Inject
	private IActionPrototypeProvider sarlActionSignatures;

	@Inject
	private IFeatureCallValidator featureCallValidator;

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private LocalClassAwareTypeNames localClassAwareTypeNames;

	@Inject
	private SARLExpressionHelper expressionHelper;

	@Inject
	private IProgrammaticWarningSuppressor warningSuppressor;

	@Inject
	private IQualifiedNameConverter qualifiedNameConverter;

	// Update thet annotation target information
	{
		final ImmutableMultimap.Builder<Class<?>, ElementType> result = ImmutableMultimap.builder();
		result.putAll(this.targetInfos);
		result.put(SarlAgent.class, ElementType.TYPE);
		result.put(SarlCapacity.class, ElementType.TYPE);
		result.put(SarlSkill.class, ElementType.TYPE);
		result.put(SarlEvent.class, ElementType.TYPE);
		result.put(SarlBehavior.class, ElementType.TYPE);
		result.put(SarlSpace.class, ElementType.TYPE);
		result.put(SarlClass.class, ElementType.TYPE);
		result.put(SarlInterface.class, ElementType.TYPE);
		result.put(SarlEnumeration.class, ElementType.TYPE);
		result.putAll(SarlAnnotationType.class, ElementType.ANNOTATION_TYPE, ElementType.TYPE);
		result.put(SarlField.class, ElementType.FIELD);
		result.put(SarlAction.class, ElementType.METHOD);
		result.put(SarlFormalParameter.class, ElementType.PARAMETER);
		// Override the target informations
		try {
			final Field field = XtendValidator.class.getDeclaredField("targetInfos"); //$NON-NLS-1$
			field.setAccessible(true);
			field.set(this, result.build());
		} catch (Exception exception) {
			throw new Error(exception);
		}
	}

	/** Copied from the Xtend validtor.
	 *
	 * <p>TODO: Change the visilibility in the Xtend validator.
	 *
	 * @param annotationTarget the target to test.
	 * @return <code>true</code> if the annotation target is relevant for validation.
	 */
	protected boolean isRelevantAnnotationTarget(final XtendAnnotationTarget annotationTarget) {
		return any(this.targetInfos.keySet(), new Predicate<Class<?>>() {
			@Override
			public boolean apply(Class<?> input) {
				return input.isInstance(annotationTarget);
			}
		});
	}

	@Override
	protected IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject eObject) {
		final IssueSeverities severities = super.getIssueSeverities(context, eObject);
		return this.warningSuppressor.getIssueSeverities(context, eObject, severities);
	}

	/** Replies if the given issue is ignored for the given object.
	 *
	 * @param issueCode the code if the issue.
	 * @param currentObject the current object.
	 * @return <code>true</code> if the issue is ignored.
	 * @see #isIgnored(String)
	 */
	protected boolean isIgnored(String issueCode, EObject currentObject) {
		final IssueSeverities severities = getIssueSeverities(getContext(), currentObject);
		return severities.isIgnored(issueCode);
	}

	/** Replies the canonical name of the given object.
	 *
	 * @param object - the object.
	 * @return the canonical name or <code>null</code> if it cannot be computed.
	 */
	protected String canonicalName(EObject object) {
		if (object instanceof JvmIdentifiableElement) {
			return ((JvmIdentifiableElement) object).getQualifiedName();
		}
		final EObject jvmElement = this.associations.getPrimaryJvmElement(object);
		if (jvmElement instanceof JvmIdentifiableElement) {
			return ((JvmIdentifiableElement) jvmElement).getQualifiedName();
		}
		return null;
	}

	/** Space keyword is reserved.
	 *
	 * @param space - the space to check.
	 */
	@Check
	public void checkSpaceUse(SarlSpace space) {
		error(MessageFormat.format(
					Messages.SARLValidator_0,
					this.grammarAccess.getSpaceKeyword()),
					space,
					null);
	}

	/** Emit a warning when the "fires" keyword is used.
	 *
	 * @param action - the action to check.
	 */
	@Check
	public void checkFiresKeywordUse(SarlAction action) {
		if (!action.getFiredEvents().isEmpty()) {
			warning(MessageFormat.format(
					Messages.SARLValidator_1,
					this.grammarAccess.getFiresKeyword()),
					action,
					null);
		}
	}

	/** Emit a warning when the "requires" keyword is used.
	 *
	 * @param statement - the statement to check.
	 */
	@Check
	public void checkRequiredCapacityUse(SarlRequiredCapacity statement) {
		warning(MessageFormat.format(
				Messages.SARLValidator_0,
				this.grammarAccess.getRequiresKeyword()),
				statement,
				null);
	}

	/** Check if the correct SARL libraries are on the classpath.
	 *
	 * <p>This function is overriding the function given by the Xtend validator
	 * for having finer tests, and firing a warning in place of an error.
	 *
	 * @param sarlScript - the SARL script.
	 */
	@Check(CheckType.NORMAL)
	@Override
	public void checkClassPath(XtendFile sarlScript) {
		final TypeReferences typeReferences = getServices().getTypeReferences();

		if (!Utils.isCompatibleJREVersion()) {
			error(
					MessageFormat.format(
							Messages.SARLValidator_3,
							System.getProperty("java.specification.version"), //$NON-NLS-1$
							SARLVersion.MINIMAL_JDK_VERSION),
					sarlScript,
					XtendPackage.Literals.XTEND_FILE__PACKAGE,
					JDK_NOT_ON_CLASSPATH);
		} else {
			final GeneratorConfig generatorConfiguration = getGeneratorConfig(sarlScript);
			final JavaVersion javaVersion = JavaVersion.fromQualifier(SARLVersion.MINIMAL_JDK_VERSION);
			final JavaVersion generatorVersion = generatorConfiguration.getJavaSourceVersion();
			if (generatorVersion == null
				|| javaVersion == null
				|| !generatorVersion.isAtLeast(javaVersion)) {
				error(
						MessageFormat.format(
								Messages.SARLValidator_4,
								generatorVersion,
								SARLVersion.MINIMAL_JDK_VERSION),
						sarlScript,
						XtendPackage.Literals.XTEND_FILE__PACKAGE,
						JDK_NOT_ON_CLASSPATH);
			}
		}

		if (!Utils.isCompatibleXtextVersion()) {
			error(
					MessageFormat.format(
							Messages.SARLValidator_5,
							XtextVersion.getCurrent(),
							SARLVersion.MINIMAL_XTEXT_VERSION),
					sarlScript,
					XtendPackage.Literals.XTEND_FILE__PACKAGE,
					XBASE_LIB_NOT_ON_CLASSPATH);
		} else {
			final JvmType type = typeReferences.findDeclaredType(ToStringBuilder.class.getName(), sarlScript);
			if (type == null) {
				error(
						MessageFormat.format(
								Messages.SARLValidator_6,
								SARLVersion.MINIMAL_XTEXT_VERSION),
						sarlScript,
						XtendPackage.Literals.XTEND_FILE__PACKAGE,
						XBASE_LIB_NOT_ON_CLASSPATH);
			}
		}


		final String sarlOnClasspath = Utils.getSARLLibraryVersionOnClasspath(typeReferences, sarlScript);
		if (Strings.isNullOrEmpty(sarlOnClasspath)) {
			error(
					Messages.SARLValidator_7,
					sarlScript,
					XtendPackage.Literals.XTEND_FILE__PACKAGE,
					io.sarl.lang.validation.IssueCodes.SARL_LIB_NOT_ON_CLASSPATH);
		} else if (!Utils.isCompatibleSARLLibraryVersion(sarlOnClasspath)) {
			error(
					MessageFormat.format(Messages.SARLValidator_8,
					sarlOnClasspath, SARLVersion.SPECIFICATION_RELEASE_VERSION),
					sarlScript,
					XtendPackage.Literals.XTEND_FILE__PACKAGE,
					io.sarl.lang.validation.IssueCodes.INVALID_SARL_LIB_ON_CLASSPATH);
		}
	}

	@Check
	@Override
	protected void checkModifiers(XtendConstructor constructor) {
		final XtendTypeDeclaration declaringType = constructor.getDeclaringType();
		if (declaringType != null) {
			if (declaringType instanceof SarlEvent
					|| declaringType instanceof SarlAgent
					|| declaringType instanceof SarlSkill
					|| declaringType instanceof SarlBehavior) {
				final String typeName = ((XtendTypeDeclaration) constructor.eContainer()).getName();
				this.constructorModifierValidator.checkModifiers(constructor,
						MessageFormat.format(Messages.SARLValidator_9, typeName));
			} else {
				super.checkModifiers(constructor);
			}
		}
	}

	@Check
	@Override
	protected void checkModifiers(XtendFunction function) {
		final XtendTypeDeclaration declaringType = function.getDeclaringType();
		if (declaringType != null) {
			if (declaringType instanceof SarlAgent) {
				final String typeName = ((XtendTypeDeclaration) function.eContainer()).getName();
				this.methodInAgentModifierValidator.checkModifiers(function,
						MessageFormat.format(Messages.SARLValidator_10, function.getName(), typeName));
			} else if (declaringType instanceof SarlCapacity) {
				final String typeName = ((XtendTypeDeclaration) function.eContainer()).getName();
				this.methodInCapacityModifierValidator.checkModifiers(function,
						MessageFormat.format(Messages.SARLValidator_10, function.getName(), typeName));
			} else if (declaringType instanceof SarlSkill) {
				final String typeName = ((XtendTypeDeclaration) function.eContainer()).getName();
				this.methodInSkillModifierValidator.checkModifiers(function,
						MessageFormat.format(Messages.SARLValidator_10, function.getName(), typeName));
			} else if (declaringType instanceof SarlBehavior) {
				final String typeName = ((XtendTypeDeclaration) function.eContainer()).getName();
				this.methodInBehaviorModifierValidator.checkModifiers(function,
						MessageFormat.format(Messages.SARLValidator_10, function.getName(), typeName));
			} else {
				super.checkModifiers(function);
			}
		}
	}

	@Check
	@Override
	protected void checkModifiers(XtendField field) {
		final XtendTypeDeclaration declaringType = field.getDeclaringType();
		if (declaringType != null) {
			if (declaringType instanceof SarlEvent) {
				final String typeName = ((XtendTypeDeclaration) field.eContainer()).getName();
				this.fieldInEventModifierValidator.checkModifiers(field,
						MessageFormat.format(Messages.SARLValidator_10, field.getName(), typeName));
			} else if (declaringType instanceof SarlAgent) {
				final String typeName = ((XtendTypeDeclaration) field.eContainer()).getName();
				this.fieldInAgentModifierValidator.checkModifiers(field,
						MessageFormat.format(Messages.SARLValidator_10, field.getName(), typeName));
			} else if (declaringType instanceof SarlSkill) {
				final String typeName = ((XtendTypeDeclaration) field.eContainer()).getName();
				this.fieldInSkillModifierValidator.checkModifiers(field,
						MessageFormat.format(Messages.SARLValidator_10, field.getName(), typeName));
			} else if (declaringType instanceof SarlBehavior) {
				final String typeName = ((XtendTypeDeclaration) field.eContainer()).getName();
				this.fieldInBehaviorModifierValidator.checkModifiers(field,
						MessageFormat.format(Messages.SARLValidator_10, field.getName(), typeName));
			} else {
				super.checkModifiers(field);
			}
		}
	}

	/** Check if the modifiers for the SARL events.
	 *
	 * @param event the event.
	 */
	@Check
	protected void checkModifiers(SarlEvent event) {
		this.eventModifierValidator.checkModifiers(event,
				MessageFormat.format(Messages.SARLValidator_9, event.getName()));
	}

	/** Check the modifiers for the SARL agents.
	 *
	 * @param agent the agent.
	 */
	@Check
	protected void checkModifiers(SarlAgent agent) {
		this.agentModifierValidator.checkModifiers(agent,
				MessageFormat.format(Messages.SARLValidator_9, agent.getName()));
	}

	/** Check the modifiers for the SARL behaviors.
	 *
	 * @param behavior the behavior.
	 */
	@Check
	protected void checkModifiers(SarlBehavior behavior) {
		this.behaviorModifierValidator.checkModifiers(behavior,
				MessageFormat.format(Messages.SARLValidator_9, behavior.getName()));
	}

	/** Check the modifiers for the SARL capacities.
	 *
	 * @param capacity the capacity.
	 */
	@Check
	protected void checkModifiers(SarlCapacity capacity) {
		this.capacityModifierValidator.checkModifiers(capacity,
				MessageFormat.format(Messages.SARLValidator_9, capacity.getName()));
	}

	/** Check the modifiers for the SARL skills.
	 *
	 * @param skill the skill.
	 */
	@Check
	protected void checkModifiers(SarlSkill skill) {
		this.skillModifierValidator.checkModifiers(skill,
				MessageFormat.format(Messages.SARLValidator_9, skill.getName()));
	}

	@Check
	@Override
	protected void checkModifiers(XtendInterface oopInterface) {
		final EObject econtainer = oopInterface.eContainer();
		if (econtainer instanceof SarlAgent) {
			this.nestedInterfaceInAgentModifierValidator.checkModifiers(oopInterface,
					MessageFormat.format(Messages.SARLValidator_9, oopInterface.getName()));
		} else {
			super.checkModifiers(oopInterface);
		}
	}

	@Check
	@Override
	protected void checkModifiers(XtendClass oopClass) {
		final EObject econtainer = oopClass.eContainer();
		if (econtainer instanceof SarlAgent) {
			this.nestedClassInAgentModifierValidator.checkModifiers(oopClass,
					MessageFormat.format(Messages.SARLValidator_9, oopClass.getName()));
		} else {
			super.checkModifiers(oopClass);
		}
		// TODO remove this constraint when it is removed from the Xtend validator.
		if (!oopClass.isStatic()
				&& ((econtainer instanceof SarlAgent)
				|| (econtainer instanceof SarlBehavior)
				|| (econtainer instanceof SarlSkill))) {
			error(Messages.SARLValidator_25, XTEND_TYPE_DECLARATION__NAME, -1, MISSING_STATIC_MODIFIER);
		}
	}

	@Check
	@Override
	protected void checkModifiers(XtendEnum oopEnum) {
		final EObject econtainer = oopEnum.eContainer();
		if (econtainer instanceof SarlAgent) {
			this.nestedEnumerationInAgentModifierValidator.checkModifiers(oopEnum,
					MessageFormat.format(Messages.SARLValidator_9, oopEnum.getName()));
		} else {
			super.checkModifiers(oopEnum);
		}
	}

	@Check
	@Override
	protected void checkModifiers(XtendAnnotationType oopAnnotationType) {
		final EObject econtainer = oopAnnotationType.eContainer();
		if (econtainer instanceof SarlAgent) {
			this.nestedAnnotationTypeInAgentModifierValidator.checkModifiers(oopAnnotationType,
					MessageFormat.format(Messages.SARLValidator_9, oopAnnotationType.getName()));
		} else {
			super.checkModifiers(oopAnnotationType);
		}
	}

	/** Check the container for the SARL agents.
	 *
	 * @param agent the agent.
	 */
	@Check
	public void checkContainerType(SarlAgent agent) {
		final XtendTypeDeclaration declaringType = agent.getDeclaringType();
		if (declaringType != null) {
			final String name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLValidator_28, name),
					agent,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check the container for the SARL behaviors.
	 *
	 * @param behavior the behavior.
	 */
	@Check
	public void checkContainerType(SarlBehavior behavior) {
		final XtendTypeDeclaration declaringType = behavior.getDeclaringType();
		if (declaringType != null) {
			final String name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLValidator_29, name),
					behavior,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check the container for the SARL capacities.
	 *
	 * @param capacity the capacity.
	 */
	@Check
	public void checkContainerType(SarlCapacity capacity) {
		final XtendTypeDeclaration declaringType = capacity.getDeclaringType();
		if (declaringType != null) {
			final String name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLValidator_30, name),
					capacity,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check the container for the SARL skills.
	 *
	 * @param skill the skill.
	 */
	@Check
	public void checkContainerType(SarlSkill skill) {
		final XtendTypeDeclaration declaringType = skill.getDeclaringType();
		if (declaringType != null) {
			final String name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLValidator_31, name),
					skill,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check if the modifiers for the SARL events.
	 *
	 * @param event the event.
	 */
	@Check
	public void checkContainerType(SarlEvent event) {
		final XtendTypeDeclaration declaringType = event.getDeclaringType();
		if (declaringType != null) {
			final String name = canonicalName(declaringType);
			assert name != null;
			error(MessageFormat.format(Messages.SARLValidator_32, name),
					event,
					null,
					INVALID_NESTED_DEFINITION);
		}
	}

	/** Check if all the fields are initialized in a SARL event.
	 *
	 * @param event the event.
	 */
	@Check
	public void checkFinalFieldInitialization(SarlEvent event) {
		final JvmGenericType inferredType = this.associations.getInferredType(event);
		if (inferredType != null) {
			super.checkFinalFieldInitialization(inferredType);
		}
	}

	/** Check if all the fields are initialized in a SARL behavior.
	 *
	 * @param behavior the behavior.
	 */
	@Check
	public void checkFinalFieldInitialization(SarlBehavior behavior) {
		final JvmGenericType inferredType = this.associations.getInferredType(behavior);
		if (inferredType != null) {
			super.checkFinalFieldInitialization(inferredType);
		}
	}

	/** Check if all the fields are initialized in a SARL skill.
	 *
	 * @param skill the skill.
	 */
	@Check
	public void checkFinalFieldInitialization(SarlSkill skill) {
		final JvmGenericType inferredType = this.associations.getInferredType(skill);
		if (inferredType != null) {
			super.checkFinalFieldInitialization(inferredType);
		}
	}

	/** Check if all the fields are initialized in a SARL agent.
	 *
	 * @param agent the agent.
	 */
	@Check
	public void checkFinalFieldInitialization(SarlAgent agent) {
		final JvmGenericType inferredType = this.associations.getInferredType(agent);
		if (inferredType != null) {
			super.checkFinalFieldInitialization(inferredType);
		}
	}

	/** Check the super constructors.
	 *
	 * @param container - the container.
	 * @param feature - the syntactic feature related to the supertypes.
	 * @param defaultSignatures - the signatures of the default constructors for the given container.
	 */
	@SuppressWarnings({"unchecked", "checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity",
			"checkstyle:nestedifdepth"})
	protected void checkSuperConstructor(
			XtendTypeDeclaration container,
			EStructuralFeature feature,
			Collection<ActionParameterTypes> defaultSignatures) {
		final JvmDeclaredType jvmElement = this.associations.getInferredType(container);
		if (jvmElement != null) {
			final Map<ActionParameterTypes, JvmConstructor> superConstructors =
					CollectionLiterals.newTreeMap((Comparator<ActionParameterTypes>) null);
			final JvmTypeReference typeRef = jvmElement.getExtendedClass();
			final JvmType supertype = (typeRef == null) ? null : typeRef.getType();
			if (supertype instanceof JvmGenericType) {
				final JvmGenericType jvmSuperElement = (JvmGenericType) supertype;
				for (final JvmConstructor superConstructor : jvmSuperElement.getDeclaredConstructors()) {
					final ActionParameterTypes sig = this.sarlActionSignatures.createParameterTypesFromJvmModel(
							superConstructor.isVarArgs(), superConstructor.getParameters());
					superConstructors.put(sig, superConstructor);
				}
			}

			final ActionParameterTypes voidKey = this.sarlActionSignatures.createParameterTypesForVoid();
			boolean hasDeclaredConstructor = false;

			for (final XtendMember member : container.getMembers()) {
				if (member instanceof SarlConstructor) {
					final SarlConstructor constructor = (SarlConstructor) member;
					hasDeclaredConstructor = true;
					boolean invokeDefaultConstructor = true;
					final XExpression body = constructor.getExpression();
					if (body instanceof XBlockExpression) {
						final XBlockExpression block = (XBlockExpression) body;
						if (!block.getExpressions().isEmpty()) {
							final XExpression firstStatement = block.getExpressions().get(0);
							if (firstStatement instanceof XConstructorCall || isDelegateConstructorCall(firstStatement)) {
								invokeDefaultConstructor = false;
							}
						}
					} else if (body instanceof XConstructorCall || isDelegateConstructorCall(body)) {
						invokeDefaultConstructor = false;
					}
					if (invokeDefaultConstructor && !superConstructors.containsKey(voidKey)) {
						final List<String> issueData = newArrayList();
						for (final ActionParameterTypes defaultSignature : defaultSignatures) {
							issueData.add(defaultSignature.toString());
						}
						error(Messages.SARLValidator_33,
								member,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								MUST_INVOKE_SUPER_CONSTRUCTOR,
								toArray(issueData, String.class));
					}
				}
			}

			if (!hasDeclaredConstructor) {
				for (final ActionParameterTypes defaultSignature : defaultSignatures) {
					if (!superConstructors.containsKey(defaultSignature)) {
						final List<String> issueData = newArrayList();
						for (final JvmConstructor superConstructor : superConstructors.values()) {
							issueData.add(EcoreUtil.getURI(superConstructor).toString());
							issueData.add(doGetReadableSignature(container.getName(), superConstructor.getParameters()));
						}
						error(Messages.SARLValidator_33,
								container, feature, MISSING_CONSTRUCTOR, toArray(issueData, String.class));
					}
				}
			}
		}
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param agent the SARL element.
	 */
	@Check
	public void checkSuperConstructor(SarlAgent agent) {
		checkSuperConstructor(
				agent,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Agent.class, agent));
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param behavior the SARL element.
	 */
	@Check
	public void checkSuperConstructor(SarlBehavior behavior) {
		checkSuperConstructor(
				behavior,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Behavior.class, behavior));
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param skill the SARL element.
	 */
	@Check
	public void checkSuperConstructor(SarlSkill skill) {
		checkSuperConstructor(
				skill,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Skill.class, skill));
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param event the SARL element.
	 */
	@Check
	public void checkSuperConstructor(SarlEvent event) {
		checkSuperConstructor(
				event,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Event.class, event));
	}

	private Collection<ActionParameterTypes> doGetConstructorParameterTypes(Class<?> type, Notifier context) {
		final Collection<ActionParameterTypes> parameters = new ArrayList<>();
		final JvmTypeReference typeReference = this.typeReferences.getTypeForName(type, context);
		final JvmType jvmType = typeReference.getType();
		if (jvmType instanceof JvmDeclaredType) {
			final JvmDeclaredType declaredType = (JvmDeclaredType) jvmType;
			for (final JvmConstructor constructor : declaredType.getDeclaredConstructors()) {
				final ActionParameterTypes types = this.sarlActionSignatures.createParameterTypesFromJvmModel(
						constructor.isVarArgs(), constructor.getParameters());
				if (types != null) {
					parameters.add(types);
				}
			}
		}
		if (parameters.isEmpty()) {
			parameters.add(this.sarlActionSignatures.createParameterTypesForVoid());
		}
		return parameters;
	}

	/** Check if the super default constructor is correctly invoked.
	 *
	 * @param xtendClass the Xtend element.
	 */
	@Check
	@Override
	public void checkDefaultSuperConstructor(XtendClass xtendClass) {
		checkSuperConstructor(
				xtendClass,
				XTEND_TYPE_DECLARATION__NAME,
				doGetConstructorParameterTypes(Object.class, xtendClass));
	}

	/** Check if the call is forbidden.
	 *
	 * <p>One example of a forbidden feature is {@link System#exit(int)}.
	 *
	 * @param expression - the expression.
	 */
	@Check(CheckType.FAST)
	public void checkForbiddenCalls(XAbstractFeatureCall expression) {
		// specific message for System.exit
		if (expression.getFeature() != null) {
			final JvmIdentifiableElement feature = expression.getFeature();
			final String id = feature.getQualifiedName();
			if ("java.lang.System.exit".equals(id)) { //$NON-NLS-1$
				error(
						Messages.SARLValidator_35,
						expression,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						FORBIDDEN_REFERENCE);
				return;
			}
		}
		if (this.featureCallValidator.isDisallowedCall(expression)) {
			error(
					MessageFormat.format(
						Messages.SARLValidator_36,
						expression.getFeature().getIdentifier()),
					expression,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					FORBIDDEN_REFERENCE);
		}
	}

	/** Check if the call is discouraged.
	 *
	 * <p>One example of a discouraged feature is {@link System#err}.
	 *
	 * @param expression - the expression.
	 */
	@Check(CheckType.FAST)
	public void checkDiscouragedCalls(XAbstractFeatureCall expression) {
		if (!isIgnored(DISCOURAGED_REFERENCE)
				&& this.featureCallValidator.isDiscouragedCall(expression)) {
			addIssue(
					MessageFormat.format(Messages.SARLValidator_37,
					// FIXME: this.serializer.serialize(expression)
					expression.getConcreteSyntaxFeatureName()),
					expression,
					DISCOURAGED_REFERENCE);
		}
	}

	/** Check if the default values of the formal parameters have a compatible type with the formal parameter.
	 *
	 * @param param - the formal parameter to check.
	 */
	@Check
	public void checkDefaultValueTypeCompatibleWithParameterType(SarlFormalParameter param) {
		if (param.getDefaultValue() != null) {
			final JvmTypeReference rawType = param.getParameterType();
			assert rawType != null;
			final LightweightTypeReference toType = toLightweightTypeReference(rawType, true);
			final LightweightTypeReference fromType = getActualType(param.getDefaultValue());
			if (!Utils.canCast(fromType, toType, true, false, true)) {
				error(MessageFormat.format(
						Messages.SARLValidator_38,
						getNameOfTypes(fromType), canonicalName(toType)),
						param,
						SARL_FORMAL_PARAMETER__DEFAULT_VALUE,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INCOMPATIBLE_TYPES,
						canonicalName(fromType),
						canonicalName(toType));
			}
		}
	}

	/** Check if the given action has a valid name.
	 *
	 * @param action - the action to test.
	 * @see SARLFeatureNameValidator
	 */
	@Check(CheckType.FAST)
	public void checkActionName(SarlAction action) {
		final JvmOperation inferredType = this.associations.getDirectlyInferredOperation(action);
		final QualifiedName name = QualifiedName.create(inferredType.getQualifiedName('.').split("\\.")); //$NON-NLS-1$
		if (this.featureNames.isDisallowedName(name)) {
			final String validName = Utils.fixHiddenMember(action.getName());
			error(MessageFormat.format(
					Messages.SARLValidator_39,
					action.getName()),
					action,
					XTEND_FUNCTION__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					INVALID_MEMBER_NAME,
					validName);
		} else if (!isIgnored(DISCOURAGED_FUNCTION_NAME)
				&& this.featureNames.isDiscouragedName(name)) {
			warning(MessageFormat.format(
					Messages.SARLValidator_39,
					action.getName()),
					action,
					XTEND_FUNCTION__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					DISCOURAGED_FUNCTION_NAME);
		}
	}

	/** Check if the given field has a valid name.
	 *
	 * @param field - the field to test.
	 * @see SARLFeatureNameValidator
	 */
	@Check(CheckType.FAST)
	public void checkFieldName(SarlField field) {
		final JvmField inferredType = this.associations.getJvmField(field);
		final QualifiedName name = Utils.getQualifiedName(inferredType);
		if (this.featureNames.isDisallowedName(name)) {
			final String validName = Utils.fixHiddenMember(field.getName());
			error(MessageFormat.format(
					Messages.SARLValidator_41,
					field.getName()),
					field,
					XTEND_FIELD__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					VARIABLE_NAME_DISALLOWED,
					validName);
		}
	}

	/** Check if the given field has a name that is shadowing an inherited field.
	 *
	 * @param field - the field to test.
	 */
	@Check
	public void checkFieldNameShadowing(SarlField field) {
		if (!isIgnored(VARIABLE_NAME_SHADOWING)
				&& !Utils.isHiddenMember(field.getName())) {
			final JvmField inferredField = this.associations.getJvmField(field);
			final Map<String, JvmField> inheritedFields = new TreeMap<>();
			Utils.populateInheritanceContext(
					inferredField.getDeclaringType(),
					null, null,
					inheritedFields,
					null, null,
					this.sarlActionSignatures);

			final JvmField inheritedField = inheritedFields.get(field.getName());
			if (inheritedField != null) {
				int nameIndex = 0;
				String newName = field.getName() + nameIndex;
				while (inheritedFields.containsKey(newName)) {
					++nameIndex;
					newName = field.getName() + nameIndex;
				}
				addIssue(MessageFormat.format(
						Messages.SARLValidator_42,
						field.getName(),
						inferredField.getDeclaringType().getQualifiedName(),
						inheritedField.getQualifiedName()),
						field,
						XTEND_FIELD__NAME,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						VARIABLE_NAME_SHADOWING,
						newName);
			}
		}
	}

	/** Caution: This function is overridden for translating the MISSING_OVERRIDE error into a warning,
	 * and emit a warning when a return type should be specified.
	 *
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	protected void doCheckFunctionOverrides(EObject sourceElement, IResolvedOperation resolved,
			List<IResolvedOperation> allInherited) {
		boolean overrideProblems = false;
		List<IResolvedOperation> exceptionMismatch = null;
		for (final IResolvedOperation inherited: allInherited) {
			if (inherited.getOverrideCheckResult().hasProblems()) {
				overrideProblems = true;
				final EnumSet<OverrideCheckDetails> details = inherited.getOverrideCheckResult().getDetails();
				if (details.contains(OverrideCheckDetails.IS_FINAL)) {
					error(MessageFormat.format(Messages.SARLValidator_43, inherited.getSimpleSignature()),
							sourceElement,
							nameFeature(sourceElement), OVERRIDDEN_FINAL);
				} else if (details.contains(OverrideCheckDetails.REDUCED_VISIBILITY)) {
					error(MessageFormat.format(Messages.SARLValidator_44,
							inherited.getSimpleSignature()),
							sourceElement, nameFeature(sourceElement), OVERRIDE_REDUCES_VISIBILITY);
				} else if (details.contains(OverrideCheckDetails.EXCEPTION_MISMATCH)) {
					if (exceptionMismatch == null) {
						exceptionMismatch = Lists.newArrayListWithCapacity(allInherited.size());
					}
					exceptionMismatch.add(inherited);
				} else if (details.contains(OverrideCheckDetails.RETURN_MISMATCH)) {
					error(MessageFormat.format(Messages.SARLValidator_45,
							inherited.getSimpleSignature()),
							sourceElement,
							returnTypeFeature(sourceElement), INCOMPATIBLE_RETURN_TYPE,
							inherited.getResolvedReturnType().getIdentifier());
				}
			} else if (!isIgnored(RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED, sourceElement)
					&& sourceElement instanceof SarlAction) {
				final SarlAction function = (SarlAction) sourceElement;
				if (function.getReturnType() == null && !inherited.getResolvedReturnType().isPrimitiveVoid()) {
					warning(MessageFormat.format(Messages.SARLValidator_46,
							resolved.getResolvedReturnType().getHumanReadableName()),
							sourceElement,
							returnTypeFeature(sourceElement), RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED,
							inherited.getResolvedReturnType().getIdentifier());
				}
			}
		}
		if (exceptionMismatch != null) {
			createExceptionMismatchError(resolved, sourceElement, exceptionMismatch);
		}
		if (sourceElement instanceof SarlAction) {
			final SarlAction function = (SarlAction) sourceElement;
			if (!overrideProblems && !function.isOverride() && !function.isStatic()
					&& !isIgnored(MISSING_OVERRIDE, sourceElement)) {
				warning(MessageFormat.format(Messages.SARLValidator_47,
						resolved.getSimpleSignature(),
						getDeclaratorName(resolved)),
						function,
						XTEND_FUNCTION__NAME, MISSING_OVERRIDE);
			}
			if (!overrideProblems && function.isOverride() && function.isStatic()) {
				for (final IResolvedOperation inherited: allInherited) {
					error(MessageFormat.format(Messages.SARLValidator_48,
							resolved.getSimpleSignature(),
							getDeclaratorName(resolved),
							resolved.getSimpleSignature(),
							getDeclaratorName(inherited)),
							function, XTEND_FUNCTION__NAME,
							function.getModifiers().indexOf(Messages.SARLValidator_49),
							OBSOLETE_OVERRIDE);
				}
			}
		}
	}

	private boolean checkRedundantInterface(
			XtendTypeDeclaration element,
			EReference structuralElement,
			LightweightTypeReference lightweightInterfaceReference,
			List<LightweightTypeReference> knownInterfaces) {
		int index = 0;
		for (final LightweightTypeReference previousInterface : knownInterfaces) {
			if (memberOfTypeHierarchy(previousInterface, lightweightInterfaceReference)) {
				error(MessageFormat.format(
						Messages.SARLValidator_50,
						canonicalName(lightweightInterfaceReference)),
						element,
						structuralElement,
						// The index of the element to highlight in the super-types
						knownInterfaces.size(),
						REDUNDANT_INTERFACE_IMPLEMENTATION,
						canonicalName(lightweightInterfaceReference),
						"pre"); //$NON-NLS-1$
				return true;
			} else if (memberOfTypeHierarchy(lightweightInterfaceReference, previousInterface)) {
				error(MessageFormat.format(
						Messages.SARLValidator_50,
						canonicalName(previousInterface)),
						element,
						structuralElement,
						index,
						REDUNDANT_INTERFACE_IMPLEMENTATION,
						canonicalName(previousInterface),
						"post"); //$NON-NLS-1$
			}
			++index;
		}
		return false;
	}

	private void checkRedundantInterfaces(
			XtendTypeDeclaration element,
			EReference structuralElement,
			Iterable<? extends JvmTypeReference> interfaces,
			Iterable<? extends JvmTypeReference> superTypes) {
		final List<LightweightTypeReference> knownInterfaces = CollectionLiterals.newArrayList();
		for (final JvmTypeReference interfaceRef : interfaces) {
			final LightweightTypeReference lightweightInterfaceReference = toLightweightTypeReference(interfaceRef);
			// Check the interface against the other interfaces
			if (!checkRedundantInterface(
					element, structuralElement,
					lightweightInterfaceReference,
					knownInterfaces)) {
				// Check the interface against the super-types
				if (superTypes != null && !isIgnored(REDUNDANT_INTERFACE_IMPLEMENTATION, element)) {
					for (final JvmTypeReference superType : superTypes) {
						final LightweightTypeReference lightweightSuperType = toLightweightTypeReference(superType);
						if (memberOfTypeHierarchy(lightweightSuperType, lightweightInterfaceReference)) {
							addIssue(MessageFormat.format(
									Messages.SARLValidator_52,
									canonicalName(lightweightInterfaceReference),
									canonicalName(lightweightSuperType)),
									element,
									structuralElement,
									// The index of the element to highlight in the super-types
									knownInterfaces.size(),
									REDUNDANT_INTERFACE_IMPLEMENTATION,
									canonicalName(lightweightInterfaceReference),
									"unknow"); //$NON-NLS-1$
						}
					}
				}
			}
			// Prepare next loop
			knownInterfaces.add(lightweightInterfaceReference);
		}
	}

	/** Check if implemented interfaces of a skill are redundant.
	 *
	 * @param skill the skill.
	 */
	@Check
	public void checkRedundantImplementedInterfaces(SarlSkill skill) {
		checkRedundantInterfaces(
				skill,
				SARL_SKILL__IMPLEMENTS,
				skill.getImplements(),
				Utils.singletonList(skill.getExtends()));
	}

	/** Check if implemented interfaces of a Xtend Class are redundant.
	 *
	 * @param xtendClass the class.
	 */
	@Check
	public void checkRedundantImplementedInterfaces(SarlClass xtendClass) {
		checkRedundantInterfaces(
				xtendClass,
				XTEND_CLASS__IMPLEMENTS,
				xtendClass.getImplements(),
				Utils.singletonList(xtendClass.getExtends()));
	}

	/** Check if implemented interfaces of a Xtend Interface are redundant.
	 *
	 * @param xtendInterface the interface.
	 */
	@Check
	public void checkRedundantImplementedInterfaces(SarlInterface xtendInterface) {
		checkRedundantInterfaces(
				xtendInterface,
				XTEND_INTERFACE__EXTENDS,
				xtendInterface.getExtends(),
				Collections.<JvmTypeReference>emptyList());
	}

	/** Check the type of the behavior unit's guard.
	 *
	 * @param behaviorUnit - the behavior unit.
	 */
	@Check(CheckType.FAST)
	public void checkBehaviorUnitGuardType(SarlBehaviorUnit behaviorUnit) {
		final XExpression guard = behaviorUnit.getGuard();
		if (guard != null) {
			if (this.expressionHelper.hasDeepSideEffects(guard)) {
				error(Messages.SARLValidator_53,
						guard,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_INNER_EXPRESSION);
				return;
			}
			if (guard instanceof XBooleanLiteral) {
				final XBooleanLiteral booleanLiteral = (XBooleanLiteral) guard;
				if (booleanLiteral.isIsTrue()) {
					if (!isIgnored(DISCOURAGED_BOOLEAN_EXPRESSION)) {
						addIssue(Messages.SARLValidator_54,
								booleanLiteral,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								DISCOURAGED_BOOLEAN_EXPRESSION);
					}
				} else if (!isIgnored(UNREACHABLE_BEHAVIOR_UNIT)) {
					addIssue(Messages.SARLValidator_55,
							behaviorUnit,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							UNREACHABLE_BEHAVIOR_UNIT,
							behaviorUnit.getName().getSimpleName());
				}
				return;
			}

			final LightweightTypeReference fromType = getActualType(guard);
			if (!fromType.isAssignableFrom(Boolean.TYPE)) {
				error(MessageFormat.format(
						Messages.SARLValidator_38,
						getNameOfTypes(fromType), boolean.class.getName()),
						guard,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INCOMPATIBLE_TYPES);
			}
		}
	}

	/** Check the type of the capacity uses.
	 *
	 * @param uses - the capacity uses.
	 */
	@Check(CheckType.FAST)
	public void checkCapacityTypeForUses(SarlCapacityUses uses) {
		for (final JvmParameterizedTypeReference usedType : uses.getCapacities()) {
			final LightweightTypeReference ref = toLightweightTypeReference(usedType);
			if (ref != null && !ref.isSubtypeOf(Capacity.class)) {
				error(MessageFormat.format(
						Messages.SARLValidator_57,
						usedType.getQualifiedName(),
						Messages.SARLValidator_58,
						this.grammarAccess.getUsesKeyword()),
						usedType,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INVALID_CAPACITY_TYPE,
						usedType.getSimpleName());
			}
		}
	}

	/** Check the types of the "requires" statement.
	 *
	 * @param requires - the "requires" statement.
	 */
	@Check(CheckType.FAST)
	public void checkCapacityTypeForRequires(SarlRequiredCapacity requires) {
		for (final JvmParameterizedTypeReference requiredType : requires.getCapacities()) {
			final LightweightTypeReference ref = toLightweightTypeReference(requiredType);
			if (ref != null && !ref.isSubtypeOf(Capacity.class)) {
				error(MessageFormat.format(
						Messages.SARLValidator_57,
						requiredType.getQualifiedName(),
						Messages.SARLValidator_58,
						this.grammarAccess.getRequiresKeyword()),
						requiredType,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INVALID_CAPACITY_TYPE,
						requiredType.getSimpleName());
			}
		}
	}

	/** Check the types of the parameters of the "fires" statement.
	 *
	 * @param action - the signature that contains the "fires" statement.
	 */
	@Check(CheckType.FAST)
	public void checkActionFires(SarlAction action) {
		for (final JvmTypeReference event : action.getFiredEvents()) {
			final LightweightTypeReference ref = toLightweightTypeReference(event);
			if (ref != null && !ref.isSubtypeOf(Event.class)) {
				error(MessageFormat.format(
						Messages.SARLValidator_57,
						event.getQualifiedName(),
						Messages.SARLValidator_62,
						this.grammarAccess.getFiresKeyword()),
						event,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INVALID_FIRING_EVENT_TYPE,
						event.getSimpleName());
			}
		}
	}

	/** Check the super type.
	 *
	 * @param element - the child type.
	 * @param feature - the syntactic feature related to the supertypes.
	 * @param superTypes - the current super types.
	 * @param expectedType - the expected root type.
	 * @param onlySubTypes - if <code>true</code> only the subtype of the <code>expectedType</code> are valid;
	 * <code>false</code> if the <code>expectedType</code> is allowed.
	 * @return the count of supertypes.
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity"})
	protected int checkSuperTypes(
			XtendTypeDeclaration element,
			EReference feature,
			List<? extends JvmTypeReference> superTypes,
			Class<?> expectedType,
			boolean onlySubTypes) {
		int nbSuperTypes = 0;
		final JvmDeclaredType inferredType = this.associations.getInferredType(element);
		if (inferredType instanceof JvmGenericType) {
			final LinkedList<JvmTypeReference> inferredSuperTypes = CollectionLiterals.newLinkedList();
			inferredSuperTypes.addAll(inferredType.getSuperTypes());
			final boolean isExpectingInterface = expectedType.isInterface();
			int superTypeIndex = 0;
			for (final JvmTypeReference superType : superTypes) {
				boolean success = true;
				final JvmType jvmSuperType = (superType == null) ? null : superType.getType();
				if (jvmSuperType != null) {
					final JvmTypeReference inferredSuperType =
							(inferredSuperTypes.isEmpty()) ? null : inferredSuperTypes.removeFirst();
					final LightweightTypeReference lighweightSuperType = toLightweightTypeReference(superType);
					if (!(jvmSuperType instanceof JvmGenericType)
							|| (isExpectingInterface != ((JvmGenericType) jvmSuperType).isInterface())) {
						if (isExpectingInterface) {
							error(
									MessageFormat.format(Messages.SARLValidator_63, Messages.SARLValidator_64),
									feature,
									superTypeIndex,
									INTERFACE_EXPECTED,
									jvmSuperType.getIdentifier());
						} else {
							error(
									MessageFormat.format(Messages.SARLValidator_63, Messages.SARLValidator_66),
									feature,
									superTypeIndex,
									CLASS_EXPECTED,
									jvmSuperType.getIdentifier());
						}
						success = false;
					} else if (isFinal(lighweightSuperType)) {
						error(Messages.SARLValidator_67,
								feature,
								superTypeIndex,
								OVERRIDDEN_FINAL,
								inferredType.getIdentifier(),
								jvmSuperType.getIdentifier());
						success = false;
					} else if (!lighweightSuperType.isSubtypeOf(expectedType)
							|| (onlySubTypes && lighweightSuperType.isType(expectedType))) {
						if (onlySubTypes) {
							error(MessageFormat.format(Messages.SARLValidator_68, expectedType.getName()),
									feature,
									superTypeIndex,
									INVALID_EXTENDED_TYPE,
									jvmSuperType.getIdentifier());
						} else {
							error(MessageFormat.format(Messages.SARLValidator_69, expectedType.getName()),
									feature,
									superTypeIndex,
									INVALID_EXTENDED_TYPE,
									jvmSuperType.getIdentifier());
						}
						success = false;
					} else if (inferredSuperType == null
							|| !Objects.equal(inferredSuperType.getIdentifier(), jvmSuperType.getIdentifier())
							|| Objects.equal(inferredType.getIdentifier(), jvmSuperType.getIdentifier())
							|| hasCycleInHierarchy((JvmGenericType) inferredType, Sets.<JvmGenericType>newHashSet())) {
						error(MessageFormat.format(Messages.SARLValidator_70,
								inferredType.getQualifiedName()),
								feature,
								superTypeIndex,
								CYCLIC_INHERITANCE,
								jvmSuperType.getIdentifier());
						success = false;
					}
				} else if (superType != null) {
					error(MessageFormat.format(Messages.SARLValidator_70,
							inferredType.getQualifiedName()),
							feature,
							superTypeIndex,
							CYCLIC_INHERITANCE,
							superType.getIdentifier());
					success = false;
				}
				checkWildcardSupertype(element, superType, feature, superTypeIndex);
				++superTypeIndex;
				if (success) {
					++nbSuperTypes;
				}
			}
		}
		return nbSuperTypes;
	}

	/** Check if the supertype of the given capacity is a subtype of Capacity.
	 *
	 * @param capacity - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkSuperTypes(SarlCapacity capacity) {
		checkSuperTypes(
				capacity,
				SARL_CAPACITY__EXTENDS,
				capacity.getExtends(),
				Capacity.class,
				false);
	}

	/** Check if the supertype of the given skill is a subtype of Skill.
	 *
	 * @param skill - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkSuperType(SarlSkill skill) {
		final int nbSuperTypes = checkSuperTypes(
				skill,
				SARL_SKILL__EXTENDS,
				Utils.singletonList(skill.getExtends()),
				Skill.class,
				false);
		checkImplementedTypes(
				skill,
				SARL_SKILL__IMPLEMENTS,
				skill.getImplements(),
				Capacity.class,
				nbSuperTypes > 0 ? 0 : 1,
				true);
	}

	/** Check if the supertype of the given event is a subtype of Event.
	 *
	 * @param event - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkSuperType(SarlEvent event) {
		checkSuperTypes(
				event,
				SARL_EVENT__EXTENDS,
				Utils.singletonList(event.getExtends()),
				Event.class,
				false);
	}

	/** Check if the supertype of the given behavior is a subtype of Behavior.
	 *
	 * @param behavior - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkSuperType(SarlBehavior behavior) {
		checkSuperTypes(
				behavior,
				SARL_BEHAVIOR__EXTENDS,
				Utils.singletonList(behavior.getExtends()),
				Behavior.class,
				false);
	}

	/** Check if the supertype of the given agent is a subtype of Agent.
	 *
	 * @param agent - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkSuperType(SarlAgent agent) {
		checkSuperTypes(
				agent,
				SARL_AGENT__EXTENDS,
				Utils.singletonList(agent.getExtends()),
				Agent.class,
				false);
	}

	/** Check the implemeted type.
	 *
	 * @param element - the child type.
	 * @param feature - the syntactic feature related to the supertypes.
	 * @param implementedTypes - the current super types.
	 * @param expectedType - the expected root type.
	 * @param mandatoryNumberOfTypes - the minimal number of implemented types.
	 * @param onlySubTypes - if <code>true</code> only the subtype of the <code>expectedType</code> are valid;
	 * <code>false</code> if the <code>expectedType</code> is allowed.
	 * @return the count of supertypes.
	 */
	protected boolean checkImplementedTypes(
			XtendTypeDeclaration element,
			EReference feature,
			List<? extends JvmTypeReference> implementedTypes,
			Class<?> expectedType,
			int mandatoryNumberOfTypes,
			boolean onlySubTypes) {
		boolean success = true;
		int nb = 0;
		int index = 0;
		for (final JvmTypeReference superType : implementedTypes) {
			final LightweightTypeReference  ref = toLightweightTypeReference(superType);
			if (ref != null
					&& (!ref.isInterfaceType() || !ref.isSubtypeOf(expectedType)
					|| (onlySubTypes && ref.isType(expectedType)))) {
				final String msg;
				if (onlySubTypes) {
					msg = Messages.SARLValidator_72;
				} else {
					msg = Messages.SARLValidator_73;
				}
				error(MessageFormat.format(
						msg,
						superType.getQualifiedName(),
						expectedType.getName(),
						element.getName()),
						element,
						feature,
						index,
						INVALID_IMPLEMENTED_TYPE,
						superType.getSimpleName());
				success = false;
			} else {
				++nb;
			}
			++index;
		}
		if (nb < mandatoryNumberOfTypes) {
			error(MessageFormat.format(
					Messages.SARLValidator_74,
					expectedType.getName(),
					element.getName()),
					element,
					feature,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					MISSING_TYPE);
			success = false;
		}
		return success;
	}

	/** Check if the parameter of the bahavior unit is an event.
	 *
	 * @param behaviorUnit - the behavior unit to test.
	 */
	@Check(CheckType.FAST)
	public void checkBehaviorUnitEventType(SarlBehaviorUnit behaviorUnit) {
		final JvmTypeReference event = behaviorUnit.getName();
		final LightweightTypeReference ref = toLightweightTypeReference(event);
		if (ref == null || ref.isInterfaceType() || !ref.isSubtypeOf(Event.class)) {
			error(MessageFormat.format(
					Messages.SARLValidator_75,
					event.getQualifiedName(),
					Messages.SARLValidator_62,
					this.grammarAccess.getOnKeyword()),
					event,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					TYPE_BOUNDS_MISMATCH);
		}
	}

	/** Check if a capacity has a feature defined inside.
	 *
	 * @param capacity - the capacity to test.
	 */
	@Check(CheckType.FAST)
	public void checkCapacityFeatures(SarlCapacity capacity) {
		if (capacity.getMembers().isEmpty()) {
			if (!isIgnored(DISCOURAGED_CAPACITY_DEFINITION)) {
				addIssue(Messages.SARLValidator_77,
						capacity,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						DISCOURAGED_CAPACITY_DEFINITION,
						capacity.getName(),
						"aFunction"); //$NON-NLS-1$
			}
		}
	}

	/** Check for unused capacities.
	 *
	 * @param uses - the capacity use declaration.
	 */
	@SuppressWarnings("unchecked")
	@Check(CheckType.NORMAL)
	public void checkUnusedCapacities(SarlCapacityUses uses) {
		if (!isIgnored(UNUSED_AGENT_CAPACITY)) {
			final XtendTypeDeclaration container = uses.getDeclaringType();
			final JvmDeclaredType jvmContainer = (JvmDeclaredType) this.associations.getPrimaryJvmElement(container);
			final Map<String, JvmOperation> importedFeatures = CollectionLiterals.newHashMap();
			for (final JvmOperation operation : jvmContainer.getDeclaredOperations()) {
				if (Utils.isNameForHiddenCapacityImplementationCallingMethod(operation.getSimpleName())) {
					importedFeatures.put(operation.getSimpleName(), operation);
				}
			}

			int index = 0;
			for (final JvmTypeReference capacity : uses.getCapacities()) {
				final String fieldName = Utils.createNameForHiddenCapacityImplementationAttribute(capacity.getIdentifier());
				final String operationName = Utils.createNameForHiddenCapacityImplementationCallingMethodFromFieldName(fieldName);
				final JvmOperation operation = importedFeatures.get(operationName);
				if (operation != null && !isLocallyUsed(operation, container)) {
					addIssue(MessageFormat.format(
							Messages.SARLValidator_78,
							capacity.getSimpleName()),
							uses,
							SARL_CAPACITY_USES__CAPACITIES,
							index, UNUSED_AGENT_CAPACITY,
							capacity.getSimpleName());
				}
				++index;
			}
		}
	}

	private static Set<String> doGetPreviousCapacities(SarlCapacityUses uses, Iterator<XtendMember> iterator) {
		boolean continueToFill = true;
		final Set<String> capacityUses = CollectionLiterals.newTreeSet((Comparator<String>) null);
		while (continueToFill && iterator.hasNext()) {
			final XtendMember elt = iterator.next();
			if (elt instanceof SarlCapacityUses) {
				final SarlCapacityUses usesElt = (SarlCapacityUses) elt;
				if (usesElt == uses) {
					continueToFill = false;
				} else {
					for (final JvmTypeReference use : usesElt.getCapacities()) {
						capacityUses.add(use.getIdentifier());
					}
				}
			}
		}
		return capacityUses;
	}

	/** Check for multiple capacity use declaration.
	 *
	 * @param uses - the capacity use declaration.
	 */
	@Check(CheckType.NORMAL)
	public void checkMultipleCapacityUses(SarlCapacityUses uses) {
		if (!isIgnored(REDUNDANT_CAPACITY_USE)) {
			final XtendTypeDeclaration declaringType = uses.getDeclaringType();
			if (declaringType != null) {
				final Set<String> previousCapacityUses = doGetPreviousCapacities(uses,
						declaringType.getMembers().iterator());
				int index = 0;
				for (final JvmTypeReference capacity : uses.getCapacities()) {
					if (previousCapacityUses.contains(capacity.getIdentifier())) {
						addIssue(MessageFormat.format(
								Messages.SARLValidator_79,
								capacity.getSimpleName()),
								uses,
								SARL_CAPACITY_USES__CAPACITIES,
								index,
								REDUNDANT_CAPACITY_USE,
								capacity.getSimpleName());
					} else {
						previousCapacityUses.add(capacity.getIdentifier());
					}
					++index;
				}
			}
		}
	}

	/** Check for abstract methods.
	 *
	 * <p>Override the Xtend behavior for: <ul>
	 * <li>not generating an error when a return type is missed. Indeed, the return type if "void" by default.</li>
	 * <li>generating a warning when "abstract" is missed.</li>
	 * </ul>
	 *
	 * <p>XXX: Update this function with the code of the derived function.
	 */
	@Check
	@Override
	@SuppressWarnings("checkstyle:cyclomaticcomplexity")
	public void checkAbstract(XtendFunction function) {
		final XtendTypeDeclaration declarator = function.getDeclaringType();
		if (function.getExpression() == null || function.isAbstract()) {
			if (declarator instanceof XtendClass || declarator.isAnonymous()
					|| declarator instanceof SarlAgent || declarator instanceof SarlBehavior
					|| declarator instanceof SarlSkill) {
				if (function.isDispatch()) {
					error(MessageFormat.format(
							Messages.SARLValidator_80,
							function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
							XTEND_FUNCTION__NAME, -1, DISPATCH_FUNCTIONS_MUST_NOT_BE_ABSTRACT);
					return;
				}
				if (function.getCreateExtensionInfo() != null) {
					error(MessageFormat.format(
							Messages.SARLValidator_81,
							function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
							XTEND_FUNCTION__NAME, -1, CREATE_FUNCTIONS_MUST_NOT_BE_ABSTRACT);
					return;
				}
				if (declarator.isAnonymous()) {
					error(MessageFormat.format(
							Messages.SARLValidator_82,
							function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
							XTEND_FUNCTION__NAME, -1, MISSING_ABSTRACT_IN_ANONYMOUS);
				} else {
					final boolean isAbstract;
					if (declarator instanceof XtendClass) {
						isAbstract = ((XtendClass) declarator).isAbstract();
					} else if (declarator instanceof SarlAgent) {
						isAbstract = ((SarlAgent) declarator).isAbstract();
					} else if (declarator instanceof SarlBehavior) {
						isAbstract = ((SarlBehavior) declarator).isAbstract();
					} else if (declarator instanceof SarlSkill) {
						isAbstract = ((SarlSkill) declarator).isAbstract();
					} else {
						return;
					}
					if (!isAbstract && !function.isNative()) {
						error(MessageFormat.format(
								Messages.SARLValidator_82,
								function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
								XTEND_FUNCTION__NAME, -1, MISSING_ABSTRACT);
						return;
					}
				}

				if (!function.getModifiers().contains("abstract")) { //$NON-NLS-1$
					warning(MessageFormat.format(
							Messages.SARLValidator_84,
							function.getName(), this.localClassAwareTypeNames.getReadableName(declarator)),
							XTEND_FUNCTION__NAME, -1, MISSING_ABSTRACT,
							function.getName(),
							this.localClassAwareTypeNames.getReadableName(declarator));
				}

			} else if (declarator instanceof XtendInterface || declarator instanceof SarlCapacity) {
				if (function.getCreateExtensionInfo() != null) {
					error(MessageFormat.format(
							Messages.SARLValidator_85,
							function.getName()),
							XTEND_FUNCTION__NAME, -1, CREATE_FUNCTIONS_MUST_NOT_BE_ABSTRACT);
					return;
				}
			}
		} else if (declarator instanceof XtendInterface || declarator instanceof SarlCapacity) {
			if (!getGeneratorConfig(function).getJavaSourceVersion().isAtLeast(JAVA8)) {
				error(Messages.SARLValidator_86, XTEND_FUNCTION__NAME, -1, ABSTRACT_METHOD_WITH_BODY);
				return;
			}
		}
	}

	/** Check for reserved annotations.
	 *
	 * @param annotationTarget thee target to test.
	 */
	@Check
	public void checkReservedAnnotation(XtendAnnotationTarget annotationTarget) {
		if (!isIgnored(IssueCodes.USED_RESERVED_SARL_ANNOTATION)) {
			if (annotationTarget.getAnnotations().isEmpty() || !isRelevantAnnotationTarget(annotationTarget)) {
				return;
			}
			final QualifiedName reservedPackage = this.qualifiedNameConverter.toQualifiedName(
					EarlyExit.class.getPackage().getName());
			final String earlyExitAnnotation = EarlyExit.class.getName();
			for (final XAnnotation annotation : annotationTarget.getAnnotations()) {
				final JvmType type = annotation.getAnnotationType();
				if (type != null && !type.eIsProxy()) {
					if (Objects.equal(type.getIdentifier(), earlyExitAnnotation)) {
						// Special case: EarlyExit is allowed on events for declaring early-exit events
						if (!(annotationTarget instanceof SarlEvent)) {
							addIssue(
									MessageFormat.format(Messages.SARLValidator_87, type.getSimpleName()),
									annotation,
									IssueCodes.USED_RESERVED_SARL_ANNOTATION);
						}
					} else {
						final QualifiedName annotationName = this.qualifiedNameConverter.toQualifiedName(
								type.getIdentifier());
						if (annotationName.startsWith(reservedPackage)) {
							addIssue(
									MessageFormat.format(Messages.SARLValidator_87, type.getSimpleName()),
									annotation,
									IssueCodes.USED_RESERVED_SARL_ANNOTATION);
						}
					}
				}
			}
		}
	}

	/** The modifier validator for constructors.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected final class SARLModifierValidator extends ModifierValidator {

		/**
		 * @param modifiers - the list of the supported modifiers.
		 */
		private SARLModifierValidator(
				List<String> modifiers) {
			super(modifiers, SARLValidator.this);
		}

		/** Make this function visible for the enclosing class.
		 *
		 * @param member - the member to check.
		 * @param memberName - the name of the member, usually for the issue message.
		 */
		@Override
		protected void checkModifiers(XtendMember member, String memberName) {
			super.checkModifiers(member, memberName);
		}

	}

}
