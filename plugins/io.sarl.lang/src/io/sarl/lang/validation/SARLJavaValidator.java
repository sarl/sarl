/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.SARLLangActivator;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.util.ModelUtil;

import java.text.MessageFormat;

import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.xbase.lib.ReassignFirstArgument;

/**
 * Validator for the SARL elements.
 * <p>
 * The check type may be one of:<ul>
 * <li>{@link CheckType#FAST}: is executed after a delay of 500ms after ANY editing action (type, enter, delete);</li>
 * <li>{@link CheckType#NORMAL}: is executed after a build (manual, or automatic);</li>
 * <li>{@link CheckType#EXPENSIVE}: is executed by right clicking ANYWHERE in the editor window and chooseing "Validate".</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#validation"
 */
public class SARLJavaValidator extends AbstractSARLJavaValidator {

//	@Inject
//	private ILogicalContainerProvider logicalContainerProvider;
//
//	@Inject
//	private ActionPrototypeProvider sarlSignatureProvider;
//
//	@Inject
//	private JvmModelAssociator jvmModelAssociator;
//
//	@Inject
//	private SARLGrammarAccess grammarAccess;
//
//	@Inject
//	private SARLCodeGenerator codeGenerator;
//
//	@Inject
//	private ISerializer serializer;
//
//	/** Replies the canonical name of the given type.
//	 *
//	 * @param typeRef - the type.
//	 * @return the name of the given type.
//	 */
//	protected static String canonicalTypeName(LightweightTypeReference typeRef) {
//		if (typeRef == null) {
//			return "void"; //$NON-NLS-1$
//		}
//		return typeRef.getHumanReadableName();
//	}

	/** Check if the SARL libraries are in the classpath.
	 *
	 * This function is overriding the function given by the Xtend validator
	 * for firing a warning in place of an error.
	 *
	 * @param sarlScript - the SARL script.
	 */
	@Check(CheckType.NORMAL)
	@Override
	public void checkClassPath(XtendFile sarlScript) {
		TypeReferences typeReferences = getServices().getTypeReferences();

		String version = System.getProperty("java.specification.version"); //$NON-NLS-1$

		SARLLangActivator sarlLangBundle = SARLLangActivator.getActivator();
		String minJdkVersion = sarlLangBundle.getMinimalJdkVersion();
		String minXtextVersion = sarlLangBundle.getMinimalXtextVersion();

		if (version == null || version.isEmpty() || ModelUtil.compareVersions(version, minJdkVersion) < 0) {
			error(
					MessageFormat.format(Messages.SARLValidator_0, minJdkVersion),
					sarlScript,
					null,
					org.eclipse.xtend.core.validation.IssueCodes.JDK_NOT_ON_CLASSPATH);
		}

		if (typeReferences.findDeclaredType(ReassignFirstArgument.class, sarlScript) == null) {
			error(
					MessageFormat.format(Messages.SARLValidator_1, minXtextVersion),
					sarlScript,
					null,
					org.eclipse.xtend.core.validation.IssueCodes.XBASE_LIB_NOT_ON_CLASSPATH);
		}
	}

//	/** Check for duplicate top elements.
//	 *
//	 * @param script - the SARL script.
//	 */
//	@Check(CheckType.NORMAL)
//	public void checkDuplicateTopElements(SarlScript script) {
//		QualifiedName packageName;
//		if (script.getName() != null && !script.getName().isEmpty()) {
//			packageName = QualifiedName.create(script.getName().split("\\.")); //$NON-NLS-1$
//		} else {
//			packageName = QualifiedName.create();
//		}
//		Set<QualifiedName> names = CollectionLiterals.newTreeSet((Comparator<QualifiedName>) null);
//		for (TopElement feature : script.getElements()) {
//			if (feature instanceof NamedElement) {
//				NamedElement namedFeature = (NamedElement) feature;
//				QualifiedName featureName = packageName.append(namedFeature.getName());
//				// Check in the local file
//				if (names.contains(featureName)) {
//					error(
//							MessageFormat.format(
//									Messages.SARLValidator_2,
//									featureName.toString()),
//									feature,
//									SarlPackage.Literals.NAMED_ELEMENT__NAME,
//									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//									IssueCodes.DUPLICATE_TYPE_NAME,
//									featureName.toString());
//				} else {
//					// Check in the rest of the class path
//					names.add(featureName);
//				}
//			}
//		}
//	}
//
//	private void checkForbiddenFeatureCall(XAbstractFeatureCall expression) {
//		String id = expression.getFeature().getQualifiedName();
//		if ("java.lang.System.exit".equals(id)) { //$NON-NLS-1$
//			error(
//					Messages.SARLValidator_39,
//					expression,
//					null,
//					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
//		}
//	}
//
//	/** Check if the call is forbidden.
//	 * <p>
//	 * One example of a forbidden feature is {@link System#exit(int)}.
//	 *
//	 * @param expression - the expression.
//	 */
//	@Check(CheckType.FAST)
//	public void checkForbiddenCalls(XMemberFeatureCall expression) {
//		checkForbiddenFeatureCall(expression);
//	}
//
//	/** Check if the call is forbidden.
//	 * <p>
//	 * One example of a forbidden feature is {@link System#exit(int)}.
//	 *
//	 * @param expression - the expression.
//	 */
//	@Check(CheckType.FAST)
//	public void checkForbiddenCalls(XFeatureCall expression) {
//		checkForbiddenFeatureCall(expression);
//	}
//
//	@Check(CheckType.FAST)
//	private void checkDiscouragedFeatureCall(XAbstractFeatureCall expression) {
//		String id = expression.getFeature().getQualifiedName();
//		if (id != null) {
//			switch (id) {
//			case "java.lang.System.err": //$NON-NLS-1$
//			case "java.lang.System.out": //$NON-NLS-1$
//			case "java.lang.System.setErr": //$NON-NLS-1$
//			case "java.lang.System.setOut": //$NON-NLS-1$
//			case "java.lang.System.console": //$NON-NLS-1$
//			case "java.lang.System.inheritedChannel": //$NON-NLS-1$
//				addIssue(
//						Messages.SARLValidator_40,
//						expression,
//						org.eclipse.xtext.xbase.validation.IssueCodes.DISCOURAGED_REFERENCE);
//				break;
//			default:
//				if (id.startsWith("org.eclipse.xtext.xbase.lib.InputOutput")) { //$NON-NLS-1$
//					addIssue(
//							Messages.SARLValidator_41,
//							expression,
//							org.eclipse.xtext.xbase.validation.IssueCodes.DISCOURAGED_REFERENCE);
//				}
//			}
//		}
//	}
//
//	/** Check if the call is discouraged.
//	 * <p>
//	 * One example of a discouraged feature is {@link System#err}.
//	 *
//	 * @param expression - the expression.
//	 */
//	@Check(CheckType.FAST)
//	public void checkDiscouragedCalls(XMemberFeatureCall expression) {
//		if (!isIgnored(org.eclipse.xtext.xbase.validation.IssueCodes.DISCOURAGED_REFERENCE)) {
//			checkDiscouragedFeatureCall(expression);
//		}
//	}
//
//	/** Check if the call is discouraged.
//	 * <p>
//	 * One example of a discouraged feature is {@link System#err}.
//	 *
//	 * @param expression - the expression.
//	 */
//	@Check(CheckType.FAST)
//	public void checkDiscouragedCalls(XFeatureCall expression) {
//		if (!isIgnored(org.eclipse.xtext.xbase.validation.IssueCodes.DISCOURAGED_REFERENCE)) {
//			checkDiscouragedFeatureCall(expression);
//		}
//	}
//
//	/** Check if there is no default value specified for the variadic parameter.
//	 *
//	 * @param parameterContainer - the container of parameters.
//	 */
//	@Check(CheckType.FAST)
//	public void checkNoDefaultValueForVariadicParameter(ParameterizedFeature parameterContainer) {
//		if (parameterContainer.isVarargs()) {
//			EList<FormalParameter> params = parameterContainer.getParams();
//			assert (params != null);
//			FormalParameter lastParam = params.get(params.size() - 1);
//			if (lastParam.getDefaultValue() != null) {
//				error(
//						MessageFormat.format(
//								Messages.SARLValidator_3,
//								lastParam.getName()),
//								parameterContainer,
//								SarlPackage.Literals.PARAMETERIZED_FEATURE__VARARGS,
//								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//								IssueCodes.INVALID_USE_OF_VAR_ARG);
//			}
//		}
//	}
//
//	private void checkDefaultValueTypeCompatibleWithParameterType(FormalParameter param) {
//		JvmTypeReference rawType = param.getParameterType();
//		if (rawType != null) {
//			LightweightTypeReference toType = toLightweightTypeReference(rawType, true);
//			LightweightTypeReference fromType = getActualType(param.getDefaultValue());
//			if (!ModelUtil.canCast(fromType, toType, true, false, true)) {
//				error(MessageFormat.format(
//						Messages.SARLValidator_4,
//						getNameOfTypes(fromType), canonicalName(toType)),
//						param,
//						SarlPackage.Literals.FORMAL_PARAMETER__DEFAULT_VALUE,
//						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//						org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
//						canonicalName(fromType),
//						canonicalName(toType));
//			}
//		} else {
//			error(Messages.SARLValidator_5,
//					param,
//					SarlPackage.Literals.FORMAL_PARAMETER__DEFAULT_VALUE,
//					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//					org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_USE_OF_TYPE);
//		}
//	}
//
//	/** Check if the default values are compatible with the corresponding parameter types.
//	 *
//	 * @param parameterContainer - the container of parameters.
//	 */
//	@Check(CheckType.FAST)
//	public void checkDefaultValueTypeCompatibleWithParameterType(ParameterizedFeature parameterContainer) {
//		for (FormalParameter param : parameterContainer.getParams()) {
//			if (param.getDefaultValue() != null) {
//				checkDefaultValueTypeCompatibleWithParameterType(param);
//			}
//		}
//	}
//
//	/** Check that a feature is not multi-defined.
//	 *
//	 * @param featureContainer - the feature container.
//	 */
//	@Check(CheckType.FAST)
//	public void checkNoFeatureMultiDefinition(FeatureContainer featureContainer) {
//		Set<String> localFields = CollectionLiterals.newTreeSet((Comparator<String>) null);
//		Set<ActionPrototype> localFunctions = CollectionLiterals.newTreeSet((Comparator<ActionPrototype>) null);
//		QualifiedActionName actionID;
//		ActionParameterTypes signatureID;
//		String name;
//		EStructuralFeature errorStructFeature;
//		EObject errorFeature;
//
//		JvmIdentifiableElement container = null;
//
//		for (EObject feature : featureContainer.getFeatures()) {
//			if (container == null) {
//				container = this.logicalContainerProvider.getNearestLogicalContainer(feature);
//			}
//			if (feature instanceof Action) {
//				Action action = (Action) feature;
//				name = action.getName();
//				actionID = this.sarlSignatureProvider.createQualifiedActionName(container, name);
//				signatureID = this.sarlSignatureProvider.createParameterTypesFromSarlModel(
//						action.isVarargs(), action.getParams());
//				errorFeature = action;
//				errorStructFeature = SarlPackage.Literals.ACTION__NAME;
//			} else if (feature instanceof ActionSignature) {
//				ActionSignature signature = (ActionSignature) feature;
//				name = signature.getName();
//				actionID = this.sarlSignatureProvider.createQualifiedActionName(container, name);
//				signatureID = this.sarlSignatureProvider.createParameterTypesFromSarlModel(
//						signature.isVarargs(), signature.getParams());
//				errorFeature = signature;
//				errorStructFeature = SarlPackage.Literals.ACTION_SIGNATURE__NAME;
//			} else if (feature instanceof Constructor) {
//				Constructor constructor = (Constructor) feature;
//				name = this.grammarAccess.getConstructorAccess().getNewKeyword_1().getValue();
//				actionID = this.sarlSignatureProvider.createConstructorQualifiedName(container);
//				signatureID = this.sarlSignatureProvider.createParameterTypesFromSarlModel(
//						constructor.isVarargs(), constructor.getParams());
//				errorFeature = constructor;
//				errorStructFeature = null;
//			} else {
//				name = null;
//				actionID = null;
//				signatureID = null;
//				errorFeature = null;
//				errorStructFeature = null;
//				if (feature instanceof Attribute) {
//					Attribute attribute = (Attribute) feature;
//					if (!localFields.add(attribute.getName())) {
//						error(
//								MessageFormat.format(
//										Messages.SARLValidator_6,
//										Messages.SARLValidator_7,
//										featureContainer.getName(),
//										attribute.getName()),
//										attribute,
//										SarlPackage.Literals.ATTRIBUTE__NAME,
//										ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//										IssueCodes.DUPLICATE_FIELD,
//										attribute.getName());
//					}
//				}
//			}
//			if (actionID != null && signatureID != null) {
//				InferredPrototype sig = this.sarlSignatureProvider.getPrototypes(actionID, signatureID);
//				if (sig != null) {
//					ActionParameterTypes key = sig.getFormalParameterTypes();
//					if (!localFunctions.add(key.toActionPrototype(name))) {
//						String funcName = name + "(" + sig.toString() + ")";  //$NON-NLS-1$//$NON-NLS-2$
//						error(
//								MessageFormat.format(
//										Messages.SARLValidator_6,
//										Messages.SARLValidator_8,
//										featureContainer.getName(),
//										funcName),
//										errorFeature,
//										errorStructFeature,
//										ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//										IssueCodes.DUPLICATE_METHOD,
//										funcName);
//					}
//				}
//			}
//		}
//	}
//
//	/** Check if the name of an action is valid.
//	 * <p>
//	 * Name prefixes are reserved by the SARL specification.
//	 *
//	 * @param action - the action to check.
//	 * @see ModelUtil#isHiddenAction(String)
//	 */
//	@Check(CheckType.FAST)
//	public void checkActionName(ActionSignature action) {
//		if (ModelUtil.isHiddenAction(action.getName())) {
//			String validName1 = ModelUtil.fixHiddenAction(action.getName());
//			String validName2 = ModelUtil.removeHiddenAction(action.getName());
//			error(
//					MessageFormat.format(
//							Messages.SARLValidator_9,
//							action.getName()),
//							action,
//							SarlPackage.Literals.ACTION_SIGNATURE__NAME,
//							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//							IssueCodes.INVALID_MEMBER_NAME,
//							Messages.SARLValidator_8,
//							action.getName(), validName1, validName2);
//		}
//	}
//
//	/** Check if the given action has a valid name.
//	 *
//	 * @param action - the action to test.
//	 */
//	@Check(CheckType.FAST)
//	public void checkActionName(Action action) {
//		if (ModelUtil.isHiddenAction(action.getName())) {
//			String validName1 = ModelUtil.fixHiddenAction(action.getName());
//			String validName2 = ModelUtil.removeHiddenAction(action.getName());
//			error(
//					MessageFormat.format(
//							Messages.SARLValidator_9,
//							action.getName()),
//							action,
//							SarlPackage.Literals.ACTION__NAME,
//							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//							IssueCodes.INVALID_MEMBER_NAME,
//							Messages.SARLValidator_8,
//							action.getName(), validName1, validName2);
//		}
//	}
//
//	/** Check if the given attribute has a valid name.
//	 *
//	 * @param attribute - the attribute to check.
//	 */
//	@Check(CheckType.FAST)
//	public void checkAttributeName(Attribute attribute) {
//		if (ModelUtil.isHiddenAttribute(attribute.getName())) {
//			String validName = ModelUtil.fixHiddenAttribute(attribute.getName());
//			error(
//					MessageFormat.format(
//							Messages.SARLValidator_10,
//							attribute.getName()),
//							attribute,
//							SarlPackage.Literals.ATTRIBUTE__NAME,
//							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//							IssueCodes.INVALID_MEMBER_NAME,
//							Messages.SARLValidator_11,
//							attribute.getName(), validName);
//		}
//	}
//
//	/** Replies the JVM generic type for the given element.
//	 *
//	 * @param element - the element from which the JvmGenericType must be retreived.
//	 * @return the generic type of the given element.
//	 */
//	protected JvmGenericType getJvmGenericType(EObject element) {
//		if (element instanceof JvmGenericType) {
//			return (JvmGenericType) element;
//		}
//		for (EObject obj : getServices().getJvmModelAssociations().getJvmElements(element)) {
//			if (obj instanceof JvmGenericType) {
//				return (JvmGenericType) obj;
//			}
//		}
//		return null;
//	}
//
//	/** Check of the final fields are initialized for the given event.
//	 *
//	 * @param event - the event to check.
//	 */
//	@Check(CheckType.FAST)
//	public void checkFinalFieldInitialization(io.sarl.lang.sarl.Event event) {
//		JvmGenericType type = getJvmGenericType(event);
//		if (type != null) {
//			checkFinalFieldInitialization(type);
//		}
//	}
//
//	/** Check of the final fields are initialized for the given agent.
//	 *
//	 * @param agent - the agent to check.
//	 */
//	@Check(CheckType.FAST)
//	public void checkFinalFieldInitialization(Agent agent) {
//		JvmGenericType type = getJvmGenericType(agent);
//		if (type != null) {
//			checkFinalFieldInitialization(type);
//		}
//	}
//
//	/** Check of the final fields are initialized for the given behavior.
//	 *
//	 * @param behavior - the behavior to check.
//	 */
//	@Check(CheckType.FAST)
//	public void checkFinalFieldInitialization(Behavior behavior) {
//		JvmGenericType type = getJvmGenericType(behavior);
//		if (type != null) {
//			checkFinalFieldInitialization(type);
//		}
//	}
//
//	/** Check of the final fields are initialized for the given skill.
//	 *
//	 * @param skill - the skill to check.
//	 */
//	@Check(CheckType.FAST)
//	public void checkFinalFieldInitialization(Skill skill) {
//		JvmGenericType type = getJvmGenericType(skill);
//		if (type != null) {
//			checkFinalFieldInitialization(type);
//		}
//	}
//
//	/** {@inheritDoc}
//	 */
//	@Override
//	protected void reportUninitializedField(JvmField field) {
//		EObject sarlElement = getServices().getJvmModelAssociations().getPrimarySourceElement(field);
//		error(
//				MessageFormat.format(
//						Messages.SARLValidator_12,
//						field.getSimpleName()),
//						sarlElement,
//						SarlPackage.Literals.ATTRIBUTE__NAME,
//						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//						org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_INITIALIZATION);
//	}
//
//	/** {@inheritDoc}
//	 */
//	@Override
//	protected void reportUninitializedField(JvmField field, JvmConstructor constructor) {
//		error(
//				MessageFormat.format(
//						Messages.SARLValidator_38,
//						field.getSimpleName(),
//						constructor.toString()),
//						constructor,
//						null,
//						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//						org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_INITIALIZATION);
//	}
//
//	private boolean checkRedundantInterface(
//			InheritingElement element,
//			EReference structuralElement,
//			LightweightTypeReference lightweightInterfaceReference,
//			List<LightweightTypeReference> knownInterfaces) {
//		int index = 0;
//		for (LightweightTypeReference previousInterface : knownInterfaces) {
//			if (memberOfTypeHierarchy(previousInterface, lightweightInterfaceReference)) {
//				error(
//						MessageFormat.format(
//								Messages.SARLValidator_13,
//								canonicalName(lightweightInterfaceReference)),
//								element,
//								structuralElement,
//								// The index of the element to highlight in the super-types
//								knownInterfaces.size(),
//								IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
//								canonicalName(lightweightInterfaceReference),
//						"pre"); //$NON-NLS-1$
//				return true;
//			} else if (memberOfTypeHierarchy(lightweightInterfaceReference, previousInterface)) {
//				error(
//						MessageFormat.format(
//								Messages.SARLValidator_13,
//								canonicalName(previousInterface)),
//								element,
//								structuralElement,
//								index,
//								IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
//								canonicalName(previousInterface),
//						"post"); //$NON-NLS-1$
//			}
//			++index;
//		}
//		return false;
//	}
//
//	private void checkRedundantInterfaces(
//			InheritingElement element,
//			EReference structuralElement,
//			Iterable<? extends JvmTypeReference> interfaces,
//			Iterable<? extends JvmTypeReference> superTypes) {
//		List<LightweightTypeReference> knownInterfaces = CollectionLiterals.newArrayList();
//		for (JvmTypeReference interfaceRef : interfaces) {
//			LightweightTypeReference lightweightInterfaceReference = toLightweightTypeReference(interfaceRef);
//			// Check the interface against the other interfaces
//			if (!checkRedundantInterface(
//					element, structuralElement,
//					lightweightInterfaceReference,
//					knownInterfaces)) {
//				// Check the interface against the super-types
//				if (superTypes != null && !isIgnored(IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION)) {
//					for (JvmTypeReference superType : superTypes) {
//						LightweightTypeReference lightweightSuperType = toLightweightTypeReference(superType);
//						if (memberOfTypeHierarchy(lightweightSuperType, lightweightInterfaceReference)) {
//							addIssue(
//									MessageFormat.format(
//											Messages.SARLValidator_14,
//											canonicalName(lightweightInterfaceReference),
//											canonicalName(lightweightSuperType)),
//											element,
//											structuralElement,
//											// The index of the element to highlight in the super-types
//											knownInterfaces.size(),
//											IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
//											canonicalName(lightweightInterfaceReference),
//									"unknow"); //$NON-NLS-1$
//						}
//					}
//				}
//			}
//			// Prepare next loop
//			knownInterfaces.add(lightweightInterfaceReference);
//		}
//	}
//
//	/** Check the inherited features.
//	 *
//	 * @param element - the child element.
//	 */
//	@SuppressWarnings("unchecked")
//	@Check(CheckType.FAST)
//	public void checkInheritedFeatures(InheritingElement element) {
//		JvmGenericType jvmElement = getJvmGenericType(element);
//		if (jvmElement != null) {
//			Map<ActionPrototype, JvmOperation> finalOperations =
//					CollectionLiterals.newTreeMap((Comparator<ActionPrototype>) null);
//			Map<ActionPrototype, JvmOperation> overridableOperations =
//					CollectionLiterals.newTreeMap((Comparator<ActionPrototype>) null);
//			Map<String, JvmField> inheritedFields =
//					CollectionLiterals.newTreeMap((Comparator<String>) null);
//			Map<ActionPrototype, JvmOperation> operationsToImplement =
//					CollectionLiterals.newTreeMap((Comparator<ActionPrototype>) null);
//
//			ModelUtil.populateInheritanceContext(
//					jvmElement,
//					finalOperations, overridableOperations,
//					inheritedFields, operationsToImplement,
//					null, this.sarlSignatureProvider);
//
//			if (jvmElement.isInterface()) {
//				checkRedundantInterfaces(
//						element, SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
//						element.getSuperTypes(),
//						null);
//			} else if (element instanceof ImplementingElement) {
//				ImplementingElement iElement = (ImplementingElement) element;
//				checkRedundantInterfaces(
//						iElement,
//						SarlPackage.Literals.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES,
//						iElement.getImplementedTypes(),
//						iElement.getSuperTypes());
//			}
//
//			for (EObject feature : element.getFeatures()) {
//				if (feature instanceof Attribute) {
//					Attribute attribute = (Attribute) feature;
//					if (!isIgnored(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING)) {
//						if (!ModelUtil.isHiddenAttribute(attribute.getName())) {
//							JvmField inheritedField = inheritedFields.get(attribute.getName());
//							if (inheritedField != null) {
//								int nameIndex = 0;
//								String newName = attribute.getName() + nameIndex;
//								while (inheritedFields.containsKey(newName)) {
//									++nameIndex;
//									newName = attribute.getName() + nameIndex;
//								}
//								addIssue(
//										MessageFormat.format(
//												Messages.SARLValidator_15,
//												attribute.getName(),
//												jvmElement.getQualifiedName(),
//												inheritedField.getQualifiedName()),
//												feature,
//												SarlPackage.Literals.ATTRIBUTE__NAME,
//												ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//												org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
//												attribute.getName(),
//												newName);
//							}
//						}
//					}
//				} else if (feature instanceof Action) {
//					Action action = (Action) feature;
//					checkInheritedActionElement(
//							finalOperations,
//							overridableOperations,
//							operationsToImplement,
//							action,
//							action.getName(),
//							action,
//							action.getType(),
//							SarlPackage.Literals.ACTION__NAME,
//							SarlPackage.Literals.ACTION__TYPE);
//				} else if (feature instanceof ActionSignature) {
//					ActionSignature signature = (ActionSignature) feature;
//					checkInheritedActionElement(
//							finalOperations,
//							overridableOperations,
//							operationsToImplement,
//							signature,
//							signature.getName(),
//							signature,
//							signature.getType(),
//							SarlPackage.Literals.ACTION_SIGNATURE__NAME,
//							SarlPackage.Literals.ACTION_SIGNATURE__TYPE);
//				}
//			}
//
//			if (!jvmElement.isAbstract() && !jvmElement.isInterface() && !operationsToImplement.isEmpty()) {
//
//				// The missed function may be generated on the Java side
//				// (see generateMissedFunction function in SARLJvmModelInferrer).
//				for (JvmOperation javaOp : jvmElement.getDeclaredOperations()) {
//					ActionParameterTypes sig = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
//							javaOp.isVarArgs(), javaOp.getParameters());
//					ActionPrototype actionKey = this.sarlSignatureProvider.createActionPrototype(javaOp.getSimpleName(), sig);
//					operationsToImplement.remove(actionKey);
//				}
//
//				// Now, we are sure that there is missed operations
//				if (!operationsToImplement.isEmpty()) {
//					ImportManager importManager = new ImportManager();
//					List<String> data = CollectionLiterals.newLinkedList();
//					Iterator<JvmOperation> iterator = operationsToImplement.values().iterator();
//					while (iterator.hasNext()) {
//						JvmOperation value = iterator.next();
//						if (!ModelUtil.hasAnnotation(value, Generated.class)) {
//							// Get quick fix information
//							ActionSignature signature = this.codeGenerator.createActionSignature(value, importManager);
//							String sarlCode = ModelUtil.getActionSignatureString(signature, this.serializer,
//									this.grammarAccess, importManager);
//							// Sometimes, the serializer added tabular and new line characters to the sarlCode
//							// due to the formating rules.
//							sarlCode = sarlCode.trim();
//							// Add the data for the quick fix
//							data.add(sarlCode);
//							JvmTypeReference returnType = value.getReturnType();
//							LightweightTypeReference lwRef;
//							if (returnType == null) {
//								lwRef = null;
//							} else {
//								lwRef = toLightweightTypeReference(returnType);
//							}
//							data.add(ModelUtil.getDefaultValueForType(lwRef));
//							// Generate a simplified name
//							String simplifiedName =
//									sarlCode.replaceFirst(
//											"^\\s*" //$NON-NLS-1$
//											+ Pattern.quote(
//													this.grammarAccess.getActionSignatureAccess().getDefKeyword_1().getValue())
//											+ "\\s+", //$NON-NLS-1$
//											""); //$NON-NLS-1$
//							// Generate the error
//							if (iterator.hasNext()) {
//								// No need to quick fix
//								error(
//										MessageFormat.format(
//												Messages.SARLValidator_18,
//												simplifiedName),
//										element,
//										SarlPackage.Literals.NAMED_ELEMENT__NAME,
//										ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//										IssueCodes.MISSING_METHOD_IMPLEMENTATION);
//							} else {
//								// Finalize the data collection to send to the quick fix module
//								data.add(0, Strings.nullToEmpty(null));
//								data.addAll(0, importManager.getImports());
//								//
//								String[] dataArray = new String[data.size()];
//								data.toArray(dataArray);
//								error(
//										MessageFormat.format(
//												Messages.SARLValidator_18,
//												simplifiedName),
//										element,
//										SarlPackage.Literals.NAMED_ELEMENT__NAME,
//										ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//										IssueCodes.MISSING_METHOD_IMPLEMENTATION,
//										// Provides the prototypes for the quick fixes
//										dataArray);
//							}
//						}
//					}
//				}
//			}
//		}
//	}
//
//	private void checkInheritedActionElement(
//			Map<ActionPrototype, JvmOperation> finalOperations,
//			Map<ActionPrototype, JvmOperation> overridableOperations,
//			Map<ActionPrototype, JvmOperation> operationsToImplement,
//			EObject referenceObject,
//			String name,
//			ParameterizedFeature paramOwner,
//			JvmTypeReference type,
//			EAttribute nameAttribute,
//			EReference typeAttribute) {
//		ActionParameterTypes sig = this.sarlSignatureProvider.createParameterTypesFromSarlModel(
//				paramOwner.isVarargs(), paramOwner.getParams());
//		ActionPrototype actionKey = this.sarlSignatureProvider.createActionPrototype(name, sig);
//		if (finalOperations.containsKey(actionKey)) {
//			error(
//					MessageFormat.format(
//							Messages.SARLValidator_16,
//							actionKey.toString()),
//							referenceObject,
//							nameAttribute,
//							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//							IssueCodes.OVERRIDDEN_FINAL_OPERATION,
//							actionKey.toString());
//		} else {
//			JvmOperation implementableFunction = operationsToImplement.remove(actionKey);
//			if (implementableFunction != null) {
//				LightweightTypeReference currentReturnType = (type == null) ? null : toLightweightTypeReference(type);
//				JvmTypeReference typeRef = implementableFunction.getReturnType();
//				LightweightTypeReference inheritedReturnType = (typeRef == null) ? null : toLightweightTypeReference(typeRef);
//				if (!ModelUtil.canCast(currentReturnType, inheritedReturnType, false, true, true)) {
//					assert (inheritedReturnType != null);
//					error(
//							MessageFormat.format(
//									Messages.SARLValidator_17,
//									canonicalTypeName(currentReturnType),
//									canonicalTypeName(inheritedReturnType),
//									actionKey.toString()),
//									referenceObject,
//									typeAttribute,
//									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//									org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
//									canonicalTypeName(currentReturnType),
//									inheritedReturnType.getIdentifier());
//				}
//			} else {
//				JvmOperation superOperation = overridableOperations.get(actionKey);
//				if (superOperation != null) {
//					LightweightTypeReference currentReturnType = (type == null) ? null : toLightweightTypeReference(type);
//					JvmTypeReference typeRef = superOperation.getReturnType();
//					LightweightTypeReference inheritedReturnType = (typeRef == null) ? null : toLightweightTypeReference(typeRef);
//					if (!ModelUtil.canCast(currentReturnType, inheritedReturnType, false, true, true)) {
//						assert (inheritedReturnType != null);
//						error(
//								MessageFormat.format(
//										Messages.SARLValidator_17,
//										canonicalTypeName(currentReturnType),
//										canonicalTypeName(inheritedReturnType),
//										actionKey.toString()),
//										referenceObject,
//										typeAttribute,
//										ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//										org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
//										canonicalTypeName(currentReturnType),
//										inheritedReturnType.getIdentifier());
//					}
//				}
//			}
//		}
//	}
//
//	@SuppressWarnings("unchecked")
//	private void checkImplicitConstructorCall(FeatureContainer container, ActionParameterTypes[] defaultSignatures) {
//		JvmGenericType jvmElement = getJvmGenericType(container);
//		if (jvmElement != null) {
//			Map<ActionParameterTypes, JvmConstructor> superConstructors =
//					CollectionLiterals.newTreeMap((Comparator<ActionParameterTypes>) null);
//			JvmTypeReference typeRef = jvmElement.getExtendedClass();
//			JvmType supertype = (typeRef == null) ? null : typeRef.getType();
//			if (supertype != null) {
//				JvmGenericType jvmSuperElement = getJvmGenericType(supertype);
//				if (jvmSuperElement != null) {
//					for (JvmConstructor superConstructor : jvmSuperElement.getDeclaredConstructors()) {
//						ActionParameterTypes sig = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
//								superConstructor.isVarArgs(), superConstructor.getParameters());
//						superConstructors.put(sig, superConstructor);
//					}
//				}
//			}
//
//			ActionParameterTypes voidKey = this.sarlSignatureProvider.createParameterTypesForVoid();
//			boolean hasDeclaredConstructor = false;
//
//			for (EObject feature : container.getFeatures()) {
//				if (feature instanceof Constructor) {
//					Constructor constructor = (Constructor) feature;
//					hasDeclaredConstructor = true;
//					boolean invokeDefaultConstructor = true;
//					XExpression body = constructor.getBody();
//					if (body instanceof XBlockExpression) {
//						XBlockExpression block = (XBlockExpression) body;
//						if (!block.getExpressions().isEmpty()) {
//							XExpression firstStatement = block.getExpressions().get(0);
//							if (firstStatement instanceof XConstructorCall) {
//								invokeDefaultConstructor = false;
//							} else if (firstStatement instanceof XFeatureCall) {
//								JvmIdentifiableElement calledFeature = ((XFeatureCall) firstStatement).getFeature();
//								if (calledFeature instanceof JvmConstructor) {
//									invokeDefaultConstructor = false;
//								}
//							}
//						}
//					} else if (body instanceof XConstructorCall) {
//						invokeDefaultConstructor = false;
//					} else if (body instanceof XFeatureCall) {
//						JvmIdentifiableElement calledFeature = ((XFeatureCall) body).getFeature();
//						if (calledFeature instanceof JvmConstructor) {
//							invokeDefaultConstructor = false;
//						}
//					}
//					if (invokeDefaultConstructor && !superConstructors.containsKey(voidKey)) {
//						error(
//								Messages.SARLValidator_19,
//								feature,
//								SarlPackage.Literals.CONSTRUCTOR__BODY,
//								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//								IssueCodes.MISSING_CONSTRUCTOR);
//					}
//				}
//			}
//
//			if (!hasDeclaredConstructor) {
//				for (ActionParameterTypes defaultSignature : defaultSignatures) {
//					if (!superConstructors.containsKey(defaultSignature)) {
//						assert (supertype != null);
//						error(
//								MessageFormat.format(
//										Messages.SARLValidator_20,
//										this.sarlSignatureProvider.createActionPrototype(
//												supertype.getSimpleName(),
//												defaultSignature)),
//												container,
//												SarlPackage.Literals.NAMED_ELEMENT__NAME,
//												ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//												IssueCodes.MISSING_CONSTRUCTOR);
//					}
//				}
//			}
//
//		}
//	}
//
//	/** Check the implicit call to the super constructor.
//	 *
//	 * @param event - the child type.
//	 */
//	@Check(CheckType.FAST)
//	public void checkImplicitConstructorCall(io.sarl.lang.sarl.Event event) {
//		checkImplicitConstructorCall(event, new ActionParameterTypes[] {
//				this.sarlSignatureProvider.createParameterTypesForVoid(),
//				this.sarlSignatureProvider.createParameterTypesFromString("io.sarl.lang.core.Address"), //$NON-NLS-1$
//		});
//	}
//
//	/** Check the implicit call to the super constructor.
//	 *
//	 * @param behavior - the child type.
//	 */
//	@Check(CheckType.FAST)
//	public void checkImplicitConstructorCall(Behavior behavior) {
//		checkImplicitConstructorCall(behavior, new ActionParameterTypes[] {
//				this.sarlSignatureProvider.createParameterTypesFromString("io.sarl.lang.core.Agent"), //$NON-NLS-1$
//		});
//	}
//
//	/** Check the type of the behavior unit's guard.
//	 *
//	 * @param behaviorUnit - the behavior unit.
//	 */
//	@Check(CheckType.FAST)
//	public void checkBehaviorUnitGuardType(BehaviorUnit behaviorUnit) {
//		XExpression guard = behaviorUnit.getGuard();
//		if (guard != null) {
//			if (guard instanceof XBooleanLiteral) {
//				XBooleanLiteral bLiteral = (XBooleanLiteral) guard;
//				if (bLiteral.isIsTrue()) {
//					if (!isIgnored(IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION)) {
//						addIssue(Messages.SARLValidator_21,
//								bLiteral,
//								null,
//								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//								IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION);
//					}
//				} else if (!isIgnored(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT)) {
//					addIssue(Messages.SARLValidator_22,
//							behaviorUnit,
//							null,
//							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//							IssueCodes.UNREACHABLE_BEHAVIOR_UNIT,
//							behaviorUnit.getName().getSimpleName());
//				}
//				return;
//			}
//
//			LightweightTypeReference fromType = getActualType(guard);
//			if (!fromType.isAssignableFrom(Boolean.TYPE)) {
//				error(MessageFormat.format(Messages.SARLValidator_23,
//						getNameOfTypes(fromType), boolean.class.getName()),
//						behaviorUnit.getGuard(),
//						null,
//						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//						org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES);
//			}
//		}
//	}
//
//	/** Check the type of the capacity uses.
//	 *
//	 * @param uses - the capacity uses.
//	 */
//	@Check(CheckType.FAST)
//	public void checkCapacityTypeForUses(CapacityUses uses) {
//		for (JvmParameterizedTypeReference usedType : uses.getCapacitiesUsed()) {
//			LightweightTypeReference ref = toLightweightTypeReference(usedType);
//			if (ref != null && !ref.isSubtypeOf(Capacity.class)) {
//				error(
//						MessageFormat.format(
//								Messages.SARLValidator_24,
//								usedType.getQualifiedName(),
//								Messages.SARLValidator_25,
//								this.grammarAccess.getCapacityUsesAccess().getUsesKeyword_1().getValue()),
//								usedType,
//								null,
//								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//								IssueCodes.INVALID_CAPACITY_TYPE,
//								usedType.getSimpleName());
//			}
//		}
//	}
//
//	/** Check the types of the "requires" statement.
//	 *
//	 * @param requires - the "requires" statement.
//	 */
//	@Check(CheckType.FAST)
//	public void checkCapacityTypeForRequires(RequiredCapacity requires) {
//		for (JvmParameterizedTypeReference requiredType : requires.getRequiredCapacities()) {
//			LightweightTypeReference ref = toLightweightTypeReference(requiredType);
//			if (ref != null && !ref.isSubtypeOf(Capacity.class)) {
//				error(
//						MessageFormat.format(
//								Messages.SARLValidator_24,
//								requiredType.getQualifiedName(),
//								Messages.SARLValidator_25,
//								this.grammarAccess.getRequiredCapacityAccess().getRequiresKeyword_1().getValue()),
//								requiredType,
//								null,
//								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//								IssueCodes.INVALID_CAPACITY_TYPE,
//								requiredType.getSimpleName());
//			}
//		}
//	}
//
//	/** Check the types of the parameters of the "fires" statement.
//	 *
//	 * @param action - the signature that contains the "fires" statement.
//	 */
//	@Check(CheckType.FAST)
//	public void checkActionSignatureFires(ActionSignature action) {
//		for (JvmParameterizedTypeReference event : action.getFiredEvents()) {
//			LightweightTypeReference ref = toLightweightTypeReference(event);
//			if (ref != null && !ref.isSubtypeOf(Event.class)) {
//				error(
//						MessageFormat.format(
//								Messages.SARLValidator_24,
//								event.getQualifiedName(),
//								Messages.SARLValidator_26,
//								this.grammarAccess.getActionSignatureAccess().getFiresKeyword_5_0().getValue()),
//								event,
//								null,
//								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//								IssueCodes.INVALID_FIRING_EVENT_TYPE,
//								event.getSimpleName());
//			}
//		}
//	}
//
//	/** Check the types of the parameters of the "fires" statement.
//	 *
//	 * @param action - the signature that contains the "fires" statement.
//	 */
//	@Check(CheckType.FAST)
//	public void checkActionFires(Action action) {
//		for (JvmParameterizedTypeReference event : action.getFiredEvents()) {
//			LightweightTypeReference ref = toLightweightTypeReference(event);
//			if (ref != null && !ref.isSubtypeOf(Event.class)) {
//				error(
//						MessageFormat.format(
//								Messages.SARLValidator_24,
//								event.getQualifiedName(),
//								Messages.SARLValidator_26,
//								this.grammarAccess.getActionSignatureAccess().getFiresKeyword_5_0().getValue()),
//								event,
//								null,
//								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//								IssueCodes.INVALID_FIRING_EVENT_TYPE,
//								event.getSimpleName());
//			}
//		}
//	}

	/** Check the super type.
	 *
	 * @param element - the child type.
	 * @param expectedType - the expected root type.
	 * @param onlySubTypes - if <code>true</code> only the subtype of the <code>expectedType</code> are valid;
	 * <code>false</code> if the <code>expectedType</code> is allowed.
	 * @return the count of supertypes.
	 */
	protected int checkSuperTypes(XtendTypeDeclaration element, Class<?> expectedType, boolean onlySubTypes) {
		int nbSuperTypes = 0;
		JvmGenericType inferredType = getJvmGenericType(element);
		if (inferredType != null) {
			LinkedList<JvmTypeReference> inferredSuperTypes = CollectionLiterals.newLinkedList();
			inferredSuperTypes.addAll(inferredType.getSuperTypes());
			boolean isExpectingInterface = expectedType.isInterface();
			int superTypeIndex = 0;
			for (JvmTypeReference superType : element.getSuperTypes()) {
				boolean success = true;
				JvmType jvmSuperType = (superType == null) ? null : superType.getType();
				if (jvmSuperType != null) {
					final JvmTypeReference inferredSuperType =
							(inferredSuperTypes.isEmpty()) ? null : inferredSuperTypes.removeFirst();
					LightweightTypeReference lighweightSuperType = toLightweightTypeReference(superType);
					if (!(jvmSuperType instanceof JvmGenericType)
							|| (isExpectingInterface != ((JvmGenericType) jvmSuperType).isInterface())) {
						if (isExpectingInterface) {
							error(
									MessageFormat.format(Messages.SARLValidator_27, Messages.SARLValidator_28),
									SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
									superTypeIndex,
									IssueCodes.INVALID_EXTENDED_TYPE,
									inferredType.getIdentifier(),
									jvmSuperType.getIdentifier());
						} else {
							error(
									MessageFormat.format(Messages.SARLValidator_27, Messages.SARLValidator_29),
									SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
									superTypeIndex,
									IssueCodes.INVALID_EXTENDED_TYPE,
									inferredType.getIdentifier(),
									jvmSuperType.getIdentifier());
						}
						success = false;
					} else if (isFinal(lighweightSuperType)) {
						error(Messages.SARLValidator_30,
								SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
								superTypeIndex,
								IssueCodes.OVERRIDDEN_FINAL_TYPE,
								inferredType.getIdentifier(),
								jvmSuperType.getIdentifier());
						success = false;
					} else if (!lighweightSuperType.isSubtypeOf(expectedType)
							|| ((onlySubTypes) && (lighweightSuperType.isType(expectedType)))) {
						if (onlySubTypes) {
							error(MessageFormat.format(Messages.SARLValidator_31, expectedType.getName()),
									SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
									superTypeIndex,
									IssueCodes.INVALID_EXTENDED_TYPE,
									inferredType.getIdentifier(),
									jvmSuperType.getIdentifier());
						} else {
							error(MessageFormat.format(Messages.SARLValidator_32, expectedType.getName()),
									SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
									superTypeIndex,
									IssueCodes.INVALID_EXTENDED_TYPE,
									inferredType.getIdentifier(),
									jvmSuperType.getIdentifier());
						}
						success = false;
					} else if (inferredSuperType == null
							|| !Objects.equal(inferredSuperType.getIdentifier(), jvmSuperType.getIdentifier())
							|| Objects.equal(inferredType.getIdentifier(), jvmSuperType.getIdentifier())) {
						error(MessageFormat.format(Messages.SARLValidator_33,
								inferredType.getQualifiedName()),
								SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
								superTypeIndex,
								IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
								inferredType.getIdentifier(),
								jvmSuperType.getIdentifier());
						success = false;
					}
				} else {
					error(MessageFormat.format(Messages.SARLValidator_33,
							inferredType.getQualifiedName()),
							SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
							superTypeIndex,
							IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
							inferredType.getIdentifier());
					success = false;
				}
				++superTypeIndex;
				if (success) {
					++nbSuperTypes;
				}
			}
		}
		return nbSuperTypes;
	}

//	/** Check the implemeted type.
//	 *
//	 * @param element - the child type.
//	 * @param expectedType - the expected root type.
//	 * @param mandatoryNumberOfTypes - the minimal number of implemented types.
//	 * @param onlySubTypes - if <code>true</code> only the subtype of the <code>expectedType</code> are valid;
//	 * <code>false</code> if the <code>expectedType</code> is allowed.
//	 * @return the count of supertypes.
//	 */
//	protected boolean checkImplementedTypes(ImplementingElement element, Class<?> expectedType,
//			int mandatoryNumberOfTypes, boolean onlySubTypes) {
//		boolean success = true;
//		int nb = 0;
//		for (JvmTypeReference superType : element.getImplementedTypes()) {
//			LightweightTypeReference  ref = toLightweightTypeReference(superType);
//			if (ref != null
//				&& (!ref.isInterfaceType() || !ref.isSubtypeOf(expectedType)
//					|| (onlySubTypes && ref.isType(expectedType)))) {
//				String msg;
//				if (onlySubTypes) {
//					msg = Messages.SARLValidator_34;
//				} else {
//					msg = Messages.SARLValidator_35;
//				}
//				error(
//						MessageFormat.format(
//								msg,
//								superType.getQualifiedName(),
//								expectedType.getName(),
//								element.getName()),
//								superType,
//								null,
//								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//								IssueCodes.INVALID_IMPLEMENTED_TYPE,
//								superType.getSimpleName());
//				success = false;
//			} else {
//				++nb;
//			}
//		}
//		if (nb < mandatoryNumberOfTypes) {
//			error(
//					MessageFormat.format(
//							Messages.SARLValidator_36,
//							expectedType.getName(),
//							element.getName()),
//							element,
//							SarlPackage.Literals.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES,
//							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//							org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_TYPE);
//			success = false;
//		}
//		return success;
//	}

	/** Check if the supertype of the given event is a subtype of Event.
	 *
	 * @param event - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkEventSuperType(io.sarl.lang.sarl.Event event) {
		checkSuperTypes(event, Event.class, false);
	}

	/** Check if the supertype of the given behavior is a subtype of Behavior.
	 *
	 * @param behavior - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkBehaviorSuperType(Behavior behavior) {
		checkSuperTypes(behavior, io.sarl.lang.core.Behavior.class, false);
	}

	/** Check if the supertype of the given agent is a subtype of Agent.
	 *
	 * @param agent - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkAgentSuperType(Agent agent) {
		checkSuperTypes(agent, io.sarl.lang.core.Agent.class, false);
	}

	/** Check if the supertype of the given capacity is a subtype of Capacity.
	 *
	 * @param capacity - the type to test.
	 */
	@Check(CheckType.FAST)
	public void checkCapacitySuperType(Capacity capacity) {
		checkSuperTypes(capacity, Capacity.class, false);
	}

//	/** Check if the supertype of the given skill is a subtype of Skill.
//	 *
//	 * @param skill - the type to test.
//	 */
//	@Check(CheckType.FAST)
//	public void checkSkillSuperType(Skill skill) {
//		int nbSuperTypes = checkSuperTypes(skill, io.sarl.lang.core.Skill.class, false);
//		checkImplementedTypes(skill, Capacity.class,
//				(nbSuperTypes > 0) ? 0 : 1,
//						true);
//	}
//
//	/** Check if the parameter of the bahavior unit is an event.
//	 *
//	 * @param behaviorUnit - the behavior unit to test.
//	 */
//	@Check(CheckType.FAST)
//	public void checkBehaviorUnitEventType(BehaviorUnit behaviorUnit) {
//		JvmTypeReference event = behaviorUnit.getName();
//		LightweightTypeReference ref = toLightweightTypeReference(event);
//		if (ref == null || ref.isInterfaceType() || !ref.isSubtypeOf(Event.class)) {
//			error(
//					MessageFormat.format(
//							Messages.SARLValidator_24,
//							event.getQualifiedName(),
//							Messages.SARLValidator_26,
//							this.grammarAccess.getBehaviorUnitAccess().getOnKeyword_1().getValue()),
//							event,
//							null,
//							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//							org.eclipse.xtext.xbase.validation.IssueCodes.TYPE_BOUNDS_MISMATCH);
//		}
//	}
//
//	/** Check if a capacity has a feature defined inside.
//	 *
//	 * @param capacity - the capacity to test.
//	 */
//	@Check(CheckType.FAST)
//	public void checkCapacityFeatures(io.sarl.lang.sarl.Capacity capacity) {
//		if (capacity.getFeatures().isEmpty()) {
//			if (!isIgnored(IssueCodes.DISCOURAGED_CAPACITY_DEFINITION)) {
//				addIssue(Messages.SARLValidator_37,
//						capacity,
//						null,
//						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
//						IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
//						capacity.getName(),
//						"aFunction"); //$NON-NLS-1$
//			}
//		}
//	}
//
//	private static JvmAnnotationReference findFirstAnnotation(JvmOperation operation, final String annotationId) {
//		return IterableExtensions.findFirst(operation.getAnnotations(),
//				new Functions.Function1<JvmAnnotationReference, Boolean>() {
//			@Override
//			public Boolean apply(JvmAnnotationReference it) {
//				return Objects.equal(it.getAnnotation().getIdentifier(), annotationId);
//			}
//		});
//	}
//
//	private boolean findLocalUsage(Collection<JvmOperation> operations, EObject container) {
//		Iterator<JvmOperation> iterator = operations.iterator();
//		boolean found = false;
//		while (!found && iterator.hasNext()) {
//			JvmOperation operation = iterator.next();
//			if (isLocallyUsed(operation, container)) {
//				found = true;
//			}
//		}
//		return found;
//	}
//
//	/** Check for unused capacities.
//	 *
//	 * @param uses - the capacity use declaration.
//	 */
//	@SuppressWarnings("unchecked")
//	@Check(CheckType.NORMAL)
//	public void checkUnusedCapacities(CapacityUses uses) {
//		if (!isIgnored(IssueCodes.UNUSED_AGENT_CAPACITY)) {
//			JvmIdentifiableElement jvmContainer = this.logicalContainerProvider.getNearestLogicalContainer(uses);
//			EObject container = this.jvmModelAssociator.getPrimarySourceElement(jvmContainer);
//
//			final String annotationId = ImportedCapacityFeature.class.getName();
//			Multimap<String, JvmOperation> importedFeatures = Multimaps.newMultimap(
//					CollectionLiterals.<String, Collection<JvmOperation>>newHashMap(),
//					new Supplier<Collection<JvmOperation>>() {
//						@Override
//						public Collection<JvmOperation> get() {
//							return CollectionLiterals.<JvmOperation>newArrayList();
//						}
//					});
//			Iterable<EObject> fitleredObjects = IterableExtensions.filter(jvmContainer.eContents(),
//					new Functions.Function1<EObject, Boolean>() {
//				@SuppressWarnings("synthetic-access")
//				@Override
//				public Boolean apply(EObject it) {
//					if (it instanceof JvmOperation) {
//						JvmOperation operation = (JvmOperation) it;
//						if (findFirstAnnotation(operation, annotationId) != null) {
//							return Boolean.TRUE;
//						}
//					}
//					return Boolean.FALSE;
//				}
//
//			});
//			for (EObject method : fitleredObjects) {
//				JvmOperation m = (JvmOperation) method;
//				EList<JvmAnnotationValue> annotationValues = findFirstAnnotation(m, annotationId).getValues();
//				JvmTypeAnnotationValue annotationType = (JvmTypeAnnotationValue) annotationValues.get(0);
//				JvmTypeReference capacityType = annotationType.getValues().get(0);
//				importedFeatures.put(capacityType.getIdentifier(), m);
//			}
//
//			int index = 0;
//			for (JvmTypeReference capacity : uses.getCapacitiesUsed()) {
//				Collection<JvmOperation> operations = importedFeatures.get(capacity.getIdentifier());
//				if (!operations.isEmpty()) {
//					if (!findLocalUsage(operations, container)) {
//						addIssue(
//								MessageFormat.format(
//										Messages.SARLValidator_42,
//										capacity.getSimpleName()),
//										uses,
//										SarlPackage.Literals.CAPACITY_USES__CAPACITIES_USED,
//										index, IssueCodes.UNUSED_AGENT_CAPACITY,
//										capacity.getSimpleName());
//					}
//				}
//				++index;
//			}
//		}
//	}
//
//	private static Set<String> findPreviousCapacities(CapacityUses uses, Iterator<EObject> iterator) {
//		boolean continueToFill = true;
//		Set<String> capacityUses = CollectionLiterals.newTreeSet((Comparator<String>) null);
//		while (continueToFill && iterator.hasNext()) {
//			EObject elt = iterator.next();
//			if (elt instanceof CapacityUses) {
//				CapacityUses usesElt = (CapacityUses) elt;
//				if (usesElt == uses) {
//					continueToFill = false;
//				} else {
//					for (JvmTypeReference use : usesElt.getCapacitiesUsed()) {
//						capacityUses.add(use.getIdentifier());
//					}
//				}
//			}
//		}
//		return capacityUses;
//	}
//
//	/** Check for multiple capacity use declaration.
//	 *
//	 * @param uses - the capacity use declaration.
//	 */
//	@Check(CheckType.NORMAL)
//	public void checkMultipleCapacityUses(CapacityUses uses) {
//		if (!isIgnored(IssueCodes.REDUNDANT_CAPACITY_USE)) {
//			JvmIdentifiableElement jvmContainer = this.logicalContainerProvider.getNearestLogicalContainer(uses);
//			EObject container = this.jvmModelAssociator.getPrimarySourceElement(jvmContainer);
//			if (container instanceof FeatureContainer) {
//				FeatureContainer fContainer = (FeatureContainer) container;
//				Set<String> previousCapacityUses = findPreviousCapacities(uses, fContainer.getFeatures().iterator());
//				int index = 0;
//				for (JvmTypeReference capacity : uses.getCapacitiesUsed()) {
//					if (previousCapacityUses.contains(capacity.getIdentifier())) {
//						addIssue(
//								MessageFormat.format(
//										Messages.SARLValidator_43,
//										capacity.getSimpleName()),
//										uses,
//										SarlPackage.Literals.CAPACITY_USES__CAPACITIES_USED,
//										index,
//										IssueCodes.REDUNDANT_CAPACITY_USE,
//										capacity.getSimpleName());
//					} else {
//						previousCapacityUses.add(capacity.getIdentifier());
//					}
//					++index;
//				}
//			}
//		}
//	}

}
