/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlCastedExpression;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.sarl.SarlEnumLiteral;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlFactory;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.xtend.core.xtend.XtendPackage;

import org.eclipse.xtext.common.types.TypesPackage;

import org.eclipse.xtext.xbase.XbasePackage;

import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;

import org.eclipse.xtext.xtype.XtypePackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class SarlPackageImpl extends EPackageImpl implements SarlPackage
{
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlScriptEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlFieldEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.5
	 * @generated
	 */
	private EClass sarlBreakExpressionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.7
	 * @generated
	 */
	private EClass sarlContinueExpressionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.6
	 * @generated
	 */
	private EClass sarlAssertExpressionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlActionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlConstructorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlBehaviorUnitEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlCapacityUsesEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlRequiredCapacityEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlClassEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlInterfaceEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlEnumerationEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlAnnotationTypeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlEnumLiteralEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlEventEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.9
	 * @generated
	 */
	private EClass sarlCastedExpressionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlSpaceEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlArtifactEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlAgentEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlCapacityEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlBehaviorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlSkillEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass sarlFormalParameterEClass = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see io.sarl.lang.sarl.SarlPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private SarlPackageImpl()
	{
		super(eNS_URI, SarlFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 *
	 * <p>This method is used to initialize {@link SarlPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static SarlPackage init()
	{
		if (isInited) return (SarlPackage)EPackage.Registry.INSTANCE.getEPackage(SarlPackage.eNS_URI);

		// Obtain or create and register package
		Object registeredSarlPackage = EPackage.Registry.INSTANCE.get(eNS_URI);
		SarlPackageImpl theSarlPackage = registeredSarlPackage instanceof SarlPackageImpl ? (SarlPackageImpl)registeredSarlPackage : new SarlPackageImpl();

		isInited = true;

		// Initialize simple dependencies
		XtendPackage.eINSTANCE.eClass();
		TypesPackage.eINSTANCE.eClass();
		XbasePackage.eINSTANCE.eClass();
		XtypePackage.eINSTANCE.eClass();
		XAnnotationsPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theSarlPackage.createPackageContents();

		// Initialize created meta-data
		theSarlPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theSarlPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(SarlPackage.eNS_URI, theSarlPackage);
		return theSarlPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlScript()
	{
		return sarlScriptEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlField()
	{
		return sarlFieldEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.5
	 * @generated
	 */
	@Override
	public EClass getSarlBreakExpression()
	{
		return sarlBreakExpressionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.7
	 * @generated
	 */
	@Override
	public EClass getSarlContinueExpression()
	{
		return sarlContinueExpressionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.6
	 * @generated
	 */
	@Override
	public EClass getSarlAssertExpression()
	{
		return sarlAssertExpressionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.6
	 * @generated
	 */
	@Override
	public EReference getSarlAssertExpression_Condition()
	{
		return (EReference)sarlAssertExpressionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.6
	 * @generated
	 */
	@Override
	public EAttribute getSarlAssertExpression_Message()
	{
		return (EAttribute)sarlAssertExpressionEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.6
	 * @generated
	 */
	@Override
	public EAttribute getSarlAssertExpression_IsStatic()
	{
		return (EAttribute)sarlAssertExpressionEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlAction()
	{
		return sarlActionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlAction_FiredEvents()
	{
		return (EReference)sarlActionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlConstructor()
	{
		return sarlConstructorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlBehaviorUnit()
	{
		return sarlBehaviorUnitEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlBehaviorUnit_Name()
	{
		return (EReference)sarlBehaviorUnitEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlBehaviorUnit_Guard()
	{
		return (EReference)sarlBehaviorUnitEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlBehaviorUnit_Expression()
	{
		return (EReference)sarlBehaviorUnitEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlCapacityUses()
	{
		return sarlCapacityUsesEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlCapacityUses_Capacities()
	{
		return (EReference)sarlCapacityUsesEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlRequiredCapacity()
	{
		return sarlRequiredCapacityEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlRequiredCapacity_Capacities()
	{
		return (EReference)sarlRequiredCapacityEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlClass()
	{
		return sarlClassEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlInterface()
	{
		return sarlInterfaceEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlEnumeration()
	{
		return sarlEnumerationEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlAnnotationType()
	{
		return sarlAnnotationTypeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlEnumLiteral()
	{
		return sarlEnumLiteralEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlEvent()
	{
		return sarlEventEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlEvent_Extends()
	{
		return (EReference)sarlEventEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.9
	 * @generated
	 */
	@Override
	public EClass getSarlCastedExpression()
	{
		return sarlCastedExpressionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.9
	 * @generated
	 */
	@Override
	public EReference getSarlCastedExpression_Feature()
	{
		return (EReference)sarlCastedExpressionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.9
	 * @generated
	 */
	@Override
	public EReference getSarlCastedExpression_Receiver()
	{
		return (EReference)sarlCastedExpressionEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @since 0.9
	 * @generated
	 */
	@Override
	public EReference getSarlCastedExpression_Argument()
	{
		return (EReference)sarlCastedExpressionEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlSpace()
	{
		return sarlSpaceEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlSpace_Extends()
	{
		return (EReference)sarlSpaceEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlArtifact()
	{
		return sarlArtifactEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlArtifact_Extends()
	{
		return (EReference)sarlArtifactEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlAgent()
	{
		return sarlAgentEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlAgent_Extends()
	{
		return (EReference)sarlAgentEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlCapacity()
	{
		return sarlCapacityEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlCapacity_Extends()
	{
		return (EReference)sarlCapacityEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlBehavior()
	{
		return sarlBehaviorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlBehavior_Extends()
	{
		return (EReference)sarlBehaviorEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlSkill()
	{
		return sarlSkillEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlSkill_Extends()
	{
		return (EReference)sarlSkillEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlSkill_Implements()
	{
		return (EReference)sarlSkillEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSarlFormalParameter()
	{
		return sarlFormalParameterEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSarlFormalParameter_DefaultValue()
	{
		return (EReference)sarlFormalParameterEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public SarlFactory getSarlFactory()
	{
		return (SarlFactory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents()
	{
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		sarlScriptEClass = createEClass(SARL_SCRIPT);

		sarlFieldEClass = createEClass(SARL_FIELD);

		sarlBreakExpressionEClass = createEClass(SARL_BREAK_EXPRESSION);

		sarlContinueExpressionEClass = createEClass(SARL_CONTINUE_EXPRESSION);

		sarlAssertExpressionEClass = createEClass(SARL_ASSERT_EXPRESSION);
		createEReference(sarlAssertExpressionEClass, SARL_ASSERT_EXPRESSION__CONDITION);
		createEAttribute(sarlAssertExpressionEClass, SARL_ASSERT_EXPRESSION__MESSAGE);
		createEAttribute(sarlAssertExpressionEClass, SARL_ASSERT_EXPRESSION__IS_STATIC);

		sarlActionEClass = createEClass(SARL_ACTION);
		createEReference(sarlActionEClass, SARL_ACTION__FIRED_EVENTS);

		sarlConstructorEClass = createEClass(SARL_CONSTRUCTOR);

		sarlBehaviorUnitEClass = createEClass(SARL_BEHAVIOR_UNIT);
		createEReference(sarlBehaviorUnitEClass, SARL_BEHAVIOR_UNIT__NAME);
		createEReference(sarlBehaviorUnitEClass, SARL_BEHAVIOR_UNIT__GUARD);
		createEReference(sarlBehaviorUnitEClass, SARL_BEHAVIOR_UNIT__EXPRESSION);

		sarlCapacityUsesEClass = createEClass(SARL_CAPACITY_USES);
		createEReference(sarlCapacityUsesEClass, SARL_CAPACITY_USES__CAPACITIES);

		sarlRequiredCapacityEClass = createEClass(SARL_REQUIRED_CAPACITY);
		createEReference(sarlRequiredCapacityEClass, SARL_REQUIRED_CAPACITY__CAPACITIES);

		sarlClassEClass = createEClass(SARL_CLASS);

		sarlInterfaceEClass = createEClass(SARL_INTERFACE);

		sarlEnumerationEClass = createEClass(SARL_ENUMERATION);

		sarlAnnotationTypeEClass = createEClass(SARL_ANNOTATION_TYPE);

		sarlEnumLiteralEClass = createEClass(SARL_ENUM_LITERAL);

		sarlEventEClass = createEClass(SARL_EVENT);
		createEReference(sarlEventEClass, SARL_EVENT__EXTENDS);

		sarlCastedExpressionEClass = createEClass(SARL_CASTED_EXPRESSION);
		createEReference(sarlCastedExpressionEClass, SARL_CASTED_EXPRESSION__FEATURE);
		createEReference(sarlCastedExpressionEClass, SARL_CASTED_EXPRESSION__RECEIVER);
		createEReference(sarlCastedExpressionEClass, SARL_CASTED_EXPRESSION__ARGUMENT);

		sarlSpaceEClass = createEClass(SARL_SPACE);
		createEReference(sarlSpaceEClass, SARL_SPACE__EXTENDS);

		sarlArtifactEClass = createEClass(SARL_ARTIFACT);
		createEReference(sarlArtifactEClass, SARL_ARTIFACT__EXTENDS);

		sarlAgentEClass = createEClass(SARL_AGENT);
		createEReference(sarlAgentEClass, SARL_AGENT__EXTENDS);

		sarlCapacityEClass = createEClass(SARL_CAPACITY);
		createEReference(sarlCapacityEClass, SARL_CAPACITY__EXTENDS);

		sarlBehaviorEClass = createEClass(SARL_BEHAVIOR);
		createEReference(sarlBehaviorEClass, SARL_BEHAVIOR__EXTENDS);

		sarlSkillEClass = createEClass(SARL_SKILL);
		createEReference(sarlSkillEClass, SARL_SKILL__EXTENDS);
		createEReference(sarlSkillEClass, SARL_SKILL__IMPLEMENTS);

		sarlFormalParameterEClass = createEClass(SARL_FORMAL_PARAMETER);
		createEReference(sarlFormalParameterEClass, SARL_FORMAL_PARAMETER__DEFAULT_VALUE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents()
	{
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		XtendPackage theXtendPackage = (XtendPackage)EPackage.Registry.INSTANCE.getEPackage(XtendPackage.eNS_URI);
		XbasePackage theXbasePackage = (XbasePackage)EPackage.Registry.INSTANCE.getEPackage(XbasePackage.eNS_URI);
		TypesPackage theTypesPackage = (TypesPackage)EPackage.Registry.INSTANCE.getEPackage(TypesPackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		sarlScriptEClass.getESuperTypes().add(theXtendPackage.getXtendFile());
		sarlFieldEClass.getESuperTypes().add(theXtendPackage.getXtendField());
		sarlBreakExpressionEClass.getESuperTypes().add(theXbasePackage.getXExpression());
		sarlContinueExpressionEClass.getESuperTypes().add(theXbasePackage.getXExpression());
		sarlAssertExpressionEClass.getESuperTypes().add(theXbasePackage.getXExpression());
		sarlActionEClass.getESuperTypes().add(theXtendPackage.getXtendFunction());
		sarlConstructorEClass.getESuperTypes().add(theXtendPackage.getXtendConstructor());
		sarlBehaviorUnitEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
		sarlCapacityUsesEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
		sarlRequiredCapacityEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
		sarlClassEClass.getESuperTypes().add(theXtendPackage.getXtendClass());
		sarlInterfaceEClass.getESuperTypes().add(theXtendPackage.getXtendInterface());
		sarlEnumerationEClass.getESuperTypes().add(theXtendPackage.getXtendEnum());
		sarlAnnotationTypeEClass.getESuperTypes().add(theXtendPackage.getXtendAnnotationType());
		sarlEnumLiteralEClass.getESuperTypes().add(theXtendPackage.getXtendEnumLiteral());
		sarlEventEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
		sarlCastedExpressionEClass.getESuperTypes().add(theXbasePackage.getXCastedExpression());
		sarlSpaceEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
		sarlArtifactEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
		sarlAgentEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
		sarlCapacityEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
		sarlBehaviorEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
		sarlSkillEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
		sarlFormalParameterEClass.getESuperTypes().add(theXtendPackage.getXtendParameter());

		// Initialize classes and features; add operations and parameters
		initEClass(sarlScriptEClass, SarlScript.class, "SarlScript", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlFieldEClass, SarlField.class, "SarlField", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlBreakExpressionEClass, SarlBreakExpression.class, "SarlBreakExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlContinueExpressionEClass, SarlContinueExpression.class, "SarlContinueExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlAssertExpressionEClass, SarlAssertExpression.class, "SarlAssertExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlAssertExpression_Condition(), theXbasePackage.getXExpression(), null, "condition", null, 0, 1, SarlAssertExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getSarlAssertExpression_Message(), ecorePackage.getEString(), "message", null, 0, 1, SarlAssertExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getSarlAssertExpression_IsStatic(), ecorePackage.getEBoolean(), "isStatic", null, 0, 1, SarlAssertExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(sarlActionEClass, SarlAction.class, "SarlAction", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlAction_FiredEvents(), theTypesPackage.getJvmTypeReference(), null, "firedEvents", null, 0, -1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(sarlConstructorEClass, SarlConstructor.class, "SarlConstructor", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlBehaviorUnitEClass, SarlBehaviorUnit.class, "SarlBehaviorUnit", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlBehaviorUnit_Name(), theTypesPackage.getJvmParameterizedTypeReference(), null, "name", null, 0, 1, SarlBehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getSarlBehaviorUnit_Guard(), theXbasePackage.getXExpression(), null, "guard", null, 0, 1, SarlBehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getSarlBehaviorUnit_Expression(), theXbasePackage.getXExpression(), null, "expression", null, 0, 1, SarlBehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(sarlCapacityUsesEClass, SarlCapacityUses.class, "SarlCapacityUses", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlCapacityUses_Capacities(), theTypesPackage.getJvmParameterizedTypeReference(), null, "capacities", null, 0, -1, SarlCapacityUses.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(sarlRequiredCapacityEClass, SarlRequiredCapacity.class, "SarlRequiredCapacity", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlRequiredCapacity_Capacities(), theTypesPackage.getJvmParameterizedTypeReference(), null, "capacities", null, 0, -1, SarlRequiredCapacity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(sarlClassEClass, SarlClass.class, "SarlClass", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlInterfaceEClass, SarlInterface.class, "SarlInterface", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlEnumerationEClass, SarlEnumeration.class, "SarlEnumeration", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlAnnotationTypeEClass, SarlAnnotationType.class, "SarlAnnotationType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlEnumLiteralEClass, SarlEnumLiteral.class, "SarlEnumLiteral", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(sarlEventEClass, SarlEvent.class, "SarlEvent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlEvent_Extends(), theTypesPackage.getJvmParameterizedTypeReference(), null, "extends", null, 0, 1, SarlEvent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(sarlEventEClass, ecorePackage.getEBoolean(), "isAbstract", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(sarlCastedExpressionEClass, SarlCastedExpression.class, "SarlCastedExpression", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlCastedExpression_Feature(), theTypesPackage.getJvmOperation(), null, "feature", null, 0, 1, SarlCastedExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getSarlCastedExpression_Receiver(), theXbasePackage.getXExpression(), null, "receiver", null, 0, 1, SarlCastedExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getSarlCastedExpression_Argument(), theXbasePackage.getXExpression(), null, "argument", null, 0, 1, SarlCastedExpression.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(sarlSpaceEClass, SarlSpace.class, "SarlSpace", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlSpace_Extends(), theTypesPackage.getJvmParameterizedTypeReference(), null, "extends", null, 0, 1, SarlSpace.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(sarlSpaceEClass, ecorePackage.getEBoolean(), "isAbstract", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(sarlArtifactEClass, SarlArtifact.class, "SarlArtifact", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlArtifact_Extends(), theTypesPackage.getJvmParameterizedTypeReference(), null, "extends", null, 0, 1, SarlArtifact.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(sarlArtifactEClass, ecorePackage.getEBoolean(), "isAbstract", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(sarlAgentEClass, SarlAgent.class, "SarlAgent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlAgent_Extends(), theTypesPackage.getJvmParameterizedTypeReference(), null, "extends", null, 0, 1, SarlAgent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(sarlAgentEClass, ecorePackage.getEBoolean(), "isAbstract", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(sarlAgentEClass, ecorePackage.getEBoolean(), "isStrictFloatingPoint", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(sarlCapacityEClass, SarlCapacity.class, "SarlCapacity", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlCapacity_Extends(), theTypesPackage.getJvmParameterizedTypeReference(), null, "extends", null, 0, -1, SarlCapacity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(sarlBehaviorEClass, SarlBehavior.class, "SarlBehavior", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlBehavior_Extends(), theTypesPackage.getJvmParameterizedTypeReference(), null, "extends", null, 0, 1, SarlBehavior.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(sarlBehaviorEClass, ecorePackage.getEBoolean(), "isAbstract", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(sarlBehaviorEClass, ecorePackage.getEBoolean(), "isStrictFloatingPoint", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(sarlSkillEClass, SarlSkill.class, "SarlSkill", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlSkill_Extends(), theTypesPackage.getJvmParameterizedTypeReference(), null, "extends", null, 0, 1, SarlSkill.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getSarlSkill_Implements(), theTypesPackage.getJvmParameterizedTypeReference(), null, "implements", null, 0, -1, SarlSkill.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(sarlSkillEClass, ecorePackage.getEBoolean(), "isAbstract", 0, 1, IS_UNIQUE, IS_ORDERED);

		addEOperation(sarlSkillEClass, ecorePackage.getEBoolean(), "isStrictFloatingPoint", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(sarlFormalParameterEClass, SarlFormalParameter.class, "SarlFormalParameter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSarlFormalParameter_DefaultValue(), theXbasePackage.getXExpression(), null, "defaultValue", null, 0, 1, SarlFormalParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Create resource
		createResource(eNS_URI);
	}

} //SarlPackageImpl
