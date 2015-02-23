/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlActionSignature;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlFactory;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.xtend.core.xtend.XtendPackage;

import org.eclipse.xtext.common.types.TypesPackage;

import org.eclipse.xtext.xbase.XbasePackage;

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
  private EClass sarlActionSignatureEClass = null;

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
  private EClass sarlConstructorEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass sarlEventEClass = null;

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
    SarlPackageImpl theSarlPackage = (SarlPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof SarlPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new SarlPackageImpl());

    isInited = true;

    // Initialize simple dependencies
    XtendPackage.eINSTANCE.eClass();

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
  public EClass getSarlScript()
  {
    return sarlScriptEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSarlScript_Name()
  {
    return (EAttribute)sarlScriptEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlScript_ImportSection()
  {
    return (EReference)sarlScriptEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlScript_Elements()
  {
    return (EReference)sarlScriptEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlActionSignature()
  {
    return sarlActionSignatureEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlActionSignature_TypeParameters()
  {
    return (EReference)sarlActionSignatureEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSarlActionSignature_Name()
  {
    return (EAttribute)sarlActionSignatureEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlActionSignature_Parameters()
  {
    return (EReference)sarlActionSignatureEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSarlActionSignature_Varargs()
  {
    return (EAttribute)sarlActionSignatureEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlActionSignature_ReturnType()
  {
    return (EReference)sarlActionSignatureEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlActionSignature_CreateExtensionInfo()
  {
    return (EReference)sarlActionSignatureEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlActionSignature_Exceptions()
  {
    return (EReference)sarlActionSignatureEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlActionSignature_FiredEvents()
  {
    return (EReference)sarlActionSignatureEClass.getEStructuralFeatures().get(7);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlAction()
  {
    return sarlActionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAction_TypeParameters()
  {
    return (EReference)sarlActionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSarlAction_Name()
  {
    return (EAttribute)sarlActionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAction_Parameters()
  {
    return (EReference)sarlActionEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSarlAction_Varargs()
  {
    return (EAttribute)sarlActionEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAction_ReturnType()
  {
    return (EReference)sarlActionEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAction_CreateExtensionInfo()
  {
    return (EReference)sarlActionEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAction_Exceptions()
  {
    return (EReference)sarlActionEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAction_FiredEvents()
  {
    return (EReference)sarlActionEClass.getEStructuralFeatures().get(7);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAction_Expression()
  {
    return (EReference)sarlActionEClass.getEStructuralFeatures().get(8);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlBehaviorUnit()
  {
    return sarlBehaviorUnitEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlBehaviorUnit_Name()
  {
    return (EReference)sarlBehaviorUnitEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlBehaviorUnit_Guard()
  {
    return (EReference)sarlBehaviorUnitEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlBehaviorUnit_Body()
  {
    return (EReference)sarlBehaviorUnitEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlCapacityUses()
  {
    return sarlCapacityUsesEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlCapacityUses_CapacitiesUsed()
  {
    return (EReference)sarlCapacityUsesEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlRequiredCapacity()
  {
    return sarlRequiredCapacityEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlRequiredCapacity_RequiredCapacities()
  {
    return (EReference)sarlRequiredCapacityEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlConstructor()
  {
    return sarlConstructorEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlConstructor_TypeParameters()
  {
    return (EReference)sarlConstructorEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlConstructor_Parameters()
  {
    return (EReference)sarlConstructorEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getSarlConstructor_Varargs()
  {
    return (EAttribute)sarlConstructorEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlConstructor_Exceptions()
  {
    return (EReference)sarlConstructorEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlConstructor_Expression()
  {
    return (EReference)sarlConstructorEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlEvent()
  {
    return sarlEventEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlEvent_SuperTypes()
  {
    return (EReference)sarlEventEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlEvent_Features()
  {
    return (EReference)sarlEventEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlAgent()
  {
    return sarlAgentEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAgent_SuperTypes()
  {
    return (EReference)sarlAgentEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlAgent_Features()
  {
    return (EReference)sarlAgentEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlCapacity()
  {
    return sarlCapacityEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlCapacity_SuperTypes()
  {
    return (EReference)sarlCapacityEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlCapacity_Features()
  {
    return (EReference)sarlCapacityEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlBehavior()
  {
    return sarlBehaviorEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlBehavior_SuperTypes()
  {
    return (EReference)sarlBehaviorEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlBehavior_Features()
  {
    return (EReference)sarlBehaviorEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlSkill()
  {
    return sarlSkillEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlSkill_SuperTypes()
  {
    return (EReference)sarlSkillEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlSkill_ImplementedTypes()
  {
    return (EReference)sarlSkillEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlSkill_Features()
  {
    return (EReference)sarlSkillEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSarlFormalParameter()
  {
    return sarlFormalParameterEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSarlFormalParameter_DefaultValue()
  {
    return (EReference)sarlFormalParameterEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
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
    createEAttribute(sarlScriptEClass, SARL_SCRIPT__NAME);
    createEReference(sarlScriptEClass, SARL_SCRIPT__IMPORT_SECTION);
    createEReference(sarlScriptEClass, SARL_SCRIPT__ELEMENTS);

    sarlActionSignatureEClass = createEClass(SARL_ACTION_SIGNATURE);
    createEReference(sarlActionSignatureEClass, SARL_ACTION_SIGNATURE__TYPE_PARAMETERS);
    createEAttribute(sarlActionSignatureEClass, SARL_ACTION_SIGNATURE__NAME);
    createEReference(sarlActionSignatureEClass, SARL_ACTION_SIGNATURE__PARAMETERS);
    createEAttribute(sarlActionSignatureEClass, SARL_ACTION_SIGNATURE__VARARGS);
    createEReference(sarlActionSignatureEClass, SARL_ACTION_SIGNATURE__RETURN_TYPE);
    createEReference(sarlActionSignatureEClass, SARL_ACTION_SIGNATURE__CREATE_EXTENSION_INFO);
    createEReference(sarlActionSignatureEClass, SARL_ACTION_SIGNATURE__EXCEPTIONS);
    createEReference(sarlActionSignatureEClass, SARL_ACTION_SIGNATURE__FIRED_EVENTS);

    sarlActionEClass = createEClass(SARL_ACTION);
    createEReference(sarlActionEClass, SARL_ACTION__TYPE_PARAMETERS);
    createEAttribute(sarlActionEClass, SARL_ACTION__NAME);
    createEReference(sarlActionEClass, SARL_ACTION__PARAMETERS);
    createEAttribute(sarlActionEClass, SARL_ACTION__VARARGS);
    createEReference(sarlActionEClass, SARL_ACTION__RETURN_TYPE);
    createEReference(sarlActionEClass, SARL_ACTION__CREATE_EXTENSION_INFO);
    createEReference(sarlActionEClass, SARL_ACTION__EXCEPTIONS);
    createEReference(sarlActionEClass, SARL_ACTION__FIRED_EVENTS);
    createEReference(sarlActionEClass, SARL_ACTION__EXPRESSION);

    sarlBehaviorUnitEClass = createEClass(SARL_BEHAVIOR_UNIT);
    createEReference(sarlBehaviorUnitEClass, SARL_BEHAVIOR_UNIT__NAME);
    createEReference(sarlBehaviorUnitEClass, SARL_BEHAVIOR_UNIT__GUARD);
    createEReference(sarlBehaviorUnitEClass, SARL_BEHAVIOR_UNIT__BODY);

    sarlCapacityUsesEClass = createEClass(SARL_CAPACITY_USES);
    createEReference(sarlCapacityUsesEClass, SARL_CAPACITY_USES__CAPACITIES_USED);

    sarlRequiredCapacityEClass = createEClass(SARL_REQUIRED_CAPACITY);
    createEReference(sarlRequiredCapacityEClass, SARL_REQUIRED_CAPACITY__REQUIRED_CAPACITIES);

    sarlConstructorEClass = createEClass(SARL_CONSTRUCTOR);
    createEReference(sarlConstructorEClass, SARL_CONSTRUCTOR__TYPE_PARAMETERS);
    createEReference(sarlConstructorEClass, SARL_CONSTRUCTOR__PARAMETERS);
    createEAttribute(sarlConstructorEClass, SARL_CONSTRUCTOR__VARARGS);
    createEReference(sarlConstructorEClass, SARL_CONSTRUCTOR__EXCEPTIONS);
    createEReference(sarlConstructorEClass, SARL_CONSTRUCTOR__EXPRESSION);

    sarlEventEClass = createEClass(SARL_EVENT);
    createEReference(sarlEventEClass, SARL_EVENT__SUPER_TYPES);
    createEReference(sarlEventEClass, SARL_EVENT__FEATURES);

    sarlAgentEClass = createEClass(SARL_AGENT);
    createEReference(sarlAgentEClass, SARL_AGENT__SUPER_TYPES);
    createEReference(sarlAgentEClass, SARL_AGENT__FEATURES);

    sarlCapacityEClass = createEClass(SARL_CAPACITY);
    createEReference(sarlCapacityEClass, SARL_CAPACITY__SUPER_TYPES);
    createEReference(sarlCapacityEClass, SARL_CAPACITY__FEATURES);

    sarlBehaviorEClass = createEClass(SARL_BEHAVIOR);
    createEReference(sarlBehaviorEClass, SARL_BEHAVIOR__SUPER_TYPES);
    createEReference(sarlBehaviorEClass, SARL_BEHAVIOR__FEATURES);

    sarlSkillEClass = createEClass(SARL_SKILL);
    createEReference(sarlSkillEClass, SARL_SKILL__SUPER_TYPES);
    createEReference(sarlSkillEClass, SARL_SKILL__IMPLEMENTED_TYPES);
    createEReference(sarlSkillEClass, SARL_SKILL__FEATURES);

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
    XtypePackage theXtypePackage = (XtypePackage)EPackage.Registry.INSTANCE.getEPackage(XtypePackage.eNS_URI);
    XtendPackage theXtendPackage = (XtendPackage)EPackage.Registry.INSTANCE.getEPackage(XtendPackage.eNS_URI);
    TypesPackage theTypesPackage = (TypesPackage)EPackage.Registry.INSTANCE.getEPackage(TypesPackage.eNS_URI);
    XbasePackage theXbasePackage = (XbasePackage)EPackage.Registry.INSTANCE.getEPackage(XbasePackage.eNS_URI);

    // Create type parameters

    // Set bounds for type parameters

    // Add supertypes to classes
    sarlActionSignatureEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
    sarlActionEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
    sarlBehaviorUnitEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
    sarlCapacityUsesEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
    sarlRequiredCapacityEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
    sarlConstructorEClass.getESuperTypes().add(theXtendPackage.getXtendMember());
    sarlEventEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
    sarlAgentEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
    sarlCapacityEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
    sarlBehaviorEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
    sarlSkillEClass.getESuperTypes().add(theXtendPackage.getXtendTypeDeclaration());
    sarlFormalParameterEClass.getESuperTypes().add(theXtendPackage.getXtendParameter());

    // Initialize classes and features; add operations and parameters
    initEClass(sarlScriptEClass, SarlScript.class, "SarlScript", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getSarlScript_Name(), ecorePackage.getEString(), "name", null, 0, 1, SarlScript.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlScript_ImportSection(), theXtypePackage.getXImportSection(), null, "importSection", null, 0, 1, SarlScript.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlScript_Elements(), theXtendPackage.getXtendTypeDeclaration(), null, "elements", null, 0, -1, SarlScript.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlActionSignatureEClass, SarlActionSignature.class, "SarlActionSignature", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlActionSignature_TypeParameters(), theTypesPackage.getJvmTypeParameter(), null, "typeParameters", null, 0, -1, SarlActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getSarlActionSignature_Name(), ecorePackage.getEString(), "name", null, 0, 1, SarlActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlActionSignature_Parameters(), theXtendPackage.getXtendParameter(), null, "parameters", null, 0, -1, SarlActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getSarlActionSignature_Varargs(), ecorePackage.getEBoolean(), "varargs", null, 0, 1, SarlActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlActionSignature_ReturnType(), theTypesPackage.getJvmTypeReference(), null, "returnType", null, 0, 1, SarlActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlActionSignature_CreateExtensionInfo(), theXtendPackage.getCreateExtensionInfo(), null, "createExtensionInfo", null, 0, 1, SarlActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlActionSignature_Exceptions(), theTypesPackage.getJvmTypeReference(), null, "exceptions", null, 0, -1, SarlActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlActionSignature_FiredEvents(), theTypesPackage.getJvmTypeReference(), null, "firedEvents", null, 0, -1, SarlActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlActionEClass, SarlAction.class, "SarlAction", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlAction_TypeParameters(), theTypesPackage.getJvmTypeParameter(), null, "typeParameters", null, 0, -1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getSarlAction_Name(), ecorePackage.getEString(), "name", null, 0, 1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlAction_Parameters(), theXtendPackage.getXtendParameter(), null, "parameters", null, 0, -1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getSarlAction_Varargs(), ecorePackage.getEBoolean(), "varargs", null, 0, 1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlAction_ReturnType(), theTypesPackage.getJvmTypeReference(), null, "returnType", null, 0, 1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlAction_CreateExtensionInfo(), theXtendPackage.getCreateExtensionInfo(), null, "createExtensionInfo", null, 0, 1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlAction_Exceptions(), theTypesPackage.getJvmTypeReference(), null, "exceptions", null, 0, -1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlAction_FiredEvents(), theTypesPackage.getJvmTypeReference(), null, "firedEvents", null, 0, -1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlAction_Expression(), theXbasePackage.getXExpression(), null, "expression", null, 0, 1, SarlAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlBehaviorUnitEClass, SarlBehaviorUnit.class, "SarlBehaviorUnit", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlBehaviorUnit_Name(), theTypesPackage.getJvmParameterizedTypeReference(), null, "name", null, 0, 1, SarlBehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlBehaviorUnit_Guard(), theXbasePackage.getXExpression(), null, "guard", null, 0, 1, SarlBehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlBehaviorUnit_Body(), theXbasePackage.getXExpression(), null, "body", null, 0, 1, SarlBehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlCapacityUsesEClass, SarlCapacityUses.class, "SarlCapacityUses", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlCapacityUses_CapacitiesUsed(), theTypesPackage.getJvmParameterizedTypeReference(), null, "capacitiesUsed", null, 0, -1, SarlCapacityUses.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlRequiredCapacityEClass, SarlRequiredCapacity.class, "SarlRequiredCapacity", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlRequiredCapacity_RequiredCapacities(), theTypesPackage.getJvmParameterizedTypeReference(), null, "requiredCapacities", null, 0, -1, SarlRequiredCapacity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlConstructorEClass, SarlConstructor.class, "SarlConstructor", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlConstructor_TypeParameters(), theTypesPackage.getJvmTypeParameter(), null, "typeParameters", null, 0, -1, SarlConstructor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlConstructor_Parameters(), theXtendPackage.getXtendParameter(), null, "parameters", null, 0, -1, SarlConstructor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getSarlConstructor_Varargs(), ecorePackage.getEBoolean(), "varargs", null, 0, 1, SarlConstructor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlConstructor_Exceptions(), theTypesPackage.getJvmTypeReference(), null, "exceptions", null, 0, -1, SarlConstructor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlConstructor_Expression(), theXbasePackage.getXExpression(), null, "expression", null, 0, 1, SarlConstructor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlEventEClass, SarlEvent.class, "SarlEvent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlEvent_SuperTypes(), theTypesPackage.getJvmParameterizedTypeReference(), null, "superTypes", null, 0, -1, SarlEvent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlEvent_Features(), theXtendPackage.getXtendMember(), null, "features", null, 0, -1, SarlEvent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlAgentEClass, SarlAgent.class, "SarlAgent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlAgent_SuperTypes(), theTypesPackage.getJvmParameterizedTypeReference(), null, "superTypes", null, 0, -1, SarlAgent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlAgent_Features(), theXtendPackage.getXtendMember(), null, "features", null, 0, -1, SarlAgent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlCapacityEClass, SarlCapacity.class, "SarlCapacity", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlCapacity_SuperTypes(), theTypesPackage.getJvmParameterizedTypeReference(), null, "superTypes", null, 0, -1, SarlCapacity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlCapacity_Features(), theXtendPackage.getXtendMember(), null, "features", null, 0, -1, SarlCapacity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlBehaviorEClass, SarlBehavior.class, "SarlBehavior", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlBehavior_SuperTypes(), theTypesPackage.getJvmParameterizedTypeReference(), null, "superTypes", null, 0, -1, SarlBehavior.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlBehavior_Features(), theXtendPackage.getXtendMember(), null, "features", null, 0, -1, SarlBehavior.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlSkillEClass, SarlSkill.class, "SarlSkill", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlSkill_SuperTypes(), theTypesPackage.getJvmParameterizedTypeReference(), null, "superTypes", null, 0, -1, SarlSkill.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlSkill_ImplementedTypes(), theTypesPackage.getJvmParameterizedTypeReference(), null, "implementedTypes", null, 0, -1, SarlSkill.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlSkill_Features(), theXtendPackage.getXtendMember(), null, "features", null, 0, -1, SarlSkill.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sarlFormalParameterEClass, SarlFormalParameter.class, "SarlFormalParameter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSarlFormalParameter_DefaultValue(), theXbasePackage.getXExpression(), null, "defaultValue", null, 0, 1, SarlFormalParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    // Create resource
    createResource(eNS_URI);
  }

} //SarlPackageImpl
