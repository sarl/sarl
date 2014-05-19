/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.AgentFeature;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.BehaviorFeature;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.EventFeature;
import io.sarl.lang.sarl.Feature;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.FormalParameter;
import io.sarl.lang.sarl.ImplementingElement;
import io.sarl.lang.sarl.InheritingElement;
import io.sarl.lang.sarl.NamedElement;
import io.sarl.lang.sarl.RequiredCapacity;
import io.sarl.lang.sarl.SarlFactory;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.sarl.SkillFeature;
import io.sarl.lang.sarl.TopElement;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

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
  private EClass topElementEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass namedElementEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass featureEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass featureContainerEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass inheritingElementEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass implementingElementEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass eventFeatureEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass agentFeatureEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass behaviorFeatureEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass skillFeatureEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass formalParameterEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass eventEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass capacityEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass agentEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass behaviorEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass skillEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass attributeEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass capacityUsesEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass behaviorUnitEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass requiredCapacityEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass constructorEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass actionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass actionSignatureEClass = null;

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
    XbasePackage.eINSTANCE.eClass();
    XtypePackage.eINSTANCE.eClass();

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
  public EClass getTopElement()
  {
    return topElementEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getNamedElement()
  {
    return namedElementEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getNamedElement_Name()
  {
    return (EAttribute)namedElementEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getFeature()
  {
    return featureEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getFeatureContainer()
  {
    return featureContainerEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getFeatureContainer_Features()
  {
    return (EReference)featureContainerEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getInheritingElement()
  {
    return inheritingElementEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getInheritingElement_SuperTypes()
  {
    return (EReference)inheritingElementEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getImplementingElement()
  {
    return implementingElementEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getImplementingElement_ImplementedTypes()
  {
    return (EReference)implementingElementEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEventFeature()
  {
    return eventFeatureEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAgentFeature()
  {
    return agentFeatureEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getBehaviorFeature()
  {
    return behaviorFeatureEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSkillFeature()
  {
    return skillFeatureEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getFormalParameter()
  {
    return formalParameterEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormalParameter_Name()
  {
    return (EAttribute)formalParameterEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getFormalParameter_ParameterType()
  {
    return (EReference)formalParameterEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getFormalParameter_DefaultValue()
  {
    return (EReference)formalParameterEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEvent()
  {
    return eventEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getCapacity()
  {
    return capacityEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getCapacity_Actions()
  {
    return (EReference)capacityEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAgent()
  {
    return agentEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getBehavior()
  {
    return behaviorEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSkill()
  {
    return skillEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAttribute()
  {
    return attributeEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAttribute_Writeable()
  {
    return (EAttribute)attributeEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAttribute_Name()
  {
    return (EAttribute)attributeEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAttribute_Type()
  {
    return (EReference)attributeEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAttribute_InitialValue()
  {
    return (EReference)attributeEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getCapacityUses()
  {
    return capacityUsesEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getCapacityUses_CapacitiesUsed()
  {
    return (EReference)capacityUsesEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getBehaviorUnit()
  {
    return behaviorUnitEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getBehaviorUnit_Event()
  {
    return (EReference)behaviorUnitEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getBehaviorUnit_Guard()
  {
    return (EReference)behaviorUnitEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getBehaviorUnit_Body()
  {
    return (EReference)behaviorUnitEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getRequiredCapacity()
  {
    return requiredCapacityEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getRequiredCapacity_RequiredCapacities()
  {
    return (EReference)requiredCapacityEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getConstructor()
  {
    return constructorEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getConstructor_Params()
  {
    return (EReference)constructorEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getConstructor_Varargs()
  {
    return (EAttribute)constructorEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getConstructor_Body()
  {
    return (EReference)constructorEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAction()
  {
    return actionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAction_Signature()
  {
    return (EReference)actionEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getAction_Body()
  {
    return (EReference)actionEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getActionSignature()
  {
    return actionSignatureEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getActionSignature_Name()
  {
    return (EAttribute)actionSignatureEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getActionSignature_Params()
  {
    return (EReference)actionSignatureEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getActionSignature_Varargs()
  {
    return (EAttribute)actionSignatureEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getActionSignature_Type()
  {
    return (EReference)actionSignatureEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getActionSignature_FiredEvents()
  {
    return (EReference)actionSignatureEClass.getEStructuralFeatures().get(4);
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

    topElementEClass = createEClass(TOP_ELEMENT);

    namedElementEClass = createEClass(NAMED_ELEMENT);
    createEAttribute(namedElementEClass, NAMED_ELEMENT__NAME);

    featureEClass = createEClass(FEATURE);

    featureContainerEClass = createEClass(FEATURE_CONTAINER);
    createEReference(featureContainerEClass, FEATURE_CONTAINER__FEATURES);

    inheritingElementEClass = createEClass(INHERITING_ELEMENT);
    createEReference(inheritingElementEClass, INHERITING_ELEMENT__SUPER_TYPES);

    implementingElementEClass = createEClass(IMPLEMENTING_ELEMENT);
    createEReference(implementingElementEClass, IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES);

    eventFeatureEClass = createEClass(EVENT_FEATURE);

    agentFeatureEClass = createEClass(AGENT_FEATURE);

    behaviorFeatureEClass = createEClass(BEHAVIOR_FEATURE);

    skillFeatureEClass = createEClass(SKILL_FEATURE);

    formalParameterEClass = createEClass(FORMAL_PARAMETER);
    createEAttribute(formalParameterEClass, FORMAL_PARAMETER__NAME);
    createEReference(formalParameterEClass, FORMAL_PARAMETER__PARAMETER_TYPE);
    createEReference(formalParameterEClass, FORMAL_PARAMETER__DEFAULT_VALUE);

    eventEClass = createEClass(EVENT);

    capacityEClass = createEClass(CAPACITY);
    createEReference(capacityEClass, CAPACITY__ACTIONS);

    agentEClass = createEClass(AGENT);

    behaviorEClass = createEClass(BEHAVIOR);

    skillEClass = createEClass(SKILL);

    attributeEClass = createEClass(ATTRIBUTE);
    createEAttribute(attributeEClass, ATTRIBUTE__WRITEABLE);
    createEAttribute(attributeEClass, ATTRIBUTE__NAME);
    createEReference(attributeEClass, ATTRIBUTE__TYPE);
    createEReference(attributeEClass, ATTRIBUTE__INITIAL_VALUE);

    capacityUsesEClass = createEClass(CAPACITY_USES);
    createEReference(capacityUsesEClass, CAPACITY_USES__CAPACITIES_USED);

    behaviorUnitEClass = createEClass(BEHAVIOR_UNIT);
    createEReference(behaviorUnitEClass, BEHAVIOR_UNIT__EVENT);
    createEReference(behaviorUnitEClass, BEHAVIOR_UNIT__GUARD);
    createEReference(behaviorUnitEClass, BEHAVIOR_UNIT__BODY);

    requiredCapacityEClass = createEClass(REQUIRED_CAPACITY);
    createEReference(requiredCapacityEClass, REQUIRED_CAPACITY__REQUIRED_CAPACITIES);

    constructorEClass = createEClass(CONSTRUCTOR);
    createEReference(constructorEClass, CONSTRUCTOR__PARAMS);
    createEAttribute(constructorEClass, CONSTRUCTOR__VARARGS);
    createEReference(constructorEClass, CONSTRUCTOR__BODY);

    actionEClass = createEClass(ACTION);
    createEReference(actionEClass, ACTION__SIGNATURE);
    createEReference(actionEClass, ACTION__BODY);

    actionSignatureEClass = createEClass(ACTION_SIGNATURE);
    createEAttribute(actionSignatureEClass, ACTION_SIGNATURE__NAME);
    createEReference(actionSignatureEClass, ACTION_SIGNATURE__PARAMS);
    createEAttribute(actionSignatureEClass, ACTION_SIGNATURE__VARARGS);
    createEReference(actionSignatureEClass, ACTION_SIGNATURE__TYPE);
    createEReference(actionSignatureEClass, ACTION_SIGNATURE__FIRED_EVENTS);
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
    TypesPackage theTypesPackage = (TypesPackage)EPackage.Registry.INSTANCE.getEPackage(TypesPackage.eNS_URI);
    XbasePackage theXbasePackage = (XbasePackage)EPackage.Registry.INSTANCE.getEPackage(XbasePackage.eNS_URI);

    // Create type parameters

    // Set bounds for type parameters

    // Add supertypes to classes
    featureEClass.getESuperTypes().add(this.getEventFeature());
    featureEClass.getESuperTypes().add(this.getAgentFeature());
    featureEClass.getESuperTypes().add(this.getBehaviorFeature());
    featureEClass.getESuperTypes().add(this.getSkillFeature());
    featureContainerEClass.getESuperTypes().add(this.getNamedElement());
    inheritingElementEClass.getESuperTypes().add(this.getTopElement());
    inheritingElementEClass.getESuperTypes().add(this.getFeatureContainer());
    implementingElementEClass.getESuperTypes().add(this.getTopElement());
    implementingElementEClass.getESuperTypes().add(this.getInheritingElement());
    eventEClass.getESuperTypes().add(this.getInheritingElement());
    capacityEClass.getESuperTypes().add(this.getInheritingElement());
    agentEClass.getESuperTypes().add(this.getInheritingElement());
    behaviorEClass.getESuperTypes().add(this.getInheritingElement());
    skillEClass.getESuperTypes().add(this.getImplementingElement());
    attributeEClass.getESuperTypes().add(this.getFeature());
    capacityUsesEClass.getESuperTypes().add(this.getFeature());
    behaviorUnitEClass.getESuperTypes().add(this.getFeature());
    requiredCapacityEClass.getESuperTypes().add(this.getFeature());
    constructorEClass.getESuperTypes().add(this.getFeature());
    actionEClass.getESuperTypes().add(this.getFeature());
    actionSignatureEClass.getESuperTypes().add(this.getFeature());

    // Initialize classes and features; add operations and parameters
    initEClass(sarlScriptEClass, SarlScript.class, "SarlScript", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getSarlScript_Name(), ecorePackage.getEString(), "name", null, 0, 1, SarlScript.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlScript_ImportSection(), theXtypePackage.getXImportSection(), null, "importSection", null, 0, 1, SarlScript.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getSarlScript_Elements(), this.getTopElement(), null, "elements", null, 0, -1, SarlScript.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(topElementEClass, TopElement.class, "TopElement", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(namedElementEClass, NamedElement.class, "NamedElement", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getNamedElement_Name(), ecorePackage.getEString(), "name", null, 0, 1, NamedElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(featureEClass, Feature.class, "Feature", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(featureContainerEClass, FeatureContainer.class, "FeatureContainer", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getFeatureContainer_Features(), ecorePackage.getEObject(), null, "features", null, 0, -1, FeatureContainer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(inheritingElementEClass, InheritingElement.class, "InheritingElement", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getInheritingElement_SuperTypes(), this.getInheritingElement(), null, "superTypes", null, 0, -1, InheritingElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(implementingElementEClass, ImplementingElement.class, "ImplementingElement", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getImplementingElement_ImplementedTypes(), this.getInheritingElement(), null, "implementedTypes", null, 0, -1, ImplementingElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(eventFeatureEClass, EventFeature.class, "EventFeature", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(agentFeatureEClass, AgentFeature.class, "AgentFeature", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(behaviorFeatureEClass, BehaviorFeature.class, "BehaviorFeature", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(skillFeatureEClass, SkillFeature.class, "SkillFeature", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(formalParameterEClass, FormalParameter.class, "FormalParameter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getFormalParameter_Name(), ecorePackage.getEString(), "name", null, 0, 1, FormalParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getFormalParameter_ParameterType(), theTypesPackage.getJvmTypeReference(), null, "parameterType", null, 0, 1, FormalParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getFormalParameter_DefaultValue(), theXbasePackage.getXExpression(), null, "defaultValue", null, 0, 1, FormalParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(eventEClass, Event.class, "Event", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(capacityEClass, Capacity.class, "Capacity", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getCapacity_Actions(), this.getFeature(), null, "actions", null, 0, -1, Capacity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(agentEClass, Agent.class, "Agent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(behaviorEClass, Behavior.class, "Behavior", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(skillEClass, Skill.class, "Skill", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(attributeEClass, Attribute.class, "Attribute", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getAttribute_Writeable(), ecorePackage.getEBoolean(), "writeable", null, 0, 1, Attribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getAttribute_Name(), ecorePackage.getEString(), "name", null, 0, 1, Attribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAttribute_Type(), theTypesPackage.getJvmTypeReference(), null, "type", null, 0, 1, Attribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAttribute_InitialValue(), theXbasePackage.getXExpression(), null, "initialValue", null, 0, 1, Attribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(capacityUsesEClass, CapacityUses.class, "CapacityUses", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getCapacityUses_CapacitiesUsed(), this.getCapacity(), null, "capacitiesUsed", null, 0, -1, CapacityUses.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(behaviorUnitEClass, BehaviorUnit.class, "BehaviorUnit", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getBehaviorUnit_Event(), this.getEvent(), null, "event", null, 0, 1, BehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getBehaviorUnit_Guard(), theXbasePackage.getXExpression(), null, "guard", null, 0, 1, BehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getBehaviorUnit_Body(), theXbasePackage.getXExpression(), null, "body", null, 0, 1, BehaviorUnit.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(requiredCapacityEClass, RequiredCapacity.class, "RequiredCapacity", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getRequiredCapacity_RequiredCapacities(), this.getCapacity(), null, "requiredCapacities", null, 0, -1, RequiredCapacity.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(constructorEClass, Constructor.class, "Constructor", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getConstructor_Params(), this.getFormalParameter(), null, "params", null, 0, -1, Constructor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getConstructor_Varargs(), ecorePackage.getEBoolean(), "varargs", null, 0, 1, Constructor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getConstructor_Body(), theXbasePackage.getXExpression(), null, "body", null, 0, 1, Constructor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(actionEClass, Action.class, "Action", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getAction_Signature(), this.getFeature(), null, "signature", null, 0, 1, Action.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getAction_Body(), theXbasePackage.getXExpression(), null, "body", null, 0, 1, Action.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(actionSignatureEClass, ActionSignature.class, "ActionSignature", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getActionSignature_Name(), ecorePackage.getEString(), "name", null, 0, 1, ActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getActionSignature_Params(), this.getFormalParameter(), null, "params", null, 0, -1, ActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getActionSignature_Varargs(), ecorePackage.getEBoolean(), "varargs", null, 0, 1, ActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getActionSignature_Type(), theTypesPackage.getJvmTypeReference(), null, "type", null, 0, 1, ActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getActionSignature_FiredEvents(), this.getEvent(), null, "firedEvents", null, 0, -1, ActionSignature.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, !IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    // Create resource
    createResource(eNS_URI);
  }

} //SarlPackageImpl
