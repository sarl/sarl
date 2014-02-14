/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see io.sarl.lang.sarl.SarlFactory
 * @model kind="package"
 * @generated
 */
public interface SarlPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "sarl";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.sarl.io/lang/SARL";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "sarl";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  SarlPackage eINSTANCE = io.sarl.lang.sarl.impl.SarlPackageImpl.init();

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ModelImpl <em>Model</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ModelImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getModel()
   * @generated
   */
  int MODEL = 0;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__NAME = 0;

  /**
   * The feature id for the '<em><b>Import Section</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__IMPORT_SECTION = 1;

  /**
   * The feature id for the '<em><b>Elements</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL__ELEMENTS = 2;

  /**
   * The number of structural features of the '<em>Model</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int MODEL_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.AbstractElementImpl <em>Abstract Element</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.AbstractElementImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAbstractElement()
   * @generated
   */
  int ABSTRACT_ELEMENT = 1;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ABSTRACT_ELEMENT__NAME = 0;

  /**
   * The number of structural features of the '<em>Abstract Element</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ABSTRACT_ELEMENT_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.EventImpl <em>Event</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.EventImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getEvent()
   * @generated
   */
  int EVENT = 2;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT__NAME = ABSTRACT_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Super Type</b></em>' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT__SUPER_TYPE = ABSTRACT_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT__FEATURES = ABSTRACT_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Event</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_FEATURE_COUNT = ABSTRACT_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.CapacityImpl <em>Capacity</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.CapacityImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getCapacity()
   * @generated
   */
  int CAPACITY = 3;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY__NAME = ABSTRACT_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Super Type</b></em>' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY__SUPER_TYPE = ABSTRACT_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Actions</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY__ACTIONS = ABSTRACT_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Capacity</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY_FEATURE_COUNT = ABSTRACT_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.AgentImpl <em>Agent</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.AgentImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAgent()
   * @generated
   */
  int AGENT = 4;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT__NAME = ABSTRACT_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Super Type</b></em>' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT__SUPER_TYPE = ABSTRACT_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT__FEATURES = ABSTRACT_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Agent</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT_FEATURE_COUNT = ABSTRACT_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.BehaviorImpl <em>Behavior</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.BehaviorImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehavior()
   * @generated
   */
  int BEHAVIOR = 5;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR__NAME = ABSTRACT_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Super Type</b></em>' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR__SUPER_TYPE = ABSTRACT_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR__FEATURES = ABSTRACT_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Behavior</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_FEATURE_COUNT = ABSTRACT_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.EventFeatureImpl <em>Event Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.EventFeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getEventFeature()
   * @generated
   */
  int EVENT_FEATURE = 6;

  /**
   * The number of structural features of the '<em>Event Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_FEATURE_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.AgentFeatureImpl <em>Agent Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.AgentFeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAgentFeature()
   * @generated
   */
  int AGENT_FEATURE = 7;

  /**
   * The number of structural features of the '<em>Agent Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT_FEATURE_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.BehaviorFeatureImpl <em>Behavior Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.BehaviorFeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehaviorFeature()
   * @generated
   */
  int BEHAVIOR_FEATURE = 8;

  /**
   * The number of structural features of the '<em>Behavior Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_FEATURE_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.AttributeImpl <em>Attribute</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.AttributeImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAttribute()
   * @generated
   */
  int ATTRIBUTE = 9;

  /**
   * The feature id for the '<em><b>Writeable</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE__WRITEABLE = EVENT_FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE__NAME = EVENT_FEATURE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE__TYPE = EVENT_FEATURE_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Initial Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE__INITIAL_VALUE = EVENT_FEATURE_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Attribute</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE_FEATURE_COUNT = EVENT_FEATURE_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.CapacityUsesImpl <em>Capacity Uses</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.CapacityUsesImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getCapacityUses()
   * @generated
   */
  int CAPACITY_USES = 10;

  /**
   * The feature id for the '<em><b>Capacities Used</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY_USES__CAPACITIES_USED = AGENT_FEATURE_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Capacity Uses</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY_USES_FEATURE_COUNT = AGENT_FEATURE_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.BehaviorUnitImpl <em>Behavior Unit</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.BehaviorUnitImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehaviorUnit()
   * @generated
   */
  int BEHAVIOR_UNIT = 11;

  /**
   * The feature id for the '<em><b>Event</b></em>' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_UNIT__EVENT = AGENT_FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Guard</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_UNIT__GUARD = AGENT_FEATURE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Body</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_UNIT__BODY = AGENT_FEATURE_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Behavior Unit</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_UNIT_FEATURE_COUNT = AGENT_FEATURE_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ActionImpl <em>Action</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ActionImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAction()
   * @generated
   */
  int ACTION = 12;

  /**
   * The feature id for the '<em><b>Signature</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION__SIGNATURE = AGENT_FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Body</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION__BODY = AGENT_FEATURE_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Action</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_FEATURE_COUNT = AGENT_FEATURE_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ActionSignatureImpl <em>Action Signature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ActionSignatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getActionSignature()
   * @generated
   */
  int ACTION_SIGNATURE = 13;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__NAME = 0;

  /**
   * The feature id for the '<em><b>Params</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__PARAMS = 1;

  /**
   * The feature id for the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__TYPE = 2;

  /**
   * The feature id for the '<em><b>Fired Events</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__FIRED_EVENTS = 3;

  /**
   * The number of structural features of the '<em>Action Signature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE_FEATURE_COUNT = 4;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ParameterImpl <em>Parameter</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ParameterImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getParameter()
   * @generated
   */
  int PARAMETER = 14;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETER__NAME = 0;

  /**
   * The feature id for the '<em><b>Parameter Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETER__PARAMETER_TYPE = 1;

  /**
   * The feature id for the '<em><b>Var Arg</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETER__VAR_ARG = 2;

  /**
   * The number of structural features of the '<em>Parameter</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETER_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.RequiredCapacityImpl <em>Required Capacity</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.RequiredCapacityImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getRequiredCapacity()
   * @generated
   */
  int REQUIRED_CAPACITY = 15;

  /**
   * The feature id for the '<em><b>Required Capacities</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int REQUIRED_CAPACITY__REQUIRED_CAPACITIES = AGENT_FEATURE_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Required Capacity</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int REQUIRED_CAPACITY_FEATURE_COUNT = AGENT_FEATURE_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SkillImpl <em>Skill</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SkillImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSkill()
   * @generated
   */
  int SKILL = 16;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL__NAME = ABSTRACT_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Implemented Capacities</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL__IMPLEMENTED_CAPACITIES = ABSTRACT_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL__FEATURES = ABSTRACT_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Skill</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL_FEATURE_COUNT = ABSTRACT_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SkillFeatureImpl <em>Skill Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SkillFeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSkillFeature()
   * @generated
   */
  int SKILL_FEATURE = 17;

  /**
   * The number of structural features of the '<em>Skill Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL_FEATURE_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ConstructorImpl <em>Constructor</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ConstructorImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getConstructor()
   * @generated
   */
  int CONSTRUCTOR = 18;

  /**
   * The feature id for the '<em><b>Params</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCTOR__PARAMS = EVENT_FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Body</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCTOR__BODY = EVENT_FEATURE_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Constructor</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCTOR_FEATURE_COUNT = EVENT_FEATURE_FEATURE_COUNT + 2;


  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Model <em>Model</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Model</em>'.
   * @see io.sarl.lang.sarl.Model
   * @generated
   */
  EClass getModel();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.Model#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.Model#getName()
   * @see #getModel()
   * @generated
   */
  EAttribute getModel_Name();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.Model#getImportSection <em>Import Section</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Import Section</em>'.
   * @see io.sarl.lang.sarl.Model#getImportSection()
   * @see #getModel()
   * @generated
   */
  EReference getModel_ImportSection();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.Model#getElements <em>Elements</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Elements</em>'.
   * @see io.sarl.lang.sarl.Model#getElements()
   * @see #getModel()
   * @generated
   */
  EReference getModel_Elements();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.AbstractElement <em>Abstract Element</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Abstract Element</em>'.
   * @see io.sarl.lang.sarl.AbstractElement
   * @generated
   */
  EClass getAbstractElement();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.AbstractElement#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.AbstractElement#getName()
   * @see #getAbstractElement()
   * @generated
   */
  EAttribute getAbstractElement_Name();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Event <em>Event</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Event</em>'.
   * @see io.sarl.lang.sarl.Event
   * @generated
   */
  EClass getEvent();

  /**
   * Returns the meta object for the reference '{@link io.sarl.lang.sarl.Event#getSuperType <em>Super Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference '<em>Super Type</em>'.
   * @see io.sarl.lang.sarl.Event#getSuperType()
   * @see #getEvent()
   * @generated
   */
  EReference getEvent_SuperType();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.Event#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.Event#getFeatures()
   * @see #getEvent()
   * @generated
   */
  EReference getEvent_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Capacity <em>Capacity</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Capacity</em>'.
   * @see io.sarl.lang.sarl.Capacity
   * @generated
   */
  EClass getCapacity();

  /**
   * Returns the meta object for the reference '{@link io.sarl.lang.sarl.Capacity#getSuperType <em>Super Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference '<em>Super Type</em>'.
   * @see io.sarl.lang.sarl.Capacity#getSuperType()
   * @see #getCapacity()
   * @generated
   */
  EReference getCapacity_SuperType();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.Capacity#getActions <em>Actions</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Actions</em>'.
   * @see io.sarl.lang.sarl.Capacity#getActions()
   * @see #getCapacity()
   * @generated
   */
  EReference getCapacity_Actions();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Agent <em>Agent</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Agent</em>'.
   * @see io.sarl.lang.sarl.Agent
   * @generated
   */
  EClass getAgent();

  /**
   * Returns the meta object for the reference '{@link io.sarl.lang.sarl.Agent#getSuperType <em>Super Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference '<em>Super Type</em>'.
   * @see io.sarl.lang.sarl.Agent#getSuperType()
   * @see #getAgent()
   * @generated
   */
  EReference getAgent_SuperType();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.Agent#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.Agent#getFeatures()
   * @see #getAgent()
   * @generated
   */
  EReference getAgent_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Behavior <em>Behavior</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Behavior</em>'.
   * @see io.sarl.lang.sarl.Behavior
   * @generated
   */
  EClass getBehavior();

  /**
   * Returns the meta object for the reference '{@link io.sarl.lang.sarl.Behavior#getSuperType <em>Super Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference '<em>Super Type</em>'.
   * @see io.sarl.lang.sarl.Behavior#getSuperType()
   * @see #getBehavior()
   * @generated
   */
  EReference getBehavior_SuperType();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.Behavior#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.Behavior#getFeatures()
   * @see #getBehavior()
   * @generated
   */
  EReference getBehavior_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.EventFeature <em>Event Feature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Event Feature</em>'.
   * @see io.sarl.lang.sarl.EventFeature
   * @generated
   */
  EClass getEventFeature();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.AgentFeature <em>Agent Feature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Agent Feature</em>'.
   * @see io.sarl.lang.sarl.AgentFeature
   * @generated
   */
  EClass getAgentFeature();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.BehaviorFeature <em>Behavior Feature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Behavior Feature</em>'.
   * @see io.sarl.lang.sarl.BehaviorFeature
   * @generated
   */
  EClass getBehaviorFeature();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Attribute <em>Attribute</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Attribute</em>'.
   * @see io.sarl.lang.sarl.Attribute
   * @generated
   */
  EClass getAttribute();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.Attribute#isWriteable <em>Writeable</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Writeable</em>'.
   * @see io.sarl.lang.sarl.Attribute#isWriteable()
   * @see #getAttribute()
   * @generated
   */
  EAttribute getAttribute_Writeable();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.Attribute#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.Attribute#getName()
   * @see #getAttribute()
   * @generated
   */
  EAttribute getAttribute_Name();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.Attribute#getType <em>Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Type</em>'.
   * @see io.sarl.lang.sarl.Attribute#getType()
   * @see #getAttribute()
   * @generated
   */
  EReference getAttribute_Type();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.Attribute#getInitialValue <em>Initial Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Initial Value</em>'.
   * @see io.sarl.lang.sarl.Attribute#getInitialValue()
   * @see #getAttribute()
   * @generated
   */
  EReference getAttribute_InitialValue();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.CapacityUses <em>Capacity Uses</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Capacity Uses</em>'.
   * @see io.sarl.lang.sarl.CapacityUses
   * @generated
   */
  EClass getCapacityUses();

  /**
   * Returns the meta object for the reference list '{@link io.sarl.lang.sarl.CapacityUses#getCapacitiesUsed <em>Capacities Used</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference list '<em>Capacities Used</em>'.
   * @see io.sarl.lang.sarl.CapacityUses#getCapacitiesUsed()
   * @see #getCapacityUses()
   * @generated
   */
  EReference getCapacityUses_CapacitiesUsed();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.BehaviorUnit <em>Behavior Unit</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Behavior Unit</em>'.
   * @see io.sarl.lang.sarl.BehaviorUnit
   * @generated
   */
  EClass getBehaviorUnit();

  /**
   * Returns the meta object for the reference '{@link io.sarl.lang.sarl.BehaviorUnit#getEvent <em>Event</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference '<em>Event</em>'.
   * @see io.sarl.lang.sarl.BehaviorUnit#getEvent()
   * @see #getBehaviorUnit()
   * @generated
   */
  EReference getBehaviorUnit_Event();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.BehaviorUnit#getGuard <em>Guard</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Guard</em>'.
   * @see io.sarl.lang.sarl.BehaviorUnit#getGuard()
   * @see #getBehaviorUnit()
   * @generated
   */
  EReference getBehaviorUnit_Guard();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.BehaviorUnit#getBody <em>Body</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Body</em>'.
   * @see io.sarl.lang.sarl.BehaviorUnit#getBody()
   * @see #getBehaviorUnit()
   * @generated
   */
  EReference getBehaviorUnit_Body();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Action <em>Action</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Action</em>'.
   * @see io.sarl.lang.sarl.Action
   * @generated
   */
  EClass getAction();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.Action#getSignature <em>Signature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Signature</em>'.
   * @see io.sarl.lang.sarl.Action#getSignature()
   * @see #getAction()
   * @generated
   */
  EReference getAction_Signature();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.Action#getBody <em>Body</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Body</em>'.
   * @see io.sarl.lang.sarl.Action#getBody()
   * @see #getAction()
   * @generated
   */
  EReference getAction_Body();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.ActionSignature <em>Action Signature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Action Signature</em>'.
   * @see io.sarl.lang.sarl.ActionSignature
   * @generated
   */
  EClass getActionSignature();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.ActionSignature#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.ActionSignature#getName()
   * @see #getActionSignature()
   * @generated
   */
  EAttribute getActionSignature_Name();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.ActionSignature#getParams <em>Params</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Params</em>'.
   * @see io.sarl.lang.sarl.ActionSignature#getParams()
   * @see #getActionSignature()
   * @generated
   */
  EReference getActionSignature_Params();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.ActionSignature#getType <em>Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Type</em>'.
   * @see io.sarl.lang.sarl.ActionSignature#getType()
   * @see #getActionSignature()
   * @generated
   */
  EReference getActionSignature_Type();

  /**
   * Returns the meta object for the reference list '{@link io.sarl.lang.sarl.ActionSignature#getFiredEvents <em>Fired Events</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference list '<em>Fired Events</em>'.
   * @see io.sarl.lang.sarl.ActionSignature#getFiredEvents()
   * @see #getActionSignature()
   * @generated
   */
  EReference getActionSignature_FiredEvents();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Parameter <em>Parameter</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Parameter</em>'.
   * @see io.sarl.lang.sarl.Parameter
   * @generated
   */
  EClass getParameter();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.Parameter#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.Parameter#getName()
   * @see #getParameter()
   * @generated
   */
  EAttribute getParameter_Name();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.Parameter#getParameterType <em>Parameter Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Parameter Type</em>'.
   * @see io.sarl.lang.sarl.Parameter#getParameterType()
   * @see #getParameter()
   * @generated
   */
  EReference getParameter_ParameterType();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.Parameter#isVarArg <em>Var Arg</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Var Arg</em>'.
   * @see io.sarl.lang.sarl.Parameter#isVarArg()
   * @see #getParameter()
   * @generated
   */
  EAttribute getParameter_VarArg();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.RequiredCapacity <em>Required Capacity</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Required Capacity</em>'.
   * @see io.sarl.lang.sarl.RequiredCapacity
   * @generated
   */
  EClass getRequiredCapacity();

  /**
   * Returns the meta object for the reference list '{@link io.sarl.lang.sarl.RequiredCapacity#getRequiredCapacities <em>Required Capacities</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference list '<em>Required Capacities</em>'.
   * @see io.sarl.lang.sarl.RequiredCapacity#getRequiredCapacities()
   * @see #getRequiredCapacity()
   * @generated
   */
  EReference getRequiredCapacity_RequiredCapacities();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Skill <em>Skill</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Skill</em>'.
   * @see io.sarl.lang.sarl.Skill
   * @generated
   */
  EClass getSkill();

  /**
   * Returns the meta object for the reference list '{@link io.sarl.lang.sarl.Skill#getImplementedCapacities <em>Implemented Capacities</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference list '<em>Implemented Capacities</em>'.
   * @see io.sarl.lang.sarl.Skill#getImplementedCapacities()
   * @see #getSkill()
   * @generated
   */
  EReference getSkill_ImplementedCapacities();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.Skill#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.Skill#getFeatures()
   * @see #getSkill()
   * @generated
   */
  EReference getSkill_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SkillFeature <em>Skill Feature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Skill Feature</em>'.
   * @see io.sarl.lang.sarl.SkillFeature
   * @generated
   */
  EClass getSkillFeature();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Constructor <em>Constructor</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Constructor</em>'.
   * @see io.sarl.lang.sarl.Constructor
   * @generated
   */
  EClass getConstructor();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.Constructor#getParams <em>Params</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Params</em>'.
   * @see io.sarl.lang.sarl.Constructor#getParams()
   * @see #getConstructor()
   * @generated
   */
  EReference getConstructor_Params();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.Constructor#getBody <em>Body</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Body</em>'.
   * @see io.sarl.lang.sarl.Constructor#getBody()
   * @see #getConstructor()
   * @generated
   */
  EReference getConstructor_Body();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  SarlFactory getSarlFactory();

  /**
   * <!-- begin-user-doc -->
   * Defines literals for the meta objects that represent
   * <ul>
   *   <li>each class,</li>
   *   <li>each feature of each class,</li>
   *   <li>each enum,</li>
   *   <li>and each data type</li>
   * </ul>
   * <!-- end-user-doc -->
   * @generated
   */
  interface Literals
  {
    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.ModelImpl <em>Model</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.ModelImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getModel()
     * @generated
     */
    EClass MODEL = eINSTANCE.getModel();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute MODEL__NAME = eINSTANCE.getModel_Name();

    /**
     * The meta object literal for the '<em><b>Import Section</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__IMPORT_SECTION = eINSTANCE.getModel_ImportSection();

    /**
     * The meta object literal for the '<em><b>Elements</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference MODEL__ELEMENTS = eINSTANCE.getModel_Elements();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.AbstractElementImpl <em>Abstract Element</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.AbstractElementImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAbstractElement()
     * @generated
     */
    EClass ABSTRACT_ELEMENT = eINSTANCE.getAbstractElement();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ABSTRACT_ELEMENT__NAME = eINSTANCE.getAbstractElement_Name();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.EventImpl <em>Event</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.EventImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getEvent()
     * @generated
     */
    EClass EVENT = eINSTANCE.getEvent();

    /**
     * The meta object literal for the '<em><b>Super Type</b></em>' reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EVENT__SUPER_TYPE = eINSTANCE.getEvent_SuperType();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EVENT__FEATURES = eINSTANCE.getEvent_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.CapacityImpl <em>Capacity</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.CapacityImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getCapacity()
     * @generated
     */
    EClass CAPACITY = eINSTANCE.getCapacity();

    /**
     * The meta object literal for the '<em><b>Super Type</b></em>' reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CAPACITY__SUPER_TYPE = eINSTANCE.getCapacity_SuperType();

    /**
     * The meta object literal for the '<em><b>Actions</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CAPACITY__ACTIONS = eINSTANCE.getCapacity_Actions();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.AgentImpl <em>Agent</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.AgentImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAgent()
     * @generated
     */
    EClass AGENT = eINSTANCE.getAgent();

    /**
     * The meta object literal for the '<em><b>Super Type</b></em>' reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference AGENT__SUPER_TYPE = eINSTANCE.getAgent_SuperType();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference AGENT__FEATURES = eINSTANCE.getAgent_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.BehaviorImpl <em>Behavior</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.BehaviorImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehavior()
     * @generated
     */
    EClass BEHAVIOR = eINSTANCE.getBehavior();

    /**
     * The meta object literal for the '<em><b>Super Type</b></em>' reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference BEHAVIOR__SUPER_TYPE = eINSTANCE.getBehavior_SuperType();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference BEHAVIOR__FEATURES = eINSTANCE.getBehavior_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.EventFeatureImpl <em>Event Feature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.EventFeatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getEventFeature()
     * @generated
     */
    EClass EVENT_FEATURE = eINSTANCE.getEventFeature();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.AgentFeatureImpl <em>Agent Feature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.AgentFeatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAgentFeature()
     * @generated
     */
    EClass AGENT_FEATURE = eINSTANCE.getAgentFeature();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.BehaviorFeatureImpl <em>Behavior Feature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.BehaviorFeatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehaviorFeature()
     * @generated
     */
    EClass BEHAVIOR_FEATURE = eINSTANCE.getBehaviorFeature();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.AttributeImpl <em>Attribute</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.AttributeImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAttribute()
     * @generated
     */
    EClass ATTRIBUTE = eINSTANCE.getAttribute();

    /**
     * The meta object literal for the '<em><b>Writeable</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ATTRIBUTE__WRITEABLE = eINSTANCE.getAttribute_Writeable();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ATTRIBUTE__NAME = eINSTANCE.getAttribute_Name();

    /**
     * The meta object literal for the '<em><b>Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ATTRIBUTE__TYPE = eINSTANCE.getAttribute_Type();

    /**
     * The meta object literal for the '<em><b>Initial Value</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ATTRIBUTE__INITIAL_VALUE = eINSTANCE.getAttribute_InitialValue();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.CapacityUsesImpl <em>Capacity Uses</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.CapacityUsesImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getCapacityUses()
     * @generated
     */
    EClass CAPACITY_USES = eINSTANCE.getCapacityUses();

    /**
     * The meta object literal for the '<em><b>Capacities Used</b></em>' reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CAPACITY_USES__CAPACITIES_USED = eINSTANCE.getCapacityUses_CapacitiesUsed();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.BehaviorUnitImpl <em>Behavior Unit</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.BehaviorUnitImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehaviorUnit()
     * @generated
     */
    EClass BEHAVIOR_UNIT = eINSTANCE.getBehaviorUnit();

    /**
     * The meta object literal for the '<em><b>Event</b></em>' reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference BEHAVIOR_UNIT__EVENT = eINSTANCE.getBehaviorUnit_Event();

    /**
     * The meta object literal for the '<em><b>Guard</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference BEHAVIOR_UNIT__GUARD = eINSTANCE.getBehaviorUnit_Guard();

    /**
     * The meta object literal for the '<em><b>Body</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference BEHAVIOR_UNIT__BODY = eINSTANCE.getBehaviorUnit_Body();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.ActionImpl <em>Action</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.ActionImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAction()
     * @generated
     */
    EClass ACTION = eINSTANCE.getAction();

    /**
     * The meta object literal for the '<em><b>Signature</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ACTION__SIGNATURE = eINSTANCE.getAction_Signature();

    /**
     * The meta object literal for the '<em><b>Body</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ACTION__BODY = eINSTANCE.getAction_Body();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.ActionSignatureImpl <em>Action Signature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.ActionSignatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getActionSignature()
     * @generated
     */
    EClass ACTION_SIGNATURE = eINSTANCE.getActionSignature();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ACTION_SIGNATURE__NAME = eINSTANCE.getActionSignature_Name();

    /**
     * The meta object literal for the '<em><b>Params</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ACTION_SIGNATURE__PARAMS = eINSTANCE.getActionSignature_Params();

    /**
     * The meta object literal for the '<em><b>Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ACTION_SIGNATURE__TYPE = eINSTANCE.getActionSignature_Type();

    /**
     * The meta object literal for the '<em><b>Fired Events</b></em>' reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference ACTION_SIGNATURE__FIRED_EVENTS = eINSTANCE.getActionSignature_FiredEvents();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.ParameterImpl <em>Parameter</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.ParameterImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getParameter()
     * @generated
     */
    EClass PARAMETER = eINSTANCE.getParameter();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute PARAMETER__NAME = eINSTANCE.getParameter_Name();

    /**
     * The meta object literal for the '<em><b>Parameter Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PARAMETER__PARAMETER_TYPE = eINSTANCE.getParameter_ParameterType();

    /**
     * The meta object literal for the '<em><b>Var Arg</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute PARAMETER__VAR_ARG = eINSTANCE.getParameter_VarArg();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.RequiredCapacityImpl <em>Required Capacity</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.RequiredCapacityImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getRequiredCapacity()
     * @generated
     */
    EClass REQUIRED_CAPACITY = eINSTANCE.getRequiredCapacity();

    /**
     * The meta object literal for the '<em><b>Required Capacities</b></em>' reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference REQUIRED_CAPACITY__REQUIRED_CAPACITIES = eINSTANCE.getRequiredCapacity_RequiredCapacities();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SkillImpl <em>Skill</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SkillImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSkill()
     * @generated
     */
    EClass SKILL = eINSTANCE.getSkill();

    /**
     * The meta object literal for the '<em><b>Implemented Capacities</b></em>' reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SKILL__IMPLEMENTED_CAPACITIES = eINSTANCE.getSkill_ImplementedCapacities();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SKILL__FEATURES = eINSTANCE.getSkill_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SkillFeatureImpl <em>Skill Feature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SkillFeatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSkillFeature()
     * @generated
     */
    EClass SKILL_FEATURE = eINSTANCE.getSkillFeature();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.ConstructorImpl <em>Constructor</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.ConstructorImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getConstructor()
     * @generated
     */
    EClass CONSTRUCTOR = eINSTANCE.getConstructor();

    /**
     * The meta object literal for the '<em><b>Params</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CONSTRUCTOR__PARAMS = eINSTANCE.getConstructor_Params();

    /**
     * The meta object literal for the '<em><b>Body</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CONSTRUCTOR__BODY = eINSTANCE.getConstructor_Body();

  }

} //SarlPackage
