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
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlScriptImpl <em>Script</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlScriptImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlScript()
   * @generated
   */
  int SARL_SCRIPT = 0;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SCRIPT__NAME = 0;

  /**
   * The feature id for the '<em><b>Import Section</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SCRIPT__IMPORT_SECTION = 1;

  /**
   * The feature id for the '<em><b>Elements</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SCRIPT__ELEMENTS = 2;

  /**
   * The number of structural features of the '<em>Script</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SCRIPT_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.TopElementImpl <em>Top Element</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.TopElementImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getTopElement()
   * @generated
   */
  int TOP_ELEMENT = 1;

  /**
   * The number of structural features of the '<em>Top Element</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int TOP_ELEMENT_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.NamedElementImpl <em>Named Element</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.NamedElementImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getNamedElement()
   * @generated
   */
  int NAMED_ELEMENT = 2;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NAMED_ELEMENT__NAME = 0;

  /**
   * The number of structural features of the '<em>Named Element</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int NAMED_ELEMENT_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.EventFeatureImpl <em>Event Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.EventFeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getEventFeature()
   * @generated
   */
  int EVENT_FEATURE = 7;

  /**
   * The number of structural features of the '<em>Event Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_FEATURE_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.FeatureImpl <em>Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.FeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getFeature()
   * @generated
   */
  int FEATURE = 3;

  /**
   * The number of structural features of the '<em>Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FEATURE_FEATURE_COUNT = EVENT_FEATURE_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.FeatureContainerImpl <em>Feature Container</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.FeatureContainerImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getFeatureContainer()
   * @generated
   */
  int FEATURE_CONTAINER = 4;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FEATURE_CONTAINER__NAME = NAMED_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FEATURE_CONTAINER__FEATURES = NAMED_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Feature Container</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FEATURE_CONTAINER_FEATURE_COUNT = NAMED_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.InheritingElementImpl <em>Inheriting Element</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.InheritingElementImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getInheritingElement()
   * @generated
   */
  int INHERITING_ELEMENT = 5;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INHERITING_ELEMENT__NAME = TOP_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INHERITING_ELEMENT__FEATURES = TOP_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INHERITING_ELEMENT__SUPER_TYPES = TOP_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Inheriting Element</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INHERITING_ELEMENT_FEATURE_COUNT = TOP_ELEMENT_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ImplementingElementImpl <em>Implementing Element</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ImplementingElementImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getImplementingElement()
   * @generated
   */
  int IMPLEMENTING_ELEMENT = 6;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IMPLEMENTING_ELEMENT__NAME = TOP_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IMPLEMENTING_ELEMENT__FEATURES = TOP_ELEMENT_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IMPLEMENTING_ELEMENT__SUPER_TYPES = TOP_ELEMENT_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Implemented Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES = TOP_ELEMENT_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Implementing Element</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int IMPLEMENTING_ELEMENT_FEATURE_COUNT = TOP_ELEMENT_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.AgentFeatureImpl <em>Agent Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.AgentFeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAgentFeature()
   * @generated
   */
  int AGENT_FEATURE = 8;

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
  int BEHAVIOR_FEATURE = 9;

  /**
   * The number of structural features of the '<em>Behavior Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_FEATURE_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SkillFeatureImpl <em>Skill Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SkillFeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSkillFeature()
   * @generated
   */
  int SKILL_FEATURE = 10;

  /**
   * The number of structural features of the '<em>Skill Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL_FEATURE_FEATURE_COUNT = 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ParameterizedFeatureImpl <em>Parameterized Feature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ParameterizedFeatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getParameterizedFeature()
   * @generated
   */
  int PARAMETERIZED_FEATURE = 11;

  /**
   * The feature id for the '<em><b>Params</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETERIZED_FEATURE__PARAMS = FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Varargs</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETERIZED_FEATURE__VARARGS = FEATURE_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Parameterized Feature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETERIZED_FEATURE_FEATURE_COUNT = FEATURE_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.FormalParameterImpl <em>Formal Parameter</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.FormalParameterImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getFormalParameter()
   * @generated
   */
  int FORMAL_PARAMETER = 12;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORMAL_PARAMETER__NAME = 0;

  /**
   * The feature id for the '<em><b>Parameter Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORMAL_PARAMETER__PARAMETER_TYPE = 1;

  /**
   * The feature id for the '<em><b>Default Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORMAL_PARAMETER__DEFAULT_VALUE = 2;

  /**
   * The number of structural features of the '<em>Formal Parameter</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORMAL_PARAMETER_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.EventImpl <em>Event</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.EventImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getEvent()
   * @generated
   */
  int EVENT = 13;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT__NAME = INHERITING_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT__FEATURES = INHERITING_ELEMENT__FEATURES;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT__SUPER_TYPES = INHERITING_ELEMENT__SUPER_TYPES;

  /**
   * The number of structural features of the '<em>Event</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_FEATURE_COUNT = INHERITING_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.CapacityImpl <em>Capacity</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.CapacityImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getCapacity()
   * @generated
   */
  int CAPACITY = 14;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY__NAME = INHERITING_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY__FEATURES = INHERITING_ELEMENT__FEATURES;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY__SUPER_TYPES = INHERITING_ELEMENT__SUPER_TYPES;

  /**
   * The number of structural features of the '<em>Capacity</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY_FEATURE_COUNT = INHERITING_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.AgentImpl <em>Agent</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.AgentImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAgent()
   * @generated
   */
  int AGENT = 15;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT__NAME = INHERITING_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT__FEATURES = INHERITING_ELEMENT__FEATURES;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT__SUPER_TYPES = INHERITING_ELEMENT__SUPER_TYPES;

  /**
   * The number of structural features of the '<em>Agent</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int AGENT_FEATURE_COUNT = INHERITING_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.BehaviorImpl <em>Behavior</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.BehaviorImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehavior()
   * @generated
   */
  int BEHAVIOR = 16;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR__NAME = INHERITING_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR__FEATURES = INHERITING_ELEMENT__FEATURES;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR__SUPER_TYPES = INHERITING_ELEMENT__SUPER_TYPES;

  /**
   * The number of structural features of the '<em>Behavior</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_FEATURE_COUNT = INHERITING_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SkillImpl <em>Skill</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SkillImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSkill()
   * @generated
   */
  int SKILL = 17;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL__NAME = IMPLEMENTING_ELEMENT__NAME;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL__FEATURES = IMPLEMENTING_ELEMENT__FEATURES;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL__SUPER_TYPES = IMPLEMENTING_ELEMENT__SUPER_TYPES;

  /**
   * The feature id for the '<em><b>Implemented Types</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL__IMPLEMENTED_TYPES = IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES;

  /**
   * The number of structural features of the '<em>Skill</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SKILL_FEATURE_COUNT = IMPLEMENTING_ELEMENT_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.AttributeImpl <em>Attribute</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.AttributeImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAttribute()
   * @generated
   */
  int ATTRIBUTE = 18;

  /**
   * The feature id for the '<em><b>Writeable</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE__WRITEABLE = FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE__NAME = FEATURE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE__TYPE = FEATURE_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Initial Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE__INITIAL_VALUE = FEATURE_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Attribute</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTRIBUTE_FEATURE_COUNT = FEATURE_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.CapacityUsesImpl <em>Capacity Uses</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.CapacityUsesImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getCapacityUses()
   * @generated
   */
  int CAPACITY_USES = 19;

  /**
   * The feature id for the '<em><b>Capacities Used</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY_USES__CAPACITIES_USED = FEATURE_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Capacity Uses</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CAPACITY_USES_FEATURE_COUNT = FEATURE_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.BehaviorUnitImpl <em>Behavior Unit</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.BehaviorUnitImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehaviorUnit()
   * @generated
   */
  int BEHAVIOR_UNIT = 20;

  /**
   * The feature id for the '<em><b>Event</b></em>' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_UNIT__EVENT = FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Guard</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_UNIT__GUARD = FEATURE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Body</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_UNIT__BODY = FEATURE_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Behavior Unit</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BEHAVIOR_UNIT_FEATURE_COUNT = FEATURE_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.RequiredCapacityImpl <em>Required Capacity</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.RequiredCapacityImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getRequiredCapacity()
   * @generated
   */
  int REQUIRED_CAPACITY = 21;

  /**
   * The feature id for the '<em><b>Required Capacities</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int REQUIRED_CAPACITY__REQUIRED_CAPACITIES = FEATURE_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Required Capacity</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int REQUIRED_CAPACITY_FEATURE_COUNT = FEATURE_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ActionImpl <em>Action</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ActionImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getAction()
   * @generated
   */
  int ACTION = 22;

  /**
   * The feature id for the '<em><b>Signature</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION__SIGNATURE = FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Body</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION__BODY = FEATURE_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Action</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_FEATURE_COUNT = FEATURE_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ConstructorImpl <em>Constructor</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ConstructorImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getConstructor()
   * @generated
   */
  int CONSTRUCTOR = 23;

  /**
   * The feature id for the '<em><b>Params</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCTOR__PARAMS = PARAMETERIZED_FEATURE__PARAMS;

  /**
   * The feature id for the '<em><b>Varargs</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCTOR__VARARGS = PARAMETERIZED_FEATURE__VARARGS;

  /**
   * The feature id for the '<em><b>Body</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCTOR__BODY = PARAMETERIZED_FEATURE_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Constructor</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONSTRUCTOR_FEATURE_COUNT = PARAMETERIZED_FEATURE_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.ActionSignatureImpl <em>Action Signature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.ActionSignatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getActionSignature()
   * @generated
   */
  int ACTION_SIGNATURE = 24;

  /**
   * The feature id for the '<em><b>Params</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__PARAMS = PARAMETERIZED_FEATURE__PARAMS;

  /**
   * The feature id for the '<em><b>Varargs</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__VARARGS = PARAMETERIZED_FEATURE__VARARGS;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__NAME = PARAMETERIZED_FEATURE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__TYPE = PARAMETERIZED_FEATURE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Fired Events</b></em>' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE__FIRED_EVENTS = PARAMETERIZED_FEATURE_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Action Signature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_SIGNATURE_FEATURE_COUNT = PARAMETERIZED_FEATURE_FEATURE_COUNT + 3;


  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlScript <em>Script</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Script</em>'.
   * @see io.sarl.lang.sarl.SarlScript
   * @generated
   */
  EClass getSarlScript();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.SarlScript#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.SarlScript#getName()
   * @see #getSarlScript()
   * @generated
   */
  EAttribute getSarlScript_Name();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlScript#getImportSection <em>Import Section</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Import Section</em>'.
   * @see io.sarl.lang.sarl.SarlScript#getImportSection()
   * @see #getSarlScript()
   * @generated
   */
  EReference getSarlScript_ImportSection();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlScript#getElements <em>Elements</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Elements</em>'.
   * @see io.sarl.lang.sarl.SarlScript#getElements()
   * @see #getSarlScript()
   * @generated
   */
  EReference getSarlScript_Elements();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.TopElement <em>Top Element</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Top Element</em>'.
   * @see io.sarl.lang.sarl.TopElement
   * @generated
   */
  EClass getTopElement();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.NamedElement <em>Named Element</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Named Element</em>'.
   * @see io.sarl.lang.sarl.NamedElement
   * @generated
   */
  EClass getNamedElement();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.NamedElement#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.NamedElement#getName()
   * @see #getNamedElement()
   * @generated
   */
  EAttribute getNamedElement_Name();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Feature <em>Feature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Feature</em>'.
   * @see io.sarl.lang.sarl.Feature
   * @generated
   */
  EClass getFeature();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.FeatureContainer <em>Feature Container</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Feature Container</em>'.
   * @see io.sarl.lang.sarl.FeatureContainer
   * @generated
   */
  EClass getFeatureContainer();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.FeatureContainer#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.FeatureContainer#getFeatures()
   * @see #getFeatureContainer()
   * @generated
   */
  EReference getFeatureContainer_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.InheritingElement <em>Inheriting Element</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Inheriting Element</em>'.
   * @see io.sarl.lang.sarl.InheritingElement
   * @generated
   */
  EClass getInheritingElement();

  /**
   * Returns the meta object for the reference list '{@link io.sarl.lang.sarl.InheritingElement#getSuperTypes <em>Super Types</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference list '<em>Super Types</em>'.
   * @see io.sarl.lang.sarl.InheritingElement#getSuperTypes()
   * @see #getInheritingElement()
   * @generated
   */
  EReference getInheritingElement_SuperTypes();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.ImplementingElement <em>Implementing Element</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Implementing Element</em>'.
   * @see io.sarl.lang.sarl.ImplementingElement
   * @generated
   */
  EClass getImplementingElement();

  /**
   * Returns the meta object for the reference list '{@link io.sarl.lang.sarl.ImplementingElement#getImplementedTypes <em>Implemented Types</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the reference list '<em>Implemented Types</em>'.
   * @see io.sarl.lang.sarl.ImplementingElement#getImplementedTypes()
   * @see #getImplementingElement()
   * @generated
   */
  EReference getImplementingElement_ImplementedTypes();

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
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SkillFeature <em>Skill Feature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Skill Feature</em>'.
   * @see io.sarl.lang.sarl.SkillFeature
   * @generated
   */
  EClass getSkillFeature();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.ParameterizedFeature <em>Parameterized Feature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Parameterized Feature</em>'.
   * @see io.sarl.lang.sarl.ParameterizedFeature
   * @generated
   */
  EClass getParameterizedFeature();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.ParameterizedFeature#getParams <em>Params</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Params</em>'.
   * @see io.sarl.lang.sarl.ParameterizedFeature#getParams()
   * @see #getParameterizedFeature()
   * @generated
   */
  EReference getParameterizedFeature_Params();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.ParameterizedFeature#isVarargs <em>Varargs</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Varargs</em>'.
   * @see io.sarl.lang.sarl.ParameterizedFeature#isVarargs()
   * @see #getParameterizedFeature()
   * @generated
   */
  EAttribute getParameterizedFeature_Varargs();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.FormalParameter <em>Formal Parameter</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Formal Parameter</em>'.
   * @see io.sarl.lang.sarl.FormalParameter
   * @generated
   */
  EClass getFormalParameter();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.FormalParameter#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.FormalParameter#getName()
   * @see #getFormalParameter()
   * @generated
   */
  EAttribute getFormalParameter_Name();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.FormalParameter#getParameterType <em>Parameter Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Parameter Type</em>'.
   * @see io.sarl.lang.sarl.FormalParameter#getParameterType()
   * @see #getFormalParameter()
   * @generated
   */
  EReference getFormalParameter_ParameterType();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.FormalParameter#getDefaultValue <em>Default Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Default Value</em>'.
   * @see io.sarl.lang.sarl.FormalParameter#getDefaultValue()
   * @see #getFormalParameter()
   * @generated
   */
  EReference getFormalParameter_DefaultValue();

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
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Capacity <em>Capacity</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Capacity</em>'.
   * @see io.sarl.lang.sarl.Capacity
   * @generated
   */
  EClass getCapacity();

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
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Behavior <em>Behavior</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Behavior</em>'.
   * @see io.sarl.lang.sarl.Behavior
   * @generated
   */
  EClass getBehavior();

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
   * Returns the meta object for class '{@link io.sarl.lang.sarl.Constructor <em>Constructor</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Constructor</em>'.
   * @see io.sarl.lang.sarl.Constructor
   * @generated
   */
  EClass getConstructor();

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
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlScriptImpl <em>Script</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlScriptImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlScript()
     * @generated
     */
    EClass SARL_SCRIPT = eINSTANCE.getSarlScript();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SARL_SCRIPT__NAME = eINSTANCE.getSarlScript_Name();

    /**
     * The meta object literal for the '<em><b>Import Section</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_SCRIPT__IMPORT_SECTION = eINSTANCE.getSarlScript_ImportSection();

    /**
     * The meta object literal for the '<em><b>Elements</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_SCRIPT__ELEMENTS = eINSTANCE.getSarlScript_Elements();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.TopElementImpl <em>Top Element</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.TopElementImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getTopElement()
     * @generated
     */
    EClass TOP_ELEMENT = eINSTANCE.getTopElement();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.NamedElementImpl <em>Named Element</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.NamedElementImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getNamedElement()
     * @generated
     */
    EClass NAMED_ELEMENT = eINSTANCE.getNamedElement();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute NAMED_ELEMENT__NAME = eINSTANCE.getNamedElement_Name();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.FeatureImpl <em>Feature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.FeatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getFeature()
     * @generated
     */
    EClass FEATURE = eINSTANCE.getFeature();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.FeatureContainerImpl <em>Feature Container</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.FeatureContainerImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getFeatureContainer()
     * @generated
     */
    EClass FEATURE_CONTAINER = eINSTANCE.getFeatureContainer();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference FEATURE_CONTAINER__FEATURES = eINSTANCE.getFeatureContainer_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.InheritingElementImpl <em>Inheriting Element</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.InheritingElementImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getInheritingElement()
     * @generated
     */
    EClass INHERITING_ELEMENT = eINSTANCE.getInheritingElement();

    /**
     * The meta object literal for the '<em><b>Super Types</b></em>' reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference INHERITING_ELEMENT__SUPER_TYPES = eINSTANCE.getInheritingElement_SuperTypes();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.ImplementingElementImpl <em>Implementing Element</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.ImplementingElementImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getImplementingElement()
     * @generated
     */
    EClass IMPLEMENTING_ELEMENT = eINSTANCE.getImplementingElement();

    /**
     * The meta object literal for the '<em><b>Implemented Types</b></em>' reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES = eINSTANCE.getImplementingElement_ImplementedTypes();

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
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SkillFeatureImpl <em>Skill Feature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SkillFeatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSkillFeature()
     * @generated
     */
    EClass SKILL_FEATURE = eINSTANCE.getSkillFeature();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.ParameterizedFeatureImpl <em>Parameterized Feature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.ParameterizedFeatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getParameterizedFeature()
     * @generated
     */
    EClass PARAMETERIZED_FEATURE = eINSTANCE.getParameterizedFeature();

    /**
     * The meta object literal for the '<em><b>Params</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PARAMETERIZED_FEATURE__PARAMS = eINSTANCE.getParameterizedFeature_Params();

    /**
     * The meta object literal for the '<em><b>Varargs</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute PARAMETERIZED_FEATURE__VARARGS = eINSTANCE.getParameterizedFeature_Varargs();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.FormalParameterImpl <em>Formal Parameter</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.FormalParameterImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getFormalParameter()
     * @generated
     */
    EClass FORMAL_PARAMETER = eINSTANCE.getFormalParameter();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORMAL_PARAMETER__NAME = eINSTANCE.getFormalParameter_Name();

    /**
     * The meta object literal for the '<em><b>Parameter Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference FORMAL_PARAMETER__PARAMETER_TYPE = eINSTANCE.getFormalParameter_ParameterType();

    /**
     * The meta object literal for the '<em><b>Default Value</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference FORMAL_PARAMETER__DEFAULT_VALUE = eINSTANCE.getFormalParameter_DefaultValue();

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
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.CapacityImpl <em>Capacity</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.CapacityImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getCapacity()
     * @generated
     */
    EClass CAPACITY = eINSTANCE.getCapacity();

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
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.BehaviorImpl <em>Behavior</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.BehaviorImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getBehavior()
     * @generated
     */
    EClass BEHAVIOR = eINSTANCE.getBehavior();

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
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.ConstructorImpl <em>Constructor</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.ConstructorImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getConstructor()
     * @generated
     */
    EClass CONSTRUCTOR = eINSTANCE.getConstructor();

    /**
     * The meta object literal for the '<em><b>Body</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CONSTRUCTOR__BODY = eINSTANCE.getConstructor_Body();

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

  }

} //SarlPackage
