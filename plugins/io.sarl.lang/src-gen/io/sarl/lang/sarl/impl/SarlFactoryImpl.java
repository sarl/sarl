/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class SarlFactoryImpl extends EFactoryImpl implements SarlFactory
{
  /**
   * Creates the default factory implementation.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static SarlFactory init()
  {
    try
    {
      SarlFactory theSarlFactory = (SarlFactory)EPackage.Registry.INSTANCE.getEFactory(SarlPackage.eNS_URI);
      if (theSarlFactory != null)
      {
        return theSarlFactory;
      }
    }
    catch (Exception exception)
    {
      EcorePlugin.INSTANCE.log(exception);
    }
    return new SarlFactoryImpl();
  }

  /**
   * Creates an instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlFactoryImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public EObject create(EClass eClass)
  {
    switch (eClass.getClassifierID())
    {
      case SarlPackage.SARL_SCRIPT: return createSarlScript();
      case SarlPackage.TOP_ELEMENT: return createTopElement();
      case SarlPackage.NAMED_ELEMENT: return createNamedElement();
      case SarlPackage.FEATURE: return createFeature();
      case SarlPackage.FEATURE_CONTAINER: return createFeatureContainer();
      case SarlPackage.INHERITING_ELEMENT: return createInheritingElement();
      case SarlPackage.IMPLEMENTING_ELEMENT: return createImplementingElement();
      case SarlPackage.EVENT_FEATURE: return createEventFeature();
      case SarlPackage.AGENT_FEATURE: return createAgentFeature();
      case SarlPackage.BEHAVIOR_FEATURE: return createBehaviorFeature();
      case SarlPackage.SKILL_FEATURE: return createSkillFeature();
      case SarlPackage.PARAMETERIZED_FEATURE: return createParameterizedFeature();
      case SarlPackage.FORMAL_PARAMETER: return createFormalParameter();
      case SarlPackage.EVENT: return createEvent();
      case SarlPackage.CAPACITY: return createCapacity();
      case SarlPackage.AGENT: return createAgent();
      case SarlPackage.BEHAVIOR: return createBehavior();
      case SarlPackage.SKILL: return createSkill();
      case SarlPackage.ATTRIBUTE: return createAttribute();
      case SarlPackage.CAPACITY_USES: return createCapacityUses();
      case SarlPackage.REQUIRED_CAPACITY: return createRequiredCapacity();
      case SarlPackage.BEHAVIOR_UNIT: return createBehaviorUnit();
      case SarlPackage.ACTION_SIGNATURE: return createActionSignature();
      case SarlPackage.ACTION: return createAction();
      case SarlPackage.CONSTRUCTOR: return createConstructor();
      default:
        throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlScript createSarlScript()
  {
    SarlScriptImpl sarlScript = new SarlScriptImpl();
    return sarlScript;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public TopElement createTopElement()
  {
    TopElementImpl topElement = new TopElementImpl();
    return topElement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NamedElement createNamedElement()
  {
    NamedElementImpl namedElement = new NamedElementImpl();
    return namedElement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Feature createFeature()
  {
    FeatureImpl feature = new FeatureImpl();
    return feature;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public FeatureContainer createFeatureContainer()
  {
    FeatureContainerImpl featureContainer = new FeatureContainerImpl();
    return featureContainer;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InheritingElement createInheritingElement()
  {
    InheritingElementImpl inheritingElement = new InheritingElementImpl();
    return inheritingElement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ImplementingElement createImplementingElement()
  {
    ImplementingElementImpl implementingElement = new ImplementingElementImpl();
    return implementingElement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EventFeature createEventFeature()
  {
    EventFeatureImpl eventFeature = new EventFeatureImpl();
    return eventFeature;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AgentFeature createAgentFeature()
  {
    AgentFeatureImpl agentFeature = new AgentFeatureImpl();
    return agentFeature;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public BehaviorFeature createBehaviorFeature()
  {
    BehaviorFeatureImpl behaviorFeature = new BehaviorFeatureImpl();
    return behaviorFeature;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SkillFeature createSkillFeature()
  {
    SkillFeatureImpl skillFeature = new SkillFeatureImpl();
    return skillFeature;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ParameterizedFeature createParameterizedFeature()
  {
    ParameterizedFeatureImpl parameterizedFeature = new ParameterizedFeatureImpl();
    return parameterizedFeature;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public FormalParameter createFormalParameter()
  {
    FormalParameterImpl formalParameter = new FormalParameterImpl();
    return formalParameter;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Event createEvent()
  {
    EventImpl event = new EventImpl();
    return event;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Capacity createCapacity()
  {
    CapacityImpl capacity = new CapacityImpl();
    return capacity;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Agent createAgent()
  {
    AgentImpl agent = new AgentImpl();
    return agent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Behavior createBehavior()
  {
    BehaviorImpl behavior = new BehaviorImpl();
    return behavior;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Skill createSkill()
  {
    SkillImpl skill = new SkillImpl();
    return skill;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Attribute createAttribute()
  {
    AttributeImpl attribute = new AttributeImpl();
    return attribute;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public CapacityUses createCapacityUses()
  {
    CapacityUsesImpl capacityUses = new CapacityUsesImpl();
    return capacityUses;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public RequiredCapacity createRequiredCapacity()
  {
    RequiredCapacityImpl requiredCapacity = new RequiredCapacityImpl();
    return requiredCapacity;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public BehaviorUnit createBehaviorUnit()
  {
    BehaviorUnitImpl behaviorUnit = new BehaviorUnitImpl();
    return behaviorUnit;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ActionSignature createActionSignature()
  {
    ActionSignatureImpl actionSignature = new ActionSignatureImpl();
    return actionSignature;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Action createAction()
  {
    ActionImpl action = new ActionImpl();
    return action;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Constructor createConstructor()
  {
    ConstructorImpl constructor = new ConstructorImpl();
    return constructor;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlPackage getSarlPackage()
  {
    return (SarlPackage)getEPackage();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @deprecated
   * @generated
   */
  @Deprecated
  public static SarlPackage getPackage()
  {
    return SarlPackage.eINSTANCE;
  }

} //SarlFactoryImpl
