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
    return new SarlFactoryImplCustom();
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
      case SarlPackage.SARL_FIELD: return createSarlField();
      case SarlPackage.SARL_ACTION: return createSarlAction();
      case SarlPackage.SARL_CONSTRUCTOR: return createSarlConstructor();
      case SarlPackage.SARL_BEHAVIOR_UNIT: return createSarlBehaviorUnit();
      case SarlPackage.SARL_CAPACITY_USES: return createSarlCapacityUses();
      case SarlPackage.SARL_REQUIRED_CAPACITY: return createSarlRequiredCapacity();
      case SarlPackage.SARL_CLASS: return createSarlClass();
      case SarlPackage.SARL_INTERFACE: return createSarlInterface();
      case SarlPackage.SARL_ENUMERATION: return createSarlEnumeration();
      case SarlPackage.SARL_ANNOTATION_TYPE: return createSarlAnnotationType();
      case SarlPackage.SARL_EVENT: return createSarlEvent();
      case SarlPackage.SARL_AGENT: return createSarlAgent();
      case SarlPackage.SARL_CAPACITY: return createSarlCapacity();
      case SarlPackage.SARL_BEHAVIOR: return createSarlBehavior();
      case SarlPackage.SARL_SKILL: return createSarlSkill();
      case SarlPackage.SARL_FORMAL_PARAMETER: return createSarlFormalParameter();
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
  public SarlField createSarlField()
  {
    SarlFieldImpl sarlField = new SarlFieldImpl();
    return sarlField;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlAction createSarlAction()
  {
    SarlActionImpl sarlAction = new SarlActionImpl();
    return sarlAction;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlConstructor createSarlConstructor()
  {
    SarlConstructorImpl sarlConstructor = new SarlConstructorImpl();
    return sarlConstructor;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlBehaviorUnit createSarlBehaviorUnit()
  {
    SarlBehaviorUnitImpl sarlBehaviorUnit = new SarlBehaviorUnitImpl();
    return sarlBehaviorUnit;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlCapacityUses createSarlCapacityUses()
  {
    SarlCapacityUsesImpl sarlCapacityUses = new SarlCapacityUsesImpl();
    return sarlCapacityUses;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlRequiredCapacity createSarlRequiredCapacity()
  {
    SarlRequiredCapacityImpl sarlRequiredCapacity = new SarlRequiredCapacityImpl();
    return sarlRequiredCapacity;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlClass createSarlClass()
  {
    SarlClassImpl sarlClass = new SarlClassImpl();
    return sarlClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlInterface createSarlInterface()
  {
    SarlInterfaceImpl sarlInterface = new SarlInterfaceImpl();
    return sarlInterface;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlEnumeration createSarlEnumeration()
  {
    SarlEnumerationImpl sarlEnumeration = new SarlEnumerationImpl();
    return sarlEnumeration;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlAnnotationType createSarlAnnotationType()
  {
    SarlAnnotationTypeImpl sarlAnnotationType = new SarlAnnotationTypeImpl();
    return sarlAnnotationType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlEvent createSarlEvent()
  {
    SarlEventImpl sarlEvent = new SarlEventImpl();
    return sarlEvent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlAgent createSarlAgent()
  {
    SarlAgentImpl sarlAgent = new SarlAgentImpl();
    return sarlAgent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlCapacity createSarlCapacity()
  {
    SarlCapacityImpl sarlCapacity = new SarlCapacityImpl();
    return sarlCapacity;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlBehavior createSarlBehavior()
  {
    SarlBehaviorImpl sarlBehavior = new SarlBehaviorImpl();
    return sarlBehavior;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlSkill createSarlSkill()
  {
    SarlSkillImpl sarlSkill = new SarlSkillImpl();
    return sarlSkill;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlFormalParameter createSarlFormalParameter()
  {
    SarlFormalParameterImpl sarlFormalParameter = new SarlFormalParameterImpl();
    return sarlFormalParameter;
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
