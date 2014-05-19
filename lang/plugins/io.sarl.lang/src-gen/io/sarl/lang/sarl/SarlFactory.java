/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see io.sarl.lang.sarl.SarlPackage
 * @generated
 */
public interface SarlFactory extends EFactory
{
  /**
   * The singleton instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  SarlFactory eINSTANCE = io.sarl.lang.sarl.impl.SarlFactoryImpl.init();

  /**
   * Returns a new object of class '<em>Script</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Script</em>'.
   * @generated
   */
  SarlScript createSarlScript();

  /**
   * Returns a new object of class '<em>Top Element</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Top Element</em>'.
   * @generated
   */
  TopElement createTopElement();

  /**
   * Returns a new object of class '<em>Named Element</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Named Element</em>'.
   * @generated
   */
  NamedElement createNamedElement();

  /**
   * Returns a new object of class '<em>Feature</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Feature</em>'.
   * @generated
   */
  Feature createFeature();

  /**
   * Returns a new object of class '<em>Feature Container</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Feature Container</em>'.
   * @generated
   */
  FeatureContainer createFeatureContainer();

  /**
   * Returns a new object of class '<em>Inheriting Element</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Inheriting Element</em>'.
   * @generated
   */
  InheritingElement createInheritingElement();

  /**
   * Returns a new object of class '<em>Implementing Element</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Implementing Element</em>'.
   * @generated
   */
  ImplementingElement createImplementingElement();

  /**
   * Returns a new object of class '<em>Event Feature</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Event Feature</em>'.
   * @generated
   */
  EventFeature createEventFeature();

  /**
   * Returns a new object of class '<em>Agent Feature</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Agent Feature</em>'.
   * @generated
   */
  AgentFeature createAgentFeature();

  /**
   * Returns a new object of class '<em>Behavior Feature</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Behavior Feature</em>'.
   * @generated
   */
  BehaviorFeature createBehaviorFeature();

  /**
   * Returns a new object of class '<em>Skill Feature</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Skill Feature</em>'.
   * @generated
   */
  SkillFeature createSkillFeature();

  /**
   * Returns a new object of class '<em>Formal Parameter</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Formal Parameter</em>'.
   * @generated
   */
  FormalParameter createFormalParameter();

  /**
   * Returns a new object of class '<em>Event</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Event</em>'.
   * @generated
   */
  Event createEvent();

  /**
   * Returns a new object of class '<em>Capacity</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Capacity</em>'.
   * @generated
   */
  Capacity createCapacity();

  /**
   * Returns a new object of class '<em>Agent</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Agent</em>'.
   * @generated
   */
  Agent createAgent();

  /**
   * Returns a new object of class '<em>Behavior</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Behavior</em>'.
   * @generated
   */
  Behavior createBehavior();

  /**
   * Returns a new object of class '<em>Skill</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Skill</em>'.
   * @generated
   */
  Skill createSkill();

  /**
   * Returns a new object of class '<em>Attribute</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Attribute</em>'.
   * @generated
   */
  Attribute createAttribute();

  /**
   * Returns a new object of class '<em>Capacity Uses</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Capacity Uses</em>'.
   * @generated
   */
  CapacityUses createCapacityUses();

  /**
   * Returns a new object of class '<em>Behavior Unit</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Behavior Unit</em>'.
   * @generated
   */
  BehaviorUnit createBehaviorUnit();

  /**
   * Returns a new object of class '<em>Required Capacity</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Required Capacity</em>'.
   * @generated
   */
  RequiredCapacity createRequiredCapacity();

  /**
   * Returns a new object of class '<em>Constructor</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Constructor</em>'.
   * @generated
   */
  Constructor createConstructor();

  /**
   * Returns a new object of class '<em>Action</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Action</em>'.
   * @generated
   */
  Action createAction();

  /**
   * Returns a new object of class '<em>Action Signature</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Action Signature</em>'.
   * @generated
   */
  ActionSignature createActionSignature();

  /**
   * Returns the package supported by this factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the package supported by this factory.
   * @generated
   */
  SarlPackage getSarlPackage();

} //SarlFactory
