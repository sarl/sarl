/**
 */
package io.sarl.lang.sarl.util;

import io.sarl.lang.sarl.*;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see io.sarl.lang.sarl.SarlPackage
 * @generated
 */
public class SarlSwitch<T> extends Switch<T>
{
  /**
   * The cached model package
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected static SarlPackage modelPackage;

  /**
   * Creates an instance of the switch.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlSwitch()
  {
    if (modelPackage == null)
    {
      modelPackage = SarlPackage.eINSTANCE;
    }
  }

  /**
   * Checks whether this is a switch for the given package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @parameter ePackage the package in question.
   * @return whether this is a switch for the given package.
   * @generated
   */
  @Override
  protected boolean isSwitchFor(EPackage ePackage)
  {
    return ePackage == modelPackage;
  }

  /**
   * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the first non-null result returned by a <code>caseXXX</code> call.
   * @generated
   */
  @Override
  protected T doSwitch(int classifierID, EObject theEObject)
  {
    switch (classifierID)
    {
      case SarlPackage.SARL_SCRIPT:
      {
        SarlScript sarlScript = (SarlScript)theEObject;
        T result = caseSarlScript(sarlScript);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.TOP_ELEMENT:
      {
        TopElement topElement = (TopElement)theEObject;
        T result = caseTopElement(topElement);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.NAMED_ELEMENT:
      {
        NamedElement namedElement = (NamedElement)theEObject;
        T result = caseNamedElement(namedElement);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.FEATURE:
      {
        Feature feature = (Feature)theEObject;
        T result = caseFeature(feature);
        if (result == null) result = caseEventFeature(feature);
        if (result == null) result = caseAgentFeature(feature);
        if (result == null) result = caseBehaviorFeature(feature);
        if (result == null) result = caseSkillFeature(feature);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.FEATURE_CONTAINER:
      {
        FeatureContainer featureContainer = (FeatureContainer)theEObject;
        T result = caseFeatureContainer(featureContainer);
        if (result == null) result = caseNamedElement(featureContainer);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.INHERITING_ELEMENT:
      {
        InheritingElement inheritingElement = (InheritingElement)theEObject;
        T result = caseInheritingElement(inheritingElement);
        if (result == null) result = caseTopElement(inheritingElement);
        if (result == null) result = caseFeatureContainer(inheritingElement);
        if (result == null) result = caseNamedElement(inheritingElement);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.IMPLEMENTING_ELEMENT:
      {
        ImplementingElement implementingElement = (ImplementingElement)theEObject;
        T result = caseImplementingElement(implementingElement);
        if (result == null) result = caseInheritingElement(implementingElement);
        if (result == null) result = caseTopElement(implementingElement);
        if (result == null) result = caseFeatureContainer(implementingElement);
        if (result == null) result = caseNamedElement(implementingElement);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.EVENT_FEATURE:
      {
        EventFeature eventFeature = (EventFeature)theEObject;
        T result = caseEventFeature(eventFeature);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.AGENT_FEATURE:
      {
        AgentFeature agentFeature = (AgentFeature)theEObject;
        T result = caseAgentFeature(agentFeature);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.BEHAVIOR_FEATURE:
      {
        BehaviorFeature behaviorFeature = (BehaviorFeature)theEObject;
        T result = caseBehaviorFeature(behaviorFeature);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.SKILL_FEATURE:
      {
        SkillFeature skillFeature = (SkillFeature)theEObject;
        T result = caseSkillFeature(skillFeature);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.PARAMETERIZED_FEATURE:
      {
        ParameterizedFeature parameterizedFeature = (ParameterizedFeature)theEObject;
        T result = caseParameterizedFeature(parameterizedFeature);
        if (result == null) result = caseFeature(parameterizedFeature);
        if (result == null) result = caseEventFeature(parameterizedFeature);
        if (result == null) result = caseAgentFeature(parameterizedFeature);
        if (result == null) result = caseBehaviorFeature(parameterizedFeature);
        if (result == null) result = caseSkillFeature(parameterizedFeature);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.FORMAL_PARAMETER:
      {
        FormalParameter formalParameter = (FormalParameter)theEObject;
        T result = caseFormalParameter(formalParameter);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.EVENT:
      {
        Event event = (Event)theEObject;
        T result = caseEvent(event);
        if (result == null) result = caseInheritingElement(event);
        if (result == null) result = caseTopElement(event);
        if (result == null) result = caseFeatureContainer(event);
        if (result == null) result = caseNamedElement(event);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.CAPACITY:
      {
        Capacity capacity = (Capacity)theEObject;
        T result = caseCapacity(capacity);
        if (result == null) result = caseInheritingElement(capacity);
        if (result == null) result = caseTopElement(capacity);
        if (result == null) result = caseFeatureContainer(capacity);
        if (result == null) result = caseNamedElement(capacity);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.AGENT:
      {
        Agent agent = (Agent)theEObject;
        T result = caseAgent(agent);
        if (result == null) result = caseInheritingElement(agent);
        if (result == null) result = caseTopElement(agent);
        if (result == null) result = caseFeatureContainer(agent);
        if (result == null) result = caseNamedElement(agent);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.BEHAVIOR:
      {
        Behavior behavior = (Behavior)theEObject;
        T result = caseBehavior(behavior);
        if (result == null) result = caseInheritingElement(behavior);
        if (result == null) result = caseTopElement(behavior);
        if (result == null) result = caseFeatureContainer(behavior);
        if (result == null) result = caseNamedElement(behavior);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.SKILL:
      {
        Skill skill = (Skill)theEObject;
        T result = caseSkill(skill);
        if (result == null) result = caseImplementingElement(skill);
        if (result == null) result = caseInheritingElement(skill);
        if (result == null) result = caseTopElement(skill);
        if (result == null) result = caseFeatureContainer(skill);
        if (result == null) result = caseNamedElement(skill);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.ATTRIBUTE:
      {
        Attribute attribute = (Attribute)theEObject;
        T result = caseAttribute(attribute);
        if (result == null) result = caseFeature(attribute);
        if (result == null) result = caseEventFeature(attribute);
        if (result == null) result = caseAgentFeature(attribute);
        if (result == null) result = caseBehaviorFeature(attribute);
        if (result == null) result = caseSkillFeature(attribute);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.CAPACITY_USES:
      {
        CapacityUses capacityUses = (CapacityUses)theEObject;
        T result = caseCapacityUses(capacityUses);
        if (result == null) result = caseFeature(capacityUses);
        if (result == null) result = caseEventFeature(capacityUses);
        if (result == null) result = caseAgentFeature(capacityUses);
        if (result == null) result = caseBehaviorFeature(capacityUses);
        if (result == null) result = caseSkillFeature(capacityUses);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.REQUIRED_CAPACITY:
      {
        RequiredCapacity requiredCapacity = (RequiredCapacity)theEObject;
        T result = caseRequiredCapacity(requiredCapacity);
        if (result == null) result = caseFeature(requiredCapacity);
        if (result == null) result = caseEventFeature(requiredCapacity);
        if (result == null) result = caseAgentFeature(requiredCapacity);
        if (result == null) result = caseBehaviorFeature(requiredCapacity);
        if (result == null) result = caseSkillFeature(requiredCapacity);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.BEHAVIOR_UNIT:
      {
        BehaviorUnit behaviorUnit = (BehaviorUnit)theEObject;
        T result = caseBehaviorUnit(behaviorUnit);
        if (result == null) result = caseFeature(behaviorUnit);
        if (result == null) result = caseEventFeature(behaviorUnit);
        if (result == null) result = caseAgentFeature(behaviorUnit);
        if (result == null) result = caseBehaviorFeature(behaviorUnit);
        if (result == null) result = caseSkillFeature(behaviorUnit);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.ACTION_SIGNATURE:
      {
        ActionSignature actionSignature = (ActionSignature)theEObject;
        T result = caseActionSignature(actionSignature);
        if (result == null) result = caseParameterizedFeature(actionSignature);
        if (result == null) result = caseFeature(actionSignature);
        if (result == null) result = caseEventFeature(actionSignature);
        if (result == null) result = caseAgentFeature(actionSignature);
        if (result == null) result = caseBehaviorFeature(actionSignature);
        if (result == null) result = caseSkillFeature(actionSignature);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.ACTION:
      {
        Action action = (Action)theEObject;
        T result = caseAction(action);
        if (result == null) result = caseParameterizedFeature(action);
        if (result == null) result = caseFeature(action);
        if (result == null) result = caseEventFeature(action);
        if (result == null) result = caseAgentFeature(action);
        if (result == null) result = caseBehaviorFeature(action);
        if (result == null) result = caseSkillFeature(action);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case SarlPackage.CONSTRUCTOR:
      {
        Constructor constructor = (Constructor)theEObject;
        T result = caseConstructor(constructor);
        if (result == null) result = caseParameterizedFeature(constructor);
        if (result == null) result = caseFeature(constructor);
        if (result == null) result = caseEventFeature(constructor);
        if (result == null) result = caseAgentFeature(constructor);
        if (result == null) result = caseBehaviorFeature(constructor);
        if (result == null) result = caseSkillFeature(constructor);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      default: return defaultCase(theEObject);
    }
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Script</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Script</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSarlScript(SarlScript object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Top Element</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Top Element</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseTopElement(TopElement object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Named Element</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Named Element</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseNamedElement(NamedElement object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Feature</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Feature</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseFeature(Feature object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Feature Container</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Feature Container</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseFeatureContainer(FeatureContainer object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Inheriting Element</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Inheriting Element</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseInheritingElement(InheritingElement object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Implementing Element</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Implementing Element</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseImplementingElement(ImplementingElement object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Event Feature</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Event Feature</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseEventFeature(EventFeature object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Agent Feature</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Agent Feature</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAgentFeature(AgentFeature object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Behavior Feature</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Behavior Feature</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseBehaviorFeature(BehaviorFeature object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Skill Feature</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Skill Feature</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSkillFeature(SkillFeature object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Parameterized Feature</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Parameterized Feature</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseParameterizedFeature(ParameterizedFeature object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Formal Parameter</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Formal Parameter</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseFormalParameter(FormalParameter object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Event</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Event</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseEvent(Event object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Capacity</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Capacity</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseCapacity(Capacity object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Agent</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Agent</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAgent(Agent object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Behavior</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Behavior</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseBehavior(Behavior object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Skill</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Skill</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSkill(Skill object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Attribute</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Attribute</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAttribute(Attribute object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Capacity Uses</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Capacity Uses</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseCapacityUses(CapacityUses object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Required Capacity</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Required Capacity</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseRequiredCapacity(RequiredCapacity object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Behavior Unit</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Behavior Unit</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseBehaviorUnit(BehaviorUnit object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Action Signature</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Action Signature</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseActionSignature(ActionSignature object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Action</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Action</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAction(Action object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Constructor</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Constructor</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseConstructor(Constructor object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch, but this is the last case anyway.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject)
   * @generated
   */
  @Override
  public T defaultCase(EObject object)
  {
    return null;
  }

} //SarlSwitch
