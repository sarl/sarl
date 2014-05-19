/**
 */
package io.sarl.lang.sarl.util;

import io.sarl.lang.sarl.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see io.sarl.lang.sarl.SarlPackage
 * @generated
 */
public class SarlAdapterFactory extends AdapterFactoryImpl
{
  /**
   * The cached model package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected static SarlPackage modelPackage;

  /**
   * Creates an instance of the adapter factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SarlAdapterFactory()
  {
    if (modelPackage == null)
    {
      modelPackage = SarlPackage.eINSTANCE;
    }
  }

  /**
   * Returns whether this factory is applicable for the type of the object.
   * <!-- begin-user-doc -->
   * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
   * <!-- end-user-doc -->
   * @return whether this factory is applicable for the type of the object.
   * @generated
   */
  @Override
  public boolean isFactoryForType(Object object)
  {
    if (object == modelPackage)
    {
      return true;
    }
    if (object instanceof EObject)
    {
      return ((EObject)object).eClass().getEPackage() == modelPackage;
    }
    return false;
  }

  /**
   * The switch that delegates to the <code>createXXX</code> methods.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SarlSwitch<Adapter> modelSwitch =
    new SarlSwitch<Adapter>()
    {
      @Override
      public Adapter caseSarlScript(SarlScript object)
      {
        return createSarlScriptAdapter();
      }
      @Override
      public Adapter caseTopElement(TopElement object)
      {
        return createTopElementAdapter();
      }
      @Override
      public Adapter caseNamedElement(NamedElement object)
      {
        return createNamedElementAdapter();
      }
      @Override
      public Adapter caseFeature(Feature object)
      {
        return createFeatureAdapter();
      }
      @Override
      public Adapter caseFeatureContainer(FeatureContainer object)
      {
        return createFeatureContainerAdapter();
      }
      @Override
      public Adapter caseInheritingElement(InheritingElement object)
      {
        return createInheritingElementAdapter();
      }
      @Override
      public Adapter caseImplementingElement(ImplementingElement object)
      {
        return createImplementingElementAdapter();
      }
      @Override
      public Adapter caseEventFeature(EventFeature object)
      {
        return createEventFeatureAdapter();
      }
      @Override
      public Adapter caseAgentFeature(AgentFeature object)
      {
        return createAgentFeatureAdapter();
      }
      @Override
      public Adapter caseBehaviorFeature(BehaviorFeature object)
      {
        return createBehaviorFeatureAdapter();
      }
      @Override
      public Adapter caseSkillFeature(SkillFeature object)
      {
        return createSkillFeatureAdapter();
      }
      @Override
      public Adapter caseFormalParameter(FormalParameter object)
      {
        return createFormalParameterAdapter();
      }
      @Override
      public Adapter caseEvent(Event object)
      {
        return createEventAdapter();
      }
      @Override
      public Adapter caseCapacity(Capacity object)
      {
        return createCapacityAdapter();
      }
      @Override
      public Adapter caseAgent(Agent object)
      {
        return createAgentAdapter();
      }
      @Override
      public Adapter caseBehavior(Behavior object)
      {
        return createBehaviorAdapter();
      }
      @Override
      public Adapter caseSkill(Skill object)
      {
        return createSkillAdapter();
      }
      @Override
      public Adapter caseAttribute(Attribute object)
      {
        return createAttributeAdapter();
      }
      @Override
      public Adapter caseCapacityUses(CapacityUses object)
      {
        return createCapacityUsesAdapter();
      }
      @Override
      public Adapter caseBehaviorUnit(BehaviorUnit object)
      {
        return createBehaviorUnitAdapter();
      }
      @Override
      public Adapter caseRequiredCapacity(RequiredCapacity object)
      {
        return createRequiredCapacityAdapter();
      }
      @Override
      public Adapter caseConstructor(Constructor object)
      {
        return createConstructorAdapter();
      }
      @Override
      public Adapter caseAction(Action object)
      {
        return createActionAdapter();
      }
      @Override
      public Adapter caseActionSignature(ActionSignature object)
      {
        return createActionSignatureAdapter();
      }
      @Override
      public Adapter defaultCase(EObject object)
      {
        return createEObjectAdapter();
      }
    };

  /**
   * Creates an adapter for the <code>target</code>.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param target the object to adapt.
   * @return the adapter for the <code>target</code>.
   * @generated
   */
  @Override
  public Adapter createAdapter(Notifier target)
  {
    return modelSwitch.doSwitch((EObject)target);
  }


  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SarlScript <em>Script</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.SarlScript
   * @generated
   */
  public Adapter createSarlScriptAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.TopElement <em>Top Element</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.TopElement
   * @generated
   */
  public Adapter createTopElementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.NamedElement <em>Named Element</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.NamedElement
   * @generated
   */
  public Adapter createNamedElementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Feature <em>Feature</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Feature
   * @generated
   */
  public Adapter createFeatureAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.FeatureContainer <em>Feature Container</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.FeatureContainer
   * @generated
   */
  public Adapter createFeatureContainerAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.InheritingElement <em>Inheriting Element</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.InheritingElement
   * @generated
   */
  public Adapter createInheritingElementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.ImplementingElement <em>Implementing Element</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.ImplementingElement
   * @generated
   */
  public Adapter createImplementingElementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.EventFeature <em>Event Feature</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.EventFeature
   * @generated
   */
  public Adapter createEventFeatureAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.AgentFeature <em>Agent Feature</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.AgentFeature
   * @generated
   */
  public Adapter createAgentFeatureAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.BehaviorFeature <em>Behavior Feature</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.BehaviorFeature
   * @generated
   */
  public Adapter createBehaviorFeatureAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.SkillFeature <em>Skill Feature</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.SkillFeature
   * @generated
   */
  public Adapter createSkillFeatureAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.FormalParameter <em>Formal Parameter</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.FormalParameter
   * @generated
   */
  public Adapter createFormalParameterAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Event <em>Event</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Event
   * @generated
   */
  public Adapter createEventAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Capacity <em>Capacity</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Capacity
   * @generated
   */
  public Adapter createCapacityAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Agent <em>Agent</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Agent
   * @generated
   */
  public Adapter createAgentAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Behavior <em>Behavior</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Behavior
   * @generated
   */
  public Adapter createBehaviorAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Skill <em>Skill</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Skill
   * @generated
   */
  public Adapter createSkillAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Attribute <em>Attribute</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Attribute
   * @generated
   */
  public Adapter createAttributeAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.CapacityUses <em>Capacity Uses</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.CapacityUses
   * @generated
   */
  public Adapter createCapacityUsesAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.BehaviorUnit <em>Behavior Unit</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.BehaviorUnit
   * @generated
   */
  public Adapter createBehaviorUnitAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.RequiredCapacity <em>Required Capacity</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.RequiredCapacity
   * @generated
   */
  public Adapter createRequiredCapacityAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Constructor <em>Constructor</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Constructor
   * @generated
   */
  public Adapter createConstructorAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.Action <em>Action</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.Action
   * @generated
   */
  public Adapter createActionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link io.sarl.lang.sarl.ActionSignature <em>Action Signature</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see io.sarl.lang.sarl.ActionSignature
   * @generated
   */
  public Adapter createActionSignatureAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for the default case.
   * <!-- begin-user-doc -->
   * This default implementation returns null.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @generated
   */
  public Adapter createEObjectAdapter()
  {
    return null;
  }

} //SarlAdapterFactory
