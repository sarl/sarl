/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.RequiredCapacity;
import io.sarl.lang.sarl.SarlPackage;

import java.util.Collection;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Required Capacity</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.RequiredCapacityImpl#getRequiredCapacities <em>Required Capacities</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RequiredCapacityImpl extends AgentFeatureImpl implements RequiredCapacity
{
  /**
   * The cached value of the '{@link #getRequiredCapacities() <em>Required Capacities</em>}' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRequiredCapacities()
   * @generated
   * @ordered
   */
  protected EList<Capacity> requiredCapacities;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected RequiredCapacityImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return SarlPackage.Literals.REQUIRED_CAPACITY;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Capacity> getRequiredCapacities()
  {
    if (requiredCapacities == null)
    {
      requiredCapacities = new EObjectResolvingEList<Capacity>(Capacity.class, this, SarlPackage.REQUIRED_CAPACITY__REQUIRED_CAPACITIES);
    }
    return requiredCapacities;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case SarlPackage.REQUIRED_CAPACITY__REQUIRED_CAPACITIES:
        return getRequiredCapacities();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SarlPackage.REQUIRED_CAPACITY__REQUIRED_CAPACITIES:
        getRequiredCapacities().clear();
        getRequiredCapacities().addAll((Collection<? extends Capacity>)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.REQUIRED_CAPACITY__REQUIRED_CAPACITIES:
        getRequiredCapacities().clear();
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.REQUIRED_CAPACITY__REQUIRED_CAPACITIES:
        return requiredCapacities != null && !requiredCapacities.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //RequiredCapacityImpl
