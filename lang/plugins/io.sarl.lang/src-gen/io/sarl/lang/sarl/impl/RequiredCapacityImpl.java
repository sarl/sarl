/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.RequiredCapacity;
import io.sarl.lang.sarl.SarlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

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
public class RequiredCapacityImpl extends FeatureImpl implements RequiredCapacity
{
  /**
   * The cached value of the '{@link #getRequiredCapacities() <em>Required Capacities</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRequiredCapacities()
   * @generated
   * @ordered
   */
  protected EList<JvmParameterizedTypeReference> requiredCapacities;

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
  public EList<JvmParameterizedTypeReference> getRequiredCapacities()
  {
    if (requiredCapacities == null)
    {
      requiredCapacities = new EObjectContainmentEList<JvmParameterizedTypeReference>(JvmParameterizedTypeReference.class, this, SarlPackage.REQUIRED_CAPACITY__REQUIRED_CAPACITIES);
    }
    return requiredCapacities;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case SarlPackage.REQUIRED_CAPACITY__REQUIRED_CAPACITIES:
        return ((InternalEList<?>)getRequiredCapacities()).basicRemove(otherEnd, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
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
        getRequiredCapacities().addAll((Collection<? extends JvmParameterizedTypeReference>)newValue);
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
